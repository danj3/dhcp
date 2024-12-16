%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dhcp_server).

-behaviour(gen_server).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0, register_handler/2, register_handlers/1]).
-ignore_xref([start_link/0, register_handler/2, register_handlers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TBL, dhcp_sessions).

-record(state, {socket, handlers = []}).

-type session_id() :: {mac(), non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_handler(Handler::atom(), Config::any()) -> ok | {error, _}.
register_handler(Handler, Config) when is_atom(Handler)->
    gen_server:call(?SERVER, {register, Handler, Config}).

-spec register_handlers([{Handler::atom(), Config::any()} | _]) -> ok | {error, _}.
register_handlers([{Handler, Config} | Rest]) when is_atom(Handler)->
    ok = gen_server:call(?SERVER, {register, Handler, Config}),
    register_handlers(Rest);
register_handlers([]) ->
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Socket} = gen_udp:open(67, [binary,
                                     inet,
                                     {broadcast, true},
                                     {reuseaddr, true},
                                     {active, true}
                                    ]),
    ets:new(?TBL, [bag, {read_concurrency, true}, named_table]),

    {ok, #state{socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({register, H, Config}, _From, State = #state{handlers = Hs}) ->
    case H:registered(Config) of
        {ok, IConfig} ->
          {reply, ok, State#state{ handlers = [{H, IConfig} | Hs]}};
        {error, Why} ->
            {reply, {error, Why}, State}
    end;

handle_call(socket, _From, State = #state{socket = Socket}) ->
    {reply, Socket, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({udp, _Socket, IP, 68, Packet}, State) ->
    lager:info("dhcp udp rec from ~p~n", [IP]),
    case dhcp_package:decode(Packet) of
        {ok, D} ->
            case lookup_session(D#dhcp_package.chaddr, D#dhcp_package.xid) of
                {Id, nil} ->
                    do_start_child(D, Id, State);
                {_Id, Pid} ->
                    gen_statem:cast(Pid, D)
            end;
        E ->
            lager:warning("Decoding failed: ~p (~p)", [E, Packet])
    end,
    {noreply, State};

handle_info(Info, State) ->
    lager:info("dhcp input ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec lookup_session(mac(), non_neg_integer()) -> {session_id(), pid() | nil}.
lookup_session(Chaddr, Xid) ->
    Id = {Chaddr, Xid},
    case {ets:lookup(?TBL, Id), discover} of
        {[], discover} ->
            {Id, nil};
        {[{_Id, Pid}], _} ->
            case process_info(Pid) of
                undefined ->
                    {Id, nil};
                _ ->
                    {Id, Pid}
            end
    end.

do_start_child(Pkg, ID, _State = #state{socket=Socket, handlers=Handlers}) ->
    case dhcp_match:match(Pkg, Handlers) of
        {ok, H} ->
            {ok, Pid1} = supervisor:start_child(dhcp_fsm_sup, [Socket, H]),
            gen_statem:cast(Pid1, Pkg),
            ets:insert(?TBL, {ID, Pid1});
        undefined ->
            lager:info("discarded ID=~p MT=~p package=~p",
                       [Pkg#dhcp_package.xid,
                        Pkg#dhcp_package.message_type,
                        Pkg]),
            ok
    end.
