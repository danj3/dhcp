%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 20 May 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dhcp_fsm).

-behaviour(gen_statem).

-include("dhcp.hrl").

%% API
-export([start_link/2]).

%% callbacks
-export([
         init/1,
         terminate/3,
         code_change/4]).

-export([callback_mode/0
        ]).

-export([
         initial/3,
         offered/3,
         bound/3
        ]).

-ignore_xref([initial/3, offered/3, bound/3, start_link/2]).

-define(SERVER, ?MODULE).

-define(S(S), (1000*S)).

-record(state, {xid = 0,
                last = {0, 0, 0},
                handler,
                handler_state,
                socket::inet:socket(),
                server_identifier::integer(),
                yiaddr,
                initial_timeout = 10,
                offer_timeout = 10,
                request_timeout = 30
               }).
-type handler() :: {atom(), map()}.
-type socket() :: inet:socket().
-type init_args() :: [socket() | [handler()]].

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(socket(), handler()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Socket, Handler) ->
    gen_statem:start_link(?MODULE, [Socket, Handler], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3, 4] or
%% gen_fsm:start_link/[3, 4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------

-spec init(init_args()) -> {ok, initial, #state{}, integer()}.
init([Socket, {HandlerModule, _} = Handler]) ->
    {ok, {IpAddress, _Port}} = inet:sockname(Socket),
    Ip32 = dhcp:tpl_to_ip(IpAddress),
    {ok, HandlerState} = dhcp_handler:init(Handler),
    {ok, Ti} = application:get_env(initial_timeout),
    {ok, To} = application:get_env(offer_timeout),
    {ok, Tr} = application:get_env(request_timeout),
    {ok, initial, #state{handler = HandlerModule,
                         handler_state = HandlerState,
                         socket = Socket,
                         server_identifier = Ip32,
                         initial_timeout = Ti,
                         offer_timeout = To,
                         request_timeout = Tr,
                         last = current_time()}, ?S(10)}.

callback_mode() ->
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
initial(timeout, _Content, State) ->
    lager:warning("[DHCP] timeout in initial."),
    {stop, normal, State};

initial(cast,
        Pkg = #dhcp_package{xid = XId, message_type = discover},
        State = #state{initial_timeout = Ti,
                       offer_timeout = To}) ->
    case delegate(discover, Pkg,
                  [{ip_address_lease_time, 3000}], State) of
        {ok, RPkg, State1} ->
            YiAddr = dhcp_package:get_yiaddr(RPkg),
            {next_state, offered, State1#state{xid = XId, yiaddr = YiAddr},
             ?S(To)};
        {ok, State1} ->
            {next_state, initial, State1, ?S(Ti)};
        E ->
            lager:warning("[DHCP] callback module returned ~p", [E]),
            {next_state, initial, State#state{xid = XId}, ?S(Ti)}
    end;

initial(cast,
        Pkg = #dhcp_package{message_type = request},
        State = #state{initial_timeout = Ti}) ->
    case delegate(request, Pkg,
                  [{ip_address_lease_time, 3000}], State) of
        {ok, RPkg, State1} ->
            Timeout = dhcp_package:get_option(ip_address_lease_time, RPkg),
            YiAddr = dhcp_package:get_yiaddr(RPkg),
            {next_state, bound, State1#state{yiaddr = YiAddr}, ?S(Timeout)};
        {ok, State1} ->
            {next_state, initial, State1, ?S(Ti)};
        E ->
            lager:warning("[DHCP] callback module returned ~p", [E]),
            {next_state, initial, State, ?S(Ti)}
    end.


offered(timeout, _Content, State) ->
    lager:warning("[DHCP] offer timed out."),
    {stop, normal, State};

offered(cast,
        Pkg = #dhcp_package{xid = _XId1, message_type = request},
        State = #state{xid = _XId2, server_identifier = Si, yiaddr = YiAddr,
                       offer_timeout = To}) ->
    case {dhcp_package:get_option(dhcp_server_identifier, Pkg),
          dhcp_package:get_option(requested_ip_address, Pkg)} of
        {Si, YiAddr} ->
            case delegate(request, Pkg, State) of
                {ok, RPkg, State1} ->
                    ReqLeaseT = dhcp_package:get_option(ip_address_lease_time,
                                                        3000, RPkg),
                    Timeout = dhcp_package:get_option(ip_address_lease_time,
                                                      ReqLeaseT,
                                                      Pkg),
                    {next_state, bound, State#state{last=current_time(),
                                                    handler_state = State1},
                     ?S(Timeout)};
                {ok, State1} ->
                    {next_state, offered, State#state{last=current_time(),
                                                      handler_state = State1},
                     ?S(To)};
                _ ->
                    {stop, normal, State}
            end;
        {Si, _} ->
            lager:error("[DHCP] invalid ip requested!"),
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end;

offered(cast,
        #dhcp_package{xid = _XId1, message_type = decline},
        State = #state{xid = _XId2}) ->
    {stop, normal, State};

offered(cast,
        #dhcp_package{}, State = #state{offer_timeout = To}) ->
    {next_state, offered, State, ?S(To)}.


bound(timeout, _Content, State) ->
    lager:warning("[DHCP] timeout in bound."),
    {stop, normal, State};

bound(cast,
      Pkg = #dhcp_package{xid = _XId1, message_type = release},
      State = #state{xid = _XId2, handler = M, handler_state = S}) ->
    case dhcp_handler:release(M, Pkg, S) of
        {ok, S1} ->
            {stop, normal, State#state{handler_state = S1}};
        _ ->
            {stop, normal, State}
    end;

bound(cast,
      Pkg = #dhcp_package{xid = _XId1, message_type = request},
      State = #state{xid = _XId2, server_identifier = Si, yiaddr = YiAddr}) ->
    case {dhcp_package:get_option(dhcp_server_identifier, Pkg),
          dhcp_package:get_option(requested_ip_address, Pkg)} of
        {Si, YiAddr} ->
            case delegate(request, Pkg, State) of
                {ok, RPkg, State1} ->
                    Timeout = dhcp_package:get_option(ip_address_lease_time,
                                                      RPkg),
                    {next_state, bound, State#state{last=current_time(),
                                                    handler_state = State1},
                     Timeout};
                _ ->
                    {stop, normal, State}
            end;
        {Si, _} ->
            lager:error("[DHCP] invalid ip requested!"),
            {stop, normal, State};
        _ ->
            {stop, normal, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
delegate(F, Pkg, State) ->
    delegate(F, Pkg, [], State).

delegate(F, Pkg, Opts, State = #state{handler = M}) ->
    RPkg = lists:foldl(fun(O, P) ->
                               dhcp_package:set_option(O, P)
                       end,
                       dhcp_package:clone(Pkg),
                       [{dhcp_server_identifier, State#state.server_identifier}
                        | Opts]),
    RPkg1 = dhcp_package:set_op(reply, RPkg),
    case  dhcp_handler:F(M, RPkg1, Pkg, State#state.handler_state) of
        {ok, Reply, S1} ->
            R = case Reply of
                    #dhcp_package{} ->
                        Reply;
                    {Op, R0} when Op =:= ack orelse Op =:= nck ->
                        dhcp_package:set_message_type(Op, R0);
                    {Op, IP, Mask, R0} when Op =:= offer orelse Op =:= ack ->
                        set_res(Op, IP, Mask, R0);
                    {Op, IP, Mask, GWInfo, R0} when Op =:= offer orelse
                                                    Op =:= ack ->
                        set_res(Op, IP, Mask, GWInfo, R0)
                end,
            case dhcp_package:valid_reply(R) of
                true ->
                    Dst = reply_addr(R),
                    {ok, Bin} = dhcp_package:encode(R),
                    gen_udp:send(State#state.socket, Dst, 68, Bin),
                    {ok, R, State#state{handler_state = S1,
                                        last=current_time()}};
                false ->
                    ErrStr = "[DHCP] invalid reply package for ~p:~p -> ~p",
                    lager:error(ErrStr, [M, F, R])
            end;
        {ok, S1} ->
            {ok, State#state{handler_state = S1, last=current_time()}};
        E ->
            E
    end.

set_res(Op, IP, Mask, Gw, R0) when is_integer(Gw) ->
    dhcp_package:set_option({router_address, [Gw]}, set_res(Op, IP, Mask, R0));
set_res(Op, IP, Mask, Gws, R0) when is_list(Gws) ->
    dhcp_package:set_option({router_address, Gws}, set_res(Op, IP, Mask, R0)).

set_res(Op, IP, Mask, R0) ->
    R1 = dhcp_package:set_yiaddr(IP, R0),
    R2 = dhcp_package:set_option({subnet_mask, Mask}, R1),
    dhcp_package:set_message_type(Op, R2).


reply_addr(#dhcp_package{flags = [broadcast]}) ->
    {255, 255, 255, 255};
reply_addr(#dhcp_package{message_type = offer}) ->
    {255, 255, 255, 255};
reply_addr(#dhcp_package{message_type = nck}) ->
    {255, 255, 255, 255};
reply_addr(#dhcp_package{ciaddr = 0}) ->
    {255, 255, 255, 255};
reply_addr(#dhcp_package{ciaddr = Addr}) ->
    <<A:8, B:8, C:8, D:8>> = <<Addr:32>>,
    {A, B, C, D}.

-ifdef(time_correction).
current_time() -> erlang:timestamp().
-else.
current_time() -> erlang:now().
-endif.
