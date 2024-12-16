-module(dhcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Handlers = handlers(),
    Pid = dhcp_sup:start_link(),
    dhcp_server:register_handlers(Handlers),
    Pid.

stop(_State) ->
    ok.

handlers() ->
    {ok, Handlers = [_|_]} = application:get_env(dhcp, handlers),
    true = lists:all(fun ({Atom, _Config}) when is_atom(Atom) ->
                            true;
                         (_) ->
                             false
                     end, Handlers),
    Handlers.
