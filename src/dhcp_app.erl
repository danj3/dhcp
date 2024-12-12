-module(dhcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Handlers} = application:get_env(dhcp, handlers),
    Pid = dhcp_sup:start_link(),
    dhcp_server:register_handlers(Handlers),
    Pid.

stop(_State) ->
    ok.
