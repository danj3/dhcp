-module(dhcp_handler).
-include("dhcp.hrl").

-export([init/1, discover/4, request/4, release/3]).

-ignore_xref([behaviour_info/1]).
-type state() :: any().
-type config() :: any().

-callback match(Msg::dhcp:package(), Config::any()) -> true | false.

%% When handler is registered, this is called from within the
%% dhcp_server process, allowing for shared set up that will
%% be passed to each handler process spawned in response to
%% a dhcp request
-callback registered(Config::any()) -> InitConfig::any().

% HandlerModule allows the handler to return itself or
% another module, but specifically allows the
% handler init to separate the Config
-callback init(InitConfig::config()) ->
    {ok, state()}.

-callback discover(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, {offer, IP, Netmask, ReplyPkg}, state()} |
    {ok, {offer, IP, Netmask, GW, ReplyPkg}, state()} |
    {ok, {offer, IP, Netmask, [GW], ReplyPkg}, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp:package(),
                        RequestPkg::dhcp:package(),
                        IP::dhcp:ip(),
                        Netmask::dhcp:ip(),
                        GW::dhcp:ip().


-callback request(ReplyPkg, RequestPkg, state()) ->
    {ok, state()} |
    {ok, {ack, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, GW, ReplyPkg}, state()} |
    {ok, {ack, IP, Netmask, [GW], ReplyPkg}, state()} |
    {ok, {nck, ReplyPkg}, state()} |
    {ok, ReplyPkg, state()} |
    {error, any()} when ReplyPkg::dhcp:package(),
                        RequestPkg::dhcp:package(),
                        IP::dhcp:ip(),
                        Netmask::dhcp:ip(),
                        GW::dhcp:ip().

-callback release(RequestPkg, state()) ->
    {ok, state()} |
    {error, any()} when RequestPkg::dhcp:package().


init({Handler, Config}) ->
    Handler:init(Config).

request(Handler, ReplyPkg, RequestPkg, State) ->
    Handler:request(ReplyPkg, RequestPkg, State).

discover(Handler, ReplyPkg, RequestPkg, State) ->
    Handler:discover(ReplyPkg, RequestPkg, State).

release(Handler, RequestPkg, State) ->
    Handler:release(RequestPkg, State).
