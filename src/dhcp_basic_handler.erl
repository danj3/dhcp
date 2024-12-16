-module(dhcp_basic_handler).
-behaviour(dhcp_handler).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
  match/2, registered/1, init/1,
  discover/3, request/3, release/2]).

-record(lease, {
  chaddr, % client hardware address
  ciaddr, % client allocated ip address
  sname
  }).
-define(IP_START, {192, 168, 0, 10}).
-define(IP_END, {192, 168, 0, 100}).
-define(NETMASK, {255, 255, 255, 0}).
-define(LEASE_TABLE, dhcp_basic_leases).

-record(config, {
                 lease_start::ip_tpl(),
                 range_start::ip(),
                 range_end::ip(),
                 netmask::ip_tpl(),
                 leases::ets:table()
                }).

-record(state, {
                chaddr::dhcp:mac() | undefined,
                ciaddr::ip_tpl() | undefined,
                xid::dhcp:int32() | undefined,
                config::#config{}
               }).

match(_Pkg, _Config) -> false.

-spec registered(map()) -> {ok, #config{}}.
registered(Config) ->
    RangeStartTpl=maps:get(range_start, Config, ?IP_START),
    RangeStart = dhcp:tpl_to_ip(RangeStartTpl),
    RangeEnd = dhcp:tpl_to_ip(maps:get(range_end, Config, ?IP_END)),
    Netmask = maps:get(netmask, Config, ?NETMASK),
    Table = ets:new(?LEASE_TABLE, [set, {read_concurrency, true}, named_table]),
    ets:insert(Table, {next_lease, RangeStart-1}),
    {ok, #config{
            lease_start=RangeStartTpl,
            range_start=RangeStart,
            range_end = RangeEnd,
            netmask = Netmask,
            leases = Table
           }}.

-spec init(#config{}) -> {ok, #state{}}.
init(Config) ->
    {ok, #state{config=Config}}.

discover(ReplyPkg,
         #dhcp_package{chaddr = Chaddr} = _RequestPkg,
         #state{config = #config{leases= Table, netmask = Netmask}} = State) ->

    Ip = ets:update_counter(Table, next_lease, 1),
    %% Add upper boundary check
    IpTpl = dhcp:ip_to_tpl(Ip),
    case ets:insert_new(Table, {Chaddr, #lease{ciaddr=Ip, chaddr=Chaddr}}) of
        true ->
            {ok, {offer, IpTpl, Netmask, ReplyPkg}, 
             State#state{ciaddr = IpTpl, chaddr = Chaddr}};
        false ->
            {error, lease_exhaustion}
    end.

request(ReplyPkg,
        #dhcp_package{chaddr = Chaddr} = _RequestPkg,
        #state{config = #config{leases= Table, netmask = Netmask}} = State) ->

    [{_,#lease{ciaddr=Ip}}] = ets:lookup(Table, Chaddr),
    %IpTpl = dhcp:ip_to_tpl(Ip),
    {ok, {ack, Ip, dhcp:tpl_to_ip(Netmask), ReplyPkg}, State}.

release(_RequestPkg, State) ->
    {ok, State}.

-ifdef(TEST).

registered_test() ->
    {ok, Config = #config{
            range_start=RangeStart,
            leases=Table
           }} = registered(#{}),
    RangeStartIp=dhcp:tpl_to_ip(?IP_START),
    ?assertEqual(RangeStart, RangeStartIp),
    ?assertEqual([{next_lease,RangeStart-1}], ets:lookup(Table, next_lease)),
    DR=discover(reply,
                #dhcp_package{chaddr=1234},
                Config),
    ?assertEqual(DR, ok).

-endif.
