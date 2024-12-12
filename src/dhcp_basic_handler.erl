-module(dhcp_basic_handler).
-behaviour(dhcp_handler).

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
  range_start::dhcp:ip(),
  range_end::dhcp:ip(),
  netmask::dhcp:ip_tpl(),
  leases::ets:table(),
  server_ip::dhcp:ip_tpl()
  }).

-record(state, {
  table::ets:table(),
  chaddr::dhcp:mac(),
  ciaddr::dhcp:ip_tpl(),
  xid::dhcp:int32()
  }).

match(_Pkg, _Config) -> true.

registered(Config) ->
  RangeStart = dhcp:tpl_to_ip(maps:get(range_start, Config, ?IP_START)),
  RangeEnd = dhcp:tpl_to_ip(maps:get(range_end, Config, ?IP_END)),
  Netmask = maps:get(netmask, Config, ?NETMASK),
  Table = ets:new(?LEASE_TABLE, [set, {read_concurrency, true}, named_table]),
  ets:insert(Table, {next_lease, RangeStart-1}),
  {ok, #config{
    range_start=RangeStart,
    range_end = RangeEnd,
    netmask = Netmask,
    leases = Table
  }}.

init(#{server_ip := ServerIp} = Config) ->
  {ok, ?MODULE, #{config => Config}, ServerIp }.

discover(ReplyPkg,
    #{chaddr := Chaddr} = _RequestPkg,
    #{table := Table, netmask := Netmask} = State) ->

  Ip = ets:update_counter(Table, next_lease, 1),
  %% Add upper boundary check
  IpTpl = dhcp:ip_to_tpl(Ip),
  case ets:inert_new(Table, {Chaddr, #lease{ciaddr=Ip, chaddr=Chaddr}}) of
    true ->
      {ok, {offer, IpTpl, Netmask, ReplyPkg}, 
        State#state{ciaddr = Ip, chaddr = Chaddr}};
    false ->
      {error, lease_exhaustion}
  end.

request(ReplyPkg, #{chaddr := Chaddr} = _RequestPkg,
  #{table := Table, netmask := Netmask} = State) ->

  [{_,#lease{ciaddr=Ip}}] = ets:lookup(Table, Chaddr),
  IpTpl = dhcp:ip_to_tpl(Ip),
  {ok, {offer, IpTpl, Netmask, ReplyPkg}, State}.

release(_RequestPkg, State) ->
  {ok, State}.
