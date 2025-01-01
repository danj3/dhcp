-module(dhcp_basic_handler).
-behaviour(dhcp_handler).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(ETS_ACCESS, public).
-else.
-define(ETS_ACCESS, protected).
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
    Table = ets:new(?LEASE_TABLE, [set, {read_concurrency, true}, ?ETS_ACCESS, named_table]),
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
         #dhcp_package{chaddr = Chaddr} = RequestPkg,
         #state{config = #config{leases= Table, netmask = Netmask, range_end = RangeEnd}} = State) ->

    Ip = ets:update_counter(Table, next_lease, 1),

    if Ip < RangeEnd ->
            IpTpl = dhcp:ip_to_tpl(Ip),
            case lease_add(RequestPkg, Ip, State) of
                true ->
                    {ok, {offer, IpTpl, Netmask, ReplyPkg}, 
                     State#state{ciaddr = IpTpl, chaddr = Chaddr}};
                false ->
                    {error, lease_exhaustion} % TODO this is not quite correct
            end;
       Ip >= RangeEnd ->
            {error, lease_exhaustion}
    end.

request(ReplyPkg,
        RequestPkg,
        #state{config = #config{netmask = Netmask}} = State) ->

    case lease_find(RequestPkg, State) of
        {ok, Ip} ->
            {ok, {ack, Ip, dhcp:tpl_to_ip(Netmask), ReplyPkg}, State};
        {error, not_found} ->
            {ok, {nck, ReplyPkg}, State}
    end.


release(_RequestPkg, State) ->
    {ok, State}.

lease_add(#dhcp_package{chaddr = Chaddr}, Ip, #state{config = #config{leases=Table}}) ->
    ets:insert_new(Table, {Chaddr, #lease{ciaddr=Ip, chaddr=Chaddr}}).

lease_find(#dhcp_package{chaddr = Chaddr}, #state{config = #config{leases=Table}}) ->
    case ets:lookup(Table, Chaddr) of
        [{_,#lease{ciaddr=Ip}}] ->
            {ok, Ip};
        [] ->
            {error, not_found}
    end.

-ifdef(TEST).

-define(DHCP1, "AQEGAP8iTzsAAAAACioKmgAAAAAAAAAAAAAAAORluHlawAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABjglNjNQEDOQIF3DcFAQMcBioMB3NlbnNvcnM9BwHkZbh5WsD/AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=").

handler_test_() ->
    {
     setup,
     fun () ->
             {ok, Config} = registered(#{}),
             #state{config=Config}
     end,
     fun (State) ->
             io:fwrite(user,"~w~n", [State]),
             Leases=(State#state.config)#config.leases,
             % ets:update_counter(Leases, next_lease, 1),
             [
              fun () ->
                      ?assertEqual(
                         [{next_lease, dhcp:tpl_to_ip(?IP_START)-1}],
                         ets:lookup(Leases, next_lease)
                        )
              end,
              fun () ->
                      ets:update_counter(Leases, next_lease, 1),
                      ?assertEqual(
                         [{next_lease, dhcp:tpl_to_ip(?IP_START)}],
                         ets:lookup(Leases, next_lease)
                        )
              end,
              fun () ->
                      #state{
                         config=#config{
                                   range_start=RangeStart,
                                   netmask=Netmask,
                                   leases=Table
                                  }} = State,
                      RangeStartIp=dhcp:tpl_to_ip(?IP_START),
                      ?assertEqual(RangeStart, RangeStartIp),
                      ?assertEqual([{next_lease,RangeStart}], ets:lookup(Table, next_lease)),
                      DR=discover(reply,
                                  #dhcp_package{chaddr=1234},
                                  State),
                      NextIp=dhcp:ip_to_tpl(RangeStart+1),
                      ?assertMatch({ok, {offer, NextIp, Netmask, reply}, _}, DR)
              end,
              fun () ->
                      Payload=base64:decode(?DHCP1),
                      ?assert(is_binary(Payload)),
                      {ok,Package}=dhcp_package:decode(Payload),
                      ?assertMatch(#dhcp_package{op=request,message_type=request}, Package),
                      NextIp=ets:update_counter(Leases, next_lease, 1),
                      lease_add(Package, NextIp, State),
                      {ok,Reply,_}=request(request, Package, State),
                      ?assertMatch({ack, NextIp, _, request}, Reply)
              end
             ]
     end}.

-endif.
