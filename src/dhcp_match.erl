%%%-------------------------------------------------------------------
%%% @author Dan Janowski <danj3us@gmail.com>
%%% @copyright (C) 2024, Dan Janowski
%%% @doc
%%% Externalize matching functions, allows Handler to
%%% define any direct, indirect, or general matching
%%% @end
%%%-------------------------------------------------------------------

-module(dhcp_match).

-include("dhcp.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([match/2, match_pkg/2,
         match_mac/2, match_fields/2, match_opts/2]).

-type mac_match_byte() :: byte() | '_'.
-type mac_match() :: '_' | {mac_match_byte(), mac_match_byte(),
                            mac_match_byte(), mac_match_byte(),
                            mac_match_byte(), mac_match_byte()}.
-type field_match() :: {dhcp:package_fields(),
                        dhcp:op() | dhcp:htype() | byte() | dhcp:int32() |
                        dhcp:ip() | dhcp:mac() | binary() |
                        dhcp:message_type() | dhcp:flags()}.
-type option_match() :: dhcp:option().
-type match_spec() :: {mac_match(), [field_match()], [option_match()]}.


-spec match_pkg(Pkg::package(), Spec::match_spec()) -> boolean().
match_pkg(Pkg, {Mac, Fields, Opts}) ->
    match_mac(dhcp_package:get_chaddr(Pkg), Mac) andalso
        match_fields(Pkg, Fields) andalso
        match_opts(Pkg, Opts).

-spec match_mac(dhcp:mac(), mac_match()) -> boolean().
match_mac({A, B, C, D, E, F}, {A, B, C, D, E, F}) -> true;
match_mac({A, B, C, D, E, _}, {A, B, C, D, E, '_'}) -> true;
match_mac({A, B, C, D, _, _}, {A, B, C, D, '_', '_'}) -> true;
match_mac({A, B, C, _, _, _}, {A, B, C, '_', '_', '_'}) -> true;
match_mac({A, B, _, _, _, _}, {A, B, '_', '_', '_', '_'}) -> true;
match_mac({A, _, _, _, _, _}, {A, '_', '_', '_', '_', '_'}) -> true;
match_mac({_, _, _, _, _, _}, {'_', '_', '_', '_', '_', '_'}) -> true;
match_mac({_, _, _, _, _, _}, '_') -> true;
match_mac(_, _) -> false.

-spec match_fields(P::package(), [field_match()]) ->
                          boolean().
match_fields(P, [{F, V} | Fs]) ->
    (dhcp_package:get_field(F, P) =:= V) andalso
        match_fields(P, Fs);
match_fields(_P, []) ->
    true.

-spec match_opts(P::package(), [dhcp:option()]) -> boolean().
match_opts(P, [{O, V} | Fs]) ->
    (dhcp_package:get_option(O, P) =:= V) andalso
        match_fields(P, Fs);
match_opts(_, []) ->
    true.


-spec match(Pkg::package(), [{Handler::atom(), Config::any()}]) ->
                   {ok, {Handler::atom(), Config::any()}} | undefined.
match(Pkg, [{Handler, Config} | R]) ->
    case Handler:match(Pkg, Config) of
        true ->
            {ok, {Handler, Config}};
        false ->
            match(Pkg, R)
    end;
match(_Pkg, []) ->
    undefined.
