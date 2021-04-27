-module(rtmp_handshake).

-export([decode_c0/1, decode_c1/1, decode_c2/1]).
-export([encode_s0/1, encode_s1/2, encode_s2/3]).

-include("rtmp_handshake.hrl").

-spec decode_c0(binary()) -> {ok, #c0{}, binary} | {error, unknown_version, integer()}.
decode_c0(<<3, Rest/binary>>) ->
    {ok, #c0{version = 3}, Rest};
decode_c0(<<Vsn, _Rest/binary>>) ->
    {error, unknown_version, Vsn}.

-spec encode_s0(integer()) -> binary().
encode_s0(Version) ->
    <<Version:8>>.

-spec decode_c1(binary()) -> {ok, #c1{}, binary()} | {error, insufficient_data | bad_format}.
decode_c1(Bin) when byte_size(Bin) < 1536 ->
    {error, insufficient_data};
decode_c1(<<Time:32, 0:32, RandomBytes:1528/binary, Rest/binary>>) ->
    {ok, #c1{time = Time, random_bytes = RandomBytes}, Rest};
decode_c1(_Bin) ->
    {error, bad_format}.

-spec decode_c2(binary()) -> {ok, #c2{}, binary()} | {error, insufficient_data}.
decode_c2(Bin) when byte_size(Bin) < 1536 ->
    {error, insufficient_data};
decode_c2(<<Time:32, Time2:32, Echo:1528/binary, Rest/binary>>) ->
    {ok, #c2{time = Time, time2 = Time2, random_echo = Echo}, Rest}.

-spec encode_s1(integer(), binary()) -> binary().
encode_s1(Time, RandomBytes) ->
    <<Time:32, 0:32, RandomBytes/binary>>.

-spec encode_s2(integer(), integer(), binary()) -> binary().
encode_s2(Time, Time2, RandomEcho) ->
    <<Time:32, Time2:32, RandomEcho/binary>>.
