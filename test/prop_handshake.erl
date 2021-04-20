-module(prop_handshake).

-include_lib("proper/include/proper.hrl").
-include_lib("../src/rtmp_handshake.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_version_fails_on_version_other_than_3() ->
    ?FORALL(
        Version,
        integer_8bit_not_3(),
        begin
            Bin = <<Version>>,
            {error, unknown_version, Version} == rtmp_handshake:decode_c0(Bin)
        end
    ).

prop_can_encode_version() ->
    ?FORALL(Version, integer_8bit(), <<Version>> == rtmp_handshake:encode_s0(Version)).

prop_can_decode_proper_c1() ->
    ?FORALL(
        {Time, Random, Full},
        c1(),
        begin
            {ok, C1, <<>>} = rtmp_handshake:decode_c1(Full),
            C1#c1.time == Time andalso C1#c1.random_bytes == Random
        end
    ).

prop_cannot_decode_small_c1() ->
    ?FORALL(
        Bin,
        small_data(),
        {error, insufficient_data} == rtmp_handshake:decode_c1(Bin)
    ).

prop_cannot_decode_bad_format_c1() ->
    ?FORALL(Bin, bad_c1(), {error, bad_format} == rtmp_handshake:decode_c1(Bin)).

prop_can_encode_s1() ->
    ?FORALL(
        {Time, RandomBytes},
        {handshake_time(), random_bytes()},
        begin
            Bin = rtmp_handshake:encode_s1(Time, RandomBytes),
            byte_size(Bin) == 1536
        end
    ).

prop_can_decode_proper_c2() ->
    ?FORALL(
        {Time, Time2, Random, Full},
        c2(),
        begin
            {ok, C2, <<>>} = rtmp_handshake:decode_c2(Full),
            C2#c2.time == Time andalso C2#c2.time2 == Time2 andalso C2#c2.random_echo == Random
        end
    ).

prop_cannot_decode_small_c2() ->
    ?FORALL(
        Bin,
        small_data(),
        {error, insufficient_data} == rtmp_handshake:decode_c2(Bin)
    ).

prop_can_encode_s2() ->
    ?FORALL(
        {Time, Time2, RandomBytes},
        {handshake_time(), handshake_time(), random_bytes()},
        begin
            Bin = rtmp_handshake:encode_s2(Time, Time2, RandomBytes),
            byte_size(Bin) == 1536
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
integer_8bit() ->
    ?LET(N, non_neg_integer(), N rem 256).

integer_8bit_not_3() ->
    ?SUCHTHAT(N, integer_8bit(), N =/= 3).

handshake_time() ->
    non_neg_integer().

random_bytes() ->
    binary(1528).

c1() ->
    ?LET(
        {N, Random},
        {handshake_time(), random_bytes()},
        {N, Random, <<N:32, 0:32, Random/binary>>}
    ).

small_data() ->
    ?LET(N, non_neg_integer(), binary(N rem 1536)).

bad_c1() ->
    ?LET(
        {Time, Random, N},
        {handshake_time(), random_bytes(), non_neg_integer()},
        <<Time:32, (N + 1):32, Random/binary>>
    ).

c2() ->
    ?LET(
        {Time, Time2, Random},
        {handshake_time(), handshake_time(), random_bytes()},
        {Time, Time2, Random, <<Time:32, Time2:32, Random/binary>>}
    ).
