-module(gen_rtmp).

-export([start_link/2]).
-export([init/2]).

-include_lib("../include/rtmp_handshake.hrl").
-include_lib("../include/rtmp_chunk.hrl").

-record(state, {rw_mod :: module(), rw :: any()}).

start_link(RWMod, RW) ->
    Pid = spawn_link(?MODULE, init, [RWMod, RW]),
    {ok, Pid}.

init(RWMod, RW) ->
    State = #state{rw_mod = RWMod, rw = RW},

    authenticate(State).

authenticate(#state{rw_mod = RWMod, rw = RW} = State) ->
    {ok, RawC0} = RWMod:recv(RW, 1, 5000),
    {ok, C0, <<>>} = rtmp_handshake:decode_c0(RawC0),

    {ok, RawC1} = RWMod:recv(RW, 1536, 5000),
    {ok, C1, <<>>} = rtmp_handshake:decode_c1(RawC1),

    S0 = rtmp_handshake:encode_s0(C0#c0.version),
    %% RandomBytes = crypto:strong_rand_bytes(1528),
    %% S1 = rtmp_handshake:encode_s1(os:system_time(), RandomBytes),
    S1 = rtmp_handshake:encode_s1(os:system_time(), C1#c1.random_bytes),
    S2 = rtmp_handshake:encode_s2(C1#c1.time, 0, C1#c1.random_bytes),

    RWMod:send(RW, [S0, S1, S2]),

    {ok, RawC2} = RWMod:recv(RW, 1536, 5000),
    {ok, C2, <<>>} = rtmp_handshake:decode_c2(RawC2),

    %% Compare information received in C2 and sent in S1
    chunk_header(State).

chunk_header(State) ->
    {ok, Fmt, CsId} = chunk_basic_header(State),
    {ok, MessageHeader} = chunk_message_header(Fmt, State),
    io:format("Format: ~p~n", [Fmt]),
    io:format("Chunk Stream ID: ~p~n", [CsId]),
    io:format("Message Header: ~p~n", [MessageHeader]).

chunk_basic_header(#state{rw_mod = RWMod, rw = RW}) ->
    {ok, Byte} = RWMod:recv(RW, 1, 5000),

    {ok, Fmt, N, <<>>} = rtmp_chunk:decode_format(Byte),

    {ok, CsId} =
        case N of
            0 ->
                {ok, RawCsId} = RWMod:recv(RW, 1, 5000),
                rtmp_chunk:decode_basic_header_chunk_stream_id(RawCsId);
            1 ->
                {ok, RawCsId} = RWMod:recv(RW, 2, 5000),
                rtmp_chunk:decode_basic_header_chunk_stream_id(RawCsId);
            _ ->
                {ok, N}
        end,

    {ok, Fmt, CsId}.

chunk_message_header(0 = Fmt, #state{rw_mod = RWMod, rw = RW} = State) ->
    {ok, RawType0} = RWMod:recv(RW, 11, 5000),
    {ok, Type0, <<>>} = rtmp_chunk:decode_message_header(Fmt, RawType0),
    Timestamp = maybe_extended_timestamp(Type0#type0.timestamp, State),
    {ok, Type0#type0{timestamp = Timestamp}};
chunk_message_header(1 = Fmt, #state{rw_mod = RWMod, rw = RW} = State) ->
    {ok, RawType1} = RWMod:recv(RW, 7, 5000),
    {ok, Type1, <<>>} = rtmp_chunk:decode_message_header(Fmt, RawType1),
    Timestamp = maybe_extended_timestamp(Type1#type1.timestamp_delta, State),
    {ok, Type1#type1{timestamp_delta = Timestamp}};
chunk_message_header(2 = Fmt, #state{rw_mod = RWMod, rw = RW} = State) ->
    {ok, RawType2} = RWMod:recv(RW, 3, 5000),
    {ok, Type2, <<>>} = rtmp_chunk:decode_message_header(Fmt, RawType2),
    Timestamp = maybe_extended_timestamp(Type2#type2.timestamp_delta, State),
    {ok, Type2#type2{timestamp_delta = Timestamp}};
chunk_message_header(3, _State) ->
    {ok, #type3{}}.

maybe_extended_timestamp(16#FFFFFF, #state{rw_mod = RWMod, rw = RW}) ->
    {ok, RawTS} = RWMod:recv(RW, 4, 5000),
    {ok, TS, <<>>} = rtmp_chunk:decode_extended_timestamp(RawTS),
    TS;
maybe_extended_timestamp(TS, _State) ->
    TS.
