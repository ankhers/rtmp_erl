-module(prop_rtmp_chunk).

-include_lib("proper/include/proper.hrl").

-include("../src/rtmp_chunk.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_decode_basic_header() ->
    ?FORALL(
        {Bin, Fmt, CSId},
        basic_header(),
        begin
            {Fmt, CSId, <<>>} == rtmp_chunk:decode_basic_header(Bin)
        end
    ).

prop_decode_message_header() ->
    ?FORALL(
        {Bin, ETS, Fmt, Type},
        message_header(),
        begin
            {Type, <<>>} == rtmp_chunk:decode_message_header(Fmt, ETS, Bin)
        end
    ).

prop_decode_control_message() ->
    ?FORALL(
        {Bin, Type, Msg},
        control_message(),
        {Msg, <<>>} == rtmp_chunk:decode_chunk_data(2, Type, Bin)
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
encode_basic_header(Fmt, ChunkStreamId) when ChunkStreamId >= 256 ->
    CSId = ChunkStreamId - 64,
    <<Fmt:2, 1:6, CSId:16>>;
encode_basic_header(Fmt, ChunkStreamId) when ChunkStreamId >= 64 ->
    CSId = ChunkStreamId - 64,
    <<Fmt:2, 0:6, CSId:8>>;
encode_basic_header(Fmt, ChunkStreamId) ->
    <<Fmt:2, ChunkStreamId:6>>.

encode_t0(Timestamp, MsgLen, MsgTypeId, MsgStreamId) ->
    {TS, ETS} = encode_timestamps(Timestamp),
    <<TS/binary, MsgLen:24, MsgTypeId:8, MsgStreamId:32, ETS/binary>>.

encode_t1(TSDelta, MsgLen, MsgTypeId) ->
    {TS, ETS} = encode_timestamps(TSDelta),
    <<TS/binary, MsgLen:24, MsgTypeId:8, ETS/binary>>.

encode_t2(TSDelta) ->
    {TS, ETS} = encode_timestamps(TSDelta),
    <<TS/binary, ETS/binary>>.

encode_t3(TS, true) ->
    <<TS:32>>;
encode_t3(_TS, false) ->
    <<>>.

encode_timestamps(TS) when TS >= 16#FFFFFF ->
    {<<16#FFFFFF:24>>, <<TS:32>>};
encode_timestamps(TS) ->
    {<<TS:24>>, <<>>}.

encode_set_chunk_size(Size) ->
    <<0:1, Size:31>>.

encode_abort(CSId) ->
    <<CSId:32>>.

encode_acknowledgement(SeqNum) ->
    <<SeqNum:32>>.

encode_window_acknowledgement_size(AckWinSize) ->
    <<AckWinSize:32>>.

encode_set_peer_bandwidth(AckWinSize, LimitType) ->
    <<AckWinSize:32, LimitType:8>>.

limit_type(0) -> hard;
limit_type(1) -> soft;
limit_type(2) -> dynamic.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
format() ->
    oneof([0, 1, 2, 3]).

basic_header() ->
    ?LET(
        {Fmt, CSId},
        {format(), non_neg_integer()},
        {encode_basic_header(Fmt, CSId + 2), Fmt, CSId + 2}
    ).

message_header() ->
    oneof([t0(), t1(), t2(), t3()]).

t0() ->
    ?LET(
        {Timestamp, MsgLen, MsgTypeId, MsgStreamId, ETS},
        {timestamp(), non_neg_integer(), byte(), non_neg_integer(), boolean()},
        begin
            {encode_t0(Timestamp, MsgLen, MsgTypeId, MsgStreamId), ETS, 0, #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = MsgTypeId,
                message_stream_id = MsgStreamId
            }}
        end
    ).

t1() ->
    ?LET(
        {TSDelta, MsgLen, MsgTypeId, ETS},
        {timestamp(), non_neg_integer(), byte(), boolean()},
        begin
            {encode_t1(TSDelta, MsgLen, MsgTypeId), ETS, 1, #type1{
                timestamp_delta = TSDelta,
                message_length = MsgLen,
                message_type_id = MsgTypeId
            }}
        end
    ).

t2() ->
    ?LET(
        {TSDelta, ETS},
        {timestamp(), boolean()},
        begin
            {encode_t2(TSDelta), ETS, 2, #type2{
                timestamp_delta = TSDelta
            }}
        end
    ).

t3() ->
    ?LET(
        {Timestamp, ETS},
        {extended_timestamp(), boolean()},
        begin
            TS =
                if
                    ETS -> Timestamp;
                    true -> nil
                end,
            {encode_t3(Timestamp, ETS), ETS, 3, #type3{extended_timestamp = TS}}
        end
    ).

timestamp() -> oneof([simple_timestamp(), extended_timestamp()]).

simple_timestamp() -> integer(0, 16#FFFFFF - 1).

extended_timestamp() -> integer(16#FFFFFF, 16#FFFFFFFF).

control_message() ->
    oneof([
        set_chunk_size(),
        abort(),
        acknowledgement(),
        window_acknowledgement_size(),
        set_peer_bandwidth()
    ]).

set_chunk_size() ->
    ?LET(
        {Timestamp, MsgLen, Size},
        {timestamp(), non_neg_integer(), integer(1, 16#7FFFFFFF)},
        {encode_set_chunk_size(Size),
            #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = 1,
                message_stream_id = 0
            },
            #set_chunk_size{
                size = Size
            }}
    ).

abort() ->
    ?LET(
        {Timestamp, MsgLen, CSId},
        {timestamp(), non_neg_integer(), non_neg_integer()},
        {encode_abort(CSId),
            #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = 2,
                message_stream_id = 0
            },
            #abort{chunk_stream_id = CSId}}
    ).

acknowledgement() ->
    ?LET(
        {Timestamp, MsgLen, SeqNum},
        {timestamp(), non_neg_integer(), non_neg_integer()},
        {encode_acknowledgement(SeqNum),
            #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = 3,
                message_stream_id = 0
            },
            #acknowledgement{sequence_number = SeqNum}}
    ).

window_acknowledgement_size() ->
    ?LET(
        {Timestamp, MsgLen, AckWinSize},
        {timestamp(), non_neg_integer(), non_neg_integer()},
        {encode_window_acknowledgement_size(AckWinSize),
            #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = 5,
                message_stream_id = 0
            },
            #window_acknowledgement_size{window_size = AckWinSize}}
    ).

set_peer_bandwidth() ->
    ?LET(
        {Timestamp, MsgLen, AckWinSize, LimitType},
        {timestamp(), non_neg_integer(), non_neg_integer(), oneof([0, 1, 2])},
        {encode_set_peer_bandwidth(AckWinSize, LimitType),
            #type0{
                timestamp = Timestamp,
                message_length = MsgLen,
                message_type_id = 6,
                message_stream_id = 0
            },
            #set_peer_bandwidth{window_size = AckWinSize, limit_type = limit_type(LimitType)}}
    ).
