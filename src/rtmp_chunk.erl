-module(rtmp_chunk).

-export([decode_basic_header/1, decode_message_header/3, decode_chunk_data/3]).

-include("rtmp_chunk.hrl").

-type header_type() :: #type0{} | #type1{} | #type2{} | #type3{}.

-type control_message() ::
    #set_chunk_size{}
    | #abort{}
    | #acknowledgement{}
    | #window_acknowledgement_size{}
    | #set_peer_bandwidth{}.

-spec decode_basic_header(binary()) -> {format(), chunk_stream_id(), binary()}.
decode_basic_header(<<Fmt:2, 0:6, CsId, Rest/binary>>) ->
    {Fmt, CsId + 64, Rest};
decode_basic_header(<<Fmt:2, 1:6, CsId:16, Rest/binary>>) ->
    {Fmt, CsId + 64, Rest};
decode_basic_header(<<Fmt:2, CsId:6, Rest/binary>>) ->
    {Fmt, CsId, Rest}.

-spec decode_message_header(format(), boolean(), binary()) -> {header_type(), binary()}.
decode_message_header(
    0,
    _ETS,
    <<Timestamp0:24, MsgLen:24, MsgTypeId, MsgStreamId:32, Rest0/binary>>
) ->
    {Timestamp, Rest} = maybe_extended_timestamp(Timestamp0, Rest0),
    {#type0{
            timestamp = Timestamp,
            message_length = MsgLen,
            message_type_id = MsgTypeId,
            message_stream_id = MsgStreamId
        },
        Rest};
decode_message_header(1, _ETS, <<TimestampDelta0:24, MsgLen:24, MsgTypeId, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {#type1{timestamp_delta = TimestampDelta, message_length = MsgLen, message_type_id = MsgTypeId},
        Rest};
decode_message_header(2, _ETS, <<TimestampDelta0:24, Rest0/binary>>) ->
    {TimestampDelta, Rest} = maybe_extended_timestamp(TimestampDelta0, Rest0),
    {#type2{timestamp_delta = TimestampDelta}, Rest};
decode_message_header(3, true, <<Timestamp:32, Rest/binary>>) ->
    {#type3{extended_timestamp = Timestamp}, Rest};
decode_message_header(3, false, Rest) ->
    {#type3{extended_timestamp = nil}, Rest}.

-spec maybe_extended_timestamp(timestamp() | timestamp_delta(), binary()) ->
    {timestamp() | timestamp_delta(), binary()}.
maybe_extended_timestamp(16#FFFFFF, <<FullTimestamp:32, Rest/binary>>) ->
    {FullTimestamp, Rest};
maybe_extended_timestamp(Timestamp, Rest) ->
    {Timestamp, Rest}.

-spec decode_chunk_data(chunk_stream_id(), header_type(), binary()) ->
    {control_message(), binary()}.
decode_chunk_data(2, #type0{message_stream_id = 0, message_type_id = MsgTypeId}, Bin) ->
    decode_control_message(MsgTypeId, Bin);
decode_chunk_data(ChunkStreamId, HeaderType, Bin) ->
    {foo, Bin}.

decode_control_message(1, <<0:1, ChunkSize:31, Rest/binary>>) ->
    {#set_chunk_size{size = min(16#FFFFFF, ChunkSize)}, Rest};
decode_control_message(2, <<ChunkStreamId:32, Rest/binary>>) ->
    {#abort{chunk_stream_id = ChunkStreamId}, Rest};
decode_control_message(3, <<SequenceNumber:32, Rest/binary>>) ->
    {#acknowledgement{sequence_number = SequenceNumber}, Rest};
decode_control_message(5, <<AckWindowSize:32, Rest/binary>>) ->
    {#window_acknowledgement_size{window_size = AckWindowSize}, Rest};
decode_control_message(6, <<AckWindowSize:32, LimitType, Rest/binary>>) ->
    {#set_peer_bandwidth{window_size = AckWindowSize, limit_type = limit_type(LimitType)}, Rest}.

limit_type(0) -> hard;
limit_type(1) -> soft;
limit_type(2) -> dynamic.
