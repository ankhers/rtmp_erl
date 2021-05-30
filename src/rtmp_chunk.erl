-module(rtmp_chunk).

-export([
    decode_basic_header/1,
    decode_message_header/3,
    decode_control_message/2,
    decode_rtmp_message_header/1,
    retrieve_payload/2
]).

-include("../include/rtmp_chunk.hrl").

-spec decode_basic_header(binary()) ->
    {ok, format(), chunk_stream_id(), binary()} | {error, insufficient_data}.
decode_basic_header(<<Fmt:2, 0:6, CsId, Rest/binary>>) ->
    {ok, Fmt, CsId + 64, Rest};
decode_basic_header(<<Fmt:2, 1:6, CsId:16, Rest/binary>>) ->
    {ok, Fmt, CsId + 64, Rest};
decode_basic_header(<<Fmt:2, CsId:6, Rest/binary>>) ->
    {ok, Fmt, CsId, Rest};
decode_basic_header(_Bin) ->
    {error, insufficient_data}.

-spec decode_message_header(format(), boolean(), binary()) ->
    {ok, header_type(), binary()} | {error, insufficient_data}.
decode_message_header(
    0,
    _ETS,
    <<Timestamp0:24, MsgLen:24, MsgTypeId, MsgStreamId:32, Rest0/binary>>
) ->
    case maybe_extended_timestamp(Timestamp0, Rest0) of
        {ok, Timestamp, Rest} ->
            {ok,
                #type0{
                    timestamp = Timestamp,
                    message_length = MsgLen,
                    message_type_id = MsgTypeId,
                    message_stream_id = MsgStreamId
                },
                Rest};
        {error, insufficient_data} = Err ->
            Err
    end;
decode_message_header(1, _ETS, <<TimestampDelta0:24, MsgLen:24, MsgTypeId, Rest0/binary>>) ->
    case maybe_extended_timestamp(TimestampDelta0, Rest0) of
        {ok, TimestampDelta, Rest} ->
            {ok,
                #type1{
                    timestamp_delta = TimestampDelta,
                    message_length = MsgLen,
                    message_type_id = MsgTypeId
                },
                Rest};
        {error, insufficient_data} = Err ->
            Err
    end;
decode_message_header(2, _ETS, <<TimestampDelta0:24, Rest0/binary>>) ->
    case maybe_extended_timestamp(TimestampDelta0, Rest0) of
        {ok, TimestampDelta, Rest} ->
            {ok, #type2{timestamp_delta = TimestampDelta}, Rest};
        {error, insufficient_data} = Err ->
            Err
    end;
decode_message_header(3, true, <<Timestamp:32, Rest/binary>>) ->
    {ok, #type3{extended_timestamp = Timestamp}, Rest};
decode_message_header(3, false, Rest) ->
    {ok, #type3{extended_timestamp = nil}, Rest};
decode_message_header(_, _, _Bin) ->
    {error, insufficient_data}.

-spec maybe_extended_timestamp(timestamp() | timestamp_delta(), binary()) ->
    {ok, timestamp() | timestamp_delta(), binary()} | {error, insufficient_data}.
maybe_extended_timestamp(16#FFFFFF, <<FullTimestamp:32, Rest/binary>>) ->
    {ok, FullTimestamp, Rest};
maybe_extended_timestamp(16#FFFFFF, _Bin) ->
    {error, insufficient_data};
maybe_extended_timestamp(Timestamp, Rest) ->
    {ok, Timestamp, Rest}.

%% -spec decode_chunk_data(chunk_stream_id(), header_type(), binary()) ->
%%     {ok, control_message() | #rtmp_header{}, binary()} | {error, insufficient_data}.
%% decode_chunk_data(2, #type0{message_stream_id = 0, message_type_id = MsgTypeId}, Bin) ->
%%     decode_control_message(MsgTypeId, Bin);
%% decode_chunk_data(_ChunkStreamId, _HeaderType, Bin) ->
%%     decode_rtmp_message_header(Bin).

-spec decode_control_message(message_type_id(), binary()) ->
    {ok, control_message(), binary()} | {error, insufficient_data}.
decode_control_message(1, <<0:1, ChunkSize:31, Rest/binary>>) ->
    {ok, #set_chunk_size{size = min(16#FFFFFF, ChunkSize)}, Rest};
decode_control_message(1, <<0:1, _Rest/binary>>) ->
    {error, insufficient_data};
decode_control_message(2, <<ChunkStreamId:32, Rest/binary>>) ->
    {ok, #abort{chunk_stream_id = ChunkStreamId}, Rest};
decode_control_message(3, <<SequenceNumber:32, Rest/binary>>) ->
    {ok, #acknowledgement{sequence_number = SequenceNumber}, Rest};
decode_control_message(5, <<AckWindowSize:32, Rest/binary>>) ->
    {ok, #window_acknowledgement_size{window_size = AckWindowSize}, Rest};
decode_control_message(6, <<AckWindowSize:32, LimitType, Rest/binary>>) ->
    {ok, #set_peer_bandwidth{window_size = AckWindowSize, limit_type = limit_type(LimitType)},
        Rest}.

-spec decode_rtmp_message_header(binary()) ->
    {ok, #rtmp_header{}, binary()} | {error, insufficient_data}.
decode_rtmp_message_header(<<MsgType:8, PayloadLen:24, Timestamp:32, StreamId:24, Rest/binary>>) ->
    {ok,
        #rtmp_header{
            message_type = MsgType,
            payload_length = PayloadLen,
            timestamp = Timestamp,
            stream_id = StreamId
        },
        Rest};
decode_rtmp_message_header(_Bin) ->
    {error, insufficient_data}.

-spec retrieve_payload(pos_integer(), binary()) ->
    {ok, binary(), binary()} | {error, insufficient_data}.
retrieve_payload(Len, Bin) when bit_size(Bin) >= Len ->
    <<Payload:Len/binary, Rest/binary>> = Bin,
    {ok, Payload, Rest};
retrieve_payload(_Len, _Bin) ->
    {error, insufficient_data}.

limit_type(0) -> hard;
limit_type(1) -> soft;
limit_type(2) -> dynamic.
