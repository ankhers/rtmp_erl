-module(gen_rtmp).

-export([start_link/2]).
-export([init/2]).

-include_lib("../include/rtmp_handshake.hrl").
-include_lib("../include/rtmp_chunk.hrl").

-record(state, {
    rw_mod :: module(),
    rw :: any(),

    %% Number of bytes
    chunk_size :: pos_integer(),

    message_headers :: #{chunk_stream_id() => header_type()},
    data :: #{chunk_stream_id() => binary()}
}).

start_link(RWMod, RW) ->
    Pid = spawn_link(?MODULE, init, [RWMod, RW]),
    {ok, Pid}.

init(RWMod, RW) ->
    State = #state{rw_mod = RWMod, rw = RW, chunk_size = 128, message_headers = #{}, data = #{}},

    authenticate(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%%                            MAIN STATE FUNCTIONS                            %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The RTMP process begins with a handshake.
%% The client will first send a single byte for the RTMP version that it speaks
%% Then it will also send along a timestamp and some random data to later be echoed
%%
%% The server will reply with the RTMP version number
%% as well as its own timestamp and random data to be echoed back
%%
%% Finally the client and the server will echo back the data it received in the second message
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
    %% TODO: Compare information received in C2 and sent in S1

    chunk_header(State).

chunk_header(#state{message_headers = MessageHeaders} = State) ->
    {ok, Fmt, CsId} = chunk_basic_header(State),
    {ok, MessageHeader} = chunk_message_header(Fmt, State),
    io:format("Format: ~p~n", [Fmt]),
    io:format("Chunk Stream ID: ~p~n", [CsId]),
    io:format("Message Header: ~p~n", [MessageHeader]),

    CurrentType0 = maps:get(CsId, MessageHeaders, MessageHeader),

    %% In order to save some information when transferring data, the RTMP protocol
    %% will use data from previous messages when they are the same, so we need to
    %% keep track of that and adjust accordingly
    MH =
        case MessageHeader of
            #type0{} ->
                MessageHeader;
            #type1{timestamp_delta = D, message_length = ML, message_type_id = MT} ->
                CurrentType0#type0{
                    timestamp = CurrentType0#type0.timestamp + D,
                    message_length = ML,
                    message_type_id = MT
                };
            #type2{timestamp_delta = D} ->
                CurrentType0#type0{timestamp = CurrentType0#type0.timestamp + D};
            #type3{} ->
                CurrentType0
        end,

    chunk_data(CsId, MH, State#state{message_headers = maps:put(CsId, MH, MessageHeaders)}).

chunk_data(
    CsId,
    #type0{message_length = ML} = Header,
    #state{rw_mod = RWMod, rw = RW, chunk_size = ChunkSize, data = Data} = State
) ->
    CurrentData = maps:get(CsId, Data, <<>>),
    NumBytes = min(ChunkSize, ML - byte_size(CurrentData)),
    {ok, RawChunkData} = RWMod:recv(RW, NumBytes, 5000),

    WholeChunkData = <<CurrentData/binary, RawChunkData/binary>>,

    case byte_size(WholeChunkData) of
        ML ->
            NewState = State#state{data = maps:remove(CsId, Data)},
            case message_kind(CsId, Header) of
                control_message ->
                    io:format("Control Message~n"),
                    control_message(WholeChunkData, Header, NewState);
                command ->
                    io:format("Command~n"),
                    command(WholeChunkData, Header, NewState)
            end;
        _ ->
            chunk_header(State#state{data = maps:put(CsId, WholeChunkData, Data)})
    end.

control_message(RawChunkData, Header, State) ->
    {ok, CtrlMsg, <<>>} = rtmp_chunk:decode_control_message(Header#type0.message_type_id, RawChunkData),

    chunk_header(handle_control_message(CtrlMsg, State)).

command(RawChunkData, Header, State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                            %%
%%                          INTNERAL HELPER FUNCTIONS                         %%
%%                                                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

message_kind(2, #type0{message_type_id = N, message_stream_id = 0}) when
    N == 1 orelse N == 2 orelse N == 3 orelse N == 5 orelse N == 6
->
    control_message;
message_kind(3, #type0{message_stream_id = 0}) ->
    command.

handle_control_message(#set_chunk_size{size = Size}, State) ->
    io:format("SetChunkSize: ~p~n", [Size]),
    State#state{chunk_size = Size}.
