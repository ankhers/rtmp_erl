-type timestamp() :: non_neg_integer().
-type message_length() :: non_neg_integer().
-type message_type_id() :: non_neg_integer().
-type message_stream_id() :: non_neg_integer().
-type timestamp_delta() :: non_neg_integer().
-type chunk_stream_id() :: non_neg_integer().
-type format() :: non_neg_integer().

-record(type0, {
    timestamp :: timestamp(),
    message_length :: message_length(),
    message_type_id :: message_type_id(),
    message_stream_id :: message_stream_id()
}).

-record(type1, {
    timestamp_delta :: timestamp_delta(),
    message_length :: message_length(),
    message_type_id :: message_type_id()
}).

-record(type2, {
    timestamp_delta :: timestamp_delta()
}).

-record(type3, {extended_timestamp :: timestamp() | nil}).

%% Protocol Control Messages
-record(set_chunk_size, {size :: non_neg_integer()}).

-record(abort, {chunk_stream_id :: non_neg_integer()}).

-record(acknowledgement, {sequence_number :: non_neg_integer()}).

-record(window_acknowledgement_size, {window_size :: non_neg_integer()}).

-record(set_peer_bandwidth, {
    window_size :: non_neg_integer(),
    limit_type :: hard | soft | dynamic
}).

-record(rtmp_header, {
    message_type :: non_neg_integer(),
    payload_length :: non_neg_integer(),
    timestamp :: non_neg_integer(),
    stream_id :: non_neg_integer()
}).

-type header_type() :: #type0{} | #type1{} | #type2{} | #type3{}.

-type control_message() ::
    #set_chunk_size{}
    | #abort{}
    | #acknowledgement{}
    | #window_acknowledgement_size{}
    | #set_peer_bandwidth{}.
