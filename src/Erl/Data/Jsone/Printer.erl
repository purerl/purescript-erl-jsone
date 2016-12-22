-module(erl_data_jsone_printer@foreign).
-export([encode_/3]).

encode_(JsonValue, N, M) -> jsone:encode(JsonValue, [{space,N}, {indent,M}]).
