-module(erl_data_jsone_parser@foreign).
-export([jsonParserImpl/3]).

jsonParserImpl(Left,Right,JsonString) ->
  case jsone:try_decode(JsonString, [{object_format, tuple}]) of
    {ok, Value, _} -> Right(Value);
    {error, {Reason, _}} -> Left("Error decoding JSON")
  end.
