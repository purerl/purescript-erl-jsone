-module(test_main@foreign).
-export([debugMsg/1,debugVal/1]).
-include_lib("eunit/include/eunit.hrl").

debugMsg(Text) -> fun DebugMsg() -> ?debugMsg(Text) end.
debugVal(Val) -> fun DebugVal() -> ?debugVal(Val) end.
