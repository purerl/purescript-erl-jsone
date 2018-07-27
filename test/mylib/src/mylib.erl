-module(mylib).
-export([all_test/0]).
-include_lib("eunit/include/eunit.hrl").

all_test() -> (test_main@ps:main())().
