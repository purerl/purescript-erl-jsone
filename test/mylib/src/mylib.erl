-module(mylib).
-include_lib("eunit/include/eunit.hrl").
all_test() -> (test_main:main())().
