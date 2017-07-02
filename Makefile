.PHONY: ps erl all test

all: ps

ps:
	psc-package sources | xargs purs compile 'test/**/*.purs' 'src/**/*.purs'

test: ps
	cp output/*/*.erl test/mylib/src/ps/
	make -C test/mylib

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl
