
.PHONY : all test repl clear

all:
	stack run -- main.gl

test:
	stack test

repl:
	stack repl

clear :
	-rm -r dist
	rm -r dist-newstyle
