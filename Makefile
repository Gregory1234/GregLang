
.PHONY : all test repl clear

all:
	stack run -j1 -- main.gl

test:
	stack test -j1

repl:
	stack repl -j1

clear :
	rm -r .stack-work
	rm GregLang.cabal
