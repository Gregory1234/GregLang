
.PHONY : all test coverage repl doc clear

all:
	stack run -j1 -- main.gl

test:
	stack test -j1

coverage:
	stack test -j1 --coverage
	stack hpc report --all

repl:
	stack repl -j1

doc:
	stack haddock --haddock-hyperlink-source --haddock-deps

clear :
	rm -r .stack-work
	rm GregLang.cabal
