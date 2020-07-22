
.PHONY : all test coverage prof repl doc clear

all:
	stack run -j1 -- main.gl

test:
	stack test -j1

coverage:
	stack test -j1 --coverage
	stack hpc report --all

prof:
	stack build --profile --executable-profiling --library-profiling
	stack exec --profile GregLangCompiler -- main.gl +RTS -p

repl:
	stack repl -j1

doc:
	stack haddock --haddock-hyperlink-source --haddock-deps

clear :
	rm -r .stack-work
	rm GregLang.cabal
