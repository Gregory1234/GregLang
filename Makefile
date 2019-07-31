
.PHONY : all test

all:
	cabal new-run GregLangCompiler -- main.gl

test :
	cabal new-test --enable-tests GregLangTests
