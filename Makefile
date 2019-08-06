
.PHONY : all test prof-test clear

all:
	cabal new-run GregLangCompiler -- main.gl

test :
	cabal new-test --enable-tests GregLangTests

prof-test :
	cabal new-test --ghc-options "-O2 -threaded -fprof-auto -with-rtsopts=\"-N -p -s -h -i0.1\"" --enable-library-profiling --enable-profiling --enable-tests  --enable-benchmarks GregLangTests

clear :
	-rm -r dist
	rm -r dist-newstyle
