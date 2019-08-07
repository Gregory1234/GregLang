
.PHONY : all test prof-test clear

all:
	cabal new-run -O2 GregLangCompiler -- main.gl

test :
	cabal new-test -O2 --enable-tests GregLangTests

prof-test :
	cabal new-test -O2 --ghc-options "-threaded -fprof-auto -with-rtsopts=\"-N -p -s -h -i0.1\"" --enable-library-profiling --enable-profiling --enable-tests  --enable-benchmarks GregLangTests

clear :
	-rm -r dist
	rm -r dist-newstyle
