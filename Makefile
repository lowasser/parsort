.PHONY : bench Benchmark

Benchmark :
	ghc --make -fllvm -O2 -optlo-O3 -optlo-std-compile-opts -optlo-loop-unroll -keep-llvm-files \
		-fno-spec-constr-count -fno-spec-constr-threshold -optlo-stats \
		-ddump-minimal-imports -Wall -fno-warn-name-shadowing -odir bench \
		 Benchmark -o Benchmark -main-is Benchmark -threaded -rtsopts $(GHC_OPTS)

bench : Benchmark
	./Benchmark +RTS -N2 -sstderr -K64m -RTS -s 10

test :
	ghc --make Tests -o Tests -main-is Tests -fforce-recomp \
	  -ddump-minimal-imports -Wall -fno-warn-name-shadowing \
	  -DDEBUG $(GHC_OPTS)
	./Tests
