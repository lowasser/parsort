.PHONY : bench Benchmark

Benchmark :
	ghc --make -fllvm -O2 -optlo-O3 -optlo-std-compile-opts -optlo-loop-unroll -keep-llvm-files \
		-fno-spec-constr-count -fno-spec-constr-threshold -optlo-stats \
		-ddump-minimal-imports -Wall -fno-warn-name-shadowing -odir bench -I. \
		 Benchmark -o Benchmark -main-is Benchmark -threaded -rtsopts $(GHC_OPTS)

test :
	ghc --make Tests -o Tests -main-is Tests -fforce-recomp \
	  -ddump-minimal-imports -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind \
	  -DDEBUG -I. $(GHC_OPTS)
	./Tests
