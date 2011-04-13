bench :
	ghc --make -fllvm -O2 -optlo-O3 -optlo-std-compile-opts -optlo-loop-unroll -keep-llvm-files \
		-fno-spec-constr-count -fno-spec-constr-threshold -optlo-stats \
		 Benchmark -o Benchmark -main-is Benchmark -threaded -rtsopts

test :
	ghc --make Tests -o Tests -main-is Tests
	./Tests
