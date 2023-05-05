all: main.hs
	ghc -threaded -O2 -rtsopts main.hs bit_utils.c -no-keep-hi-files -no-keep-o-files 

run: all
	clear
	./main +RTS

.PHONY: clean

clean:
	rm -rf *.o *.hi
	rm -rf *.dyn_hi
	rm -rf *.dyn*

