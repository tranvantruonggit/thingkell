all: main.hs
	ghc main.hs bit_utils.c

run: all
	./main

.PHONY: clean

clean:
	rm -rf *.o *.hi
	rm -rf *.dyn_hi
	rm -rf *.dyn*

