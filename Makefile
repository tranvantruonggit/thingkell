all: main.hs
	ghc main.hs bit_utils.c -no-keep-hi-files -no-keep-o-files 

run: all
	clear
	./main

.PHONY: clean

clean:
	rm -rf *.o *.hi
	rm -rf *.dyn_hi
	rm -rf *.dyn*

