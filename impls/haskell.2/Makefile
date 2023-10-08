BINS = step0_repl  step1_read_print

all: $(BINS)

$(BINS): %: %.hs
	ghc -Wall $<

clean:
	rm -rf step0_repl *.hi *.o
