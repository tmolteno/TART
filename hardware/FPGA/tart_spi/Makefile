all: pairs permute floor

.PHONY: pairs
pairs:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --outfile include/tart_pairs.v

.PHONY: permute
permute:
	runhaskell script/pairs.hs --antennae=24 --blocksize=6 --multiplex=12 --permute --outfile data/permute.txt

.PHONY: floor
floor:
	runhaskell script/floor.hs --numDSP=4 --prefix="DSP/COR/CXB" --outfile papilio/floor.ucf

.PHONY: clean
clean:
	rm -f include/tart_pairs.v papilio/floor.ucf