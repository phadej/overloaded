.PHONY : all test test-8.8 haddock

all : test

test :
	cabal run test

test-8.8 :
	cabal run test -w ghc-8.8.1

haddock :
	cabal haddock --haddock-hyperlink-source
