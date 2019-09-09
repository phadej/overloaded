all :
	cabal run test

haddock :
	cabal haddock --haddock-hyperlink-source
