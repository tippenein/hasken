all:
	runhaskell Setup.hs configure --user
	runhaskell Setup.hs build
	runhaskell Setup.hs install

