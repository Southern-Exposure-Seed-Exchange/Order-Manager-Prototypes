server: install build
	stack exec om-servant-exe

build: ./src/** ./app/** om-servant.cabal Setup.hs
	stack build --ghc-options -Wall

install: om-servant.cabal
	stack install --only-dependencies
