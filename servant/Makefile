server: install build
	stack exec om-servant-exe

build: ./src/** ./app/** om-servant.cabal Setup.hs
	stack build --pedantic

install: om-servant.cabal
	stack install --only-dependencies
