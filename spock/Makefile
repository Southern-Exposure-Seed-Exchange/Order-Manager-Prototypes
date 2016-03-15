server: build
	stack exec om-spock

build: ./src/** om-spock.cabal Setup.hs
	stack build --pedantic

install:
	stack install --only-dependencies
