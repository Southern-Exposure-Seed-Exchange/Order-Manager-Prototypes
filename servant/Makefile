server: build
	stack exec om-servant-exe

watch: build
	stack build --file-watch

clean:
	stack clean

maintainer-clean:
	stack clean --full



build: install ./src/** ./app/** om-servant.cabal Setup.hs
	stack build --pedantic

install: om-servant.cabal
	stack install --only-dependencies
