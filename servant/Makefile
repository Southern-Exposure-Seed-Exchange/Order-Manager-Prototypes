server: build
	stack exec om-servant-exe

watch: install
	stack -j 4 build --pedantic --file-watch

clean:
	stack clean

maintainer-clean:
	stack clean --full



build: install ./src/** ./app/** om-servant.cabal Setup.hs
	stack -j 4 build --pedantic

install: om-servant.cabal
	stack -j 4 install --only-dependencies
