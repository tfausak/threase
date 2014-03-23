.PHONY: bench clean format haddock hpc init install repl run test

all: install format hpc bench haddock run

bench:
	cabal configure --enable-benchmarks
	cabal build
	cabal bench

clean:
	cabal clean
	test ! -d .cabal-sandbox || cabal sandbox delete
	test ! -d .hpc || rm -r .hpc

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal configure
	cabal build
	cabal haddock --hyperlink-source
	# dist/doc/html/threase/index.html

hpc: test
	hpc report dist/hpc/tix/hspec/hspec.tix
	hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
	# tmp/hpc_index.html

init:
	cabal update
	test $(cabal --numeric-version) = '1.18.0.3' || cabal install cabal-install-1.18.0.3
	cabal sandbox init

install: init
	cabal install --enable-benchmarks --enable-tests --flags=documentation --only-dependencies

repl:
	cabal configure
	cabal build
	cabal repl lib:threase

run:
	cabal configure
	cabal build
	cabal run threase

test:
	cabal configure --enable-tests
	cabal build
	cabal test
