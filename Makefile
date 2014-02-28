.PHONY: clean format init install

bench:
	cabal configure --enable-benchmarks
	cabal build
	cabal bench

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal configure
	cabal build
	cabal haddock --hyperlink-source
	if which open; then open dist/doc/html/threase/index.html; fi

hpc: test
	hpc report dist/hpc/tix/hspec/hspec.tix
	hpc markup --destdir=tmp dist/hpc/tix/hspec/hspec.tix
	if which open; then open tmp/hpc_index.html; fi

init:
	cabal update
	cabal --numeric-version | grep -F 1.18.0.2 || cabal install cabal-install
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
