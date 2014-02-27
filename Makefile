.PHONY: clean format init install

bench:
	cabal bench

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi

format:
	git ls-files '*.hs' | xargs -n 1 scan --inplace-modify
	git ls-files '*.hs' | xargs stylish-haskell --inplace

haddock:
	cabal haddock --hyperlink-source

init:
	cabal update
	cabal --numeric-version | grep -F 1.18.0.2 || cabal install cabal-install
	cabal sandbox init

install: init
	cabal install --enable-benchmarks --enable-tests --flags=documentation --only-dependencies

repl:
	cabal repl

run:
	cabal run

test:
	cabal test
