SHELL := /usr/bin/env bash

build:
	cabal build -j

install: cabal.sandbox.config
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies

cabal.sandbox.config:
	cabal sandbox init

test:
	cabal install --enable-tests

clean:
	cabal clean
	rm -f cabal.sandbox.config .cabal-sandbox

doc:
	cabal haddock
