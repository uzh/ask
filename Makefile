.PHONY: all test clean

build:
	opam exec -- dune build

build-watch:
	opam exec -- dune build --watch

clean:
	opam exec -- dune clean

test:
	opam exec -- dune test

doc:
	opam exec -- dune build @doc
