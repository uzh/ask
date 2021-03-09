.PHONY: all test clean

build:
	opam exec -- dune build

build-watch:
	opam exec -- dune build --watch

clean:
	opam exec -- dune clean

test:
	SIHL_ENV=test ./_build/default/test/test.exe test

doc:
	opam exec -- dune build @doc
