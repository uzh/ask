.PHONY: build test-ask test-integrator test clean

build:
	opam exec -- dune build

watch:
	opam exec -- dune build --watch

clean:
	opam exec -- dune clean

test:
	SIHL_ENV=test ./_build/default/ask/test/ask_mariadb.exe
	SIHL_ENV=test ./_build/default/ask-integrator/test/ask_integrator_mariadb.exe

test-ask:
	SIHL_ENV=test ./_build/default/ask/test/ask_mariadb.exe

test-integrator:
	SIHL_ENV=test ./_build/default/ask-integrator/test/ask_integrator_mariadb.exe

doc:
	opam exec -- dune build @doc
