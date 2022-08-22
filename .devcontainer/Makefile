include ./Makefile

SHELL = zsh

.PHONY: deps
deps: ## Install development dependencies
	opam install -y dune-release ocamlformat ocaml-lsp-server
	opam install --unlock-base --deps-only --with-test --with-doc -y .
