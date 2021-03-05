# ocaml/opam post create script

sudo chown -R opam: _build

# remove and update default ocaml remote
# make sure that opam finds latest package versions
# (e.g. otherwise alcotest latest version is 1.1.0 instead of 1.2.1)
opam remote remove --all default
opam remote add default https://opam.ocaml.org

# Pins
opam pin add -yn ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git
# due to no current releases, pin to master
# opam pin add -yn sihl https://github.com/oxidizing/sihl.git\#0.2.0

# install opam packages
# e.g. when developing with emax, add also: utop merlin ocamlformat
opam install caqti-driver-mariadb ocamlformat ocaml-lsp-server sihl

# install project dependancies
opam pin add -y -n question .
OPAMSOLVERTIMEOUT=180 opam depext -y question
opam install --deps-only -y question --with-test
opam install -y alcotest-lwt

# initialize project and update environmemnt
opam init
eval $(opam env)
