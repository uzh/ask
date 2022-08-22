# ocaml/opam post create script

sudo chown -R opam: _build

opam init -a --shell=zsh

# update default ocaml remote - make sure that opam finds latest package versions
# (e.g. otherwise alcotest latest version is 1.1.0 instead of 1.2.1)
opam remote remove --all default
opam repository add default --all-switches --set-default https://opam.ocaml.org

# ensure all system dependencies are installed
repo=https://github.com/oxidizing/sihl.git
opam pin add -yn sihl $repo
opam pin add -yn sihl-storage $repo
opam pin add -yn conformist https://github.com/oxidizing/conformist.git

opam pin add -yn .
opam depext -y ask ask-integrator

make deps
