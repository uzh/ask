(lang dune 2.8)

(generate_opam_files true)

(name ask)

(version 0.1.1)

(authors "Marc Biedermann")

(license MIT)

(maintainers "marc.biedermann@econ.uzh.ch")

(homepage "https://gitlab.uzh.ch/econ/research/tools/ask")

(bug_reports "https://gitlab.uzh.ch/econ/research/tools/ask/-/issues")

(documentation "https://gitlab.uzh.ch/econ/research/tools/ask")

(package
 (name ask)
 (synopsis "A package to create questionnaires")
 (description "Generating and answering questions should be easy")
 (depends
  dune
  (ocaml
   (>= 4.11))
  (sihl
   (= 0.4.0))
  (sihl-storage
   (= 0.4.0))
  (conformist
   (= 0.4.0))
  (lwt_ssl
   (>= 1.1.3))
  (ppx_fields_conv
   (>= 0.13.0))
  (alcotest-lwt
   (and
    (>= 1.3.0)
    :with-test))
  (caqti-driver-mariadb
   (and
    (>= 1.2.1)
    :with-test))
  (ocamlformat :dev)))

(package
 (name ask-integrator)
 (synopsis "A package to link questionnaires to a' type")
 (description "Generate a link between questionnaires and types")
 (depends
  dune
  (ocaml
   (>= 4.11))
  (ask
   (= :version))
  (sihl
   (= 0.4.0))
  (sihl-storage
   (= 0.4.0))
  (conformist
   (= 0.4.0))
  (lwt_ssl
   (>= 1.1.3))
  (ppx_fields_conv
   (>= 0.13.0))
  (alcotest-lwt
   (and
    (>= 1.3.0)
    :with-test))
  (caqti-driver-mariadb
   (and
    (>= 1.2.1)
    :with-test))
  (ocamlformat :dev)))
