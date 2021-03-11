(lang dune 2.8)

(generate_opam_files true)

(name quest)

(version 0.1.0)

(authors "Marc Biedermann")

(license MIT)

(maintainers "marc.biedermann@econ.uzh.ch")

(homepage "https://gitlab.uzh.ch/econ/research/tools/quest")

(bug_reports "https://gitlab.uzh.ch/econ/research/tools/quest/-/issues")

(documentation "https://gitlab.uzh.ch/econ/research/tools/quest")

(package
 (name quest)
 (synopsis "A package to create questionnaires")
 (description "Generating and answering questions should be easy")
 (depends
  ;; General system dependencies
  (dune
   (>= 2.8))
  (ocaml
   (>= 4.11))
  (lwt
   (>= 5.3.0))
  ;; Sihl
  (sihl
   (= 0.3.0~rc2))
  (sihl-storage
   (= 0.3.0~rc2))
  (conformist
   (= 0.2.0))
  ;; Database
  (caqti-driver-mariadb
   (>= 1.2.1))
  ;; Helpers & Generation
  (ppx_fields_conv
   (>= 0.13.0))
  ;; Dev dependencies
  (ocamlformat :dev)))
