(lang dune 3.16)

(name ppx_heml)

(generate_opam_files true)

(source
 (github pjlast/heml))

(authors "Petri-Johan Last")

(maintainers "Petri-Johan Last")

(license MIT)

(using menhir 3.0)

(package
 (name ppx_heml)
 (synopsis "PPX for deriving HTML templates")
 (description "PPX for deriving HTML templates. Includes %heml")
 (depends (ocaml (>= 5.0)) dune ppxlib base ppx_string (menhir :build) (ppx_inline_test :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
