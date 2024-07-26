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
 (depends ocaml dune ppxlib ppx_string menhir)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
