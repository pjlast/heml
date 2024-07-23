(lang dune 3.16)

(name heml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Petri-Johan Last")

(maintainers "Petri-Johan Last")

(license LICENSE)

(documentation https://github.com/pjlast/heml)

(using menhir 3.0)

(package
 (name heml)
 (synopsis "PPX for deriving HTML templates")
 (description "PPX for deriving HTML templates. Includes %heml")
 (depends ocaml dune ppx_eml)
 (tags
  (topics "to describe" your project)))

(package
 (name ppx_heml)
 (synopsis "PPX for deriving HTML templates")
 (description "PPX for deriving HTML templates. Includes %heml")
 (depends ocaml dune ppxlib ppx_string menhir)
 (tags
  (topics "to describe" your project)))

(package
 (name ppx_eml)
 (synopsis "PPX for deriving embedded OCaml templates")
 (description "PPX for deriving embedded OCaml templates. Includes %eml")
 (depends ocaml dune ppxlib heml ocaml-compiler-libs base)
 (tags
  (topics "to describe" your project)))

(package
 (name ppx_html)
 (synopsis "PPX for deriving embedded OCaml templates")
 (description "PPX for deriving embedded OCaml templates. Includes %eml")
 (depends ocaml dune ppxlib heml)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
