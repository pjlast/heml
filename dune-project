(lang dune 3.16)

(name heml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name heml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

(package
 (name ppx_heml)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends ocaml dune ppxlib heml)
 (tags
  (topics "to describe" your project)))

(package
 (name ppx_eml)
 (synopsis "PPX for deriving octane tables, queries and more")
 (description "PPX for deriving octane tables, queries and more. Includes %query, %table")
 (depends ocaml dune ppxlib heml)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
