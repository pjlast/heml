type closing_tag_error =
  { tag_name: string
  ; expected_tag_name: string
  ; lexbuf: Lexing.lexbuf }

exception
  Wrong_closing_tag of
    { tag_name: string
    ; expected_tag_name: string }

type tag = [`Element of string]

type closing_tag = [`Element of string]

type element =
  { name: string
  ; contents: el list }

and code_element =
  { code: string
  ; contents: el list }

and el =
  | Element of element
  | Text of string
  | Ocaml_code of code_element

let rec el_to_string indent (el : el) =
  match el with
  | Element e ->
      let estrings = List.map (el_to_string (indent ^ "  ")) e.contents in
      let res = String.concat "\n" estrings in
      Printf.sprintf "%s<%s>\n%s\n%s</%s>" indent e.name res indent e.name
  | Text t ->
      Printf.sprintf "%s%s" indent t
  | Ocaml_code code ->
      code.code
