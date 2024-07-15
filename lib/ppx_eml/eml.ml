type string_template =
  { str : string
  ; loc_start : Lexing.position
  ; loc_end : Lexing.position
  }

type string_block =
  | Str of string
  | Str_tmpl of string_template
  | Code_tmpl of string
