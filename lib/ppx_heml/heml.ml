open Base
module I = Ocaml_common.Parser.MenhirInterpreter

let process_token parser token start_pos end_pos =
  let parser = I.offer parser (token, start_pos, end_pos) in
  let rec aux parser =
    match parser with
    | I.InputNeeded _ ->
        parser
    | I.AboutToReduce _ | I.Shifting _ ->
        aux (I.resume parser)
    | I.Rejected ->
        failwith "Rejected"
    | I.Accepted _ ->
        parser
    | I.HandlingError _ ->
        failwith "Error"
    (* aux (I.resume parser) *)
  in
  aux parser

let rec read_until_eof parser lexbuf =
  match Ocaml_common.Lexer.token lexbuf with
  | EOF ->
      parser
  | token ->
      read_until_eof
        (process_token parser token lexbuf.lex_start_p lexbuf.lex_curr_p)
        lexbuf

let rec read_until_complete parser lexbuf =
  match parser with
  | I.Accepted res ->
      res
  | _ ->
      let token = Ocaml_common.Lexer.token lexbuf in
      read_until_complete
        (process_token parser token lexbuf.lex_curr_p lexbuf.lex_curr_p)
        lexbuf

(** [set_lexbuf_position lexbuf pos] sets the lexing buffer to the given
    position.
    This should be done before parsing the lexbuf to ensure that the resulting
    positions are correct. *)
let set_lexbuf_position (lexbuf : Lexing.lexbuf) (pos : Lexing.position) =
  lexbuf.lex_curr_p <- pos ;
  lexbuf.lex_start_p <- pos ;
  lexbuf.lex_abs_pos <- pos.pos_cnum ;
  lexbuf.lex_last_pos <- pos.pos_cnum ;
  lexbuf.lex_last_action <- pos.pos_cnum

(** A text block represents a continuous block of plain text. *)
module Text = struct
  type t =
    { text: string
          (** The text itself. *)
    ; loc_start: Lexing.position
          (** The starting position of the block of text in the source
              file. *)
    ; loc_end: Lexing.position
          (** The end position of the block of text in the source file. *) }
end

module String_block = struct
  type t =
    { field: string
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end

module Int_block = struct
  type t =
    { field: string
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end

module Code_block = struct
  type t =
    { code: string
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end

module rec Element : sig
  type t =
    { name: string
    ; attributes: (string * string) list
    ; contents: Ast.t list }
end = struct
  type t =
    { name: string
    ; attributes: (string * string) list
    ; contents: Ast.t list }
end

(** The embedded OCaml abstract syntax tree.
    Because of the possibility of incomplete code blocks across multiple
    lines, the AST needs to be parsed by an incremental OCaml parser,
    with intermediate blocks also parsed by the same parser. *)
and Ast : sig
  type t =
    | Text of Text.t
    | String_block of String_block.t
    | Int_block of Int_block.t
    | Code_block of Code_block.t
    | Element of Element.t
end = struct
  type t =
    | Text of Text.t
    | String_block of String_block.t
    | Int_block of Int_block.t
    | Code_block of Code_block.t
    | Element of Element.t
end

(** An EML AST parser. The parser uses an internal Menhir incremental
      OCaml parser. *)
module Parser = struct
  type t = {parser: Parsetree.expression I.checkpoint}

  let create ~loc_start =
    let parser = Ocaml_common.Parser.Incremental.parse_expression loc_start in
    let lexbuf =
      Lexing.from_string
        {|(let b = Buffer.create 1000 in let write = Buffer.add_string b in|}
    in
    let parser = read_until_eof parser lexbuf in
    {parser}

  (** [to_parsetree parser] returns the resulting expression from the parser,
        which should evaluate to a string. *)
  let to_parsetree (parser : t) =
    let lexbuf = Lexing.from_string "Buffer.contents b)" in
    read_until_complete parser.parser lexbuf

  let parse_text (parser : t) (text : Text.t) =
    let lexbuf =
      Lexing.from_string
        ("write {__heml_string|" ^ text.text ^ "|__heml_string};")
    in
    let parser = read_until_eof parser.parser lexbuf in
    {parser= read_until_eof parser lexbuf}

  let parse_string_block (parser : t) (sb : String_block.t) =
    let lexbuf = Lexing.from_string "write" in
    let parser = read_until_eof parser.parser lexbuf in
    let lexbuf = Lexing.from_string (String.concat [sb.field; ";"]) in
    set_lexbuf_position lexbuf sb.loc_start ;
    {parser= read_until_eof parser lexbuf}

  let parse_int_block (parser : t) (ib : Int_block.t) =
    let lexbuf = Lexing.from_string "write (Stdlib.string_of_int " in
    let parser = read_until_eof parser.parser lexbuf in
    let lexbuf = Lexing.from_string (String.concat [ib.field; ");"]) in
    set_lexbuf_position lexbuf ib.loc_start ;
    {parser= read_until_eof parser lexbuf}

  let parse_code_block (parser : t) (cb : Code_block.t) =
    let lexbuf = Lexing.from_string cb.code in
    set_lexbuf_position lexbuf cb.loc_start ;
    {parser= read_until_eof parser.parser lexbuf}

  let rec parse_element (parser : t) (el : Element.t) =
    let start_tag = String.concat ["<"; el.name] in
    let start_tag =
      List.fold el.attributes ~init:start_tag ~f:(fun acc (k, v) ->
          String.concat ~sep:" " [acc; k; "=\""; v; "\""] )
    in
    let start_tag = String.concat [start_tag; ">"] in
    let lexbuf =
      Lexing.from_string
        ("write {__heml_element|" ^ start_tag ^ "\n|__heml_element};")
    in
    let parser = {parser= read_until_eof parser.parser lexbuf} in
    let parser = List.fold el.contents ~init:parser ~f:parse in
    let end_tag = String.concat ["</"; el.name; ">"] in
    let lexbuf =
      Lexing.from_string
        ("write {__heml_element|\n" ^ end_tag ^ "\n|__heml_element};")
    in
    {parser= read_until_eof parser.parser lexbuf}

  and parse (parser : t) = function
    | Ast.Text t ->
        parse_text parser t
    | String_block sb ->
        parse_string_block parser sb
    | Int_block ib ->
        parse_int_block parser ib
    | Code_block cb ->
        parse_code_block parser cb
    | Element el ->
        parse_element parser el
end
