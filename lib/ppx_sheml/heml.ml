open Base
module I = Ocaml_common.Parser.MenhirInterpreter

let process_token parser token start_pos end_pos =
  let parser = I.offer parser (token, start_pos, end_pos) in
  let rec aux parser =
    match parser with
    | I.InputNeeded _ -> parser
    | I.AboutToReduce _
     |I.Shifting _ ->
        aux (I.resume parser)
    | I.Rejected -> failwith "Rejected"
    | I.Accepted _ -> parser
    | I.HandlingError _ -> failwith "Error"
    (* aux (I.resume parser) *)
  in
  aux parser

let rec read_until_eof parser lexbuf =
  match Ocaml_common.Lexer.token lexbuf with
  | EOF -> parser
  | token ->
      read_until_eof
        (process_token parser token lexbuf.lex_start_p lexbuf.lex_curr_p)
        lexbuf

let rec read_until_complete parser lexbuf =
  match parser with
  | I.Accepted res -> res
  | _ ->
      let token = Ocaml_common.Lexer.token lexbuf in
      read_until_complete
        (process_token parser token lexbuf.lex_curr_p lexbuf.lex_curr_p)
        lexbuf

(** [set_lexbuf_position lexbuf pos] sets the lexing buffer to the given
    position. This should be done before parsing the lexbuf to ensure that the
    resulting positions are correct. *)
let set_lexbuf_position (lexbuf : Lexing.lexbuf) (pos : Lexing.position) =
  lexbuf.lex_curr_p <- pos ;
  lexbuf.lex_start_p <- pos ;
  lexbuf.lex_abs_pos <- pos.pos_cnum ;
  lexbuf.lex_last_pos <- pos.pos_cnum ;
  lexbuf.lex_last_action <- pos.pos_cnum

(** [parse_string parser str] parses the given string using the given parser. If
    an EOF is encountered, the parser stops. [?loc] sets the start position of
    the parser. *)
let parse_string ?loc parser str =
  let lexbuf = Lexing.from_string str in
  ( match loc with
  | Some loc -> set_lexbuf_position lexbuf loc
  | None -> () ) ;
  read_until_eof parser lexbuf

(** [parse_string_done parser str] parses the given string using the given
    parser. If an EOF is encountered the parser consumes it and the result is
    returned. [?loc] sets the start position of the parser. *)
let parse_string_done ?loc parser str =
  let lexbuf = Lexing.from_string str in
  ( match loc with
  | Some loc -> set_lexbuf_position lexbuf loc
  | None -> () ) ;
  read_until_complete parser lexbuf

module Doctype = struct
  type t = {name: string}
end

(** A text block represents a continuous block of plain text. *)
module Text = struct
  type t =
    { text: string  (** The text itself. *)
    ; loc_start: Lexing.position
          (** The starting position of the block of text in the source file. *)
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

type attribute =
  | String of string
  | Variable of (string * Lexing.position * Lexing.position)

module Void_element = struct
  type t =
    { name: string
    ; attributes: (string * attribute) list
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end

module rec Element : sig
  type t =
    { name: string
    ; attributes: (string * attribute) list
    ; contents: Ast.t list
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end = struct
  type t =
    { name: string
    ; attributes: (string * attribute) list
    ; contents: Ast.t list
    ; loc_start: Lexing.position
    ; loc_end: Lexing.position }
end

(** The embedded OCaml abstract syntax tree. Because of the possibility of
    incomplete code blocks across multiple lines, the AST needs to be parsed by
    an incremental OCaml parser, with intermediate blocks also parsed by the
    same parser. *)
and Ast : sig
  type t =
    | Text of Text.t
    | String_block of String_block.t
    | Int_block of Int_block.t
    | Code_block of Code_block.t
    | Element of Element.t
    | Void_element of Void_element.t
    | Doctype of Doctype.t

  exception MismatchedTags of (string * Lexing.position * Lexing.position)
end = struct
  type t =
    | Text of Text.t
    | String_block of String_block.t
    | Int_block of Int_block.t
    | Code_block of Code_block.t
    | Element of Element.t
    | Void_element of Void_element.t
    | Doctype of Doctype.t

  exception MismatchedTags of (string * Lexing.position * Lexing.position)
end

(** An EML AST parser. The parser uses an internal Menhir incremental OCaml
    parser. *)
module Parser = struct
  type t = {parser: Parsetree.expression I.checkpoint}

  let create ~loc_start buf_size =
    let parser = Ocaml_common.Parser.Incremental.parse_expression loc_start in
    let parser =
      parse_string parser
        {%string|(let b = Buffer.create %{buf_size#Int} in let write = Buffer.add_string b in|}
    in
    {parser}

  (** [to_parsetree parser] returns the resulting expression from the parser,
      which should evaluate to a string. *)
  let to_parsetree (parser : t) =
    parse_string_done parser.parser "Buffer.contents b)"

  let parse_doctype (parser : t) (doctype : Doctype.t) =
    let parser =
      parse_string parser.parser
        {%string|write "<!DOCTYPE %{doctype.name}>\n";|}
    in
    {parser}

  let parse_text (parser : t) (text : Text.t) =
    let parser =
      parse_string parser.parser
        {%string|write {__heml_string|%{text.text}|__heml_string};|}
    in
    {parser}

  let parse_string_block (parser : t) (sb : String_block.t) =
    let parser = parse_string parser.parser "write" in
    let parser =
      parse_string ~loc:sb.loc_start parser (String.concat [sb.field; ";"])
    in
    {parser}

  let parse_int_block (parser : t) (ib : Int_block.t) =
    let parser = parse_string parser.parser "write (Stdlib.string_of_int " in
    let parser =
      parse_string ~loc:ib.loc_start parser {%string|%{ib.field});|}
    in
    {parser}

  let parse_code_block (parser : t) (cb : Code_block.t) =
    let parser = parse_string ~loc:cb.loc_start parser.parser cb.code in
    {parser}

  let parse_void_element (parser : t) (el : Void_element.t) =
    if String.contains el.name '.' then
      let loc_start =
        if String.is_prefix el.name ~prefix:"." then
          {el.loc_start with pos_cnum = el.loc_start.pos_cnum + 2}
        else
          {el.loc_start with pos_cnum = el.loc_start.pos_cnum + 1}
      in
      let name = String.chop_prefix_if_exists el.name ~prefix:"." in
      let parser = parse_string ~loc:loc_start parser.parser "write (" in
      let parser = parse_string ~loc:loc_start parser name in
      let parser =
        List.fold el.attributes ~init:parser ~f:(fun parser (k, v) ->
            match v with
            | String v ->
                parse_string parser
                  {%string|~%{k}:{__heml_attr|%{v}|__heml_attr}|}
            | Variable v ->
                let v, sp, _ep = v in
                let parser = parse_string parser {%string|~%{k}:|} in
                parse_string ~loc:sp parser {%string|%{v}|} )
      in
      {parser = parse_string ~loc:loc_start parser ");"}
    else
      let start_tag = {%string|<%{el.name}|} in
      let parser =
        parse_string parser.parser
          {%string|write {__heml_element|%{start_tag}|__heml_element};|}
      in
      let parser =
        List.fold el.attributes ~init:parser ~f:(fun parser (k, v) ->
            match v with
            | String v ->
                parse_string parser
                  {%string|write {__heml_attribute| %{k}="%{v}"|__heml_attribute};|}
            | Variable v ->
                let parser =
                  parse_string parser
                    {%string|write {__heml_attribute| %{k}="|__heml_attribute};|}
                in
                let v, sp, _ep = v in
                parse_string ~loc:sp parser {%string|write (%{v} ^ "\"");|} )
      in
      let parser = parse_string parser "write \">\";" in
      {parser}

  let rec parse_element (parser : t) (el : Element.t) =
    if String.contains el.name '.' then
      let loc_start =
        if String.is_prefix el.name ~prefix:"." then
          {el.loc_start with pos_cnum = el.loc_start.pos_cnum + 2}
        else
          {el.loc_start with pos_cnum = el.loc_start.pos_cnum + 1}
      in
      let name = String.chop_prefix_if_exists el.name ~prefix:"." in
      let parser = parse_string ~loc:loc_start parser.parser "write (" in
      let parser = parse_string ~loc:loc_start parser name in
      let parser =
        List.fold el.attributes ~init:parser ~f:(fun parser (k, v) ->
            match v with
            | String v ->
                parse_string ~loc:loc_start parser
                  {%string|~%{k}:{__heml_attr|%{v}|__heml_attr}|}
            | Variable v ->
                let v, sp, _ep = v in
                let parser = parse_string parser {%string|~%{k}:|} in
                parse_string ~loc:sp parser {%string|%{v}|} )
      in
      if List.is_empty el.contents then
        {parser = parse_string ~loc:loc_start parser {|"");|}}
      else
        let contents =
          {|(let b = Buffer.create 1000 in let write = Buffer.add_string b in|}
        in
        let parser = parse_string parser contents in
        let parser = List.fold el.contents ~init:{parser} ~f:parse in
        {parser = parse_string parser.parser "Buffer.contents b));"}
    else
      let start_tag = {%string|<%{el.name}|} in
      let parser =
        parse_string parser.parser
          {%string|write {__heml_element|%{start_tag}|__heml_element};|}
      in
      let parser =
        List.fold el.attributes ~init:parser ~f:(fun parser (k, v) ->
            match v with
            | String v ->
                parse_string parser
                  {%string|write {__heml_attribute| %{k}="%{v}"|__heml_attribute};|}
            | Variable v ->
                let parser =
                  parse_string parser
                    {%string|write {__heml_attribute| %{k}="|__heml_attribute};|}
                in
                let v, sp, _ep = v in
                parse_string ~loc:sp parser {%string|write (%{v} ^ "\"");|} )
      in
      let parser = parse_string parser "write \">\";" in
      let parser = List.fold el.contents ~init:{parser} ~f:parse in
      let parser =
        parse_string parser.parser
          {%string|write {__heml_element|</%{el.name}>|__heml_element};|}
      in
      {parser}

  and parse (parser : t) = function
    | Ast.Text t -> parse_text parser t
    | String_block sb -> parse_string_block parser sb
    | Int_block ib -> parse_int_block parser ib
    | Code_block cb -> parse_code_block parser cb
    | Element el -> parse_element parser el
    | Void_element ve -> parse_void_element parser ve
    | Doctype dt -> parse_doctype parser dt
end