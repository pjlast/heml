open Base
module I = Ocaml_common.Parser.MenhirInterpreter

type string_template =
  { str: string
  ; loc_start: Lexing.position
  ; loc_end: Lexing.position }

type code_template =
  { code: string
  ; contents: string_block list
  ; next_code_template: code_template option
  ; loc_start: Lexing.position }

and string_block =
  | Str of string
  | Str_tmpl of string_template
  | Int_tmpl of string_template
  | Code_tmpl of code_template

let rec string_block_to_string (b : Buffer.t) = function
  | Str s ->
      Buffer.add_string b ("Buffer.add_string b \"" ^ s ^ "\";")
  | Str_tmpl s ->
      Buffer.add_string b ("Buffer.add_string b " ^ s.str ^ ";")
  | Int_tmpl i ->
      Buffer.add_string b
        ("Buffer.add_string b (string_of_int ( " ^ i.str ^ " ));")
  | Code_tmpl ct ->
      code_tmpl_to_string b ct

and repeat ~n s =
  let rec aux ~n' str = if n' = 1 then str else s ^ aux ~n':(n' - 1) str in
  aux ~n':n s

and code_tmpl_to_string b ct =
  Buffer.add_string b ct.code ;
  List.iter ct.contents ~f:(string_block_to_string b) ;
  let () =
    match ct.next_code_template with
    | Some next_ct ->
        code_tmpl_to_string b next_ct
    | None ->
        Buffer.add_string b " ); " ; ()
  in
  ()

let succeed v = Ok v

let fail lexbuf _ =
  let msg =
    Stdlib.Format.sprintf "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start lexbuf)
  in
  Error msg

let read_token parser token start_pos end_pos =
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
    | I.HandlingError env ->
        aux (I.resume parser)
  in
  aux parser

let rec read_until_eof parser lexbuf =
  match Ocaml_common.Lexer.token lexbuf with
  | EOF ->
      Stdio.print_endline "EOF" ; parser
  | token ->
      read_until_eof
        (read_token parser token lexbuf.lex_start_p lexbuf.lex_curr_p)
        lexbuf

let rec read_until_complete parser lexbuf =
  match parser with
  | I.Accepted res ->
      res
  | _ ->
      let token = Ocaml_common.Lexer.token lexbuf in
      read_until_complete
        (read_token parser token lexbuf.lex_curr_p lexbuf.lex_curr_p)
        lexbuf

let string_block_list_to_string b l =
  Buffer.add_string b "let b = Buffer.create 100 in " ;
  List.iter ~f:(string_block_to_string b) l ;
  Buffer.add_string b "Buffer.contents b"

let string_to_ast parser s =
  Stdio.print_endline "String to ast" ;
  let lexbuf = Lexing.from_string ("Buffer.add_string b \"" ^ s ^ "\";") in
  read_until_eof parser lexbuf

let string_template_to_ast parser loc_start1 {str; loc_start; loc_end} =
  Stdio.print_endline "String template to ast" ;
  let lexbuf = Lexing.from_string "Buffer.add_string b " in
  let parser = read_until_eof parser lexbuf in
  let lexbuf = Lexing.from_string (str ^ ";") in
  lexbuf.lex_curr_p <- loc_start ;
  lexbuf.lex_start_p <- loc_start ;
  lexbuf.lex_abs_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_action <- loc_start.pos_cnum ;
  read_until_eof parser lexbuf

let int_template_to_ast parser {str; loc_start; loc_end} =
  let lexbuf =
    Lexing.from_string ("Buffer.add_string b (string_of_int " ^ str ^ ");")
  in
  read_until_eof parser lexbuf

let rec string_block_to_ast parser loc_start = function
  | Str s ->
      string_to_ast parser s
  | Str_tmpl s ->
      string_template_to_ast parser loc_start s
  | Int_tmpl i ->
      int_template_to_ast parser i
  | Code_tmpl ct ->
      code_tmpl_to_ast parser ct

and code_tmpl_to_ast parser {code; contents; next_code_template; loc_start} =
  let lexbuf = Lexing.from_string code in
  lexbuf.lex_curr_p <- loc_start ;
  lexbuf.lex_start_p <- loc_start ;
  lexbuf.lex_abs_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_action <- loc_start.pos_cnum ;
  let parser = read_until_eof parser lexbuf in
  let parser =
    List.fold contents ~init:parser ~f:(fun parser block ->
        string_block_to_ast parser loc_start block )
  in
  match next_code_template with
  | Some next_ct ->
      code_tmpl_to_ast parser next_ct
  | None ->
      let lexbuf = Lexing.from_string ");" in
      let parser = read_until_eof parser lexbuf in
      parser

let string_block_list_to_ast ~loc l =
  let parser =
    Ocaml_common.Parser.Incremental.parse_expression loc.Location.loc_start
  in
  let lexbuf = Lexing.from_string " (let b = Buffer.create 1000 in" in
  let parser = read_until_eof parser lexbuf in
  let parser =
    List.fold l ~init:parser ~f:(fun parser block ->
        string_block_to_ast parser loc.loc_start block )
  in
  let lexbuf = Lexing.from_string "Buffer.contents b) " in
  let res = read_until_complete parser lexbuf in
  let res = res |> Ppxlib.Parse.Of_ocaml.copy_expression in
  Stdio.print_endline (res |> Ppxlib.Pprintast.string_of_expression) ;
  res
