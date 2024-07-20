open Core
open Lexing
open Html

let succeed v = Ok v

let fail lexbuf _ =
  let msg =
    Fmt.str "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
  in
  Error msg

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg ;
      Ok []
  | Parser.Error ->
      fprintf stderr "%a: syntax error\n" print_position lexbuf ;
      exit (-1)
  | Html.Wrong_closing_tag tag_err ->
      Error
        { tag_name= tag_err.tag_name
        ; expected_tag_name= tag_err.expected_tag_name
        ; lexbuf }

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Ok [value] -> (
    match value with
    | Element v ->
        print_endline v.name ; parse_and_print lexbuf
    | _ ->
        print_endline "Unimplemented" )
  | _ ->
      ()

let parse s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname= s} ;
  parse_with_error lexbuf
