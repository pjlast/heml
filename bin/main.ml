(* open Core                          *)
(* open Heml *)
(* open Lexing *)

let _ =
  {%heml|
<div>
  <%= ^ String.uppercase_ascii ( %>
  <button>
    Hello
  </button>
  <p>
    h
  </p>
  <%= ) ^ %>
</div>
|}
;;

let valid_greeting name =
  {%eml|Hello <%= name %>|}

let invalid_greeting (number : int) =
  {%eml|Hello <%= number %>|}


(* let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try Ok (Parser.prog Lexer.read lexbuf) with
  | Lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    Ok []
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
  | _ -> Error lexbuf.lex_curr_p
;;

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Ok [ value ] ->
    (match value with
     | Element v ->
       print_endline v.name;
       print_endline v.contents;
       parse_and_print lexbuf
     | _ -> print_endline "Unimplemented")
  | _ -> ()
;;

let loop filename () =
  let lexbuf = Lexing.from_string filename in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_with_error lexbuf
;; *)

let () = print_endline xx
