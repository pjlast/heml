open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  let res =
    try Ok (Parser.prog Lexer.read lexbuf) with
    | Lexer.SyntaxError (msg, sp, ep) -> Error (msg, sp, ep)
    | Parser.Error ->
        Error ("Parser error", lexbuf.lex_curr_p, lexbuf.lex_curr_p)
    | Heml.Ast.MismatchedTags (msg, startpos, endpos) ->
        Error (msg, startpos, endpos)
    | _ -> Error ("Unknown error", lexbuf.lex_curr_p, lexbuf.lex_curr_p)
  in
  res

let parse ~loc_start s =
  let lexbuf = Lexing.from_string ~with_positions:false s in
  lexbuf.lex_curr_p <- loc_start ;
  lexbuf.lex_start_p <- loc_start ;
  lexbuf.lex_abs_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_pos <- loc_start.pos_cnum ;
  lexbuf.lex_last_action <- loc_start.pos_cnum ;
  parse_with_error lexbuf
