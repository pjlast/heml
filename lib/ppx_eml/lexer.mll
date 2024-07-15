{
  (* open Lexing *)
  open Parser

  exception SyntaxError of string

  let peek_next_char lexbuf =
    let pos = lexbuf.Lexing.lex_curr_pos in
    if pos < Bytes.length lexbuf.Lexing.lex_buffer then
      Some (Bytes.get lexbuf.Lexing.lex_buffer pos)
    else
      None

  let peek_next_two_chars lexbuf =
    let pos = lexbuf.Lexing.lex_curr_pos in
    let buf_len = Bytes.length lexbuf.Lexing.lex_buffer in
    if pos + 1 < buf_len then
      Some (Bytes.get lexbuf.Lexing.lex_buffer pos, Bytes.get lexbuf.Lexing.lex_buffer (pos + 1))
    else
      None

  
  let clone_pos {Lexing.pos_cnum; pos_lnum; pos_bol; pos_fname} = {Lexing.pos_cnum; pos_lnum; pos_bol; pos_fname}
}

rule read =
  parse
  | '\n' { Lexing.new_line lexbuf; read lexbuf }
  | "<%=" { print_endline (Printf.sprintf "<= %i" lexbuf.Lexing.lex_curr_p.pos_lnum ); read_string_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
  | "<%" { read_code_block (Buffer.create 30) (clone_pos lexbuf.lex_start_p) lexbuf }
  | '%' { PERCENTAGE }
  | '>' { GT }
  | eof { EOF }
  | [^ '<' '%' '>' '\n']* { print_endline (Printf.sprintf "current_poss: %i" lexbuf.lex_curr_p.pos_cnum); STRING (Lexing.lexeme lexbuf) }

and read_string_block buf sp =
  parse
  | _ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_two_chars lexbuf with
    | Some ('%', '>') -> let ep = (clone_pos lexbuf.Lexing.lex_start_p) in
    STRING_BLOCK (Buffer.contents buf, sp, ep)
    | _ -> read_string_block buf sp lexbuf
  }

and read_code_block buf sp =
  parse
  | _ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_two_chars lexbuf with
    | Some ('%', '>') -> let ep = lexbuf.Lexing.lex_curr_p in
    STRING_BLOCK (Buffer.contents buf, sp, ep)
    | _ -> read_code_block buf sp lexbuf
  }
