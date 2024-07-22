{
  (* open Lexing *)
open Parser
open Lexing

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

let peek_next_six_chars lexbuf =
  let pos = lexbuf.Lexing.lex_curr_pos in
  let buf_len = Bytes.length lexbuf.Lexing.lex_buffer in
  if pos + 5 < buf_len then
    Some (Bytes.get lexbuf.Lexing.lex_buffer pos,
          Bytes.get lexbuf.Lexing.lex_buffer (pos + 1),
          Bytes.get lexbuf.Lexing.lex_buffer (pos + 2),
          Bytes.get lexbuf.Lexing.lex_buffer (pos + 3),
          Bytes.get lexbuf.Lexing.lex_buffer (pos + 4),
          Bytes.get lexbuf.Lexing.lex_buffer (pos + 5)
         )
  else
    None


let clone_pos {Lexing.pos_cnum; pos_lnum; pos_bol; pos_fname} = {Lexing.pos_cnum; pos_lnum; pos_bol; pos_fname}

let new_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
                         pos_lnum = pos.pos_lnum + 1;
                         pos_bol = pos.pos_cnum + 1;
                       }
}

rule read =
parse
| '\n' { let sp = clone_pos lexbuf.Lexing.lex_curr_p in Lexing.new_line lexbuf; let ep = clone_pos lexbuf.Lexing.lex_curr_p in STRING ("\n", sp, ep) }
| "<%s=" { read_string_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| "<%i=" { read_int_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| "<%=" { read_code_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| '%' { PERCENTAGE }
| '>' { GT }
| eof { EOF }
| _ { let sp = clone_pos lexbuf.Lexing.lex_curr_p in let buf = (Buffer.create 30) in Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf sp lexbuf }

and read_string buf sp =
    parse
  | '\n' { Lexing.new_line lexbuf; Buffer.add_char buf ('\n');
           match peek_next_char lexbuf with
           | None -> STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
           | Some ('<') | Some ('%') | Some ('>') -> STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
           | _ -> read_string buf sp lexbuf
         }
  | [^ '<' '%' '>' '\n']* { Buffer.add_string buf (Lexing.lexeme lexbuf);
                            match peek_next_char lexbuf with
                            | Some ('<') | Some ('%') | Some ('>') -> STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
                            | _ -> read_string buf sp lexbuf
                          }
  | _ { STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p) }

and read_string_block buf sp =
    parse
  | _ {
      let c = Lexing.lexeme lexbuf in
      if c = "\n" then
        Lexing.new_line lexbuf;
      Buffer.add_string buf (c);
      match peek_next_two_chars lexbuf with
      | Some ('%', '>') -> let ep = (clone_pos lexbuf.Lexing.lex_start_p) in
        STRING_BLOCK (Buffer.contents buf, sp, ep)
      | _ -> read_string_block buf sp lexbuf
    }

and read_int_block buf sp =
    parse
  | _ {
      let c = Lexing.lexeme lexbuf in
      if c = "\n" then
        Lexing.new_line lexbuf;
      Buffer.add_string buf (c);
      match peek_next_two_chars lexbuf with
      | Some ('%', '>') -> let ep = (clone_pos lexbuf.Lexing.lex_start_p) in
        INT_BLOCK (Buffer.contents buf, sp, ep)
      | _ -> read_int_block buf sp lexbuf
    }

and read_code_block buf sp =
    parse
  | _ {
      let c = Lexing.lexeme lexbuf in
      if c = "\n" then
        Lexing.new_line lexbuf;
      Buffer.add_string buf (c);
      match peek_next_two_chars lexbuf with
      | Some ('%', '>') -> let ep = lexbuf.Lexing.lex_curr_p in
        CODE_BLOCK (Buffer.contents buf, sp, ep)
      | _ -> read_code_block buf sp lexbuf
    }
