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
}

let newline = '\r' | '\n' | "\r\n"
let content = [^ '<' '>' '\n' '/']*

              rule read =
              parse
            | newline { read lexbuf }
            | '<' { read_open_tag (Buffer.create 17) lexbuf }
            | "</" { read_closing_tag (Buffer.create 17) lexbuf }
            | "<%=" { read_ocaml_block (Buffer.create 17) lexbuf }
            | "<%s=" { read_string_block (Buffer.create 17) lexbuf }
            | '>' { RIGHT_ANGLE }
            | '%' { PERCENTAGE }
            | '=' { EQUALS }
            | content { CONTENTS (Lexing.lexeme lexbuf) }
            | eof { EOF }
            | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string_block buf =
  parse
| _ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_two_chars lexbuf with
    | Some ('%', '>') -> STRING_BLOCK (Buffer.contents buf)
    | _ -> read_string_block buf lexbuf
  }

and read_ocaml_block buf =
  parse
| [^ '%']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_two_chars lexbuf with
    | Some ('%', '>') -> OCAML_CODE (Buffer.contents buf)
    | _ -> read_ocaml_block buf lexbuf
  }

and read_open_tag buf =
  parse
| ['a'-'z' 'A'-'Z' '-']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_char lexbuf with
    | Some ' ' ->  OPEN_TAG (Buffer.contents buf)
    | Some '>' ->  OPEN_TAG (Buffer.contents buf)
    | _ -> read_open_tag buf lexbuf
  }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_closing_tag buf =
  parse
| ['a'-'z' 'A'-'Z' '-']+ {
    Buffer.add_string buf (Lexing.lexeme lexbuf);
    match peek_next_char lexbuf with
    | Some '>' -> CLOSE_TAG (Buffer.contents buf)
    | _ -> read_closing_tag buf lexbuf
  }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
