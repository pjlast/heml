{
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

let whitespace = [' ' '\t' '\r' '\n']

rule read =
parse
| '\n' { let sp = clone_pos lexbuf.Lexing.lex_curr_p in Lexing.new_line lexbuf; let ep = clone_pos lexbuf.Lexing.lex_curr_p in STRING ("\n", sp, ep) }
| "<%s=" { read_string_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| "<%i=" { read_int_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| "<%=" { read_code_block (Buffer.create 30) (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
| '%' { PERCENTAGE }
| "%>" { PERCENTAGEGT }
| '>' { GT }
| '<' { LT }
| '=' { EQ }
| "<" ['a'-'z' 'A'-'Z' '0'-'9' '-' '.' '_']+ whitespace*
  {
    let sp = clone_pos lexbuf.Lexing.lex_start_p in
    let tag = Lexing.lexeme lexbuf in
    let len = String.length tag in
    let tag = String.sub tag 1 (len - 1) in
    let tag = String.trim tag in
    read_start_tag sp tag [] lexbuf
  }
| "</" ['a'-'z' 'A'-'Z' '0'-'9' '-' '.' '_']+ whitespace* ">"
  {
    let tag = Lexing.lexeme lexbuf in
    let len = String.length tag in
    let tag = String.sub tag 2 (len - 3) in
    let tag = String.trim tag in
    let pos = clone_pos lexbuf.Lexing.lex_curr_p in
    END_TAG (tag, pos, pos)
  }
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

and read_start_tag sp name attrs =
  parse
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace*
    {
      let attribute_name = String.trim (Lexing.lexeme lexbuf) in
      read_start_tag sp name ((attribute_name, Heml.String ("")) :: attrs) lexbuf
    }
      | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* [^ ' ' '"' '\'' '=' '<' '>' '`' '{' '}']+ whitespace*
    {
      let attribute = String.trim (Lexing.lexeme lexbuf) in
      let attr_parts = String.split_on_char '=' attribute in
      let attribute_name = String.trim (List.hd attr_parts) in
      let attribute_value = Heml.String (String.trim (List.hd (List.tl attr_parts))) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '\'' [^ '\'' '<' '>' '`']* '\'' whitespace*
    {
      let attribute = String.trim (Lexing.lexeme lexbuf) in
      let attr_parts = String.split_on_char '=' attribute in
      let attribute_name = String.trim (List.hd attr_parts) in
      let attribute_value = String.trim (List.hd (List.tl attr_parts)) in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.String (String.sub attribute_value 1 (val_len - 2)) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '\"' [^ '\"' '<' '>' '`']* '\"' whitespace*
    {
      let attribute = String.trim (Lexing.lexeme lexbuf) in
      let attr_parts = String.split_on_char '=' attribute in
      let attribute_name = String.trim (List.hd attr_parts) in
      let attribute_value = String.trim (List.hd (List.tl attr_parts)) in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.String (String.sub attribute_value 1 (val_len - 2)) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
      | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '{' [^ '{' '}' '<' '>' '`']* '}' whitespace*
    {
      let mtch = Lexing.lexeme lexbuf in
      let attr_parts = String.split_on_char '=' mtch in
      let attribute_name = String.trim (List.hd attr_parts) in
      let attribute_value = List.hd (List.tl attr_parts) in
      let attr_sp = clone_pos lexbuf.Lexing.lex_curr_p in
      let attr_sp = {attr_sp with pos_cnum = attr_sp.pos_cnum - (String.length (Base.String.lstrip attribute_value)) + 1} in
      let attribute_value = String.trim attribute_value in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.Variable (String.sub attribute_value 1 (val_len - 2), attr_sp, attr_sp) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | '>' { START_TAG_WITH_ATTRS (name, attrs, sp, sp) }
  | "/>" { SELF_CLOSING_START_TAG_WITH_ATTRS (name, attrs, sp, sp) }

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
