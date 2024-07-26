{
open Base
open Parser
open Lexing

exception SyntaxError of string * Lexing.position * Lexing.position

(** Like Base.String.substr_index but returns a Lexing.position *)
let substr_index_pos ?(start_pos = Lexing.dummy_pos) str ~pattern =
  let open Lexing in
  let strs = String.split_lines str in
  let rec loop ~pos lines =
    match lines with
    | [] -> None
    | line :: lines -> (
      match String.substr_index line ~pattern with
      | None ->
          loop
            ~pos:
              { pos with
                pos_cnum = pos.pos_cnum + 1 + String.length line
              ; pos_lnum = pos.pos_lnum + 1
              ; pos_bol = pos.pos_cnum + 1 + String.length line }
            lines
      | Some n -> Some {pos with pos_cnum = pos.pos_cnum + n} )
  in
  loop ~pos:start_pos strs

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
  | '<' { read_open_angle (clone_pos lexbuf.Lexing.lex_curr_p) lexbuf }
  | "%>" { PERCENTAGEGT }
  | eof { EOF }
  | [^ '<'] {
      let str = Lexing.lexeme lexbuf in
      if String.equal str "\n" then
        Lexing.new_line lexbuf;
      let sp = clone_pos lexbuf.Lexing.lex_start_p in
      let buf = (Buffer.create 30) in
      match peek_next_char lexbuf with
      | None -> STRING (str, sp, sp)
      | Some ('<') -> STRING (str, sp, sp)
      | _ -> Buffer.add_string buf str; read_string buf sp lexbuf
    }
  | _ {
      raise (SyntaxError ("Unexpected character", lexbuf.lex_curr_p, lexbuf.lex_curr_p))
    }

and read_open_angle sp =
  parse
  | "!--" {
      let sp = { sp with pos_cnum = sp.pos_cnum + 3 } in
      read_comment (Buffer.create 30) sp lexbuf
    }
  | "/" ['a'-'z' 'A'-'Z' '0'-'9' '-' '.' '_']+ whitespace* ">" {
      let tag = Lexing.lexeme lexbuf in
      let sp = { sp with pos_cnum = sp.pos_cnum + 1 } in
      let len = String.length tag in
      let tag = Stdlib.String.sub tag 1 (len - 2) in
      let tag = String.strip tag in
      let ep = { sp with pos_cnum = sp.pos_cnum + String.length tag } in
      END_TAG (tag, sp, ep)
    }
  | ['a'-'z' 'A'-'Z' '0'-'9' '-' '.' '_']+ whitespace* {
      let sp = { sp with pos_cnum = sp.pos_cnum - 1 } in
      let tag = Lexing.lexeme lexbuf in
      let tag = String.strip tag in
      read_start_tag sp tag [] lexbuf
    }
  | "%s=" {
      let sp = { sp with pos_cnum = sp.pos_cnum + 3 } in
      read_string_block (Buffer.create 30) sp lexbuf
    }
  | "%raw=" {
      let sp = { sp with pos_cnum = sp.pos_cnum + 5 } in
      read_raw_block (Buffer.create 30) sp lexbuf
    }
  | "%i=" {
      let sp = { sp with pos_cnum = sp.pos_cnum + 3 } in
      read_int_block (Buffer.create 30) sp lexbuf
    }
  | "%=" {
      let sp = { sp with pos_cnum = sp.pos_cnum + 2 } in
      read_code_block (Buffer.create 30) sp lexbuf
    }
  | "!DOCTYPE " whitespace* ['a'-'z']+ whitespace* ">" {
      let el = Lexing.lexeme lexbuf in
      let el = Stdlib.String.sub el 9 ((String.length el) - 10) in
      let el = String.strip el in
      DOCTYPE el
    }

(* https://html.spec.whatwg.org/multipage/syntax.html#comments *)
and read_comment buf sp =
  parse
  | "-->" {
      let comment = Buffer.contents buf in

      if String.is_prefix comment ~prefix:">" then
        raise (SyntaxError ("Comments cannot start with >",
          sp,
          {sp with pos_cnum = sp.pos_cnum + 1})
        );

      if String.is_prefix comment ~prefix:"->" then
        raise (SyntaxError ("Comments cannot start with ->",
          sp,
          {sp with pos_cnum = sp.pos_cnum + 2})
        );

      match substr_index_pos ~start_pos:sp comment ~pattern:"<!--" with
      | Some pos -> raise (SyntaxError ("Comments cannot contain <!--",
          pos,
          {pos with pos_cnum = pos.pos_cnum + 4})
        );
      | None -> ();

      match substr_index_pos ~start_pos:sp comment ~pattern:"-->" with
      | Some pos -> raise (SyntaxError ("Comments cannot contain -->",
          pos,
          {pos with pos_cnum = pos.pos_cnum + 3})
        );
      | None -> ();

      match substr_index_pos ~start_pos:sp comment ~pattern:"--!>" with
      | Some pos -> raise (SyntaxError ("Comments cannot contain --!>",
          pos,
          {pos with pos_cnum = pos.pos_cnum + 4})
        );
      | None -> ();

      let ep = lexbuf.Lexing.lex_curr_p in
      if String.is_suffix comment ~suffix:"<!-" then
        raise (SyntaxError ("Comments cannot end with <!-",
          {ep with pos_cnum = ep.pos_cnum - 6},
          {ep with pos_cnum = ep.pos_cnum - 3})
        );

      COMMENT comment
    }
  | '\n' { Lexing.new_line lexbuf; Buffer.add_char buf ('\n');
           read_comment buf sp lexbuf
    }
  | _ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf sp lexbuf
    }
  | eof { raise (SyntaxError ("Unterminated comment",
        {sp with pos_cnum = sp.pos_cnum - 4},
        sp))
    }

and read_string buf sp =
    parse
  | '\n' { Lexing.new_line lexbuf; Buffer.add_char buf ('\n');
           match peek_next_char lexbuf with
           | None | Some ('<') | Some ('%') | Some ('>') -> STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
           | _ -> read_string buf sp lexbuf
         }
  | eof { STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p) }
  | [^ '<' '%' '>' '\n']* { Buffer.add_string buf (Lexing.lexeme lexbuf);
      match peek_next_char lexbuf with
      | Some ('<') | Some ('%') | Some ('>') -> STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
      | _ -> read_string buf sp lexbuf
    }
  | _ {
      STRING (Buffer.contents buf, sp, clone_pos lexbuf.Lexing.lex_curr_p)
    }

and read_string_block buf sp =
    parse
  | _ {
      let c = Lexing.lexeme lexbuf in
      if String.equal c "\n" then
        Lexing.new_line lexbuf;
      Buffer.add_string buf (c);
      match peek_next_two_chars lexbuf with
      | Some ('%', '>') -> let ep = (clone_pos lexbuf.Lexing.lex_start_p) in
        STRING_BLOCK (Buffer.contents buf, sp, ep)
      | _ -> read_string_block buf sp lexbuf
    }

and read_raw_block buf sp =
  parse
  | "%>" {
      RAW_BLOCK (
        Buffer.contents buf,
        sp,
        clone_pos lexbuf.Lexing.lex_start_p
      )
    }
  | '\n' {
      Lexing.new_line lexbuf;
      Buffer.add_char buf '\n';
      read_raw_block buf sp lexbuf
    }
  | _ {
      Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_raw_block buf sp lexbuf
    }

and read_start_tag sp name attrs =
  parse
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace*
    {
      let attribute_name = String.strip (Lexing.lexeme lexbuf) in
      read_start_tag sp name ((attribute_name, Heml.String ("")) :: attrs) lexbuf
    }
      | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* [^ ' ' '"' '\'' '=' '<' '>' '`' '{' '}']+ whitespace*
    {
      let attribute = String.strip (Lexing.lexeme lexbuf) in
      let attr_parts = Stdlib.String.split_on_char '=' attribute in
      let attribute_name = String.strip (List.hd_exn attr_parts) in
      let attribute_value = Heml.String (String.strip (List.hd_exn (List.tl_exn attr_parts))) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '\'' [^ '\'' '<' '>' '`']* '\'' whitespace*
    {
      let attribute = String.strip (Lexing.lexeme lexbuf) in
      let attr_parts = Stdlib.String.split_on_char '=' attribute in
      let attribute_name = String.strip (List.hd_exn attr_parts) in
      let attribute_value = String.strip (List.hd_exn (List.tl_exn attr_parts)) in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.String (Stdlib.String.sub attribute_value 1 (val_len - 2)) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '\"' [^ '\"' '<' '>' '`']* '\"' whitespace*
    {
      let attribute = String.strip (Lexing.lexeme lexbuf) in
      let attr_parts = Stdlib.String.split_on_char '=' attribute in
      let attribute_name = String.strip (List.hd_exn attr_parts) in
      let attribute_value = String.strip (List.hd_exn (List.tl_exn attr_parts)) in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.String (Stdlib.String.sub attribute_value 1 (val_len - 2)) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
      | whitespace* ['a'-'z' 'A'-'Z' '0'-'9' '-']+ whitespace* "=" whitespace* '{' [^ '{' '}' '<' '>' '`']* '}' whitespace*
    {
      let mtch = Lexing.lexeme lexbuf in
      let attr_parts = Stdlib.String.split_on_char '=' mtch in
      let attribute_name = String.strip (List.hd_exn attr_parts) in
      let attribute_value = List.hd_exn (List.tl_exn attr_parts) in
      let attr_sp = clone_pos lexbuf.Lexing.lex_curr_p in
      let attr_sp = {attr_sp with pos_cnum = attr_sp.pos_cnum - (String.length (Base.String.lstrip attribute_value)) + 1} in
      let attribute_value = String.strip attribute_value in
      let val_len = String.length attribute_value in
      let attribute_value = Heml.Variable (Stdlib.String.sub attribute_value 1 (val_len - 2), attr_sp, attr_sp) in
      read_start_tag sp name ((attribute_name, attribute_value) :: attrs) lexbuf
    }
  | '>' { START_TAG_WITH_ATTRS (name, attrs, sp, sp) }
  | "/>" { SELF_CLOSING_START_TAG_WITH_ATTRS (name, attrs, sp, sp) }
  | _ { raise (SyntaxError (("Unexpected char: " ^ Lexing.lexeme lexbuf), sp, sp)) }

and read_int_block buf sp =
    parse
  | _ {
      let c = Lexing.lexeme lexbuf in
      if String.equal c "\n" then
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
      if String.equal c "\n" then
        Lexing.new_line lexbuf;
      Buffer.add_string buf (c);
      match peek_next_two_chars lexbuf with
      | Some ('%', '>') -> let ep = lexbuf.Lexing.lex_curr_p in
        CODE_BLOCK (Buffer.contents buf, sp, ep)
      | _ -> read_code_block buf sp lexbuf
    }
