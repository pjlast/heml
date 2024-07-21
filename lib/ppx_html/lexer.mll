{
  open Parser
}

let whitespace = [' ' '\t' '\r' '\n']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let tagname = alphanum+

rule read = parse
  | "<"          { LANGLE }
  | ">"          { RANGLE }
  | "</"         { LANGLESLASH }
  | "/"          { SLASH }
  | "="          { EQ }
  | "\""         { DOUBLE_QUOTE }
  | "\'"         { SINGLE_QUOTE }
  | whitespace+  { read lexbuf }
  | tagname as t {
      match t with
      | "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta" | "param" | "source" | "track" | "wbr" -> VOID_TAGNAME t
      | _ -> TAGNAME t
    }
  | eof          { EOF }
  | _            { raise (Failure "Unexpected character") }
