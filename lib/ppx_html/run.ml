let parse s =
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.main Lexer.read lexbuf) with
  | Parser.Error ->
      Printf.fprintf stderr "Parser error\n" ;
      exit (-1)
