%token PERCENTAGE
%token GT
%token <string * Lexing.position * Lexing.position> STRING
%token <string * Lexing.position * Lexing.position> STRING_BLOCK
%token <string * Lexing.position * Lexing.position> INT_BLOCK
%token <string * Lexing.position * Lexing.position> CODE_BLOCK
%token EOF

%start <Eml.Ast.t list> prog
%%

prog:
  | EOF { [] }
  | res = blocks EOF { res }

blocks:
  | (* empty *) { [] }
  | t = template b = blocks { t :: b }

template:
  | str = STRING { let (text, sp, ep) = str in Eml.Ast.Text { text = text; loc_start = sp; loc_end = ep } }
  | strtmpl = STRING_BLOCK; PERCENTAGE; GT {
      let (s, sp, ep) = strtmpl in
      print_endline (Printf.sprintf "%d %d" sp.Lexing.pos_lnum (ep.Lexing.pos_lnum + 1));
      Eml.Ast.String_block {
        field = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | inttmpl = INT_BLOCK; PERCENTAGE; GT {
      let (s, sp, ep) = inttmpl in
      Eml.Ast.Int_block {
        field = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | codetmpl = CODE_BLOCK; PERCENTAGE; GT { 
      let (c, sp, ep) = codetmpl in
      Eml.Ast.Code_block {
        code = c;
        loc_start = sp;
        loc_end = ep
      }
    }
