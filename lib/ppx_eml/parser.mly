%token PERCENTAGE
%token LPAREN
%token RPAREN
%token GT
%token <string> STRING
%token <string * Lexing.position * Lexing.position> STRING_BLOCK
%token <string * Lexing.position * Lexing.position> INT_BLOCK
%token <string * Lexing.position * Lexing.position> CODE_BLOCK
%token <string * Lexing.position * Lexing.position> CODE_CONTINUE_BLOCK
%token <string> END_CODE_BLOCK
%token END
%token BRACEEND
%token EOF

%start <Eml.string_block list> prog
%%

prog:
  | EOF { [] }
  | res = blocks EOF { res }

blocks:
  | (* empty *) { [] }
  | t = template b = blocks { t :: b }

template:
  | str = STRING { Eml.Str str }
  | strtmpl = STRING_BLOCK; PERCENTAGE; GT {
      let (s, sp, ep) = strtmpl in
      print_endline (Printf.sprintf "%d %d" sp.Lexing.pos_lnum (ep.Lexing.pos_lnum + 1));
      Eml.Str_tmpl {
        str = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | inttmpl = INT_BLOCK; PERCENTAGE; GT {
      let (s, sp, ep) = inttmpl in
      Eml.Int_tmpl {
        str = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | codetmpl = CODE_BLOCK; PERCENTAGE; GT; contents = blocks; next_codetmpl = code_continue_template { 
      let (c, sp, _) = codetmpl in
      Eml.Code_tmpl {
        code = c;
        contents = contents;
        next_code_template = Some next_codetmpl;
        loc_start = sp;
      }
    }
  | codetmpl = CODE_BLOCK; PERCENTAGE; GT; contents = blocks; BRACEEND { 
      let (c, sp, _) = codetmpl in
      Eml.Code_tmpl {
        code = c;
        contents = contents;
        next_code_template = None;
        loc_start = sp;
      }
    }

code_continue_template:
  | codetmpl = CODE_CONTINUE_BLOCK; PERCENTAGE; GT; contents = blocks; next_codetmpl = code_continue_template {
    let (c, sp, _) = codetmpl in
    {
      code = c;
      contents = contents;
      next_code_template = Some next_codetmpl;
      loc_start = sp;
  }
  }
  | codetmpl = CODE_CONTINUE_BLOCK; PERCENTAGE; GT; contents = blocks; BRACEEND {
    let (c, sp, _) = codetmpl in
    {
      code = c;
      contents = contents;
      next_code_template = None;
      loc_start = sp;
  }
  }
