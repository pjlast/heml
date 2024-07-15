%token PERCENTAGE
%token GT
%token <string> STRING
%token <string * Lexing.position * Lexing.position> STRING_BLOCK
%token <string> CODE_BLOCK
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
    Eml.Str_tmpl {
      str = s;
      loc_start = sp;
      loc_end = ep
      }
    }
  | codetmpl = CODE_BLOCK; PERCENTAGE; GT { Eml.Code_tmpl codetmpl }
