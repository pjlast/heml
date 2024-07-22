%token PERCENTAGEGT
%token PERCENTAGE
%token EQ

%token LPAREN
%token RPAREN
%token GT
%token <string> START_TAG
%token <string * (string * string) list> START_TAG_WITH_ATTRS
%token <string> END_TAG
%token LT
%token <string * string> ATTRIBUTE
%token <string * Lexing.position * Lexing.position> STRING
%token <string * Lexing.position * Lexing.position> STRING_BLOCK
%token <string * Lexing.position * Lexing.position> INT_BLOCK
%token <string * Lexing.position * Lexing.position> CODE_BLOCK
%token END
%token BRACEEND
%token EOF

%start <Heml.Ast.t list> prog
%%

prog:
  | EOF { [] }
  | res = blocks EOF { res }

blocks:
  | (* empty *) { [] }
  | t = template b = blocks { t :: b }

template:
  | start_tag = START_TAG_WITH_ATTRS; contents = list(template); end_tag_name = END_TAG
    {
      let (start_tag_name, attrs) = start_tag in
      if start_tag_name = end_tag_name then
        Heml.Ast.Element {
          name = start_tag_name;
          attributes = attrs;
          contents = contents;
        }
      else
        failwith "Invalid match"
    }
  | str = STRING { let (text, sp, ep) = str in Heml.Ast.Text { text = text; loc_start = sp; loc_end = ep } }
  | strtmpl = STRING_BLOCK; PERCENTAGEGT {
      let (s, sp, ep) = strtmpl in
      print_endline (Printf.sprintf "%d %d" sp.Lexing.pos_lnum (ep.Lexing.pos_lnum + 1));
      Heml.Ast.String_block {
        field = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | inttmpl = INT_BLOCK; PERCENTAGEGT {
      let (s, sp, ep) = inttmpl in
      Heml.Ast.Int_block {
        field = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | codetmpl = CODE_BLOCK; PERCENTAGEGT { 
      let (c, sp, ep) = codetmpl in
      Heml.Ast.Code_block {
        code = c;
        loc_start = sp;
        loc_end = ep
      }
    }

attribute:
| attr = ATTRIBUTE {
    attr
  }
