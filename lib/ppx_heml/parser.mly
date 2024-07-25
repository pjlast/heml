%token PERCENTAGEGT

%token <string> COMMENT
%token <string> DOCTYPE
%token <string * (string * Heml.attribute) list * Lexing.position * Lexing.position> START_TAG_WITH_ATTRS
%token <string * (string * Heml.attribute) list * Lexing.position * Lexing.position> SELF_CLOSING_START_TAG_WITH_ATTRS
%token <string * Lexing.position * Lexing.position> END_TAG
%token <string * Lexing.position * Lexing.position> STRING
%token <string * Lexing.position * Lexing.position> STRING_BLOCK
%token <string * Lexing.position * Lexing.position> RAW_BLOCK
%token <string * Lexing.position * Lexing.position> INT_BLOCK
%token <string * Lexing.position * Lexing.position> CODE_BLOCK
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
  | doctype = DOCTYPE {
      Heml.Ast.Doctype { name = doctype }
    }
  | comment = COMMENT {
    Heml.Ast.Comment { text = comment }
  }
  | start_tag = START_TAG_WITH_ATTRS; contents = list(template); end_tag_name = END_TAG
    {
      let (start_tag_name, attrs, sp, ep) = start_tag in
      let (end_tag_name, etsp, etep) = end_tag_name in
      if start_tag_name = end_tag_name then
        Heml.Ast.Element {
          name = start_tag_name;
          attributes = attrs;
          contents = contents;
          loc_start = sp;
          loc_end = ep;
        }
      else
        raise (Heml.Ast.MismatchedTags (Printf.sprintf "Unexpected end tag %s\nHint: Did you mean %s?" end_tag_name start_tag_name, etsp, etep))
    }
  | tag = SELF_CLOSING_START_TAG_WITH_ATTRS
    {
      let (tag_name, attrs, sp, ep) = tag in
      Heml.Ast.Void_element {
        name = tag_name;
        attributes = attrs;
        loc_start = sp;
        loc_end = ep;
      }
    }
  | str = STRING { let (text, sp, ep) = str in Heml.Ast.Text { text = text; loc_start = sp; loc_end = ep } }
  | strtmpl = STRING_BLOCK; PERCENTAGEGT {
      let (s, sp, ep) = strtmpl in
      Heml.Ast.String_block {
        field = s;
        loc_start = sp;
        loc_end = ep
      }
    }
  | rawtmpl = RAW_BLOCK {
      let (s, sp, ep) = rawtmpl in
      Heml.Ast.Raw_block {
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
