%token LEFT_ANGLE
%token RIGHT_ANGLE
%token SLASH
%token PERCENTAGE
%token EQUALS
%token <string> OPEN_TAG
%token <string> CLOSE_TAG
%token <string> CONTENTS
%token <string> OCAML_CODE
%token <string> STRING_BLOCK
%token EOF
%start <Html.el list> prog
%%

prog:
  | EOF { [] }
  | es = elements EOF { es }

elements:
  | (* empty *) { [] }
  | e = element es = elements { e :: es }

element:
  | name = OPEN_TAG; RIGHT_ANGLE; contents = elements; closing_name = CLOSE_TAG; RIGHT_ANGLE {
      if name <> closing_name then
        raise (Html.Wrong_closing_tag {tag_name = closing_name; expected_tag_name = name})
      else
        Html.Element { name = name; contents = contents }
      }
  | contents = CONTENTS {
      Html.Text contents
    }
  | code = OCAML_CODE; PERCENTAGE; RIGHT_ANGLE {
      Html.Ocaml_code { code; contents = [] }
    }

content:
  | c = CONTENTS { c }
