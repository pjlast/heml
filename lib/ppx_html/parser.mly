%token <string> TAGNAME
%token <string> VOID_TAGNAME

%token LANGLE RANGLE SLASH LANGLESLASH EQ DOUBLE_QUOTE SINGLE_QUOTE
%token EOF

%start <Html_ast.Node.t list> main
%%

main:
| es = elements EOF { es }

elements:
| (* empty *) { [] }
| e = element es = elements { e :: es }

element:
| text = TAGNAME { Html_ast.Node.Text(text) }
| start_tag = start_tag; contents = elements; closename = close_tag
  {
    let (tagname, attrs) = start_tag in
    if tagname = closename then
      Html_ast.Node.Element(tagname, attrs, contents)
    else
      raise (Failure "Mismatched tag names")
  }
| LANGLE; tagname = VOID_TAGNAME; attrs = attributes; RANGLE { Html_ast.Node.Void_element(tagname, attrs) }
| LANGLE; tagname = TAGNAME; attrs = attributes; SLASH; RANGLE { Html_ast.Node.Void_element(tagname, attrs) }

start_tag:
| LANGLE; tagname = TAGNAME; attrs = attributes; RANGLE { (tagname, attrs) }

close_tag:
| LANGLESLASH; tagname = TAGNAME; RANGLE { tagname }



attributes:
| (* empty *) { [] }
| attribute attributes { $1 :: $2 }

attribute:
| TAGNAME EQ DOUBLE_QUOTE TAGNAME DOUBLE_QUOTE { ($1, $4) }
