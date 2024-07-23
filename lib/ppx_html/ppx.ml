open Base
open Ppxlib

let expand ~ctxt:(_ctx : Expansion_context.Extension.t) (html : label) =
  match Run.parse html with
  | Ok html ->
      let output =
        List.map html ~f:Html_ast.Node.to_string |> String.concat ~sep:"\n"
      in
      "{__html_|" ^ output ^ "|__html_}"
      |> Lexing.from_string
      |> Parse.expression
  | _ -> failwith "Error"

let ppx_html_extension =
  Extension.V3.declare "html" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension ppx_html_extension

let () = Driver.register_transformation ~rules:[rule] "ppx_html"
