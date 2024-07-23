open Base
open Ppxlib

let expand ~ctxt eml =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let loc =
    { loc with
      loc_start = {loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 6}
    ; loc_end = {loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - 2} }
  in
  match Run.parse ~loc_start:loc.loc_start eml with
  | Ok processed ->
      let parser = Eml.Parser.create ~loc_start:loc.loc_start in
      let parser =
        List.fold processed ~init:parser ~f:(fun parser block ->
            Eml.Parser.parse parser block )
      in
      Eml.Parser.to_parsetree parser |> Ppxlib.Parse.Of_ocaml.copy_expression
  | _ -> Location.raise_errorf ~loc "Error"

let ppx_eml_extension =
  Extension.V3.declare "eml" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension ppx_eml_extension

let () = Driver.register_transformation ~rules:[rule] "ppx_eml"
