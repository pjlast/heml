open Base
open Ppxlib

let expand ~ctxt heml =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let loc =
    { loc with
      loc_start= {loc.loc_start with pos_cnum= loc.loc_start.pos_cnum + 6}
    ; loc_end= {loc.loc_end with pos_cnum= loc.loc_end.pos_cnum - 2} }
  in
  match Run.parse ~loc_start:loc.loc_start heml with
  | Ok processed ->
      let parser = Heml.Parser.create ~loc_start:loc.loc_start in
      let parser = List.fold processed ~init:parser ~f:Heml.Parser.parse in
      Heml.Parser.to_parsetree parser |> Ppxlib.Parse.Of_ocaml.copy_expression
  | _ ->
      Location.raise_errorf ~loc "Error"

let ppx_heml_extension =
  Extension.V3.declare "heml" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension ppx_heml_extension

let () = Driver.register_transformation ~rules:[rule] "ppx_heml"
