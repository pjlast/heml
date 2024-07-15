open Base
open Stdio
open Ppxlib

let process_list (els : Eml.string_block list) =
  List.map els ~f:(fun el ->
    match el with
    | Eml.Str s -> "\"" ^ s ^ "\""
    | Eml.Str_tmpl s -> s.str
    | Eml.Code_tmpl s -> s)
;;

let repeat ~n s =
  let rec aux ~n' str = if n' = 1 then str else s ^ aux ~n':(n' - 1) str in
  aux ~n':n s
;;

let rec build_args acc ~loc (els : Eml.string_block list) =
  match els with
  | [] -> acc
  | hd :: tl ->
    let arg =
      match hd with
      | Eml.Str s -> Nolabel, Ast_builder.Default.estring ~loc s
      | Eml.Code_tmpl s -> Nolabel, s |> Lexing.from_string |> Parse.expression
      | Eml.Str_tmpl s ->
        let lexbuf = s.str |> Lexing.from_string in
        print_endline (Printf.sprintf "LNUM %i\n" s.loc_start.pos_lnum);
        print_endline (Printf.sprintf "CNUM %i\n" s.loc_start.pos_cnum);
        print_endline (Printf.sprintf "BOL %i\n" s.loc_start.pos_bol);
        lexbuf.lex_curr_p <- s.loc_start;
        lexbuf.lex_start_p <- s.loc_start;
        lexbuf.lex_abs_pos <- s.loc_start.pos_cnum;
        lexbuf.lex_last_pos <- s.loc_start.pos_cnum;
        lexbuf.lex_last_action <- s.loc_start.pos_cnum;
        let expr = lexbuf |> Parse.expression in
        Nolabel, expr
    in
    build_args (arg :: acc) ~loc tl
;;

let expand ~ctxt eml =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let loc =
    { loc with
      loc_start = { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + 6 }
    ; loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum - 2 }
    }
  in
  Stdio.print_endline (Printf.sprintf "lnum: %i" loc.loc_start.pos_lnum);
  Stdio.print_endline (Printf.sprintf "bol:  %i" loc.loc_start.pos_bol);
  Stdio.print_endline (Printf.sprintf "cnum: %i" loc.loc_start.pos_cnum);
  match Run.parse ~loc_start:loc.loc_start eml with
  | Ok processed ->
    Ast_builder.Default.pexp_apply
      ~loc
      (Ast_builder.Default.pexp_ident
         ~loc
         (Ast_builder.Default.Located.lident ~loc "Printf.sprintf"))
      (( Nolabel
       , Ast_builder.Default.estring
           ~loc
           (repeat ~n:(List.length processed) "%s") )
       :: List.rev (build_args [] ~loc processed))
  | _ -> Location.raise_errorf ~loc "Error"
;;

(* let processed = Run.parse eml in
   match processed with
   | Ok pl ->
   let x =
   pl
   |> process_list
   |> String.concat ~sep:" ^ "
   |> Lexing.from_string
   |> Parse.expression
   in
   x
   | _ -> Location.raise_errorf ~loc "Error" *)

let ppx_eml_extension =
  Extension.V3.declare
    "eml"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let rule = Ppxlib.Context_free.Rule.extension ppx_eml_extension
let () = Driver.register_transformation ~rules:[ rule ] "ppx_eml"
