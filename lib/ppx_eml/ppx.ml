open Base
open Ppxlib
open Ocaml_common

type code_fragment =
  | Text_fragment of string
  | Code_fragment of string

let rec collapse_text_fragments acc (l : code_fragment list) =
  let rec collapse_text_fragments' acc (l : code_fragment list) =
    match l with
    | [] ->
        List.rev acc
    | Text_fragment t1 :: Text_fragment t2 :: tl ->
        let acc = Text_fragment (t1 ^ "; " ^ t2) :: acc in
        collapse_text_fragments' acc tl
    | hd :: tl ->
        collapse_text_fragments' (hd :: acc) tl
  in
  let res = collapse_text_fragments' acc l in
  if List.length res = List.length l then res
  else collapse_text_fragments [] res

let rec concat_code_fragments acc (l : code_fragment list) =
  let l = collapse_text_fragments [] l in
  match l with
  | [] ->
      acc
  | [Code_fragment c] ->
      acc ^ c
  | [Text_fragment t] ->
      acc ^ t
  | Text_fragment t1 :: Text_fragment t2 :: tl ->
      concat_code_fragments (acc ^ t1 ^ t2) tl
  | Code_fragment c1 :: Text_fragment t2 :: tl ->
      concat_code_fragments (acc ^ c1 ^ " ; " ^ t2) tl
  | Text_fragment t1 :: Code_fragment c2 :: tl ->
      concat_code_fragments (acc ^ t1 ^ " ; " ^ c2) tl
  | Code_fragment c1 :: Code_fragment c2 :: tl ->
      concat_code_fragments (acc ^ c1 ^ c2) tl

let rec process_code_contents acc (els : Eml.string_block list) =
  match els with
  | [] ->
      "[" ^ concat_code_fragments "" (List.rev acc) ^ "]"
  | hd :: tl -> (
    match hd with
    | Eml.Str s ->
        process_code_contents (Text_fragment ("{|" ^ s.text ^ "|}") :: acc) tl
    | Eml.Str_tmpl st ->
        process_code_contents (Text_fragment ("(" ^ st.str ^ ")") :: acc) tl
    | Eml.Int_tmpl it ->
        process_code_contents
          (Text_fragment ("(" ^ "Stdlib.string_of_int " ^ it.str ^ ")") :: acc)
          tl
    | Eml.Code_tmpl s ->
        process_code_contents
          (Code_fragment ("; " ^ build_code_expression s) :: acc)
          tl )

and build_code_expression (el : Eml.code_template) =
  "Stdlib.String.concat \"\" (" ^ el.code
  ^ process_code_contents [] el.contents
  ^ ")"

let repeat ~n s =
  let rec aux ~n' str = if n' = 1 then str else s ^ aux ~n':(n' - 1) str in
  aux ~n':n s

(* let rec build_args acc ~loc (els : Eml.string_block list) =
   match els with
   | [] ->
       acc
   | hd :: tl ->
       let arg =
         match hd with
         | Eml.Str s ->
             (Nolabel, Ast_builder.Default.estring ~loc s)
         | Eml.Code_tmpl s ->
             ( Nolabel
             , build_code_expression s |> Lexing.from_string |> Parse.expression
             )
         | Eml.Str_tmpl s ->
             let lexbuf = s.str |> Lexing.from_string in
             lexbuf.lex_curr_p <- s.loc_start ;
             lexbuf.lex_start_p <- s.loc_start ;
             lexbuf.lex_abs_pos <- s.loc_start.pos_cnum ;
             lexbuf.lex_last_pos <- s.loc_start.pos_cnum ;
             lexbuf.lex_last_action <- s.loc_start.pos_cnum ;
             let expr = lexbuf |> Parse.expression in
             (Nolabel, expr)
         | Eml.Int_tmpl s ->
             let lexbuf =
               "Stdlib.string_of_int " ^ s.str |> Lexing.from_string
             in
             lexbuf.lex_curr_p <- s.loc_start ;
             lexbuf.lex_start_p <- s.loc_start ;
             lexbuf.lex_abs_pos <- s.loc_start.pos_cnum ;
             lexbuf.lex_last_pos <- s.loc_start.pos_cnum ;
             lexbuf.lex_last_action <- s.loc_start.pos_cnum ;
             let expr = lexbuf |> Parse.expression in
             (Nolabel, expr)
       in
       build_args (arg :: acc) ~loc tl *)

let expand ~ctxt eml =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let loc =
    { loc with
      loc_start= {loc.loc_start with pos_cnum= loc.loc_start.pos_cnum + 6}
    ; loc_end= {loc.loc_end with pos_cnum= loc.loc_end.pos_cnum - 2} }
  in
  match Run.parse ~loc_start:loc.loc_start eml with
  | Ok processed ->
      let parser = Eml.Parser.create ~loc_start:loc.loc_start in
      let parser =
        List.fold processed ~init:parser ~f:(fun parser block ->
            Eml.Parser.parse parser block )
      in
      Eml.Parser.to_parsetree parser |> Ppxlib.Parse.Of_ocaml.copy_expression
      (* let b = Buffer.create 100 in
         Eml.string_block_list_to_string b processed ;
         Buffer.contents b |> Lexing.from_string |> Parse.expression *)
      (* Ast_builder.Default.pexp_apply ~loc
         (Ast_builder.Default.pexp_ident ~loc
            (Ast_builder.Default.Located.lident ~loc "Printf.sprintf") )
         ( ( Nolabel
           , Ast_builder.Default.estring ~loc
               (repeat ~n:(List.length processed) "%s") )
         :: List.rev (build_args [] ~loc processed) ) *)
  | _ ->
      Location.raise_errorf ~loc "Error"

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
  Extension.V3.declare "eml" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension ppx_eml_extension

let () = Driver.register_transformation ~rules:[rule] "ppx_eml"
