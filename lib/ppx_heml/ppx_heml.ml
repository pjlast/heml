(* open Base *)
open Ppxlib

let process_string input_string = Heml.Run.parse input_string

(*
   "<div>"
   String.concat "\n" (List.map "" (
   ))
   "</div>"
*)

type code_fragment =
  | Text_fragment of string
  | Code_fragment of string

let rec collapse_text_fragments acc (l : code_fragment list) =
  let rec collapse_text_fragments' acc (l : code_fragment list) =
    match l with
    | [] -> List.rev acc
    | Text_fragment t1 :: Text_fragment t2 :: tl ->
      let acc = Text_fragment (t1 ^ " ^ " ^ t2) :: acc in
      collapse_text_fragments' acc tl
    | hd :: tl -> collapse_text_fragments' (hd :: acc) tl
  in
  let res = collapse_text_fragments' acc l in
  if List.length res = List.length l
  then res
  else collapse_text_fragments [] res
;;

let rec concat_code_fragments acc (l : code_fragment list) =
  let l = collapse_text_fragments [] l in
  match l with
  | [] -> acc
  | [ Code_fragment c ] -> acc ^ c
  | [ Text_fragment t ] -> acc ^ t
  | Text_fragment t1 :: Text_fragment t2 :: tl ->
    concat_code_fragments (acc ^ t1 ^ t2) tl
  | Code_fragment c1 :: Text_fragment t2 :: tl ->
    concat_code_fragments (acc ^ c1 ^ t2) tl
  | Text_fragment t1 :: Code_fragment c2 :: tl ->
    concat_code_fragments (acc ^ t1 ^ c2) tl
  | Code_fragment c1 :: Code_fragment c2 :: tl ->
    concat_code_fragments (acc ^ c1 ^ c2) tl
;;

let rec convert_els_to_ocaml_code
  (acc : code_fragment list)
  (els : Heml.Html.el list)
  =
  match els with
  | [] -> acc
  | h :: tl ->
    convert_els_to_ocaml_code
      (match h with
       | Ocaml_code c -> convert_els_to_ocaml_code (Code_fragment c :: acc) []
       | Text t ->
         convert_els_to_ocaml_code
           (Text_fragment (Printf.sprintf "{__heml|%s|__heml}" t) :: acc)
           []
       | Element e ->
         let contents =
           convert_els_to_ocaml_code
             (Text_fragment (Printf.sprintf "{__heml|<%s>|__heml}" e.name)
              :: acc)
             e.contents
         in
         Text_fragment (Printf.sprintf "{__heml|</%s>|__heml}" e.name)
         :: contents)
      tl
;;

let expand ~ctxt eml =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match process_string eml with
  | Ok els ->
    let output =
      concat_code_fragments "" (convert_els_to_ocaml_code [] els |> List.rev)
    in
    output |> Lexing.from_string |> Parse.expression
  | Error tag_err ->
    let { loc_start; loc_end; _ } = loc in
    let line_num = tag_err.lexbuf.lex_curr_p.pos_lnum in
    let col_num = tag_err.lexbuf.lex_curr_p.pos_cnum in
    Location.raise_errorf
      ~loc:
        { Location.loc_start =
            { pos_fname = eml
            ; pos_cnum = loc_start.pos_cnum + col_num
            ; pos_lnum = loc_start.pos_lnum + line_num - 1
            ; pos_bol = loc_start.pos_bol
            }
        ; Location.loc_end =
            { pos_fname = eml
            ; pos_cnum =
                loc_start.pos_cnum
                + col_num
                + 3
                + String.length tag_err.tag_name
            ; pos_lnum = loc_end.pos_lnum + line_num - 1
            ; pos_bol = loc_end.pos_bol
            }
        ; Location.loc_ghost = false
        }
      "Unmatched opening tag %s"
      tag_err.expected_tag_name
;;

(*(try process_string eml with
  | _ -> Location.raise_errorf
  ~loc:!Ast_helper.default_loc
  "This is a custom error message");
  "\"" ^ eml ^ "\"" |> Lexing.from_string |> Parse.expression*)

let ppx_dream_eml_extension =
  Extension.V3.declare
    "heml"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
;;

let rule = Ppxlib.Context_free.Rule.extension ppx_dream_eml_extension
let () = Driver.register_transformation ~rules:[ rule ] "ppx_heml"
