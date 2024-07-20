(* open Core                          *)
(* open Heml *)
(* open Lexing *)
open Base
open Ppxlib

let _ =
  {|
<div>
  <%= begin match y with %>
  <% | 1 -> %>
  <one></one>
  <% | 2 -> %>
  <two></two>
  <% | _ -> %>
  <other></other>
  <% end %>
</div>
|}

let _ = {%heml|<div>start</div>|}

type user =
  { username: string
  ; age: int }

let the_output () =
  let stv = 123 in
  {%eml|

  Hello
  <%= List.iter [1;2;3] ~f:(fun _ -> %>
    <%i= stv %>
  <% ); %>|}

(* {%eml|
   Hello users,
   <%= List.iter users ~f:(fun u -> %>
     Hello <%s= u.username %>,
     <%= if u.age > 21 then ( %>
       You are <%i= u.age %> years old.
     <% ) else ( %>
       You are <%i= u.age %> years young.
     <% ); %>
   <% ); %>
   |} *)

let default_location =
  { loc_start= {pos_cnum= 0; pos_lnum= 0; pos_bol= 0; pos_fname= ""}
  ; loc_end= {pos_cnum= 0; pos_lnum= 0; pos_bol= 0; pos_fname= ""}
  ; loc_ghost= false }

let () = Stdio.print_endline (the_output ())

(* let print_position outx lexbuf =
     let pos = lexbuf.lex_curr_p in
     fprintf
       outx
       "%s:%d:%d"
       pos.pos_fname
       pos.pos_lnum
       (pos.pos_cnum - pos.pos_bol + 1)
   ;;

   let parse_with_error lexbuf =
     try Ok (Parser.prog Lexer.read lexbuf) with
     | Lexer.SyntaxError msg ->
       fprintf stderr "%a: %s\n" print_position lexbuf msg;
       Ok []
     | Parser.Error ->
       fprintf stderr "%a: syntax error\n" print_position lexbuf;
       exit (-1)
     | _ -> Error lexbuf.lex_curr_p
   ;;

   let rec parse_and_print lexbuf =
     match parse_with_error lexbuf with
     | Ok [ value ] ->
       (match value with
        | Element v ->
          print_endline v.name;
          print_endline v.contents;
          parse_and_print lexbuf
        | _ -> print_endline "Unimplemented")
     | _ -> ()
   ;;

   let loop filename () =
     let lexbuf = Lexing.from_string filename in
     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
     parse_with_error lexbuf
   ;; *)

(* let default_location =
     { loc_start= {pos_cnum= 0; pos_lnum= 0; pos_bol= 0; pos_fname= ""}
     ; loc_end= {pos_cnum= 0; pos_lnum= 0; pos_bol= 0; pos_fname= ""}
     ; loc_ghost= false }

   let () =
     let res =
       Ppx_eml.Run.parse ~loc_start:default_location.loc_start
         {|Hello users,
      <%= List.iter users ~f:(fun u -> %>
         Hello <%s= u.username %>,
         <%= if u.age > 21 then ( %>
           You are <%i= u.age %> years old.
         <% ) else ( %>
           You are <%i= u.age %> years young.<% ); %><% end %><% ); %><% end %>|}
     in
     match res with
     | Ok res ->
         let b = Buffer.create 100 in
         Ppx_eml.Eml.string_block_list_to_string b res ;
         Stdio.print_endline (Buffer.contents b)
     | Error _ ->
         () *)
(* Stdio.print_endline (the_output ()) *)
