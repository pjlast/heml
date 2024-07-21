open Base

let the_output () =
  let nums = [1; 2; 3] in
  let stv = 123 in
  {%eml|Hello
<%i= stv %>
<% List.iter nums ~f:(fun n -> %><%i= n %>123<% ); %>|}

let () = Stdio.print_endline (the_output ())
