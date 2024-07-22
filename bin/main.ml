open Base

let the_output () =
  let nums = [1; 2; 3] in
  let stv = 124 in
  {%heml|Hello
<%i= stv %>
<% if stv = 123 then ( %>Stv is 123<% ) else ( %>Stv is not 123 <% ); %>
<%
  List.iter nums ~f:(fun n ->
%><%i= n %>123<% ); %>
<elmnt display url=this val='string with"spaces in it'>
Some text over here
<anotherelmnt><%i= stv %></anotherelmnt>
</elmnt>
|}

let () = Stdio.print_endline (the_output ())
