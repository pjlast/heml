open Base

type user =
  { name: string
  ; age: int }

let the_output () =
  let users = [{name= "John"; age= 22}; {name= "Jane"; age= 23}] in
  let my_class = "title" in
  {%heml|
<h1 class={my_class}>
  Users
</h1>

<ul id="list">
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>
|}

let () = Stdio.print_endline (the_output ())
