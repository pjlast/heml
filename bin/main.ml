open Base

type user =
  { name: string
  ; age: int }

let user_list ~users _ =
  {%heml|
<ul id="list">
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>
|}

let () =
  let users = [{name= "John"; age= 22}; {name= "Jane"; age= 23}] in
  let my_class = "title" in
  Stdio.print_endline
    {%heml|
<h1 class={my_class}>
  Users
</h1>

<.user_list users={users}>
</.user_list>

|}
