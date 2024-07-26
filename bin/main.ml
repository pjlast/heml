type user =
  { name: string
  ; age: int }

let user_list ~users =
  {%heml|<ul id="list">
<%= List.iter (fun user -> %>
  <li id={string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ) users; %>
</ul>|}

let () =
  let users = [{name = "John"; age = 22}; {name = "Jane"; age = 23}] in
  let my_class = "title" in
  let page =
    {%heml|<Layouts.layout title="Users">
  <h1 class={my_class}>
    Users
  </h1>

  <.user_list users={users} />
</Layouts.layout>|}
  in
  print_endline page
