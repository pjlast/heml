open Base

type user =
  { name: string
  ; age: int }

let user_list ~users =
  {%heml|
<ul id="list">
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>
|}

let button contents = {%heml|<button>
  <%s= contents %>
</button>|}

let () =
  let users = [{name = "John"; age = 22}; {name = "Jane"; age = 23}] in
  let my_class = "title" in
  Stdio.print_endline
    {%heml|
<Layouts.layout title="My title">
  <!-- Some comment here -->
  <h1 class={my_class}>
    Users
  </h1>

  <br />

  <.user_list users={users} />

  <.button>
    Click me!
  </.button>
</Layouts.layout>
|}
