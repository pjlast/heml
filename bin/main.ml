open Base

type user =
  { name: string
  ; age: int }

let user_list ~users =
  {%heml|
<ul id="list">
<!-- Loop using regular OCaml code! -->
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>
|}

let button contents = {%heml|<button>
  <%raw= contents %>
</button>|}

let () =
  let users = [{name = "John"; age = 22}; {name = "Jane"; age = 23}] in
  let my_class = "testing" in
  Stdio.print_endline
    {%heml|
<Layouts.layout title="My title">
  <!-- Fully type-checked with errors displayed inline! -->
  <h1 class={my_class}>
    Users
  </h1>

  <br />

  <!-- Call custom components from within the template! -->
  <.user_list users={users} />

  <.button>
    Click me!
  </.button>
</Layouts.layout>
|}
