open Base

type user =
  { name: string
  ; age: int }

let the_output () =
  let users = [{name= "John"; age= 22}; {name= "Jane"; age= 23}] in
  {%heml|<h1 class="title">Users</h1>
  <ul id="list">
  <%= List.iter users ~f:(fun user -> %>
    <li disabled><%s= user.name %> is <%i= user.age %> years old.</li>
  <%= ); %>
  </ul>
|}

let () = Stdio.print_endline (the_output ())
