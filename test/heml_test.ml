open Base

let%test "div" = String.equal {%heml|<div></div>|} "<div></div>"

let%test "div with text" =
  String.equal {%heml|<div>Hello</div>|} "<div>Hello</div>"

let%test "div with text and attribute" =
  String.equal {%heml|<div class="foo">Hello</div>|}
    "<div class=\"foo\">Hello</div>"

let%test "div with string" =
  String.equal {%heml|<div>Hello <%s= "world" %></div>|}
    "<div>Hello world</div>"

let%test "div with html escaping" =
  let dangerous_string = "<>&" in
  String.equal {%heml|<div><%s= dangerous_string %></div>|}
    "<div>&lt;&gt;&amp;</div>"

let%test "script tags are not escaped" =
  String.equal {%heml|<script>if (1 < 2) { alert("hello"); }</script>|}
    "<script>if (1 < 2) { alert(\"hello\"); }</script>"

type user =
  { name: string
  ; age: int }

let user_list ~users =
  {%heml|<ul id="list">
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>|}

let%test "complete test" =
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
  String.equal page
    {|<!DOCTYPE html>
<head>
  <title>Users</title>
</head>
<body>
  
  <h1 class="title">
    Users
  </h1>

  <ul id="list">

  <li id="22">
    John is 22 years old.
  </li>

  <li id="23">
    Jane is 23 years old.
  </li>

</ul>

</body>
|}
