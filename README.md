# heml

`heml` (HTML + Embedded ML) is a ppx extension for easy-to-use HTML templating directly in OCaml.

It's a direct conversion of [Phoenix's HEEx templates](https://hexdocs.pm/phoenix_live_view/assigns-eex.html).

A template is started using the `{%heml|... |%}` syntax.

The following program:

```ocaml
open Base

type user =
  { name: string
  ; age: int }

let () =
  let users = [{name= "John"; age= 22}; {name= "Jane"; age= 23}] in
  let my_class = "title" in
  Stdio.print_endline {%heml|
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
```

Will output the following HTML:

```html
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
```

## Syntax

For the most part you can just write regular HTML.

For HTML attributes, values can be interpolated using `{variable}`, for example `<div class={my_class}>` will output `<div class="value of my_class">`.

For now, the variable needs to be a string.

In the rest of the template, variables can be used using `<%s= variable %>` for strings and `<%i= variable %>` for integers.

And then arbitrary OCaml code can be used using `<%= (* code *) %>`. You can open a statement in one block and close it in another. This is useful for iteration:

```html
<%= List.iter users ~f(fun user -> %>
  <!-- Do something with user -->
<%= ); %>
```

or conditionals:

```html
<%= if some_condition then ( %>
  <!-- Do something conditionally -->
<%= ); %>
```

All statements need to be unit statements (i.e. terminated with a semicolon).

If you need to write to the template inside a code block, you can use `write`:

```html
<%= write "Some text" %>
```

Internally heml uses a `Buffer.t` and each statement gets translated to some form of `Buffer.add_string`, which then returns a string using `Buffer.contents` at the end of the template.