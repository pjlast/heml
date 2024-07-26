# heml

> [!WARNING]
> This library is still early in development and things may still change quite a bit.

`heml` (HTML + Embedded ML) is a ppx extension for easy-to-use HTML templating directly in OCaml.

It's a direct conversion of [Phoenix's HEEx templates](https://hexdocs.pm/phoenix_live_view/assigns-eex.html).

Here's a quick demo of what it looks like in Neovim:

[![asciicast](https://asciinema.org/a/cuR8obvIQlichm5vXfaAEBCWR.png)](https://asciinema.org/a/cuR8obvIQlichm5vXfaAEBCWR)

## Install

This package is not yet available on opam, so to use it you're going to have to manually pin and install it:

```
opam pin ppx_heml.dev git+https://github.com/pjlast/heml.git
opam install ppx_heml
```

After which you can use it in your project by adding the following to your `dune` file:

```
(preprocess
  (pps ppx_heml))
```

## Usage

A template is written using the `{%heml|... |%}` syntax, which will return a string.

Here's a quick demo program:

<table>
<tr>
<th><code>input</code></th>
<th><code>output</code></th>
</tr>
<tr>
<td>

```ocaml
(* bin/layouts.ml *)
let layout ~title contents =
  {%heml|<!DOCTYPE html>
<head>
  <title><%s= title %></title>
</head>
<body>
  <%raw= contents %>
</body>
|}
```

```ocaml
(* bin/main.ml *)
open Base

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
  Stdio.print_endline page
```
</td>
<td>

```html
<!DOCTYPE html>
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
```
</td>
</tr>
</table>

## Syntax

For the most part you can just write regular HTML.

However, for void elements, such as `<img>` or `<br>` that don't have a closing tag, you need to self-close the element using `/>`. They will be rendered as `<img>` and `<br>`.

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

### Custom components

You can also create re-usable components, which can be called in templates:

```ocaml
let my_button ~cls contents =
  {%heml|<button class={cls}>
  <%raw= contents %>
</button>
|}

let () = print_endline {%heml|<div>
  Some content

  <.button cls="btn-primary">Click me!</.button>
</div>
|}
```

Arguments need to be labelled arguments, and the final argument will be the contents of the component.

Note that `contents` are rendered using the `<%raw= %>` tag. This outputs raw HTML instead of escaping the HTML first.

You can also create components that don't take content by calling them with the self-closing tag:

```ocaml
let user_list ~users =
  {%heml|<ul id="list">
<%= List.iter users ~f:(fun user -> %>
  <li id={Stdlib.string_of_int user.age}>
    <%s= user.name %> is <%i= user.age %> years old.
  </li>
<%= ); %>
</ul>|}

let () = let users = [{name= "John"; age= 22}; {name= "Jane"; age= 23}] in
  {%heml|<.user_list users=users />|}
```

You can also use components for layouts:

```ocaml
(* layouts.ml *)
let layout ~title contents = {%heml|<!DOCTYPE html>
<html>
  <head>
    <title><%s= title %></title>
  </head>
  <body>
    <%raw= contents %>
  </body>
</html>|}

(* main.ml *)
let () = print_endline {%heml|<Layouts.layout title="My page">
  <h1>Home page</h1>
  <p>Some text and stuff</p>
</Layouts.layout>|}
```

## Editor support

Since heml is basically HEEx, you can use the [HEEx treesitter grammar](https://github.com/phoenixframework/tree-sitter-heex) for syntax highlighting.

### Neovim

To get nice highlighting in Neovim, add a `queries/ocaml/injections.scm` file to your `.config/nvim` with the following contents:

```
((quoted_extension
            (attribute_id) @_attid
            (quoted_string_content) @injection.content)
  (#contains? @_attid "heml")
  (#set! injection.language "heex"))
```
