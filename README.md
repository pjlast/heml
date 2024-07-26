# heml

> [!WARNING]
> This library is still early in development and things may still change quite a bit.

`heml` (HTML + Embedded ML) is a ppx extension for easy-to-use HTML templating directly in OCaml.

It's a direct conversion of [Phoenix's HEEx templates](https://hexdocs.pm/phoenix_live_view/assigns-eex.html).

Here's a quick demo of what it looks like in Neovim:

[![asciicast](https://asciinema.org/a/cuR8obvIQlichm5vXfaAEBCWR.png)](https://asciinema.org/a/cuR8obvIQlichm5vXfaAEBCWR)

##### Table of Contents
- [Install](#install)
- [Usage](#usage)
  - [Syntax](#syntax)
  - [Using OCaml variables](#using-ocaml-variables)
  - [Using OCaml code](#using-ocaml-code)
  - [Components and layouts](#components-and-layouts)
  - [Validation](#validation)
- [Editor support](#editor-support)
  - [Neovim](#neovim)
- [Known limitations](#known-limitations)

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
  let users = [{name = "John"; age = 22}
              ;{name = "Jane"; age = 23}]
  in
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

### Syntax

For the most part you can just write regular HTML.

However, for void elements, such as `<img ...>` or `<br>` that don't have a closing tag, you need to self-close the element using `/>`. They will be rendered as `<img ...>` and `<br>`.

```ocaml
{%heml|<!DOCTYPE html>
<html lang="en">
  <head>
    <title>My webpage</title>
  </head>
  <body>
    <h1>Hello!</h1>

    <br />

    <p>Welcome to my web page!</p>

    <img src="https://example.com/image.png" />
  </body>
</html>|}
```

### Using OCaml variables

You can use OCaml variables directly in your templates.

#### In HTML body

Variables are inserted into the HTML body using the `<%s= string_variable %>` and `<%i= int_variable %>` tags.

```ocaml
let custom_button ~text = {%heml|<button><%s= text %></button>|}
```

#### In HTML tags

Variables are inserted into HTML tags using the `{variable}` syntax.

```ocaml
let button_with_class ~cls = {%heml|<button class={cls}>Click me!</button>|}
```

### Using OCaml code

OCaml code can be used throughout templates by using the `<%= (* code *) %>` tags. Code can be started in one tag and ended in another.
This is useful for iteration and conditionals.

```ocaml
let render_users_if_true ~users ~should_render =
  {%heml|<%= if should_render then (%>
  <%= List.iter (fun user -> %>
    <%s= user.name %> is <%i= user.age %> years old.
  <%= ) users; %>
<%= ); %>
```

You'll notice that all OCaml code should be unit statements and terminated with a semicolon.
If, for whatever reason, you want to render something withing a code block, you can use the `write` function:

```ocaml
let render_text ~text = {%heml|<%= write text; %>|}
```

### Components and layouts

It's also possible to use your own components or layouts directly in the template.

A component from another module can be used directly:

```ocaml
{%heml|<Components.button text="Click me!" />|}
```

But if you're using a component from the same module, you must prefix it with a `.` (otherwise we can't distinguish between normal HTML and components).

```ocaml
let button ~text = {%heml|<button><%s= text %></button>|}

let () = print_endline {%heml|<.button text="Click me!" />|}
```

Components can also contain HTML, in which case you need to render the contents using the `<%raw= %>` tag.
The `<%raw= %>` tag will render the contents as-is, without doing any HTML escaping, and assumes that the contents are safe.
The contents are passed as the final unlabelled argument of the function.

```ocaml
let custom_div ~cls contents =
  {%heml|<div class={cls}>
  <%raw= contents %>
</div>|}
```

Similarly, you can use the `<%raw= %>` to create something like a layout:

```ocaml
(* layouts.ml *)
let base_layout ~title contents = {%heml|<!DOCTYPE html>
<html lang="en">
<head>
  <title><%s= title %></title>
  <!-- other meta tags -->
</head>
<body>
  <%raw= contents %>
</body>
</html>|}
```

```ocaml
(* main.ml *)
let () = print_endline {%heml|<Layouts.base_layout title="My webpage">
  <h1>Hello!</h1>
  <p>Welcome to my web page!</p>
</Layouts.base_layout>|}
```

### Validation

`heml` does basic HTML validation. It won't allow you to have mismatched start and end tags, or to have unclosed tags. It also makes sure attributes and such are formatted correctly.

It does NOT do any kind of HTML element validation. I've tried to hit the sweet spot between being strict and being helpful.

So `heml` will ensure that your HTML is well-formed, but it won't check that your HTML is semantically correct.

## Editor support

`heml` leverages the OCaml LSP for feedback directly in your editor. No special LSP or plugin is required other than the standard OCaml LSP.

Since `heml` is basically HEEx, you can use the [HEEx treesitter grammar](https://github.com/phoenixframework/tree-sitter-heex) for syntax highlighting.

### Neovim

To get nice highlighting in Neovim, add a `queries/ocaml/injections.scm` file to your `.config/nvim` with the following contents:

```
((quoted_extension
            (attribute_id) @_attid
            (quoted_string_content) @injection.content)
  (#contains? @_attid "heml")
  (#set! injection.language "heex"))
```

## Known limitations

`<script></script>` tags don't support interpolation inside of the tag (yet).
