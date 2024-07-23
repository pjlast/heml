# heml

> [!WARNING]
> This library is still early in development and things may still change quite a bit.

`heml` (HTML + Embedded ML) is a ppx extension for easy-to-use HTML templating directly in OCaml.

It's a direct conversion of [Phoenix's HEEx templates](https://hexdocs.pm/phoenix_live_view/assigns-eex.html).

A template is started using the `{%heml|... |%}` syntax.

<svg fill="none" viewBox="0 0 400 400" width="400" height="400" xmlns="http://www.w3.org/2000/svg">
  <foreignObject width="100%" height="100%">
    <div xmlns="http://www.w3.org/1999/xhtml">
      <style>
        * {
          font-family: monospace
        }

        body {
          background-color: #1d2021;
          color: #ebdbb2
        }

        .String {
          color: #b8bb26
        }

        .Operator {
          color: #fe8019
        }

        .GruvboxRed {
          color: #fb4934
        }

        .GruvboxGreenBold {
          font-weight: bold;
          color: #b8bb26
        }

        .GruvboxYellow {
          color: #fabd2f
        }

        .GruvboxBlue {
          color: #83a598
        }

        .GruvboxPurple {
          color: #d3869b
        }

        .GruvboxAqua {
          color: #8ec07c
        }

        .GruvboxOrange {
          color: #fe8019
        }

        .GruvboxGreen {
          color: #b8bb26
        }

        .GruvboxFg1 {
          color: #ebdbb2
        }
      </style>
      <pre>
				<span class="GruvboxRed">let</span>
				<span class="GruvboxFg1">
					<span class="GruvboxGreenBold">button</span>
				</span>
				<span class="GruvboxFg1">contents</span>
				<span class="GruvboxBlue">=</span>
				<span class="GruvboxOrange">{%</span>
				<span class="GruvboxAqua">heml</span>|
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&lt;</span>
				</span>
				<span class="GruvboxGreen">
					<span class="GruvboxGreen">button</span>
				</span>
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&gt;</span>
				</span>
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&lt;</span>
				</span>
				<span class="GruvboxOrange">%s</span>
				<span class="Operator">
					<span class="Operator">=</span>
				</span>
				<span class="GruvboxAqua">
					<span class="GruvboxAqua">contents</span>
				</span>
				<span class="GruvboxAqua">
					<span class="GruvboxAqua">%</span>
				</span>
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&gt;</span>
				</span>
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&lt;/</span>
				</span>
				<span class="GruvboxGreen">
					<span class="GruvboxGreen">button</span>
				</span>
				<span class="GruvboxBlue">
					<span class="GruvboxBlue">&gt;</span>
				</span>|
				<span class="GruvboxOrange">
					<span class="GruvboxBlue">}</span>
				</span>
			</pre>
    </div>
  </foreignObject>
</svg>

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
  <%s= contents %>
</button>
|}

let () = print_endline {%heml|<div>
  Some content

  <.button cls="btn-primary">Click me!</.button>
</div>
|}
```

Arguments need to be labelled arguments, and the final argument will be the contents of the component.

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
