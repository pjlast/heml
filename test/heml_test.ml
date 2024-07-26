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
