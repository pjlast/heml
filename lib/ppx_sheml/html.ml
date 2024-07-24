open Base

module Div = struct
  type t = {content: string}
end

type flow_content =
  | P
  | Div of Div.t

module Body = struct
  type t = {content: flow_content list}
end

module Head = struct
  type t = {title: string}
end

module Html = struct
  type t =
    { head: Head.t
    ; body: Body.t }
end

module Doc = struct
  type t =
    { doctype: string
    ; html: Html.t }
end

let create_doc () =
  { Doc.doctype = "html"
  ; html =
      { head = {title = "Hello, World"}
      ; body = {content = [Div {content = "Hello, World"}]} } }
