open Base

type attribute = string * string

module Node = struct
  type t =
    | Text of string
    | Comment of string
    | Element of string * attribute list * t list
    | Void_element of string * attribute list

  let rec to_string = function
    | Text s ->
        s
    | Comment s ->
        String.concat ~sep:"" ["<!--"; s; "-->"]
    | Element (name, attributes, children) ->
        let attributes_string =
          List.fold ~init:""
            ~f:(fun acc (name, value) -> acc ^ " " ^ name ^ "=\"" ^ value ^ "\"")
            attributes
        in
        "<" ^ name ^ attributes_string ^ ">"
        ^ String.concat ~sep:"\n" (List.map ~f:to_string children)
        ^ "</" ^ name ^ ">"
    | Void_element (name, attributes) ->
        let attributes_string =
          List.fold ~init:""
            ~f:(fun acc (name, value) -> acc ^ " " ^ name ^ "=\"" ^ value ^ "\"")
            attributes
        in
        "<" ^ name ^ attributes_string ^ ">"
end
