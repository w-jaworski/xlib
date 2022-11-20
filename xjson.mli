
type json =
    JObject of (string * json) list
  | JArray of json list
  | JString of string
  | JVar of string
  | JNumber of string
  | JNull
  | JTrue
  | JFalse
  | JEmpty
  | JContradiction

val json_escape: string -> string

val json_to_string_fmt: string -> json -> string
val json_to_string_fmt2: string -> json -> string

val json_to_string: json -> string

val json_of_string: string -> json

val json_convert_comma: string -> string

val fold_file: ?step:int -> ?limit:int -> string -> 'a -> ('a -> json -> 'a) -> 'a
