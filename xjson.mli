
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

type 'a pat =
    AString of ('a -> string -> 'a)
  | AInt of ('a -> int -> 'a)
  | AObject of (string * 'a pat) list
  | AArray of 'a pat list (* przetwarzamy po kolei elementy listy w JArray *)

type ('a,'b) pat2 =
    BString of ('b -> string -> 'b)
  | BInt of ('b -> int -> 'b)
  | BObject of (string * ('a,'b) pat2) list
  | BArrayMap of ('b -> 'a list -> 'b) * 'a * 'a pat

val find_apattern: json -> 'a -> 'a pat -> 'a
val find_bpattern: json -> 'b -> ('a,'b) pat2 -> 'b
val find_apattern_opt: json -> 'a -> 'a pat -> 'a
val find_bpattern_opt: json -> 'b -> ('a,'b) pat2 -> 'b
val extract_apattern: json -> 'a -> 'a pat -> json * 'a
val extract_bpattern: json -> 'b -> ('a,'b) pat2 -> json * 'b
val extract_apattern_opt: json -> 'a -> 'a pat -> json * 'a
val extract_bpattern_opt: json -> 'b -> ('a,'b) pat2 -> json * 'b

