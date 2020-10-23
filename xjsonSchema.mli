open Xjson

type t
type schema
type type_expr

exception InvalidSchema of string * json

val parse_schema: json -> t list

val load: string -> t list

val string_of_type_expr: type_expr -> string

val latex_of: t -> string

val prepare_schema: t list -> schema

val assign_type: schema -> json -> type_expr
