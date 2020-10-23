open Xjson
open Xstd

type type_expr = 
    Atom of string 
  | Union of type_expr list
  | Var of string 
  | Param of type_expr * type_expr (* np. a' list *)
  | Lst of type_expr
  | Str
  | Num
  | Bool
  | Null

type field = {label: string; domain: type_expr; is_required: bool}

type t = 
    Struct of type_expr * field list
  | Enum of type_expr * string list
(*   | Lst of t *)
  
type schema = (type_expr * type_expr) list StringMap.t * type_expr list StringMap.t
  
let rec parse_type_expr = function
    JString "string" -> Str
  | JString "number" -> Num
  | JString "boolean" -> Bool
  | JString s -> Atom s
  | JObject["var",JString s] -> Var s
  | JObject["union",JArray l] -> Union(Xlist.map l parse_type_expr)
  | JObject["param",p;"collection",c] -> Param(parse_type_expr p,parse_type_expr c)
  | JObject["list",t] -> Lst(parse_type_expr t)
  | json -> failwith ("parse_type_expr: " ^ json_to_string json)
  
let parse_schema_rec = function
    JObject["range", range; "fields", JArray fields] -> 
      let range = parse_type_expr range in
      let fields = Xlist.map fields (function
          JObject["label", JString label; "domain", domain; "is-required", is_required] -> 
            let domain = parse_type_expr domain in
            let is_required = match is_required with
                JTrue -> true
              | JFalse -> false
              | json -> failwith ("parse_schema_rec 4: " ^ json_to_string json) in
            {label; domain; is_required}
        | json -> failwith ("parse_schema_rec 1: " ^ json_to_string json)) in
      Struct(range,fields)
  | JObject["range", range; "values", JArray values] -> 
      let range = parse_type_expr range in
      let values = Xlist.map values (function
          JString s -> s
        | json -> failwith ("parse_schema_rec 2: " ^ json_to_string json)) in
      Enum(range,values)
  | json -> failwith ("parse_schema_rec 3: " ^ json_to_string json)
  
let parse_schema = function
    JArray l -> List.rev (Xlist.rev_map l parse_schema_rec)
  | json -> failwith ("parse_schema: " ^ json_to_string json)
  
let load filename =
  parse_schema (json_of_string (File.load_file filename))
  
let rec string_of_type_expr = function
    Atom s -> s
  | Union l -> String.concat " + " (Xlist.map l string_of_type_expr)
  | Var s -> "'" ^ s
  | Param(a,b) -> string_of_type_expr a ^ " " ^ string_of_type_expr b
  | Lst a -> string_of_type_expr a ^ " LIST"
  | Str -> "string"
  | Num -> "number"
  | Bool -> "boolean"
  | Null -> "null"
  
let rec latex_of_type_expr = function
    Atom s -> "\\text{\\it " ^ s ^ "}"
  | Union l -> String.concat " \\cup " (Xlist.map l latex_of_type_expr)
  | Var "alpha" -> "\\alpha"
  | Param(a,b) -> latex_of_type_expr a ^ " \\; " ^ latex_of_type_expr b
  | Lst a -> latex_of_type_expr a ^ " \\; \\text{\\it LIST}"
  | Str -> "\\text{\\it string}"
  | Num -> "\\text{\\it number}"
  | Bool -> "\\text{\\it boolean}"
  | Null -> "\\text{\\it null}"
  | Var _ -> failwith "latex_of_type_expr"
  
let latex_of = function
    Struct(range,fields) ->
      "\\[\\left[\\begin{array}{ll}\n" ^ latex_of_type_expr range ^ " \\\\\n" ^
      String.concat "" (Xlist.map fields (fun f -> 
        "\\text{\\sc " ^ f.label ^ "}" ^ (if f.is_required then "\\star" else "") ^ " & " ^ 
        latex_of_type_expr f.domain ^ "\\\\\n")) ^ 
      "\\end{array}\\right]\\]"
  | Enum(range,values) -> 
     "\\[" ^ latex_of_type_expr range ^ " = \\{" ^ String.concat ", " (Xlist.map values (fun s -> "\\text{" ^ s ^ "}")) ^ "\\}\\]"
     
     
let prepare_schema l = 
  Xlist.fold l (StringMap.empty,StringMap.empty) (fun (fields,values) -> function
      Struct(range,l) -> 
        Xlist.fold l fields (fun fields f ->
          StringMap.add_inc fields f.label [range,f.domain] (fun l -> (range,f.domain) :: l)), values
    | Enum(range,l) ->
        fields, Xlist.fold l values (fun l s -> StringMap.add_inc values s [range] (fun l -> range :: l)))
  
module OrderedType = struct
  
  type t = type_expr
  let compare = compare
  
end

module TypeSet = Xset.Make(OrderedType)

exception InvalidSchema of string * json
exception InvalidSchema2 of string
  
let rec intersect_cands = function (* Argumentami są typy bez zmiennych *)
    Null,c -> c
  | c,Null -> c
  | Union l1, Union l2 -> 
      (match TypeSet.to_list (TypeSet.intersection (TypeSet.of_list l1) (TypeSet.of_list l2)) with
        [t] -> t
      | [] -> raise (InvalidSchema2 "Type contradiction")
      | l -> Union l)
  | Union l, t -> intersect_cands (Union l,Union[t])
  | t, Union l -> intersect_cands (Union[t],Union l)
  | t1, t2 -> intersect_cands (Union[t1],Union[t2])

(*let mem_type a b = 
  let l = match b with
      Union l -> l
    | _ -> b in
  try 
    let _ = intersect_cands (a,b) in true
  with _ -> false *)
  
let merge l1 l2 =
  let map = Xlist.fold l1 StringMap.empty (fun map (v,t) ->
    StringMap.add map v t) in
  try 
    let map = Xlist.fold l2 map (fun map (v,t) ->
      StringMap.add_inc map v t (fun t2 -> if t = t2 then t else raise Not_found)) in (* uproszczenie, nie uwzględnia zmiennych *)
    [StringMap.fold map [] (fun l v t -> (v,t) :: l)]
  with Not_found -> []
  
let make_unique l =
  let map = Xlist.fold l StringMap.empty (fun map l ->
    let l = Xlist.sort l compare in
    let s = String.concat "#" (Xlist.map l (fun (v,t) -> v ^ ":" ^ string_of_type_expr t)) in
    StringMap.add map s l) in
  StringMap.fold map [] (fun l _ t -> t :: l)
  
let rec unify = function 
    Null, t -> [[]]
  | t, Null -> [[]]
  | Union l, t -> make_unique (Xlist.fold l [] (fun l t2 -> unify (t2,t) @ l))
  | t, Union l -> make_unique (Xlist.fold l [] (fun l t2 -> unify (t,t2) @ l))
  | Atom s1, Atom s2 -> if s1=s2 then [[]] else []
  | Atom s, Var v -> [[v,Atom s]]
  | Var v, Atom s -> [[v,Atom s]]
  | Atom _, _ -> []
  | _, Atom _ -> []
  | Param(s1,t1), Param(s2,t2) -> 
      make_unique (Xlist.fold (unify (s1,s2)) [] (fun l l1 ->
        Xlist.fold (unify (t1,t2)) l (fun l l2 ->
          merge l1 l2 @ l)))
  | Param(s,t), Var v -> [[v,Param(s,t)]]
  | Var v, Param(s,t) -> [[v,Param(s,t)]]
  | Param _, _ -> []
  | _, Param _ -> []
  | Lst t1, Lst t2 -> unify (t1,t2)
  | Lst t, Var v -> [[v,Lst t]]
  | Var v, Lst t -> [[v,Lst t]]
  | Lst _, _ -> []
  | _, Lst _ -> []
  | Str, Str -> [[]]
  | Str, Var v -> [[v,Str]]
  | Var v, Str -> [[v,Str]]
  | Str, _ -> []
  | _, Str -> []
  | Num, Num -> [[]]
  | Num, Var v -> [[v,Num]]
  | Var v, Num -> [[v,Num]]
  | Num, _ -> []
  | _, Num -> []
  | Bool, Bool -> [[]]
  | Bool, Var v -> [[v,Bool]]
  | Var v, Bool -> [[v,Bool]]
  | Var _, Var _ -> failwith "unify: ni"
  
let rec substitute subst = function
    Atom s -> Atom s
  | Union l -> Union(Xlist.map l (substitute subst))
  | Var s -> (try Xlist.assoc subst s with Not_found -> Var s)
  | Param(a,b) -> Param(substitute subst a,substitute subst b)
  | Lst a -> Lst(substitute subst a)
  | Str -> Str
  | Num -> Num
  | Bool -> Bool
  | Null -> Null
  
let rec assign_type schema = function
    JObject l -> 
      (try
(*       print_endline ("assign_type 1: " ^ json_to_string_fmt "" (JObject l)); *)
      Xlist.fold l Null (fun cand (e,t) -> 
(*         print_endline ("assign_type 2: " ^ e); *)
        let domain = assign_type schema t in
(*         print_endline ("assign_type 3: " ^ string_of_type_expr domain); *)
        let l = try StringMap.find (fst schema) e with Not_found -> raise (InvalidSchema2 ("Unknown label '" ^ e ^ "'")) in
        let cands2 = Xlist.fold l [] (fun cands2 (cand2,domain2) ->
(*           print_endline ("assign_type 4: " ^ string_of_type_expr cand2); *)
(*           print_endline ("assign_type 5: " ^ string_of_type_expr domain2); *)
          let l = unify (domain2,domain) in
          Xlist.fold l cands2 (fun cands2 subst -> substitute subst cand2 :: cands2)) in
        if cands2 = [] then 
          let doms = string_of_type_expr (Union (TypeSet.to_list (TypeSet.of_list (Xlist.map l snd)))) in
          raise (InvalidSchema2 ("For label '" ^ e ^ "', type '" ^ doms ^ "' expected while '" ^ string_of_type_expr domain ^ "' found")) else (
(*         print_endline ("assign_type 6: " ^ string_of_type_expr cand); *)
(*         print_endline ("assign_type 7: " ^ string_of_type_expr (Union cands2)); *)
        let t = 
          try intersect_cands (cand,Union cands2) 
          with InvalidSchema2 "Type contradiction" -> raise (InvalidSchema2("For label '" ^ e ^ "', type " ^ string_of_type_expr (Union cands2) ^ " contradicts with " ^ string_of_type_expr cand)) in
(*         print_endline ("assign_type 8: " ^ string_of_type_expr t); *)
        t))
      with InvalidSchema2 s -> raise (InvalidSchema(s,JObject l)))
  | JArray l -> 
      (match TypeSet.to_list (Xlist.fold l TypeSet.empty (fun set t -> TypeSet.add set (assign_type schema t))) with
        [] -> Null
      | [t] -> Lst t
      | l -> Lst (Union l))
  | JString s -> 
      (try Union(Str :: (StringMap.find (snd schema) s;))
      with Not_found -> Str)
  | JNumber _ -> Num
  | JTrue -> Bool
  | JFalse -> Bool
  | JNull -> Null
  | _ -> failwith "assign_type"
  
