(* Copyright (c) 2017-2019, Wojciech Jaworski
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
*)

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

let json_escape s = (* FIXME: escapowanie \u2028 and \u2029 *)
  let t = Buffer.create (Xstring.size s) in
  Int.iter 0 (String.length s - 1) (fun i ->
    match String.get s i with
       '\b' -> Buffer.add_string t "\\b"
     | '\012' -> Buffer.add_string t "\\f"
     | '\n' -> Buffer.add_string t "\\n"
     | '\r' -> Buffer.add_string t "\\r"
     | '\t' -> Buffer.add_string t "\\t"
     | '"' -> Buffer.add_string t "\\\""
     | '\\' -> Buffer.add_string t "\\\\"
     | c -> 
        if Char.code c < 32 then Buffer.add_string t (Printf.sprintf "\\x%02X" (Char.code c))
        else Buffer.add_char t c);
  Buffer.contents t

let json_convert_comma n =
  match Xstring.split_delim "," n with
    [a;b] -> a ^ "." ^ b
  | [a] -> a
  | _ -> failwith ("json_convert_comma: " ^ n)

let rec json_to_string = function
    JObject l -> "{" ^ String.concat "," (List.rev (Xlist.rev_map l (fun (k,v) ->
        Printf.sprintf "\"%s\": %s" (json_escape k) (json_to_string v)))) ^ "}"
  | JArray l -> "[" ^ String.concat "," (List.rev (Xlist.rev_map l (fun v ->
        json_to_string v))) ^ "]"
  | JString s -> "\"" ^ (json_escape s) ^ "\""
  | JVar s -> "?" ^ (json_escape s)
  | JNumber n -> json_convert_comma n
  | JNull -> "null"
  | JTrue -> "true"
  | JFalse -> "false"
  | JEmpty -> (*"\"empty\""*)"null"
  | JContradiction -> "contradiction"

let rec json_to_string_fmt spaces = function
    JObject l -> "{" ^ String.concat "," (List.rev (Xlist.rev_map l (fun (k,v) ->
        Printf.sprintf "\n%s\"%s\": %s" spaces (json_escape k) (json_to_string_fmt (spaces ^ "  ") v)))) ^ "}"
  | JArray l -> "[" ^ String.concat "," (List.rev (Xlist.rev_map l (fun v ->
        Printf.sprintf "\n%s%s" spaces (json_to_string_fmt (spaces ^ "  ") v)))) ^ "]"
  | JString s -> "\"" ^ (json_escape s) ^ "\""
  | JVar s -> "?" ^ (json_escape s)
  | JNumber n -> json_convert_comma n
  | JNull -> "null"
  | JTrue -> "true"
  | JFalse -> "false"
  | JEmpty -> (*"\"empty\""*)"null"
  | JContradiction -> "contradiction"

let rec json_to_string_fmt2 spaces = function
    JObject[k,v] -> "{" ^ Printf.sprintf "\"%s\": %s" (json_escape k) (json_to_string_fmt2 (spaces ^ "  ") v) ^ "}"
  | JObject l -> "{" ^ String.concat "," (List.rev (Xlist.rev_map l (fun (k,v) ->
        Printf.sprintf "\n%s\"%s\": %s" spaces (json_escape k) (json_to_string_fmt2 (spaces ^ "  ") v)))) ^ "}"
  | JArray l -> "[" ^ String.concat "," (List.rev (Xlist.rev_map l (fun v ->
        Printf.sprintf "\n%s%s" spaces (json_to_string_fmt2 (spaces ^ "  ") v)))) ^ "]"
  | JString s -> "\"" ^ (json_escape s) ^ "\""
  | JVar s -> "?" ^ (json_escape s)
  | JNumber n -> json_convert_comma n
  | JNull -> "null"
  | JTrue -> "true"
  | JFalse -> "false"
  | JEmpty -> (*"\"empty\""*)"null"
  | JContradiction -> "contradiction"

type syntax =
      T of string
    | X of string
    | O of string
    | C of string
    | B of string * string * syntax list

let rec string_of_syntax = function
      O s -> "„" ^ s ^ "”"
    | X s -> (*"'" ^*) s (*^ "'"*)
    | T s -> "\"" ^ s ^ "\""
    | C s -> s
    | B(s,t,l) -> s ^ string_of_syntax_list l ^ t

and string_of_syntax_list l =
    String.concat " " (Xlist.map l string_of_syntax)

let rec find_atomic_symbols l =
    List.rev (Xlist.rev_map l (function
      T t -> T t
    | X "[" -> O "["
    | X "]" -> O "]"
    | X "{" -> O "{"
    | X "}" -> O "}"
    | X "," -> O ","
    | X ":" -> O ":"
    | X "null" -> C "null"
    | X "true" -> C "true"
    | X "false" -> C "false"
    | X x -> X x
    | _ -> failwith "Xjson.find_atomic_symbols"))

exception Closing_bracket_not_found
    
let rec find_brackets brackets rev = function
      (O s) :: l ->
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_brackets 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_brackets 2: " ^ string_of_syntax_list found); *)
           find_brackets brackets (B(s,t,found) :: rev) l
         with Not_found -> find_brackets brackets ((O s) :: rev) l)
    | B _ :: _ -> failwith "Xjson.find_brackets"
    | t :: l -> find_brackets brackets (t :: rev) l
    | [] -> List.rev rev

and find_rbracket rb brackets rev = function
      (O s) :: l ->
         if s = rb then List.rev rev, l else
         (try
           let t = Xlist.assoc brackets s in
(*            print_endline ("find_rbracket 1: " ^ string_of_syntax_list ((O s) :: l)); *)
           let found,l = find_rbracket t brackets [] l in
(*            print_endline ("find_rbracket 2: " ^ string_of_syntax_list found); *)
           find_rbracket rb brackets ((B(s,t,found)) :: rev) l
         with Not_found -> find_rbracket rb brackets ((O s) :: rev) l)
    | (B _) :: _ -> failwith "Xjson.find_rbracket 1"
    | t :: l -> find_rbracket rb brackets (t :: rev) l
    | [] -> raise Closing_bracket_not_found

let rec split_op_comma found rev = function
      (O ",") :: l -> split_op_comma ((List.rev rev) :: found) [] l
    | s :: l -> split_op_comma found (s :: rev) l
    | [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)

let rec merge_quoted2 rev = function
      [] -> failwith "Xjson.merge_quoted2"
    | "\"" :: tokens -> String.concat "" (List.rev rev), tokens
    | "\\" :: "\"" :: tokens -> merge_quoted2 ("\"" :: rev) tokens
    | "\\" :: "\\" :: tokens -> merge_quoted2 ("\\" :: rev) tokens
    | "\\" :: s :: tokens -> 
       (match String.get s 0 with
         'b' -> merge_quoted2 ("\b" :: rev) (Xstring.cut_prefix "b" s :: tokens)
	   | 'f' -> merge_quoted2 ("\012" :: rev) (Xstring.cut_prefix "f" s :: tokens)
	   | 'n' -> merge_quoted2 ("\n" :: rev) (Xstring.cut_prefix "n" s :: tokens)
	   | 'r' -> merge_quoted2 ("\r" :: rev) (Xstring.cut_prefix "r" s :: tokens)
	   | 't' -> merge_quoted2 ("\t" :: rev) (Xstring.cut_prefix "t" s :: tokens)
	   | 'u' -> 
	      if Xstring.size s < 5 then failwith "Xjson.merge_quoted2" else
	      let t = String.sub s 1 4 in
	      let x = Scanf.sscanf t "%x" (fun y -> y) in
	      let t2 = Xunicode.string_of_uchar x in
	      merge_quoted2 (t2 :: rev) (Xstring.cut_prefix "u0000" s :: tokens)
	   | '/' -> merge_quoted2 ("/" :: rev) (Xstring.cut_prefix "/" s :: tokens)
	   | _ -> failwith "Xjson.merge_quoted2")
    | "\\" :: _ -> failwith "Xjson.merge_quoted2"
    | s :: tokens -> merge_quoted2 (s :: rev) tokens

let rec merge_quoted rev = function
      [] -> List.rev rev
    | "\"" :: tokens -> let s, tokens = merge_quoted2 [] tokens in merge_quoted ((T s) :: rev) tokens
    | x :: tokens -> merge_quoted ((X x) :: rev) tokens

let rec merge_quoted_tail rev = function
      [] -> List.rev rev, []
    | "\"" :: tokens -> 
       (try
         let s, tokens = merge_quoted2 [] tokens in merge_quoted_tail ((T s) :: rev) tokens
       with _ -> List.rev rev, "\"" :: tokens)
    | [x] -> List.rev rev, [x]
    | x :: tokens -> merge_quoted_tail ((X x) :: rev) tokens

let is_number s =
  Int.fold 0 (String.length s-1) true (fun b i ->
    if String.get s i = '.' || String.get s i = '-' || (String.get s i >= '0' && String.get s i <= '9') then b else false)

let rec parse_tokens = function
    [B("[","]",l)] -> JArray(List.rev (Xlist.rev_map (split_op_comma [] [] l) parse_tokens))
  | [B("{","}",l)] -> JObject(List.rev (Xlist.rev_map (split_op_comma [] [] l) parse_entry))
  | [T t] -> JString t
  | [C "null"] -> JNull
  | [C "true"] -> JTrue
  | [C "false"] -> JFalse
  | [X ""] -> failwith "Xjson.parse_tokens: empty atomic symbol"
  | [X x] -> if is_number x then JNumber x else failwith ("Xjson.parse_tokens: " ^ x)
  | [] -> failwith "Xjson.parse_tokens: empty token list"
  | l -> failwith ("Xjson.parse_tokens: " ^ string_of_syntax_list l)

and parse_entry = function
    T e :: O ":" :: l -> e, parse_tokens l
  | _ -> failwith "Xjson.parse_entry"

let make_split s =
  List.rev (Xlist.rev_map (Str.full_split
                         (Str.regexp "\\]\\| \\|\t\\|\n\\|\r\\|\\:\\|{\\|}\\|,\\|\\[\\|\"\\|\\") s) (function
              Str.Text s -> s
            | Str.Delim s -> s))
            
let remove_white tokens = 
  List.rev (Xlist.fold tokens [] (fun tokens -> function
          X " " -> tokens
        | X "\t" -> tokens
        | X "\n" -> tokens
        | X "\r" -> tokens
        | t -> t :: tokens))
        
let json_of_string s =
    let tokens = make_split s in
    let tokens = merge_quoted [] tokens in
    let tokens = remove_white tokens in
    let l = find_atomic_symbols tokens in
    let l = find_brackets ["{","}";"[","]"] [] l in
    parse_tokens l

let remove_front t = function
    O s :: l -> if s = t then l else failwith ("remove_front: pat=" ^ t ^ " found=" ^ s)
  | [] -> failwith "remove_front: empty"
  | l -> failwith ("remove_front: " ^ string_of_syntax_list (Xlist.prefix 10 l))(*; failwith "remove_front"*)
 
let rec fold_file_rec limit count l s f =
(*   Printf.printf "fold_file_rec: |l|=%d %s \n%!" (Xlist.size l) (string_of_syntax_list (Xlist.prefix 10 l)); *)
  if l = [O "]"] then s,[] else
  if Xlist.size l < 3 || !count = limit then s,l else
  try
    let l = remove_front (if !count = 0 then "[" else ",") l in
    let l = remove_front "{" l in
    let first,rest = find_rbracket "}" ["{","}";"[","]"] [] l in
    let s = f s (parse_tokens [B("{","}",first)]) in
    incr count;
    fold_file_rec limit count rest s f
  with Closing_bracket_not_found -> s,l

let fold_file ?(step = 5000000) ?(limit = -1) filename s f =
  let size = ref ((Unix.stat filename).Unix.st_size) in
  let buf = Bytes.create step in
  let tail = ref "" in
  let r = ref s in
  let count = ref 0 in
  let rest = ref [] in
  File.file_in filename (fun file ->
    while !size <> 0 && !count <> limit do
(*       Printf.printf "fold_file 1: size=%d count=%d\n%!" !size !count; *)
      let tail_len = Xstring.size !tail in
      let len = min (step - tail_len) !size in
      Bytes.blit_string !tail 0 buf 0 tail_len;
      ignore (really_input file buf tail_len len);
      size := !size - len;
      let tokens = make_split (Bytes.sub_string buf 0 (len+tail_len)) in
      let tokens, tail2 = 
        if !size = 0 then merge_quoted [] tokens, []
        else merge_quoted_tail [] tokens in
      tail := String.concat "" tail2;
      let tokens = remove_white tokens in
(*       print_endline ("fold_file 2: " ^ string_of_syntax_list tokens); *)
      let l = !rest @ find_atomic_symbols tokens in
(*       print_endline ("fold_file 3: " ^ string_of_syntax_list l); *)
      let s,rest2 = 
        if !count=0 && l=[O "[";O "]"] then !r,[]
        else fold_file_rec limit count l !r f in
      if Xlist.size l = Xlist.size rest2 && l <> [] then raise Closing_bracket_not_found else ( (* step < dłudość rekordu lub błąd w jsonie *)
      r := s;
      rest := rest2)
    done;
(*     Printf.printf "fold_file 4: size=%d count=%d\n%!" !size !count; *)
(*    print_endline ("fold_file 5: " ^ string_of_syntax_list (Xlist.prefix 10 !rest)); *)
    !r)
    

