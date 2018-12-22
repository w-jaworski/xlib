(* Copyright (c) 2006-2017, Wojciech Jaworski
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

type token =
    T of string
  | B of string * string * token list

let rec find_brackets brackets rev = function
    T s :: l ->
       (try
         let t = Xlist.assoc brackets s in
         let found,l = find_rbracket t brackets [] l in
         find_brackets brackets (B(s,t,found) :: rev) l
       with Not_found -> find_brackets brackets (T s :: rev) l)
  | B _ :: _ -> failwith "find_brackets"
  | [] -> List.rev rev

and find_rbracket rb brackets rev = function
    T s :: l ->
       if s = rb then List.rev rev, l else
       (try
         let t = Xlist.assoc brackets s in
         let found,l = find_rbracket t brackets [] l in
         find_rbracket rb brackets (B(s,t,found) :: rev) l
       with Not_found -> find_rbracket rb brackets (T s :: rev) l)
  | B _ :: _ -> failwith "find_rbracket"
  | [] -> failwith "find_rbracket"

let split pat s =
  List.rev (Xlist.rev_map (Xstring.full_split pat s) (fun s -> T s))

let rec split_symbol symb rev = function
    [] -> [List.rev rev](*failwith "split_symbol"*)
  | s :: l ->
      if s = symb then
        if l = [] then (*[List.rev rev]*)failwith "split_symbol"
        else (List.rev rev) :: (split_symbol symb [] l)
      else split_symbol symb (s :: rev) l

let rec string_of_token = function
    T s -> s
  | B(a,b,l) -> a ^ String.concat "" (Xlist.map l string_of_token) ^ b

let string_of_token_list l =
  String.concat "" (Xlist.map l string_of_token)

let string_of_token_list_space l =
  String.concat " " (Xlist.map l string_of_token)


