(* Copyright (c) 2006-2021, Wojciech Jaworski
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

open Xstd

module type TokenType =
  sig

    type t
    val compare : t -> t -> int
    val to_string : t -> string
    val simplify : t -> t
    val tokenize : string -> t list

  end

module type TRIE =
    functor (Token : TokenType) ->
  sig

    type token = Token.t
    type 'a t

    val empty : 'a t
    val create : (token list * 'a) list -> 'a t
    val find : 'a t -> string -> (token list * 'a list) list
    val find_first : 'a t -> string -> token list * token list * 'a list * token list
    val load : string -> string list -> string t
    val load_multipath : (string * string list) list -> string t
    val rename_prods : 'a t -> ('a -> 'a) -> 'a t 

  end

module Make : TRIE =
  functor(Token : TokenType) ->
  struct

    module TokenMap = Xmap.Make(Token)
    type token = Token.t
    type 'a t = M of 'a t TokenMap.t * 'a list
    
    let empty = M(TokenMap.empty,[])
    
    let rec extract_paths path found (M(map,prods)) =
      let found = (List.rev path, prods) :: found in
      TokenMap.fold map found (fun found k t ->
        extract_paths (k :: path) found t)

    let print t prod_fun =
      let l = List.sort compare (extract_paths [] [] t) in
      Xlist.iter l (fun (path, prods) ->
        print_endline ((String.concat "|" (Xlist.map path Token.to_string)) ^ "\t" ^ (String.concat "|" (Xlist.map prods prod_fun))))

    let rec add_path prod orth_suf (M(map,prods)) =
      if orth_suf = [] then M(map,prod :: prods) else
      let orth = Token.simplify (List.hd orth_suf) in
      let trie = try TokenMap.find map orth with Not_found -> empty in
      let trie = add_path prod (List.tl orth_suf) trie in
      M(TokenMap.add map orth trie,prods)

    let create rules =
      let trie = Xlist.fold rules empty (fun trie (orth,prod) ->
        add_path prod orth trie) in
      trie

    let rec find_rec cand orth rev (M(map,prods)) =
      let cand = if prods = [] then cand else rev, prods, orth in
      if orth = [] then cand else 
      try 
        find_rec cand (List.tl orth) (List.hd orth :: rev) (TokenMap.find map (Token.simplify (List.hd orth))) 
      with Not_found -> cand

    let rec find_one trie orth rev =
      if orth = [] then raise Not_found else
      let matched,prods,rest = find_rec ([],[],[]) orth [] trie in
      if prods = [] then find_one trie (List.tl orth) (List.hd orth :: rev) else
      List.rev rev, List.rev matched, prods, rest

    let rec find_all trie orth rev =
    (*   print_endline "find_all 1"; *)
      if orth = [] then List.rev rev else
      try 
    (*     print_endline "find_all 2"; *)
        let pref,matched,prods,rest = find_one trie orth [] in
    (*     print_endline "find_all 3"; *)
        find_all trie rest ((matched,prods) :: (pref,[]) :: rev)
      with Not_found -> List.rev ((orth,[]) :: rev)
      
    let find trie s = 
      find_all trie (Token.tokenize s) []

    let find_first trie s =
      let orth = Token.tokenize s in
      if orth = [] then raise Not_found else
      find_one trie orth []
      
    let load path filenames =
      let rules = Xlist.fold filenames [] (fun rules filename ->
        Xlist.fold (File.load_lines (path ^ filename ^ ".tab")) rules (fun rules s ->
          if s = "" then rules else
          (Token.tokenize s, filename) :: rules)) in
      create rules

    let load_multipath paths_filenames =
      let rules = 
        Xlist.fold paths_filenames [] (fun rules (path,filenames) ->
          Xlist.fold filenames rules (fun rules filename ->
            Xlist.fold (File.load_lines (path ^ filename ^ ".tab")) rules (fun rules s ->
              if s = "" then rules else
              (Token.tokenize s, filename) :: rules))) in
      create rules
      
    let rec rename_prods (M(map,prods)) rename_fun =
      let map = TokenMap.map map (fun t -> rename_prods t rename_fun) in
      let prods = Xlist.map prods rename_fun in
      M(map,prods)

  end

module CharToken = struct

  type t = string
  let compare = compare
  
  let to_string s = s
  
  let simplify = function
    "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> "0"
  | s -> s
  
  let tokenize = Xunicode.utf8_chars_of_utf8_string

end

module CharTrie = Make(CharToken)
