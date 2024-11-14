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

let iter list f = List.iter f list
let fold list s f = List.fold_left f s list
let map list f = List.map f list
let rev_map list f = List.rev_map f list

let iter2 list list2 f = List.iter2 f list list2
let fold2 list list2 s f = List.fold_left2 f s list list2
let map2 list list2 f = List.map2 f list list2

let rec iter3 list1 list2 list3 f =
  match list1,list2,list3 with
    [],[],[] -> ()
  | x1 :: l1, x2 :: l2, x3 :: l3 -> (f x1 x2 x3; iter3 l1 l2 l3 f)
  | _ -> invalid_arg "List.iter3"

let size = List.length

let tl = List.tl
let hd = List.hd

let mem l e = List.mem e l

let sort l c = List.sort c l

let filter l f = List.filter f l

let assoc l s = List.assoc s l

let rec rev_append rev l =
  if rev = []  then l else
  rev_append (List.tl rev) (List.hd rev :: l)

let rec remove_rec s rev = function
    x :: l -> if x = s then rev_append rev l else remove_rec s (x :: rev) l
  | [] -> List.rev rev

let remove l s = 
  remove_rec s [] l

(* 'a list list -> 'a list list *)
(* [[a1;a2;...;an];[b1;b2;...;bk];[c1;c2;...;cn]] -> [[a1;b1;c1];[a1;b1;c2];...;[a1;b1;cn];[a1;b2;c1];...] *)
let rec multiply_list = function
    [] -> [[]]
  | x :: list ->
      let list = multiply_list list in
      fold x [] (fun mul a ->
	fold list mul (fun mul args ->
	  (a :: args) :: mul))


let rec transpose = function
    [] :: _ -> []
  | [] -> []
  | ll ->
      let first,rest = fold ll ([],[]) (fun (first,rest) l ->
        List.hd l :: first, List.tl l :: rest) in
      List.rev first :: (transpose (List.rev rest))

let rec remove_rec rev e = function
    [] -> List.rev rev
  | x :: l -> if x = e then remove_rec rev e l else remove_rec (x :: rev) e l

let remove_all l e =
  remove_rec [] e l

let remove_all l e =
  List.rev (fold l [] (fun l x -> if e = x then l else x :: l))
  
let rec prefix_rec n rev l =
  if n = 0 || l = [] then List.rev rev else
  prefix_rec (n-1) (List.hd l :: rev) (List.tl l)
  
let prefix n l =
  prefix_rec n [] l
  
let select_min_priority l =
  fold l (max_int,[]) (fun (best_prior,l) (prior,t) ->
    if prior > best_prior then best_prior,l else
    if prior < best_prior then prior,[t] else
    best_prior,t :: l)

let select_max_priority l =
  fold l (min_int,[]) (fun (best_prior,l) (prior,t) ->
    if prior < best_prior then best_prior,l else
    if prior > best_prior then prior,[t] else
    best_prior,t :: l)
  
