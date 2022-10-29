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

open Big_int_Z

type t = big_int

let is_empty a = eq_big_int a zero_big_int

let empty = zero_big_int

let rec full_rec n a =
  if n = 0 then a else
  full_rec (n-1) (add_big_int (shift_left_big_int a 1) unit_big_int)

let full n = full_rec n zero_big_int

let intersection a b = and_big_int a b

let union a b = or_big_int a b

let difference a b =
  xor_big_int a (and_big_int a b)

let mem a n =
  not (eq_big_int (and_big_int a (shift_left_big_int unit_big_int n)) zero_big_int)

let add a n =
  or_big_int a (shift_left_big_int unit_big_int n)

let compare = compare_big_int

let equal = eq_big_int

module OrderedBigInt = struct
  type t = big_int
  let compare = compare_big_int
end

module BigIntMap = Xmap.Make(OrderedBigInt)

let rec generate_partition_key key rev = function
    [] -> key, List.rev rev
  | m :: l ->
      generate_partition_key
        (add_big_int (shift_left_big_int key 1) (and_big_int m unit_big_int))
        ((shift_right_big_int m 1) :: rev) l

let rec is_zero_list = function
    [] -> true
  | m :: l -> if eq_big_int m zero_big_int then is_zero_list l else false

let rec create_partition_map map i l =
  if is_zero_list l then map else
  let key,l = generate_partition_key zero_big_int [] l in
  let map = BigIntMap.add_inc map key [i] (fun l -> i :: l) in
  create_partition_map map (i+1) l

let partition l =
  let map = BigIntMap.remove (create_partition_map BigIntMap.empty 0 l) zero_big_int in
  BigIntMap.fold map [] (fun part _ l ->
    Xlist.fold l zero_big_int (fun m i -> or_big_int m (shift_left_big_int unit_big_int i)) :: part)

let rec list_of_bits rev m =
  if is_empty m then rev else
  let bit = if is_empty (intersection m unit_big_int) then "0" else "1" in
  list_of_bits (bit :: rev) (shift_right_big_int m 1)

let to_string a = String.concat "" (List.rev (list_of_bits [] a))

let to_string_size size a =
  let s = to_string a in
  if String.length s > size then failwith "to_string_size" else
  s ^ String.make (size - String.length s) '0'
