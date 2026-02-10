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

let check_prefix pat s =
  let n = String.length pat in
  if n > String.length s then false else
  String.sub s 0 n = pat

let cut_prefix pat s =
  let i = String.length pat in
  let n = String.length s in
  if i >= n then "" else
  try String.sub s i (n-i) with _ -> failwith ("cut_prefix: " ^ s ^ " " ^ string_of_int i)

let check_sufix pat s =
  let n = String.length pat in
  let m = String.length s in
  if n > m then false else
  String.sub s (m-n) n = pat

let cut_sufix pat s =
  let i = String.length pat in
  let n = String.length s in
  try String.sub s 0 (n-i) with _ -> failwith ("cut_sufix: " ^ pat ^ " " ^ s)

let size s = String.length s

let split pat s = Str.split (Str.regexp pat) s
let split_delim pat s = Str.split_delim (Str.regexp pat) s

let full_split pat s =
  List.rev (List.rev_map (function
      Str.Text s -> s
    | Str.Delim s -> s) (Str.full_split (Str.regexp pat) s))

let contains_substring pat text =
  try
    let _ = Str.search_forward (Str.regexp_string pat) text 0 in
    true
  with Not_found -> false

let rec remove_trailing_spaces s =
  if s = "" then s else
  if check_sufix " " s then
    remove_trailing_spaces (cut_sufix " " s)
  else s
  
let rec remove_heading_spaces s =
  if s = "" then s else
  if check_prefix " " s then
    remove_heading_spaces (cut_prefix " " s)
  else s
  
let remove_spaces s =
  remove_heading_spaces (remove_trailing_spaces s)
  
let rec remove_trailing_white s =
  if s = "" then s else
  if check_sufix " " s then
    remove_trailing_white (cut_sufix " " s) else
  if check_sufix " " s then (*\194\160*)
    remove_trailing_white (cut_sufix " " s) else
  if check_sufix "\t" s then
    remove_trailing_white (cut_sufix "\t" s) else
  if check_sufix "\n" s then
    remove_trailing_white (cut_sufix "\n" s) else
  if check_sufix "\r" s then
    remove_trailing_white (cut_sufix "\r" s)
  else s
  
let rec remove_heading_white s =
  if s = "" then s else
  if check_prefix " " s then
    remove_heading_white (cut_prefix " " s) else
  if check_prefix " " s then
    remove_heading_white (cut_prefix " " s) else
  if check_prefix "\t" s then
    remove_heading_white (cut_prefix "\t" s) else
  if check_prefix "\n" s then
    remove_heading_white (cut_prefix "\n" s) else
  if check_prefix "\r" s then
    remove_heading_white (cut_prefix "\r" s)
  else s
  
let remove_white s =
  remove_heading_white (remove_trailing_white s)
  
