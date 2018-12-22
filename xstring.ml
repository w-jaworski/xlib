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
