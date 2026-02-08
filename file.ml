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

let file_out filename f =
  let file = open_out filename in
  let x = f file in
  close_out file;
  x

let file_out_append filename f =
  let file = open_out_gen [Open_wronly; Open_append; Open_creat] ((6*8+4)*8+4) filename in
  let x = f file in
  close_out file;
  x

let file_in filename f =
  let file = open_in filename in
  let x = f file in
  close_in file;
  x

let rec decompress_gzip ic s buf =
  let n = Gzip.input ic s 0 (Bytes.length s) in
  if n = 0 then () else (
    Buffer.add_subbytes buf s 0 n;
    decompress_gzip ic s buf)

let load_file_gzip filename =
  let ic = Gzip.open_in filename in
  let s = Bytes.create 131072 in
  let buf = Buffer.create 131072 in
  decompress_gzip ic s buf;
  Gzip.close_in ic;
  Buffer.contents buf

let rec decompress_bz2 ic s buf =
  let n = try Bz2.read ic s 0 (Bytes.length s) with End_of_file -> 0 in
  if n = 0 then () else (
    Buffer.add_subbytes buf s 0 n;
    decompress_bz2 ic s buf)

let load_file_bz2 filename =
  let c = open_in filename in
  let ic = Bz2.open_in c in
  let s = Bytes.create 131072 in
  let buf = Buffer.create 131072 in
  decompress_bz2 ic s buf;
  Bz2.close_in ic;
  close_in c;
  Buffer.contents buf

let load_file filename =
  let size = (Unix.stat filename).Unix.st_size in
  if size > Sys.max_string_length then
    if Sys.max_string_length = 16777211 then failwith ("file " ^ filename ^ " is to long, try executing program on a 64-bit platform")
    else failwith ("file " ^ filename ^ " is to long") else
  let buf = Bytes.create size in
  file_in filename (fun file ->
    ignore (really_input file buf 0 size));
  Bytes.to_string buf

let load_file_gen filename =
  if Xstring.check_sufix ".gz" filename then load_file_gzip filename else
  if Xstring.check_sufix ".bz2" filename then load_file_bz2 filename else
  load_file filename

let load_lines filename =
  List.rev (List.fold_left (fun l line ->
    if String.length line = 0 then line :: l else
    if String.sub line 0 1 = "#" then l else
    let line = if String.get line (String.length line - 1) = '\r' then
      String.sub line 0 (String.length line - 1) else line in
    line :: l) [] (Xstring.split "\n" (load_file filename)))

let rec remove_copyright = function
    "#</COPYRIGHT>" :: l -> l
  | "#</COPYRIGHT>\r" :: l -> l
  | _ :: l -> remove_copyright l
  | [] -> failwith "remove_copyright"

let rec remove_headers = function
    "" :: l -> remove_headers l
  | "#<COPYRIGHT>" :: l -> remove_headers (remove_copyright l)
  | "#<COPYRIGHT>\r" :: l -> remove_headers (remove_copyright l)
  | s :: l -> if String.get s 0 = '#' then remove_headers l else s :: l
  | [] -> []

let load_tab filename f =
  let l = Xstring.split_delim "\n" (load_file_gen filename) in
  let l = remove_headers l in
  List.rev (List.fold_left (fun l line ->
    if String.length line = 0 then l else
    if String.get line 0 = '#' then l else
    let line = if String.get line (String.length line - 1) = '\r' then
      String.sub line 0 (String.length line - 1) else line in
    (f (Xstring.split_delim "\t" line)) :: l) [] l)

let fold_tab filename s f =
  let l = Xstring.split_delim "\n" (load_file_gen filename) in
  let l = remove_headers l in
  List.fold_left (fun s line ->
    if String.length line = 0 then s else
    if String.get line 0 = '#' then s else
    let line = if String.get line (String.length line - 1) = '\r' then
      String.sub line 0 (String.length line - 1) else line in
    f s (Xstring.split_delim "\t" line)) s l

let load_attr_val_pairs filename =
  List.rev (List.fold_left (fun l s ->
    if s = "" then l else
    match Str.split (Str.regexp "=") s with
      [attr;v] -> (attr,v) :: l
    | _ -> failwith ("load_attr_val_pairs: " ^ s)) [] (load_lines filename))

let catch_no_file load_fun data =
  try load_fun data
  with Unix.Unix_error(Unix.ENOENT, "stat", filename) ->
    (prerr_endline ("File " ^ filename ^ " not found."); data)
