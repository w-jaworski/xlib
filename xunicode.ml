(* Copyright (c) 2006-2018, Wojciech Jaworski
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

open Printf

type unicode =
    Digit of string
  | Sign of string
  | Capital of string * string (* wielka * mała *)
  | ForeignCapital of string * string (* wielka * mała *)
  | Small of string * string (* wielka * mała *)
  | ForeignSmall of string * string (* wielka * mała *)
  | Emoticon of string
  | Other of string * int

let to_string = function
    Digit s -> sprintf "Digit(%s)" s
  | Sign s -> sprintf "Sign(%s)" s
  | Capital(s,t) -> sprintf "Capital(%s,%s)" s t
  | ForeignCapital(s,t) -> sprintf "ForeignCapital(%s,%s)" s t
  | Small(s,t) -> sprintf "Small(%s,%s)" s t
  | ForeignSmall(s,t) -> sprintf "ForeignSmall(%s,%s)" s t
  | Emoticon s -> sprintf "Emoticon(%s)" s
  | Other(s,x) -> sprintf "Other(%s,%x)" s x

let char_of_classified_char = function
    Digit s -> s
  | Sign s -> s
  | Capital(s,_) -> s
  | ForeignCapital(s,_) -> s
  | Small(_,t) -> t
  | ForeignSmall(_,t) -> t
  | Emoticon s -> s
  | Other(s,_) -> s


(*let rec make_tail n rev x =
  if n = 0 then rev,x else
  let v = (x land 0b111111) lor 0b10000000 in
  let x = x lsr 6 in
  make_tail (n-1) (v :: rev) x

let string_of_uchar x =
  if x < 0x80 then String.make 1 (Char.chr x) else
  let l =
    if x < 0x800 then let l,x = make_tail 1 [] x in (x lor 0b11000000) :: l else
    if x < 0x10000 then let l,x = make_tail 2 [] x in (x lor 0b11100000) :: l else
    if x < 0x200000 then let l,x = make_tail 3 [] x in (x lor 0b11110000) :: l else
    if x < 0x4000000 then let l,x = make_tail 4 [] x in (x lor 0b11111000) :: l else
    (*if x < 0x80000000 then*) let l,x = make_tail 5 [] x in (x lor 0b11111100) :: l (*else
    failwith "string_of_uchar"*) in
  let s = Bytes.create (Xlist.size l) in
  let _ = Int.fold 0 (Xlist.size l - 1) l (fun l i ->
    Bytes.set s i (Char.chr (List.hd l));
    List.tl l) in
  Bytes.to_string s
(*  let s = Bytes.create (Xlist.size l) in
  let _ = Int.fold 0 (Xlist.size l - 1) l (fun l i ->
    Bytes.set s i (List.hd l);
    List.tl l) in
  Bytes.to_string s*)
(*  let r = ref l in
  String.init (Xlist.size l) (fun _ ->
    let x = List.hd !r in
    r := List.tl !r;
    x)*)*)

let string_of_uchar x =
  let buf = Buffer.create 4 in
  let u = Uchar.of_int x in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let classify x =
  let s = string_of_uchar x in
  if x >= 0x0020 && x <= 0x002F then Sign s else
  if x >= 0x0030 && x <= 0x0039 then Digit s else
  if x >= 0x003A && x <= 0x0040 then Sign s else
  if x >= 0x0041 && x <= 0x005A then
    let lowercase = string_of_uchar (x + 0x20) in
    if x = 0x0051 || x = 0x0056 || x = 0x0058 then ForeignCapital(s,lowercase) else Capital(s,lowercase) else
  if x >= 0x005B && x <= 0x0060 then Sign s else
  if x >= 0x0061 && x <= 0x007A then
    let uppercase = string_of_uchar (x - 0x20) in
    if x = 0x0071 || x = 0x0076 || x = 0x0078 then ForeignSmall(uppercase,s) else Small(uppercase,s) else
   if x >= 0x007B && x <= 0x007E then Sign s else
  if x >= 0x00A0 && x <= 0x00BF then Sign s else
  if x >= 0x00C0 && x <= 0x00DF then
    let lowercase = string_of_uchar (x + 0x20) in
    if x = 0x00D3 then Capital(s,lowercase) else
    if x = 0x00D7 then Sign s else
    if x = 0x00DF then ForeignSmall(string_of_uchar 0x1E9E,s) else ForeignCapital(s,lowercase) else
  if x >= 0x00E0 && x <= 0x00FF then
    let uppercase = string_of_uchar (x - 0x20) in
    if x = 0x00F3 then Small(uppercase,s) else
    if x = 0x00F7 then Sign s else
    if x = 0x00FF then ForeignSmall(string_of_uchar 0x0178,s) else ForeignSmall(uppercase,s) else
  if (x >= 0x0100 && x <= 0x0137) || (x >= 0x0149 && x <= 0x0177) || (x >= 0x1E00 && x <= 0x1E95) || (x >= 0x1EA0 && x <= 0x1EFF) then
    if x mod 2 = 0 then
      let lowercase = string_of_uchar (x + 1) in
      if x = 0x0104 || x = 0x0106 || x = 0x0118 || x = 0x015A then Capital(s,lowercase) else ForeignCapital(s,lowercase)
    else
      let uppercase = string_of_uchar (x - 1) in
      if x = 0x0105 || x = 0x0107 || x = 0x0119 || x = 0x015B then Small(uppercase,s) else ForeignSmall(uppercase,s) else
  if (x >= 0x0138 && x <= 0x0148) || (x >= 0x0179 && x <= 0x017E) then
    if x mod 2 = 1 then
      let lowercase = string_of_uchar (x + 1) in
      if x = 0x0141 || x = 0x0143 || x = 0x0179 || x = 0x017B then Capital(s,lowercase) else ForeignCapital(s,lowercase)
    else
      let uppercase = string_of_uchar (x - 1) in
      if x = 0x0142 || x = 0x0144 || x = 0x017A || x = 0x017C  then Small(uppercase,s) else ForeignSmall(uppercase,s) else
  if x >= 0x1E96 && x <= 0x1E9D then ForeignSmall(s,s) else
  if x >= 0x2010 && x <= 0x2014 then Sign s else
  if x >= 0x2018 && x <= 0x2020 then Sign s else
  if x >= 0x20A0 && x <= 0x20CF then Sign s else
  if x >= 0x2639 && x <= 0x263B then Emoticon s else
  if x >= 0x1F600 && x <= 0x1F64F then Emoticon s else
  match x with
    0x0009 -> Sign s
  | 0x000A -> Sign s
  | 0x000D -> Sign s
  | 0x0178 -> ForeignCapital(s,string_of_uchar 0x00FF)
  | 0x017F -> ForeignSmall(s,"S")
  | 0x02DD -> Sign s
  | 0x03BC -> Sign s
  | 0x1E9E -> ForeignCapital(s,string_of_uchar 0x00DF)
  | 0x1E9F -> ForeignSmall(s,s)
  | 0x2022 -> Sign s
  | 0x2026 -> Sign s
  | 0x2122 -> Sign s
  | 0x2212 -> Sign s
  | 0x1F47F -> Emoticon s
  | 0x1F48F -> Emoticon s
  | 0x1F491 -> Emoticon s
  | _ -> Other(s,x)

(*
U+0000 – U+007F Basic Latin
U+0080 – U+00FF Latin-1 Supplement
U+0100 – U+017F Latin Extended-A
U+0180 – U+024F Latin Extended-B
U+0250 – U+02AF IPA Extensions

U+1E00 – U+1EFF Latin Extended Additional
U+2000 – U+206F General Punctuation
U+20A0 – U+20CF Currency Symbols

U+2C60 – U+2C7F Latin Extended-C

U+A720 – U+A7FF Latin Extended-D

U+1F600 – U+1F64F Emoticons

*)

let decode_byte pos s =
  let c = Char.code (String.get s pos) in
  if c land 0b10000000 = 0b00000000 then 7,c (*land 0b01111111*) else
  if c land 0b11000000 = 0b10000000 then 6,c land 0b00111111 else
  if c land 0b11100000 = 0b11000000 then 5,c land 0b00011111 else
  if c land 0b11110000 = 0b11100000 then 4,c land 0b00001111 else
  if c land 0b11111000 = 0b11110000 then 3,c land 0b00000111 else
  if c land 0b11111100 = 0b11111000 then 2,c land 0b00000011 else
  if c land 0b11111110 = 0b11111100 then 1,c land 0b00000001 else
  failwith ("decode_byte at position " ^ string_of_int pos ^ " of " ^ s)

let decode_byte_simple pos s =
  let c = Char.code (String.get s pos) in
  if c land 0b10000000 = 0b00000000 then 7 else
  if c land 0b11000000 = 0b10000000 then 6 else
  if c land 0b11100000 = 0b11000000 then 5 else
  if c land 0b11110000 = 0b11100000 then 4 else
  if c land 0b11111000 = 0b11110000 then 3 else
  if c land 0b11111100 = 0b11111000 then 2 else
  if c land 0b11111110 = 0b11111100 then 1 else
  failwith ("decode_byte_simple at position " ^ string_of_int pos ^ " of " ^ s)

let decode_tail n s pos =
  Int.fold pos (pos+n-1) 0 (fun y pos ->
    match decode_byte pos s with
      6,x -> (y lsl 6) lor x
    | _ -> failwith "decode_tail")

let rec unicode_chars_of_utf8_string_rec rev pos s =
  if pos = String.length s then rev else
  let n,x = decode_byte pos s in
  if n = 7 then unicode_chars_of_utf8_string_rec (x :: rev) (pos+1) s else
  if n = 6 then failwith "unicode_chars_of_utf8_string_rec" else (
  let tail = decode_tail (6 - n) s (pos+1) in
  let x = (x lsl ((6-n) * 6)) lor tail in
  unicode_chars_of_utf8_string_rec (x :: rev) (pos + 7 - n ) s)

let unicode_chars_of_utf8_string s =
  List.rev (unicode_chars_of_utf8_string_rec [] 0 s)

let classified_chars_of_utf8_string s =
  let l = unicode_chars_of_utf8_string_rec [] 0 s in
  Xlist.rev_map l classify

let lowercase_utf8_string s =
  let l = classified_chars_of_utf8_string s in
  let l = List.rev (Xlist.rev_map l (function
      Digit s -> s
    | Sign s -> s
    | Capital(_,t) -> t
    | ForeignCapital(_,t) -> t
    | Small(_,t) -> t
    | ForeignSmall(_,t) -> t
    | Emoticon s -> s
    | Other(s,_) -> s)) in
  String.concat "" l

let uppercase_utf8_string s =
  let l = classified_chars_of_utf8_string s in
  let l = List.rev (Xlist.rev_map l (function
      Digit s -> s
    | Sign s -> s
    | Capital(s,_) -> s
    | ForeignCapital(s,_) -> s
    | Small(s,_) -> s
    | ForeignSmall(s,_) -> s
    | Emoticon s -> s
    | Other(s,_) -> s)) in
  String.concat "" l

(* Uwaga: zakładam poprawność kodowania s *)
let rec utf8_chars_of_utf8_string_rec rev pos s =
  if pos = String.length s then rev else
  let n = decode_byte_simple pos s in
  let len =
    if n = 7 then 1 else
    if n = 6 then failwith "utf8_chars_of_utf8_string_rec" else
    7 - n in
  utf8_chars_of_utf8_string_rec ((String.sub s pos len) :: rev) (pos + len) s

let utf8_chars_of_utf8_string s =
  List.rev (utf8_chars_of_utf8_string_rec [] 0 s)

let first_utf8_char_of_utf8_string s =
  if s = "" then "","" else
  let n = decode_byte_simple 0 s in
  let len =
    if n = 7 then 1 else
    if n = 6 then failwith "first_utf8_char_of_utf8_string" else
    7 - n in
  String.sub s 0 len, String.sub s len (String.length s - len)

(*let rec text_to_uchars rev pos s =
  if pos = String.length s then rev else
  let n,x = decode_byte pos s in
  if n = 7 then text_to_uchars (x :: rev) (pos+1) s else
  if n = 6 then failwith "text_to_uchars" else (
(*   Printf.printf "head=%X " x; *)
  let tail = decode_tail (6 - n) s (pos+1) in
(*   Printf.printf "tail=%X " tail; *)
  let x = (x lsl ((6-n) * 6)) lor tail in
(*   Printf.printf "all=%X\n" x; *)
  text_to_uchars (x :: rev) (pos + 7 - n ) s)

let text_to_chars s =
  let l = text_to_uchars [] 0 s in
  Xlist.rev_map l classify

let text_to_chars_simple s =
  let l = text_to_uchars [] 0 s in
(*   print_endline (String.concat " " (Xlist.map l (fun c -> Printf.sprintf "%X" c))); *)
  Xlist.rev_map l string_of_uchar*)





(*let text_to_chars s =
  (try UTF8.validate s with UTF8.Malformed_code -> failwith ("Invalid UTF8 string: " ^ s));
  let r = ref [] in
  UTF8.iter (fun c ->
(*     let x = UTF8.init 1 (fun _ -> c) in *)
    r := (classify c) :: (!r)) s;
  List.rev (!r)

let text_to_chars_simple s =
  (try UTF8.validate s with UTF8.Malformed_code -> failwith ("Invalid UTF8 string: " ^ s));
  let r = ref [] in
  UTF8.iter (fun c ->
    r := (UTF8.init 1 (fun _ -> c)) :: (!r)) s;
  List.rev (!r)*)

(*let test_s = "ALA ma óż JJ>Ó>ŁĄʥʘʢ᧬h"

let _ =
  print_endline test_s;
  let l = text_to_chars_simple test_s in
  print_endline (String.concat " " l);
  let l = text_to_chars test_s in
  Xlist.iter l (fun s ->
    print_endline (to_string s))*)
