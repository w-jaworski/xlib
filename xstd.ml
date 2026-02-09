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

module OrderedFloat = struct

type t = float

let compare = compare

end

module OrderedChar = struct

  type t = char

  let compare = compare

end

module StringMap = Xmap.Make(String)
module IntMap = Xmap.Make(Int)
module FloatMap = Xmap.Make(OrderedFloat)
module CharMap = Xmap.Make(OrderedChar)

module IntSet = Xset.Make(Int)
module StringSet = Xset.Make(String)
module FloatSet = Xset.Make(OrderedFloat)

module StringQMap = Xmap.MakeQ(String)
module IntQMap = Xmap.MakeQ(Int)
module FloatQMap = Xmap.MakeQ(OrderedFloat)

module BitArrayMap = Xmap.Make(BitArray)

let print_stringqmap qmap =
  StringQMap.iter qmap (fun k v -> Printf.printf "%6d %s\n" v k)

let print_stringqmap_keys qmap =
  StringQMap.iter qmap (fun k _ -> Printf.printf "%s\n" k)


