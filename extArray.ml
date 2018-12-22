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

type 'a t = ('a array * int * int * 'a) ref

let make last zero = 
  let last = if last < 1 then 1 else last in
  ref (Array.make last zero, 0, last, zero)

let add r x =
  let a,next,last,zero = !r in
  let a,last = if next < last then a,last else
    let a2 = Array.make (2*last) zero in
    Int.iter 0 (last - 1) (fun i -> a2.(i) <- a.(i));
    a2, 2*last in
  a.(next) <- x;
  r := a,next+1,last,zero;
  next

let set r i x =
  let a,next,last,zero = !r in
  a.(i) <- x

let size r =
  let a,next,last,zero = !r in
  next

let get r i =
  let a,next,last,zero = !r in
  a.(i)

let to_array r =
  let a,next,last,zero = !r in
  let b = Array.make next zero in
  Int.iter 0 (next-1) (fun i -> b.(i) <- a.(i));
  b
  (*Array.copy a*)

let copy r =
  let a,next,last,zero = !r in
  ref (Array.copy a,next,last,zero)

let of_array a zero =
  ref (Array.copy a, Array.length a, Array.length a, zero)
