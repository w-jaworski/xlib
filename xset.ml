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

module type SetOrderedType =
  sig

    type t
    val compare : t -> t -> int

  end

module type SET =
    functor (Ord : SetOrderedType) ->
  sig

    type key = Ord.t
    type t

    val empty : t
    val is_empty : t -> bool
    val singleton : key -> t
    val add : t -> key -> t
    val mem : t -> key -> bool
    val size : t -> int
    val remove : t -> key -> t

    val min_elt : t -> key
    val max_elt : t -> key

    val iter : t -> (key -> unit) -> unit
    val fold : t -> 'b -> ('b -> key -> 'b) -> 'b
    val filter : t -> (key -> bool) -> t
    val partition : t -> (key -> bool) -> (t * t)

    val intersection : t -> t -> t
    val difference : t -> t -> t
    val union : t -> t -> t
    val of_list : key list -> t
    val to_list : t -> key list

  end

module Make : SET =
  functor(Ord : SetOrderedType) ->
  struct

    module T = Set.Make(Ord)
    type key = Ord.t
    type t = T.t

    let empty = T.empty
    let is_empty set  = T.is_empty set
    let singleton = T.singleton
    let add set key = T.add key set
    let mem set key = T.mem key set
    let size set = T.cardinal set
    let remove set key = T.remove key set

    let min_elt set = T.min_elt set
    let max_elt set = T.max_elt set

    let iter set f = T.iter f set
    let fold set s f = T.fold (fun a -> fun b -> f b a) set s
    let filter set f = T.filter f set
    let partition set f = T.partition f set

    let intersection = T.inter
    let difference = T.diff
    let union = T.union
    let of_list list = Xlist.fold list empty (fun set -> fun e -> add set e)
    let to_list set = fold set [] (fun l -> fun e -> e :: l)

  end
