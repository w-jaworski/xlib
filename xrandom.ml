
let _ = Random.self_init ()

let draw l =
  let n = Random.int (Xlist.size l) in
  List.nth l n

let rec draw_partition_rec n rev l =
  if l = [] then failwith "draw_partition_rec" else
  if n = 0 then List.hd l, (List.rev rev) @ (List.tl l)
  else draw_partition_rec (n-1) (List.hd l :: rev) (List.tl l)

let draw_partition l =
  let n = Random.int (Xlist.size l) in
  draw_partition_rec n [] l

let toss_coin p =
  p >= Random.float 1.

let rec shuffle l =
  if Xlist.size l < 2 then l else
  let x,l = draw_partition l in
  x :: (shuffle l)

let rec split_list_rec n test = function
    x :: l when n > 0 -> split_list_rec (n-1) (x :: test) l
  | l -> l, test

let split_list n l =
  let l = shuffle l in
  split_list_rec n [] l
