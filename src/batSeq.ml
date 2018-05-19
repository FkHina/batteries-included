(*
 * Copyright (C) 2009 Jeremie Dimino
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

type 'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

type 'a mappable = 'a t

exception No_more_elements

let nil () = Nil

let cons e s () = Cons(e, s)

let empty () = fun () -> Nil 

let length s =
  let rec aux acc s = match s () with
    | Nil -> acc
    | Cons(_, s) -> aux (acc + 1) s
  in
  aux 0 s

let rec compare_lengths sa sb = match sa (), sb () with 
  | Nil, Nil -> 0
  | Nil, Cons(_, _) -> -1
  | Cons(_, _), Nil -> 1
  | Cons(_, a), Cons(_, b) -> compare_lengths a b

(*$T compare_lengths
  compare_lengths (of_list []) (of_list []) = 0
  compare_lengths (of_list []) (of_list[1]) = -1
  compare_lengths (of_list []) (of_list []) = 0
  compare_lengths (of_list [1]) (of_list []) = 1
  compare_lengths (of_list [1; 2]) (of_list [3; 4]) = 0
  compare_lengths (of_list [1; 2; 3]) (of_list [3; 4]) = 1
  compare_lengths (of_list [1; 2]) (of_list [2; 3; 4]) = -1
*)

let rec compare_length_with s n = match s (), n with 
  | Nil, 0 -> 0
  | Nil, n -> -1
  | Cons(_, s), 0 -> 1
  | Cons(_, s), n -> compare_length_with s (n-1)

(*$T compare_length_with
  compare_length_with (of_list []) 0 = 0
  compare_length_with (of_list []) 1 = -1
  compare_length_with (of_list [1]) 0 = 1
  compare_length_with (of_list [1; 2]) 2 = 0
  compare_length_with (of_list [1; 2; 3]) 2 = 1
  compare_length_with (of_list [1; 2]) 3 = -1
*)

let rec enum_of_ref r =
  BatEnum.make
    ~next:(fun _ -> match !r () with
        | Nil ->
          raise BatEnum.No_more_elements
        | Cons(e, s) ->
          r := s;
          e)
    ~count:(fun _ -> length !r)
    ~clone:(fun _ -> enum_of_ref (ref !r))

let enum s = enum_of_ref (ref s)

let hd s = match s () with
  | Nil -> raise (Invalid_argument "Seq.hd")
  | Cons(e, _s) -> e

let tl s = match s () with
  | Nil -> raise (Invalid_argument "Seq.tl")
  | Cons(_e, s) -> s

let first s = match s () with
  | Nil -> raise (Invalid_argument "Seq.first")
  | Cons(e, _s) -> e

let last s =
  let rec aux e s = match s () with
    | Nil -> e
    | Cons(e, s) -> aux e s
  in
  match s () with
  | Nil -> raise (Invalid_argument "Seq.last")
  | Cons(e, s) -> aux e s

let is_empty s = s () = Nil

let peek s =
  match s () with
  | Nil -> None
  | Cons(e,s) -> Some e

(*$T peek
  peek (of_list [1;2;3;4]) = Some 1
  peek (empty ()) = None
*)

let get s = try (Some (hd s)) with
  | Invalid_argument _ -> None
  |_-> Some (hd s)

(*$T get
  get (of_list []) = None
  get (of_list [1; 2; 3; 4]) = Some 1
*)

let push s e  = cons e s       
(*$T push
  equal (push (of_list [2;3;4]) 1) (of_list [1;2;3;4])
  equal (push (of_list []) 1) (of_list [1])
*)      

let junk s =
  match s () with
  | Nil -> invalid_arg "Seq.junk : empty seq"
  | Cons(e, s) -> s

(*$T junk
  equal (junk (of_list [1;2;3;4])) (of_list [2;3;4])
  try ignore (junk ( empty () )) ; false with Invalid_argument _ -> true
*)

let at s n =
  let rec aux s n =
    match s () with
    | Nil -> raise (Invalid_argument "Seq.at")
    | Cons(e, s) ->
      if n = 0 then
        e
      else
        aux s (n - 1)
  in
  if n < 0 then
    invalid_arg "Seq.at"
  else
    aux s n

let at_opt s n =
  try Some (at s n) with _ -> None

(*$T at_opt
  at_opt (of_list []) 0 = None
  at_opt (of_list []) (-1) = None
  at_opt (of_list [1;2;3]) 2 = Some 3
*)

let rec append s1 s2 () = match s1 () with  | Nil -> s2 ()
                                            | Cons(e, s1) -> Cons(e, append s1 s2)

let concat s =
  let rec aux current rest () = match current () with
    | Cons(e, s) ->
      Cons(e, aux s rest)
    | Nil ->
      match rest () with
      | Cons(e, s) ->
        aux e s ()
      | Nil ->
        Nil
  in
  aux nil s

let flatten = concat

let make n e =
  let rec aux n () =
    if n = 0 then
      Nil
    else
      Cons(e, aux (n - 1))
  in
  if n < 0 then
    invalid_arg "Seq.make"
  else
    aux n

let init n f =
  let rec aux i () =
    if i = n then
      Nil
    else
      Cons(f i, aux (i + 1))
  in
  if n < 0 then
    invalid_arg "Seq.init"
  else
    aux 0

let singleton e = init 1 (fun _ -> e)

let repeat ?times x = match times with
  | None -> let rec aux () =
              Cons (x, aux)
    in aux
  | Some n -> init n (fun _ -> x)

(*$T repeat
  equal (repeat ~times:5 0) (of_list [0;0;0;0;0])
  equal (repeat 1 |> take 3) (of_list [1;1;1])
*)

let cycle ?times s =
  let seq =
    match times with
    | None -> let rec aux () =
                Cons(s, aux)
      in aux
    | Some n -> init n (fun _ -> s)
  in
  concat seq

(*$T cycle
  equal (cycle ~times:5 (singleton 1)) (of_list [1;1;1;1;1])
  equal (cycle (of_list [1;2]) |> take 5) (of_list [1;2;1;2;1])
*)


let rec from f () =
  let rec aux () =
    let e = try  Some (f ())
      with No_more_elements -> None
    in match e with
    | Some x -> Cons(x, aux ) 
    | _   -> Nil
  in
  aux ()

(*$T from
  equal (from (let counter = ref 0 in\
  function () -> if (!counter <10) then (let result = !counter in\
                      counter := !counter + 1; result) else raise No_more_elements))\
  (of_list  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])
*)

let  from_while f =
  from (fun () -> match f () with
      | None   -> raise No_more_elements
      | Some x -> x )

(*$T from_while
  equal (from_while (let counter = ref 0 in\
  function () -> if (!counter <10) then (let result = (Some !counter) in\
                       counter := !counter + 1; result) else None))\
  (of_list  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])
*)

let from_loop data next =
  let r = ref data in
  from(fun () -> let (a,b) = next !r in
        r := b;
        a)

let unfold data next =
  from_loop data (fun data -> match next data with
      | None   -> raise No_more_elements
      | Some x -> x )

(*$T unfold
  equal (unfold 5 (fun x -> if x = 1 then None\
         else Some (x, if x land 1 = 1 then 3 * x + 1 else x / 2)) |> take 5)\
  (of_list [5; 16; 8; 4; 2])
*)

let rec seq acc step cond () =
  if cond acc
  then begin
    Cons(acc ,seq (step acc) step cond)
  end
  else Nil

(*$T
  equal (seq 1 ((+) 1) ((>) 10)) (of_list [1;2;3;4;5;6;7;8;9])
*)

let range ?until x =
  let cond =  match until with
    | None   -> ( fun _ -> true   )
    | Some n -> ( fun m -> m <= n )
  in seq x ( ( + ) 1 ) cond

(*$T range
  range 1 ~until:5 |> List.of_seq= [1;2;3;4;5]
*)

let of_list l =
  let rec aux l () = match l with
    | [] -> Nil
    | x::l' -> Cons(x, aux l')
  in
  aux l

let rec iter f s = match s () with
  | Nil -> ()
  | Cons(e, s) -> f e; iter f s

let iteri f s =
  let rec iteri f i s = match s () with
    | Nil -> ()
    | Cons(e, s) -> f i e; iteri f (i+1) s
  in iteri f 0 s

(*$T iteri
  try iteri (fun i x -> if i<>x then raise Exit) (of_list [0;1;2;3]); true \
  with Exit -> false
*)

let rec iter2 f s1 s2 = match s1 (), s2 () with
  | Nil, _
  | _, Nil -> ()
  | Cons (x1, s1'), Cons (x2, s2') -> f x1 x2; iter2 f s1' s2'

(*$T iter2
  let r = ref 0 in \
    iter2 (fun i j -> r := !r + i*j) (of_list [1;2]) (of_list [3;2;1]); \
    !r = 3 + 2*2
*)

let rec map f s () = match s () with
  | Nil -> Nil
  | Cons(x, s) -> Cons(f x, map f s)

let mapi f s =
  let rec mapi f i s () = match s () with
    | Nil -> Nil
    | Cons(x, s) -> Cons(f i x, mapi f (i+1) s)
  in mapi f 0 s

(*$T mapi
    equal (of_list [0;0;0;0]) \
    (mapi (fun i x -> i - x) (of_list [0;1;2;3]))
*)

let rec map2 f s1 s2 () = match s1 (), s2 () with
  | Nil, _
  | _, Nil -> Nil
  | Cons (x1, s1'), Cons (x2, s2') ->
    Cons (f x1 x2, map2 f s1' s2')

(*$T map2
  equal (map2 (+) (of_list [1;2;3]) (of_list [3;2])) \
    (of_list [4;4])
*)

let rec fold_left f acc s = match s () with
  | Nil -> acc
  | Cons(e, s) -> fold_left f (f acc e) s

let rec fold_right f s acc = match s () with
  | Nil -> acc
  | Cons(e, s) -> f e (fold_right f s acc)

let reduce f s = match s () with
  | Nil -> raise (Invalid_argument "Seq.reduce")
  | Cons(e, s) -> fold_left f e s

let max s = match s () with
  | Nil -> raise (Invalid_argument "Seq.max")
  | Cons(e, s) -> fold_left Pervasives.max e s

let min s = match s () with
  | Nil -> raise (Invalid_argument "Seq.min")
  | Cons(e, s) -> fold_left Pervasives.min e s

let sum s =
  match s () with 
  | Nil -> 0 
  | Cons(e, s) -> fold_left (+) e  s

(*$T sum 
  sum (of_list [1;2;3;4;5]) = 15
*)

let fsum s =
  match s () with
  | Nil -> 0.
  | Cons(e, s) -> fold_left (+.) e s

(*$T fsum
  fsum (of_list [1.;2.;3.;4.;5.]) = 15.
  fsum (of_list []) = 0.
*)

let kahan_sum = fsum

(*$T
  kahan_sum (of_list []) = 0.
  kahan_sum (of_list [1.; 2. ]) = 3.
  let n, x = 1_000, 1.1 in \
     Float.approx_equal (float n *. x) \
                        (kahan_sum (of_list (List.make n x)))
*)

let equal ?(eq=(=)) s1 s2 =
  let rec recurse eq s1 s2 =
    match s1 (), s2 () with
    | Nil, Nil -> true
    | Nil, Cons _
    | Cons _, Nil -> false
    | Cons (x1, s1'), Cons (x2, s2') -> eq x1 x2 && recurse eq s1' s2'
  in
  recurse eq s1 s2

(*$T of_list
  equal (of_list [1;2;3]) (nil |> cons 3 |> cons 2 |> cons 1)
*)

let rec for_all f s = match s () with
  | Nil -> true
  | Cons(e, s) -> f e && for_all f s

let rec for_all2 f s1 s2 = match s1 (), s2 () with
  | Nil, Nil -> true
  | Cons(e1, s1), Cons(e2, s2) -> f e1 e2 && for_all2 f s1 s2
  | _ ->    raise (Invalid_argument "Seq.for_all2: different sequence lentgh") 

(*$T for_all2
   for_all2 (=) (of_list [1;2;3]) (of_list [3;2;1]) = false
   for_all2 (=) (of_list [1;2;3]) (of_list [1;2;3])
   for_all2 (<>) (of_list [1;2;3]) (of_list [3;2;1]) = false
   try ignore (for_all2 (=) (of_list [1;2;3]) (of_list [1;2;3;4])); false \
     with Invalid_argument _ -> true
   try ignore (for_all2 (=) (of_list [1;2]) (of_list [])); false \
     with Invalid_argument _ -> true
*)

let rec exists f s = match s () with
  | Nil -> false
  | Cons(e, s) -> f e || exists f s

let mem e s = exists ((=) e) s

let rec find f s = match s () with
  | Nil ->
    None
  | Cons(e, s) ->
    if f e then
      Some e
    else
      find f s

let rec find_map f s = match s () with
  | Nil ->
    None
  | Cons(e, s) ->
    match f e with
    | None ->
      find_map f s
    | x ->
      x

let rec filter f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    if f e then
      Cons(e, filter f s)
    else
      filter f s ()

let rec filter_map f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    match f e with
    | None ->
      filter_map f s ()
    | Some e ->
      Cons(e, filter_map f s)

let assoc key s = find_map (fun (k, v) -> if k = key then Some v else None) s

let rec take n s () =
  if n <= 0 then
    Nil
  else
    match s () with
    | Nil ->
      Nil
    | Cons(e, s) ->
      Cons(e, take (n - 1) s)

let rec drop n s =
  if n <= 0 then
    s
  else
    match s () with
    | Nil ->
      nil
    | Cons(_e, s) ->
      drop (n - 1) s

let rec take_while f s () = match s () with
  | Nil ->
    Nil
  | Cons(e, s) ->
    if f e then
      Cons(e, take_while f s)
    else
      Nil

let rec drop_while f s = match s () with
  | Nil ->
    nil
  | Cons(e, s) ->
    if f e then
      drop_while f s
    else
      cons e s

let skip n s = drop n s

(*$T
  equal  (skip 3 (of_list [1; 2; 3; 4; 5; 6; 7])) (of_list [4; 5; 6; 7])
*)

let split s = (map fst s, map snd s)

let rec combine s1 s2 () = match s1 (), s2 () with
  | Nil, Nil ->
    Nil
  | Cons(e1, s1), Cons(e2, s2) ->
    Cons((e1, e2), combine s1 s2)
  | _ ->
    raise (Invalid_argument "Seq.combine: different sequence lentgh")

let uncombine s =
  let rec aux s =
    match s () with
    | Nil -> nil, nil
    | Cons ((e1, e2), sr) ->
      let s1, s2 = aux sr  in
      cons e1 s1, cons e2 s2
  in
  aux s 

(* uncomine*)
(*$T uncombine
  let s1, s2 = uncombine (of_list [1,2;3,4;5,6;7,8;9,0]) \
  in \
  equal s1 (of_list [1;3;5;7;9]) && \
  equal s2 (of_list [2;4;6;8;0])
*)


let rec uniq s () =
  match s () with
  | Nil -> Nil
  | Cons(e, sr) -> let r = drop_while (fun x -> x = e) s in Cons(e, uniq r)

(*$T
  equal ( uniq (of_list [1;1;2;3;3;2])) (of_list [1;2;3;2])
*)

let rec uniqq s () =
  match s () with
  | Nil -> Nil
  | Cons(e, sr) -> let r = drop_while (fun x -> x == e) s in Cons(e, uniq r)

(*$T
  equal ( uniqq (of_list [1;1;2;3;3;2])) (of_list [1;2;3;2])
*)

let rec uniq_by  f s () =
  match s () with
  | Nil -> Nil
  | Cons (e, s) -> let r = drop_while (fun x -> f x e) s in Cons(e, uniq_by f r)

(*$T
  of_list ["a";"A";"b";"c";"C";"b"]\
  |> uniq_by (fun a b -> String.lowercase a = String.lowercase b) \
  |> List.of_seq = ["a";"b";"c";"b"]
*)

let partition f s=  
  let rec aux  s =  
    match s () with 
    | Nil -> nil, nil
    | Cons(e, s) -> let s1, s2 = aux s in  if f e then cons e s1, s2 else s1, cons e s2 
  in
  aux s

(*$T partition
  let yes_seq, no_seq = partition (fun x -> x mod 2 = 0)  (of_list [1;2;3;4])\
  in\
  equal yes_seq (of_list [2;4]) &&\
  equal no_seq (of_list [1;3])
*)


let rec merge test a b () =
  match a (), b () with
  | Nil, _-> b ()
  | _, Nil -> a ()
  | Cons(e1, s1), Cons(e2, s2) -> if test e1 e2 then Cons(e1, cons e2 (merge test s1 s2)) else Cons(e2, cons e1 (merge test s1 s2))

(* $T merge
   let a = of_list [1;3;5] and b = of_list [2;4]\
   in\
   equal (merge (fun x y -> x < y) a b) (of_list [1;2;3;4;5])
*)

let concat_map f s = concat (map f s)

let span test s = take_while test s, drop_while test s                    

(*$T span
  let s1, s2 = span (fun x-> x<4) (of_list [1;2;3;4;5])\
  in \
  equal s1 (of_list [1;2;3]) &&\
  equal s2 (of_list [4;5])
*)

let break test s = span (fun x -> not (test x)) s

(*$T break
  let s1, s2 = break (fun x-> x<4) (of_list [1;2;3;4;5])\
  in \
  equal s1 (of_list []) &&\
  equal s2 (of_list [1; 2; 3; 4; 5])
*)

let while_do cont f s =
  let (head, tail) = span cont s
  in
  append (f head) tail

let rec scanl f acc s () = 
  match s () with 
  | Nil -> Cons(acc, fun () -> Nil) 
  | Cons(x, xs) -> Cons(acc, scanl f (f acc x) xs)

(*$T scanl
  equal (scanl (+) 0 (of_list [1;2;3;4;5])) (of_list [0; 1; 3; 6; 10; 15])
*)

let scan f s =
  match s () with
  |Nil -> nil
  |Cons(e, sr) -> scanl f e s

(*$T scan
  equal (scan ( * )  (of_list [1;2;3;4;5;6;7;8;9;10]))\
        (of_list [1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880; 3628800] )
*)

let rec foldi f acc s1 = 
  let rec loop idx = 
    match s1 () with
    | Nil -> acc
    | Cons(e1, s1) -> foldi f (f (idx+1) e1 acc) s1 
  in loop 0

let rec fold2 f acc s1 s2 = 
  match s1 (), s2 () with
  | Nil, _ -> acc
  | _, Nil -> acc
  | Cons(e1, s1), Cons(e2, s2) -> fold2 f (f e1 e2 acc) s1 s2

let rec fold2i f acc s1 s2 = 
  let rec loop idx = 
    match s1 (), s2 () with
    | Nil, _ -> acc
    | _, Nil ->  acc
    | Cons(e1, s1), Cons(e2, s2) -> fold2i f (f (idx+1) e1 e2 acc) s1 s2
  in loop 0

let rec group f s () =
  match s() with 
  |Nil -> Nil 
  |Cons(x, s) -> let r = f x in let xs, ys = span (fun y -> r = f y) s in Cons(cons x xs, group f ys)

(*$T group
   of_list [1;2;3;4] |> group (fun x -> x) |> map List.of_seq \
    |> List.of_seq = [[1];[2];[3];[4]]
   of_list [] |> group (fun x -> x) |> List.of_seq \
   = []
   of_list [1;2;3;5;6;7;9;10;4;5] |> group (fun x -> x mod 2) \
    |> map List.of_seq\
    |> List.of_seq =  [[1];[2];[3;5];[6];[7;9];[10;4];[5]]
*)

let arg_min f s =
  match s () with
    Nil -> invalid_arg "Seq.arg_min: Empty seq"
  | Cons (e, s) -> let e, eval = ref e, ref (f e) in
    iter (fun v -> let fv = f v in
           if fv < !eval then (e := v; eval := fv)) s;
    !e

(*$T arg_min
   arg_min (fun x -> x * x + 6 * x - 5) (of_list [-5; -4; -3; -2;-1; 0; 1; 2; 3; 4; 5])      = -3
*)


let arg_max f s =
  match s () with
    Nil -> invalid_arg "Seq.arg_max: Empty seq"
  | Cons (e, s) -> let item, eval = ref e, ref (f e) in
    iter (fun v -> let fv = f v in
           if fv > !eval then (item := v; eval := fv)) s;
    !item

(*$T arg_max
   of_list ["cat"; "canary"; "dog"; "dodo"; "ant"; "cow"] \
   |> arg_max String.length = "canary"
*)

let rec group_by eq s () =
  match s () with
  | Nil -> Nil
  | Cons(e, s) -> let xs, ys = span (eq e) s in Cons(cons e xs, group_by eq ys)

(*$T group_by
   of_list [1; 3; 0; 2; 5; 4] \
   |> group_by (fun x y -> x mod 2 = y mod 2) \
   |> map List.of_seq |> List.of_seq \
   = [[1; 3]; [0; 2]; [5]; [4]]
   of_list [] |> group_by (=) |> map List.of_seq |> List.of_seq\
   = [] 
*)

let cartesian_product a b =
  let na = length a in
  let nb = length b in
  init
    (na * nb)
    (fun j -> let i = j / nb in
      at a i, at b (j - i*nb))

(*$T cartesian_product
  equal (cartesian_product (of_list [1;2]) (of_list ["a";"b"]))\
  (of_list [ 1,"a"; 1,"b"; 2,"a"; 2, "b" ])
  equal (cartesian_product (of_list [1;2;3]) (of_list [1]))\
  (of_list [1,1; 2,1; 3,1])
  equal (cartesian_product (of_list [1]) (of_list [1;2;3])) \
    (of_list [1,1; 1,2; 1,3])
*)
    
let switch test s =
  let rec aux  s  =
    match s () with
    |Nil -> nil, nil
    |Cons (e, s) -> let s1, s2 = aux s in  if test e then cons e s1, s2 else s1, cons e s2
  in
  aux s 

(*$T
  let s1, s2 = switch (fun x -> x mod 2 = 0) (of_list [ 0; 1; 2; 3; 4; 5; 6; 7])\
  in\
  equal s1 (of_list [ 0; 2; 4; 6]) &&\
  equal s2 (of_list [ 1; 3; 5; 7])
*)

let print ?(first="[") ?(last="]") ?(sep="; ") print_a out s = match s () with
  | Nil ->
    BatInnerIO.nwrite out first;
    BatInnerIO.nwrite out last
  | Cons(e, s) ->
    match s () with
    | Nil ->
      BatPrintf.fprintf out "%s%a%s" first print_a e last
    | _ ->
      BatInnerIO.nwrite out first;
      print_a out e;
      iter (BatPrintf.fprintf out "%s%a" sep print_a) s;
      BatInnerIO.nwrite out last

module Infix = struct
  (** Infix operators matching those provided by {!BatEnum.Infix} *)

  let ( -- ) a b =
    if b < a then
      nil
    else
      init (b - a + 1) (fun x -> a + x)

  let ( --^ ) a b = a -- (b - 1)

  let ( --. ) (a, step) b =
    let n = int_of_float ((b -. a) /. step) + 1 in
    if n < 0 then
      nil
    else
      init n (fun i -> float_of_int i *. step +. a)

  let ( --- ) a b =
    let n = abs (b - a) in
    if b < a then
      init n (fun x -> a - x)
    else
      a -- b

  let ( --~ ) a b =
    map Char.chr (Char.code a -- Char.code b)

  let ( // ) s f = filter f s

  let ( /@ ) s f = map f s
  let ( @/ ) = map

  let ( //@ ) s f = filter_map f s
  let ( @// ) = filter_map
end

include Infix

module Exceptionless = struct
  (* This function could be used to eliminate a lot of duplicate code below...
     let exceptionless_arg f s e =
     try Some (f s)
     with Invalid_argument e -> None
  *)

  let hd s =
    try Some (hd s)
    with Invalid_argument _ -> None

  let tl s =
    try Some (tl s)
    with Invalid_argument _ -> None

  let first s =
    try Some (first s)
    with Invalid_argument _ -> None

  let last s =
    try Some (last s)
    with Invalid_argument _ -> None

  let at s n =
    try Some (at s n)
    with Invalid_argument _ -> None

  (*
  let make n e =
    try Some (make n e)
    with Invalid_argument _ -> None

  let init n e =
    try Some (init n e)
    with Invalid_argument _ -> None
  *)

  let reduce f s =
    try Some (reduce f s)
    with Invalid_argument _ -> None

  let max s =
    try Some (max s)
    with Invalid_argument _ -> None

  let min s =
    try Some (min s)
    with Invalid_argument _ -> None

  let combine s1 s2 =
    try Some (combine s1 s2)
    with Invalid_argument _ -> None

  (*$T combine
    equal (combine (of_list [1;2]) (of_list ["a";"b"])) (of_list [1,"a"; 2,"b"])
  *)
end
