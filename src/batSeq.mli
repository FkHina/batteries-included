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

(** Sequence of elements *)

(** A sequence represent a collection of elements, for which you never
    construct the complete representation.

    Basically you should use a sequence when you would prefer using a
    list or a lazy-list but constructing the whole list explicitly
    would explode your memory.

    All functions returning a sequence operates in time and space
    O(1).

    Note that if you want a ``consumable sequence'', you should prefer
    using enumerations (from module {!BatEnum}).

    @author Jeremie Dimino
*)

type 'a t = unit -> 'a node
(** A sequence is a computation which returns a list-like node *)

and 'a node =
  | Nil
  | Cons of 'a * 'a t

include BatInterfaces.Mappable with type 'a mappable = 'a t

exception No_more_elements
(** This exception {i shall} be raised by the [next] function of 
    [from] when no more elements can be enumerated.
*)
val enum : 'a t -> 'a BatEnum.t
(** [enum s] returns the enumeration of all element of [s].

    Since enumerations are consumable and sequence are not, it is
    not possible to have the inverse operations, i.e. [of_enum] *)

(** {6 Base operations} *)

val singleton : 'a -> 'a t
    (** [singleton e] returns an 'a t  containing e, equivalent to init 1 ( fun _ -> x) *)

val length : 'a t -> int
(** Return the number of elements of the given sequence. This may
    never return if the sequence is infinite. *)

val compare_lengths : 'a t -> 'b t -> int
(**  Compare the lengths of two sequences, the computation stops after itering on the shortest sequence. *)

val compare_length_with :  'a t -> int -> int
(** Compare the length of a sequence to an integer. *)

val hd : 'a t -> 'a
(** Returns the first element of the sequence or raise [Invalid_argument] if
    the sequence is empty. *)

val tl : 'a t -> 'a t
(** Returns the sequence without its first elements or raise
    [Invalid_argument] if the sequence is empty. *)

val is_empty : 'a t -> bool
(** [is_empty e] returns true if [e] does not contains any
    element. *)

val peek : 'a t -> 'a option
(** [peek s] returns [None] if [e] is empty or [Some x] where [x] is
    the next element of [e].*)
    
 val get : 'a t -> 'a option
(** [get s] returns [None] if [s] is empty or [Some x] where [x] is
    the next element of [s].
*)

val push : 'a t -> 'a -> 'a t
(** [push s x] will add [x] at the beginning of [s]. 
*)
  
val junk : 'a t -> 'a t
(** junk s removes the first element from the sequence.
    [Invalid_argument] if the sequence is empty. *)

val first : 'a t -> 'a
(** Same as {!hd} *)

val last : 'a t -> 'a
(** Returns the last element of the sequence, or raise [Invalid_argument] if
    the sequence is empty. *)

val at : 'a t -> int -> 'a
(** [at l n] returns the element at index [n] (starting from [0]) in
    the sequence [l] or raise [Invalid_argument] is the index is
    outside of [l] bounds. *)

val at_opt : 'a t -> int -> 'a option
(** [at_opt s n] returns [Some e] where [e] is the n-th element of the given sequence or     [None] 
    @raise Invalid_argument if [n] is negative. *)

val append : 'a t -> 'a t -> 'a t
(** [append s1 s2] returns the sequence which first returns all
    elements of [s1] then all elements of [s2]. *)

val concat : 'a t t -> 'a t
(** [concat s] returns the sequence which returns all the elements
    of all the elements of [s], in the same order. *)

val flatten : 'a t t -> 'a t
(** Same as {!concat}. *)

(** {6 Constructors} *)

val nil : 'a t
(** [nil = fun () -> Nil] *)

val cons : 'a -> 'a t -> 'a t
(** [cons e s = fun () -> Cons(e, s)] *)

val empty : unit -> 'a t
(** The empty sequence contains no elements.*)
    
val make : int -> 'a -> 'a t
(** [make n e] returns the sequence of length [n] where all elements
    are [e] *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] returns the sequence returning the results of [f 0],
    [f 1].... [f (n-1)]. @raise Invalid_argument if [n < 0]. *)

val repeat : ?times:int -> 'a -> 'a t
(** repeat ~times:n x creates a  sequence filled with n times of x. It return infinite enum when ~times is absent. It returns invalid_argument when times <= 0. *)

val cycle : ?times:int -> 'a t -> 'a t
(** cycle is similar to repeat, except that the content to fill is a subseq rather than a single element. Note that times represents the times of repeating not the length of sequence.*)

val from : (unit -> 'a) -> 'a t
(** [from next] creates a sequence from the [next] function.
    [next] {i shall} return the next element of the sequence or raise
    [No_more_elements].
 *)

val from_while :  (unit -> 'a option) -> 'a t
(** Build a sequence from a function,Call the function repeatedly until it returns None.
    [from_while next] creates an sequence from the [next] function.
    [next] {i shall} return [Some x] where [x] is the next element of the
    sequence or [None] when there is no more elements. 
*)

val from_loop: 'b -> ('b -> ('a * 'b)) -> 'a t
(**[from_loop data next] creates a (possibly infinite) sequence from
   the successive results of applying [next] to [data], then to the
   result, etc. The list ends whenever the function raises
   {!BatSeq.No_more_elements}.
*)
    
val unfold: 'b -> ('b -> ('a * 'b) option) -> 'a t
(**As [from_loop], except uses option type to signal the end of the sequence.
   [unfold data next] creates a (possibly infinite) sequence from
   the successive results of applying [next] to [data], then to the
   result, etc. The sequence ends whenever the function returns [None]
   Example: [Seq.unfold n (fun x -> if x = 1 then None else Some
   (x, if x land 1 = 1 then 3 * x + 1 else x / 2))] returns the
   hailstone sequence starting at [n].
*)
    
val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
(** [seq init step cond] creates a sequence of data, which starts
    from [init],  extends by [step],  until the condition [cond]
    fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [1, 2, ... 99]. If [cond
    init] is false, the result is empty. *)
    
val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
(** seq init step cond creates a sequence of data, which starts from init, extends by step, until the condition cond fails. E.g. seq 1 ((+) 1) ((>) 100) returns 1, 2, ... 99. If cond init is false, the result is empty.*)

val range : ?until:int -> int -> int t
(**range p until:q creates a sequence of integers [p, p+1, ..., q]. If until is omitted, the enumeration is not bounded. Behaviour is not-specified once max_int has been reached.*)

val of_list : 'a list -> 'a t
(** Convenience function to build a seq from a list.
    @since 2.2.0 *)

val to_list : 'a t -> 'a list
(** Build a list from a sequence. In the result, elements appear in the
    same order as they did in the source. *) 

val of_array : 'a array -> 'a t
(** Build a sequence from an array *)

val to_array : 'a t -> 'a array
(** Build an array from a sequence *)

val of_string : string -> char t
(** Build a char sequence from a string *)

val to_string : char t -> string
(** Build a string from a char sequence *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f s] applies [f] to all the elements of the sequence. Eager. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f s] is the same as [iter f s], but [f] is given the index
    of each element (starting at 0).
    @since 2.2.0 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f s1 s2] iterates on elements of [s1] and [s2] pairwise, and
    stops when it meets the end of [s1] or [s2]
    @since 2.2.0 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f s] returns the sequence where elements are elements of
    [s] mapped with [f]. Lazy. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi f s] lazily maps elements of [s] into a new sequence,
    using [f]. [f] is also given elements' indexes.
    @since 2.2.0 *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f s1 s2] returns a sequence of elements, resulting from combininig
    elements of [s1] and [s2] at the same index using [f]. The result is as
    long as the shortest argument.
    @since 2.2.0 *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f a (cons b0 (... bn))] is [f (... (f (f a b0) b1) ...)
    bn]. Tail-recursive, eager.
*)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** [fold_right f (cons a0 (cons a1 (cons a2 ...))) b] is [f a0 (f
    a1 (f a2 ...))].
    Not tail-recursive, eager.
*)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** [reduce f (cons e s)] is [fold_left f e s].

    @raise Invalid_argument on empty sequences. *)

val max : 'a t -> 'a
(** [max s] returns the largest value in [s] as judged by
    [Pervasives.compare]

    @raise Invalid_argument on empty sequences. *)

val min : 'a t -> 'a
(** [min s] returns the smallest value in [s] as judged by
    [Pervasives.compare]

    @raise Invalid_argument on empty sequences. *)

val sum : int t -> int
(** sum returns the sum of the given int sequence. If the argument is empty, returns 0.*)

val fsum : float t -> float
(** sum returns the sum of the given float sequence. If the argument is empty, returns 0.0.*)

val kahan_sum : float t -> float
(** [kahan_sum l] returns a numerically-accurate sum of the floats of
    [l]. See {!BatArray.fsum} for more details.
*)
  
val equal : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** [equal ~eq s1 s2] compares elements of [s1] and [s2] pairwise
      using [eq]
      @param eq optional equality function (default {!Pervasives.(=)})
      @since 2.2.0 *)

(** {6 Sequence scanning}

    Most functions in the following sections have a shortcut semantic
    similar to the behavior of the usual (&&) and (||) operators :
    they will force the sequence until they find an satisfying
    element, and then return immediately.

    For example, [for_all] will only diverge if the sequence begins
    with an infinite number of true elements --- elements for which
    the predicate [p] returns [true].
*)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p (cons a0 (cons a1 ...))] checks if all elements of the
    given sequence satisfy the predicate [p]. That is, it returns
    [(p a0) && (p a1) && ...]. Eager, shortcut.
*)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** As {!Seq.for_all} but on two sequences.
    @raise Invalid_argument if the two sequences have different lengths.
*)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p (cons a0 (cons a1 ...))] checks if at least one element of
    the sequence satisfies the predicate [p]. That is, it returns
    [(p a0) || (p a1) || ...]. Eager, shortcut.
*)

val mem : 'a -> 'a t -> bool
(** [mem a l] is true if and only if [a] is equal to an element of
    [l]. Eager, shortcut.
*)

(** {6 Sequence searching} *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p s] returns the first element of [s] such as [p e]
    returns [true], if any. Eager, shortcut.
*)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map p s] finds the first element of [s] for which [p e]
    returns [Some r], if any. Eager, short-cut.
*)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p s] returns the sequence of elements of [s] satisfying
    [p]. Lazy.

    {b Note} filter is lazy in that it returns a lazy sequence, but
    each element in the result is eagerly searched in the input
    sequence. Therefore, the access to a given element in the result
    will diverge if it is preceded, in the input sequence, by
    infinitely many false elements (elements on which the predicate
    [p] returns [false]).

    Other functions that may drop an unbound number of elements
    ([filter_map], [take_while], etc.) have the same behavior.
*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f s] returns the sequence of elements filtered and
    mapped by [f]. Lazy.
*)

(** {6 Association sequences} *)

val assoc : 'a -> ('a * 'b) t -> 'b option
(** [assoc a s] returns the value associated with key [a] in the
    sequence of pairs [s]. Eager, shortcut.  *)

(** {6 Sequence transformations} *)

val take : int -> 'a t -> 'a t
(** [take n s] returns up to the [n] first elements from sequence
    [s], if available. Lazy. *)

val drop : int -> 'a t -> 'a t
(** [drop n s] returns [s] without the first [n] elements, or the
    empty sequence if [s] have less than [n] elements. Lazy. *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** [take_while f s] returns the first elements of sequence [s]
    which satisfy the predicate [f]. Lazy. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while f s] returns the sequence [s] with the first
    elements satisfying the predicate [f] dropped. Lazy. *)

val skip : int -> 'a t -> 'a t
(** aleas to drop.*)


(** {6 Sequence of pairs} *)

val split : ('a * 'b) t -> 'a t * 'b t
(** [split s = (map fst s, map snd s)]. Lazy. *)

val combine : 'a t -> 'b t -> ('a * 'b) t
(** Transform a pair of sequences into a sequence of pairs. Lazy.
    @raise Invalid_argument if given sequences of different length. *)

val uncombine : ('a * 'b) t -> 'a t * 'b t
(**  uncombine is the opposite of combine.*)

val uniq : 'a t -> 'a t
(** uniq e returns a duplicate of e with repeated values omitted (similar to unix's uniq command).It uses structural equality to compare consecutive elements.*)

val uniqq : 'a t -> 'a t
(** uniqq e behaves as uniq e except it uses physical equality to compare consecutive elements.*)

val uniq_by : ('a -> 'a -> bool) -> (unit -> 'a node) -> 'a t
(** uniq_by cmp e behaves as uniq e except it allows to specify a comparison function.*)

val partition :  ('a -> bool) -> 'a t -> 'a t * 'a t
(** partition test seq splits seq into two sequences, where the first seq has all the elements satisfying test, the second one is opposite. The order of elements in the source seq is preserved.                                        
*)

val merge : ('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t
(** merge test a b merge the elements from a and b into a single sequence. At each step, test is applied to the first element xa of a and the first element xb of b to determine which should get first into resulting sequence. If test xa xb returns true, xa (the first element of a) is used, otherwise xb is used. If a or b runs out of elements, the process will append all elements of the other sequence to the result.

   For example, if a and b are sequences of integers sorted in increasing order, then merge (<) a b will also be sorted.
*)

val arg_min : ('a -> 'b) -> 'a t -> 'a

val arg_max : ('a -> 'b) -> 'a t -> 'a

(** arg_min f xs returns the x in xs for which f x is minimum. Similarly for arg_max, except it returns the maximum. If multiple values reach the maximum, one of them is returned. (currently the first, but this is not guaranteed)

    Example: List.enum ["cat"; "canary"; "dog"; "dodo"; "ant"; "cow"] |> arg_max String.length = "canary"

    @raise Invalid_argument if the input seq is empty.*)

val concat_map :  ('a -> 'b t) -> 'a t -> 'b t
(** concat_map f e is the same as concat (map f e).*)

val span : ('a -> bool) -> 'a t -> 'a t * 'a t
(** span test s produces two sequences (hd, tl), such that hd is the same as take_while test s and tl is the same as drop_while test s. *)                                     

val break : ('a -> bool) -> 'a t -> 'a t * 'a t
(** Negated span. break test s is equivalent to span (fun e -> not (test e)) s. *)

val while_do : ('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
(** while_do cont f e is a loop on e using f as body and cont as condition for continuing.
    If e contains elements x0, x1, x2..., then if cont x0 is false, x0 is returned as such and treatment stops. On the other hand, if cont x0 is true, f x0 is returned and the loop proceeds with x1...
*)                                                  

val scanl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
(** A variant of fold producing an sequence of its intermediate values. If s contains x1, x2, ..., scanl f init s is the sequence containing init, f init x1, f (f init x1) x2...*)

val scan : ('a -> 'a -> 'a) -> 'a t -> 'a t
(** scan is similar to scanl but without the init value: if s contains x1, x2, x3 ..., scan f s is the sequence containing x1, f x1 x2, f (f x1 x2) x3... 
    For instance, scan ( * ) (1 -- 10) will produce a sequence containing the successive values of the factorial function.*)

val foldi : (int -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  
val fold2 : ('a -> 'b -> 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** fold2 is similar to fold_left but will fold over two enumerations at the same time until one of the two enumerations ends.*)

val fold2i : (int -> 'a -> 'b -> 'c -> 'c) -> 'c -> 'a t -> 'b t -> 'c
(** similar to previousfold2  except that they call the function with one additional argument which is an index starting at 0 and incremented after each call to the function.*)

val group : ('a -> 'b) -> 'a t -> 'a t t
(** group test e devides e into a sequence of sequences, where each sub-sequence is the longest continuous enumeration of elements whose test results are the same.*)

val group_by : ('a -> 'a -> bool) -> 'a t -> 'a t t
(** group_by eq e divides e into a sequence of sequences, where each sub-sequence is the longest continuous enumeration of elements that are equal, as judged by eq.*)

val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product of the two sequences.
*)

val switch : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [switch test seq] splits [seq] into two sequences, where the first sequence has
    all the elements satisfying [test], the second sequence is opposite. The
    order of elements in the source seq is preserved. *)
                                       
(** {6 Printing} *)

val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit
(**Print the contents of a sequence*)

module Infix : sig
  (** Infix operators matching those provided by {!BatEnum.Infix} *)

  val ( -- ) : int -> int -> int t
  val ( --^ ) : int -> int -> int t
  val ( --. ) : float * float -> float -> float t
  val ( --- ) : int -> int -> int t
  val ( --~ ) : char -> char -> char t
  val ( // ) : 'a t -> ('a -> bool) -> 'a t
  val ( /@ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( @/ ) : ('a -> 'b) -> 'a t -> 'b t
  val ( //@ ) : 'a t -> ('a -> 'b option) -> 'b t
  val ( @// ) : ('a -> 'b option) -> 'a t -> 'b t
end

include module type of Infix

module Exceptionless : sig
  val hd : 'a t -> 'a option
  val tl : 'a t -> 'a t option
  val first : 'a t -> 'a option
  val last : 'a t -> 'a option
  val at : 'a t -> int -> 'a option
  (*
  val make : int -> 'a -> 'a t
  val init : int -> (int -> 'a) -> 'a t
  *)
  val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a option
  val max : 'a t -> 'a option
  val min : 'a t -> 'a option
  val combine : 'a t -> 'b t -> ('a * 'b) t option
end
