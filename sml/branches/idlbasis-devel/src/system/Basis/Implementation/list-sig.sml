(* list-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from list.mldoc (v. 1.9; 2000-05-24)
 *)

signature LIST =
  sig
    datatype list = datatype list
    exception Empty
    val null : 'a list -> bool
    val length : 'a list -> int
    val @ : 'a list * 'a list -> 'a list
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val last : 'a list -> 'a
    val getItem : 'a list -> ('a * 'a list) option
    val nth : 'a list * int -> 'a
    val take : 'a list * int -> 'a list
    val drop : 'a list * int -> 'a list
    val rev : 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list
    val app : ('a -> unit) -> 'a list -> unit
    val map : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list
    val find : ('a -> bool) -> 'a list -> 'a option
    val filter : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val all : ('a -> bool) -> 'a list -> bool
    val tabulate : int * (int -> 'a) -> 'a list
    val collate : ('a * 'a -> order) -> 'a list * 'a list -> order
    
  end
