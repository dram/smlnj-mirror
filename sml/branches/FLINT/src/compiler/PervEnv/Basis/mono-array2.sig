(* mono-array2.sig
 *
 * COPYRIGHT (c) 1997 AT&T Research.
 *)

signature MONO_ARRAY2 =
  sig

    eqtype array
    type elem
    type region = {
	base : array,
	row : int, col : int,
	nrows : int option, ncols : int option
      }
    datatype traversal = datatype Array2.traversal

    structure Vector : MONO_VECTOR
      sharing type elem = Vector.elem

    val array : (int * int * elem) -> array
    val fromList : elem list list -> array
    val tabulate : traversal -> (int * int * ((int * int) -> elem)) -> array
    val sub : (array * int * int) -> elem
    val update : (array * int * int * elem) -> unit
    val dimensions : array -> (int * int)
    val nCols : array -> int
    val nRows : array -> int
    val row : (array * int) -> Vector.vector
    val column : (array * int) -> Vector.vector
    val copy : {
	    src : region, dst : array, dst_row : int, dst_col : int
	  } -> unit

    val appi    : traversal -> ((int * int * elem) -> unit) -> region -> unit
    val app     : traversal -> (elem -> unit) -> array -> unit
    val modifyi : traversal -> ((int * int * elem) -> elem) -> region -> unit
    val modify  : traversal -> (elem -> elem) -> array -> unit
    val foldi   : traversal -> ((int * int * elem * 'a) -> 'a) -> 'a -> region -> 'a
    val fold    : traversal -> ((elem * 'a) -> 'a) -> 'a -> array -> 'a

  end


(*
 * $Log: mono-array2.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:03  george
 * Version 110.5
 *
 *)
