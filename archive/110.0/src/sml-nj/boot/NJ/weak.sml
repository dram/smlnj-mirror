(* weak.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Weak :> WEAK =
  struct
    type 'a weak = 'a
    fun weak (x : 'a) : 'a weak = InlineT.mkspecial(Tags.special_weak, x)
    fun strong (x : 'a weak) : 'a option =
	  if InlineT.getspecial x = Tags.special_weak
	    then SOME(InlineT.PolyArray.sub(InlineT.cast x, 0))
	    else NONE
    type weak' = Assembly.object
    fun weak' x = InlineT.mkspecial(Tags.special_weak, x)
    fun strong' x = InlineT.getspecial x = Tags.special_weak
  end

structure Susp :> SUSP =
  struct
    type 'a susp = 'a
    fun delay (f : unit -> 'a) = InlineT.mkspecial(Tags.special_unevaled_susp, f)
    fun force (x : 'a susp) =
	  if InlineT.getspecial x = Tags.special_unevaled_susp
	    then let
	      val y : 'a = InlineT.PolyArray.sub (InlineT.cast x, 0) ()
	      in
		InlineT.PolyArray.update (InlineT.cast x, 0, y);
		InlineT.setspecial(InlineT.cast x, Tags.special_evaled_susp);
		y
	      end
	    else InlineT.PolyArray.sub (InlineT.cast x, 0)
  end 


(*
 * $Log: weak.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:40  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1  1997/02/11 20:44:49  george
 *   Version 109.25.1
 *
 * Revision 1.2  1997/01/31  20:39:45  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:18  george
 *   Version 109.24
 *
 *)
