(* time-comp.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * Time a compile of the SML/NJ system.
 *
 *)

local
structure T = Time
structure Tm = Timer
fun cvt {usr, gc, sys} = {
	usr = T.toString usr,
	gc = T.toString gc,
	sys = T.toString sys,
	tot = T.toString(T.+(usr, T.+(sys, gc)))
      }
in
fun make () = let
      val t0 = Tm.startCPUTimer()
      in
	CMB.make();
	cvt (Tm.checkCPUTimer t0)
      end
end;


(*
 * $Log: time-comp.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:38:07  george
 *   Version 109.24
 *
 *)
