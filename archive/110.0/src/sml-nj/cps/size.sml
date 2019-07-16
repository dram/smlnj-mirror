(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
structure CPSsize =
struct
local open CPS

fun cells_list (l : 'a list) = 2 * length l
fun descriptors_list (l : 'a list) = length l

val rec cells =
  fn RECORD(_,vl,w,ce) => 2 + 3 + cells_list vl + cells ce
   | SELECT(i,v,w,_,ce) => 2 + 4 + cells ce
   | OFFSET(i,v,w,ce) => 2 + 4 + cells ce
   | APP(v,vl) => 2 + 2 + cells_list vl
   | FIX(l,ce) => 2 + 2 + cells_list l + cells ce
	    + foldr (fn((_,f,vl,_,e),b) => 3 + cells_list vl + cells e + b) 0 l
   | SWITCH(v,c,l) => 2 + 3 + cells_list l + foldr (fn(a,b) => cells a + b) 0 l
   | LOOKER(_,vl,w,_,ce) => 2+4+ cells_list vl + cells ce
   | SETTER(_,vl,ce) => 2+3+ cells_list vl + cells ce
   | ARITH(_,vl,w,_,ce) => 2+4+ cells_list vl + cells ce
   | PURE(_,vl,w,_,ce) => 2+4+ cells_list vl + cells ce
   | BRANCH(_,vl,c,e1,e2) => 2+5+cells_list vl + cells e1 + cells e2

val rec descriptors =
  fn RECORD(_,vl,w,ce) => 2 + descriptors_list vl + descriptors ce
   | SELECT(i,v,w,_,ce) => 2 + descriptors ce
   | OFFSET(i,v,w,ce) => 2 + descriptors ce
   | APP(v,vl) => 2 + descriptors_list vl
   | FIX(l,ce) => 2 + descriptors_list l + descriptors ce
		+ foldr (fn((_,f,vl,_,e),b) => 1 + descriptors_list vl
			+ descriptors e + b) 0 l
   | SWITCH(v,c,l) => 2 + descriptors_list l
			+ foldr (fn(a,b) => descriptors a + b) 0 l
   | LOOKER(_,vl,w,_,ce) => 2 + descriptors_list vl + descriptors ce
   | SETTER(_,vl,ce) => 2+ descriptors_list vl + descriptors ce
   | ARITH(_,vl,w,_,ce) => 2+ descriptors_list vl + descriptors ce
   | PURE(_,vl,w,_,ce) => 2+ descriptors_list vl + descriptors ce
   | BRANCH(_,vl,c,e1,e2) => 2+descriptors_list vl 
                                + descriptors e1 + descriptors e2
in

fun printsize ce =
  let val c = cells ce
      val d = descriptors ce
   in app Control.Print.say["CPSsize: #cells = ", Int.toString c, "; #descriptors = ",
			   Int.toString d, "; total = ", Int.toString(c+d), "\n"]
  end

end (* local *)

end (* structure CPSsize *)

(*
 * $Log: size.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:32  george
 *   Version 109.24
 *
 *)
