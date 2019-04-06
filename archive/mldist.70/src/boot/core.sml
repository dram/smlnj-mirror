(* Copyright 1989 by AT&T Bell Laboratories *)
(*
The following are already in the symbol table:
     1) Magical words that can be free in signatures (from PrimTypes):
		int string bool unit real list array ref exn
     2) Built-in constructors (from PrimTypes):
		:: nil ref true false
     3) Built-in structures:
		PrimTypes InLine
	The InLine structure is not typed (all values have type alpha).
All matches in this file should be exhaustive; the match and bind exceptions
 are not defined at this stage of bootup, so any uncaught match will cause
 an unpredictable error.
*)


functor CoreFunc(Assembly : ASSEMBLY) =
  struct
    structure Assembly = Assembly

    exception Bind
    exception Match

    exception Ord        (* for strings, bytearray update bounds check *)
    exception Range      (* for bytearray update *)
    exception Subscript  (* for arrays *)

    val current = Assembly.current
    val toplevel = Assembly.A.create_b 18

    val getDebugf = ref ()

    val forcer_p = ref (fn () => ())

    fun getDebug x = InLine.! getDebugf x

    fun stringequal(a,b) =
	  if InLine.ieql(a,b) then true
	    else InLine.boxed a andalso InLine.boxed b andalso
	      let val len = InLine.slength a
		in if InLine.ieql(len,InLine.slength b)
		  then let
		    fun f 0 = true
		      | f i = let val j = InLine.-(i,1)
			      in if InLine.ieql(InLine.ordof(a,j), InLine.ordof(b,j))
			           then f j else false
			      end
		    in f len end
		  else false
		end

    local
       val ieql = InLine.ieql and cast = InLine.cast and sub = InLine.subscript
       and boxed = InLine.boxed and op * = InLine.*
       and + = InLine.+  and  - = InLine.-
    in
    fun polyequal(a : 'a, b : 'a) = ieql(a,b)
      orelse
      (boxed a andalso boxed b
       andalso
	  let val taga = sub(a,~1)
	  in  case InLine.andb(taga,7)
		of 0 (* tag_record div 2 *) => 
		     if ieql(sub(b,~1),taga)
			 then let val lenm1 = -(InLine.alength a, 1)
				  fun m j = if ieql(j,lenm1)
					     then polyequal(sub(a,j),sub(b,j))
					     else polyequal(sub(a,j),sub(b,j))
						  andalso m(+(j,1))
			       in if ieql(lenm1,1) 
				      andalso ieql(sub(a,1),sub(b,1))
				      then polyequal(sub(a,0),sub(b,0))
				      else m 0
			      end
			 else false
	         | 4 (* tag_array div 2 *) => false
		 | 5 (* tag_bytearray div 2 *) => false
		 | _ => stringequal(cast a, cast b)
	  end)
     end (* local *)

end
