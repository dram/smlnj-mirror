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

signature CORE = 
sig
   structure Assembly : ASSEMBLY
   exception Bind
   exception Match
   val stringequal : string * string -> bool
   val polyequal : 'a * 'a -> bool
end

functor CoreFunc(Assembly : ASSEMBLY) : CORE =
struct
    structure Assembly = Assembly

    exception Bind
    exception Match

    val stringequal = Assembly.seql

    local
	val width_tags = 4
	val power_tags = 16
	val tag_record =	1
	val tag_array =	9
	val tag_bytearray =	11
	val tag_string =	15
	val tag_embedded =	7
	val tag_closure =	13
	val tag_backptr =	5
	val tag_forwarded =	3

       val ieql = InLine.ieql
       val cast = InLine.cast
       val sub = InLine.subscript
       val boxed = InLine.boxed
       val / = InLine.div
       val op * = InLine.*
       val + = InLine.+
       val - = InLine.-
     in fun polyequal(a : 'a, b : 'a) = ieql(a,b)
      orelse
      (boxed a andalso boxed b
       andalso
	  (* assumes identical tags, with string=embedded *)
	  let val tag : int = InLine.subscript(a,~1)
	      val tag = +(+(tag,tag),1)
	      val tag = -(tag,op *(/(tag,power_tags),power_tags))
	      val alen = InLine.alength a
	      fun maprec () =
		    let fun m i = if ieql(i,alen) then true
	 			  else if polyequal(sub(a,i), sub(b,i))
				    then m(InLine.+(i,1))
				  else false
		    in  m 0
		    end
	  in  if ieql(tag,tag_array) then false
	      else if ieql(tag,tag_bytearray) then false
	      else ieql(alen,InLine.alength(b)) 
		   andalso  if ieql(tag,tag_record) then maprec()
			    else Assembly.seql(cast a, cast b)
	  end)
  end
end

