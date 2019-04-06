(* Copyright 1989,1990,1991 by AT&T Bell Laboratories *)
(* util/arrayext.sml *)

signature ARRAYEXT =
sig
  val copy : '1a array  -> '1a array
  val app : ('a -> 'b) * 'a array -> unit
  val remap : ('1a -> '1a) * '1a array -> unit
end

structure ArrayExt : ARRAYEXT =
struct

  open Array
  infix 9 sub (* Aargh! *)

  (* copy -- used in ApplyFct *)

  fun copy a =
      if Array.length a=0 then arrayoflist nil
      else
	 let val new = array(Array.length a,a sub 0)
	    fun loop i = (update(new,i,a sub i); loop(i+1))
	 in loop 0
	    handle Subscript => new
	 end

  (* app -- used in EqTypes, AbstractFct, ApplyFct *)

  fun app (f,a) =  
      let fun loop i = (f(a sub i); loop(i+1))
       in loop 0 handle Subscript => ()
      end

  (* remap -- used in ApplyFct, AbstractFct *)

  fun remap (f,a) =
      let fun loop i = (update(a,i,f(a sub i)); loop(i+1))
       in loop 0 handle Subscript => ()
      end

end
