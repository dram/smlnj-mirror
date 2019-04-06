(* Can't give a null signature because other functions get the ref
   datatype from Overloads (open PrimTypes). *)
structure Overloads =
struct
 open PrimTypes

 structure Hidden = struct
        val imul : (int * int -> int) = InLine.*
        val iadd : (int * int -> int) = InLine.+
        val isub : (int * int -> int) = InLine.-
        val idiv : (int * int -> int) = InLine.div
        val ilss : (int * int -> bool) = InLine.<
        val ileq : (int * int -> bool) = InLine.<=
        val igtr : (int * int -> bool) = InLine.>
        val igeq : (int * int -> bool) = InLine.>=
	val alength : 'a array -> int = InLine.alength
        val fadd : real * real -> real = InLine.fadd
        val fdiv : real * real -> real = InLine.fdiv
        val feql : real * real -> bool = InLine.feql
        val fge : real * real -> bool = InLine.fge
        val fgt : real * real -> bool = InLine.fgt
        val fle : real * real -> bool = InLine.fle
        val flt : real * real -> bool = InLine.flt
        val fmul : real * real -> real = InLine.fmul
        val fneg : real -> real = InLine.fneg
        val fneq : real * real -> bool = InLine.fneq
        val fsub : real * real -> real = InLine.fsub
	val ineq : int * int -> bool = InLine.ineq
	val slength : string -> int = InLine.slength
	val ineg : int -> int = InLine.~
  end

 local open Hidden
  in

  overload ~ : ('a->'a) as ineg and fneg
  overload + : ('a*'a -> 'a) as iadd and fadd
  overload - : ('a*'a -> 'a) as isub and fsub
  overload * : ('a*'a -> 'a) as imul and fmul
  overload < : ('a*'a -> bool) as ilss and flt and 
				Initial.Pervasives.String.<
  overload > : ('a*'a -> bool) as igtr and fgt and 
				Initial.Pervasives.String.>
  overload <= : ('a*'a -> bool) as ileq and fle and 
				Initial.Pervasives.String.<=
  overload >= : ('a*'a -> bool) as igeq and fge and 
				Initial.Pervasives.String.>=
  overload abs : ('a->'a) as Initial.Pervasives.Integer.abs 
			 and Initial.Pervasives.Real.abs
  overload length : ('a -> int) as Initial.Pervasives.String.length and alength
			       and Initial.Pervasives.List.length
  overload makestring : ('a -> string)
        as Initial.Pervasives.Bool.makestring 
        and Initial.Pervasives.Integer.makestring 
	and Initial.Pervasives.Real.makestring
  overload print : ('a -> 'a)
        as Initial.Pervasives.Bool.print 
        and Initial.Pervasives.Integer.print 
	and Initial.Pervasives.Real.print
	and Initial.Pervasives.String.print
 end

  (* These seem to be necessary... *)
  infix 7 * / div mod
  infix 6 + - ^
  infixr 5 :: @
  infix 4 = <> < > <= >=
  infix 3 o := sub
  infix before
	
  val ! : 'a ref -> 'a = InLine.!
  val op := : 'a ref * 'a -> unit = InLine.:=

  val op = : ''a * ''a -> bool  = InLine.=
  val op <> : ''a * ''a -> bool = InLine.<>

end
(*
structure InLine = struct end  (* hide previous structure *)
*)
