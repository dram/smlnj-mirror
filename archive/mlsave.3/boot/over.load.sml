structure Overloads =
struct
  open PrimTypes
  val intequal = InLinePrim.ieql : (int * int -> bool)
  val refequal = InLinePrim.ieql : ('a ref * 'a ref -> bool)
  val arrayequal = InLinePrim.ieql : ('a array * 'a array -> bool)
  val realequal = InLinePrim.feql : (real * real -> bool)
  val ! = InLinePrim.! : ('a ref -> 'a)
  val := = InLinePrim.:= : ('a ref * 'a -> unit)

 structure Boot = struct
        val imul  : (int * int -> int) = InLinePrim.*
        val iadd  :  (int * int -> int) = InLinePrim.+
        val isub  :  (int * int -> int) = InLinePrim.-
        val idiv  :  (int * int -> int) = InLinePrim.div
        val ilss  :  (int * int -> bool) = InLinePrim.<
        val ileq  :  (int * int -> bool) = InLinePrim.<=
        val igtr  :  (int * int -> bool) = InLinePrim.>
        val igeq  :  (int * int -> bool) = InLinePrim.>=
	val alength  : 'a array -> int = InLinePrim.alength
        val fadd  : real * real -> real = InLinePrim.fadd
        val fdiv  : real * real -> real = InLinePrim.fdiv
        val feql  : real * real -> bool = InLinePrim.feql
        val fge  : real * real -> bool = InLinePrim.fge
        val fgt  : real * real -> bool = InLinePrim.fgt
        val fle  : real * real -> bool = InLinePrim.fle
        val flt  : real * real -> bool = InLinePrim.flt
        val fmul  : real * real -> real = InLinePrim.fmul
        val fneg  : real -> real = InLinePrim.fneg
        val fneq  : real * real -> bool = InLinePrim.fneq
        val fsub  : real * real -> real = InLinePrim.fsub
	val ineq  : int * int -> bool = InLinePrim.ineq
	val slength  : string -> int = InLinePrim.slength
	val ineg  : int -> int = InLinePrim.~
  end

 local open Boot
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
  overload length : ('a -> int) as slength and alength
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

 structure Boot = Initial.Boot

  infix 3 o := sub
  infix 4 = <> < > <= >=
  infixr 5 ::
  infix 5 @
  infix 6 + - ^
  infix 7 * / div mod

  val op = = InLinePrim.=
  val op <> = InLinePrim.<>

end
