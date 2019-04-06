(* Standard environment, as defined in Definition of SML, Version 2 *)
(* Warning:  real-number exceptions not done right *)

structure Standard = 
  struct

    infix 7 * / div mod
    infix 6 + - ^
    infixr 5 :: @
    infix 4 = <> < > <= >=
    infix 3 o :=

    val ! = Ref.!
    val op := = Ref.:=
    val not = Bool.not
    val floor = Real.floor
    val truncate = Real.truncate
    val ceiling = Real.ceiling
    val real = Real.real
    val sin = Real.sin and cos = Real.cos 
    and sqrt = Real.sqrt and arctan = Real.arctan
    and exp = Real.exp and ln = Real.ln
    and rev = List.rev and map = List.map and op @ = List.@
    exception Floor=Real.Floor and Sqrt=Real.Sqrt
    exception Exp=Real.Exp and Ln=Real.Ln
    val op / = Real./
    
    exception Mod
    exception Quot
    exception Prod
    exception Sum
    exception Diff
    exception Neg
    exception Chr = String.Chr and Ord = String.Ord
    exception Div = Integer.Div

    val op + = fn (a,b) => Integer.+(a,b) handle Integer.Overflow => raise Sum
    val op * = fn (a,b) => Integer.*(a,b) handle Integer.Overflow => raise Prod
    val op div(a,b) = Integer.div(a,b) handle Integer.Overflow => raise Div
			(* handles minint div ~1 case *)
    val op - = fn (a,b) => Integer.-(a,b) handle Integer.Overflow => raise Diff
    val ~ = fn a => Integer.~ a handle Integer.Overflow => raise Neg
    val op mod = fn (a,b) => Integer.mod(a,b) handle Div => raise Mod
					   | Integer.Overflow => raise Mod

    type instream = IO.instream
    type outstream = IO.outstream
    exception Io = IO.Io
    val std_in = IO.std_in
    val std_out = IO.std_out
    val open_in = IO.open_in
    val open_out = IO.open_out
    val close_in = IO.close_in
    val close_out = IO.close_out
    val output = IO.output
    val input = IO.input
    val lookahead = IO.lookahead
    val end_of_stream = IO.end_of_stream

    val size = String.size
    val explode = String.explode and implode = String.implode
    val op ^ = String.^
    val chr = String.chr and ord = String.ord
        
    val op o = General.o and op = = General.= and op <> = General.<>
    exception Bind= General.Bind and Match = General.Match
    exception Interrupt = General.Interrupt
  end (* structure Standard *)

structure Standard =
  struct
    open Standard
    overload ~ : ('a->'a) as Integer.~ and Real.~
    overload + : ('a*'a -> 'a) as Standard.+ and Real.+
    overload - : ('a*'a -> 'a) as Standard.- and Real.-
    overload * : ('a*'a -> 'a) as Standard.* and Real.*
    overload < : ('a*'a -> bool) as Integer.< and Real.<
    overload > : ('a*'a -> bool) as Integer.> and Real.>
    overload <= : ('a*'a -> bool) as Integer.<= and Real.<=
    overload >= : ('a*'a -> bool) as Integer.>= and Real.>=
    overload abs : ('a->'a) as Integer.abs and Real.abs

  end (* structure Environ *)
