(* ieee-real.sml
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *)

structure IEEEReal : IEEE_REAL =
  struct

  (* this may cause portability problems to 64-bit systems *)
    structure Int = Int31

    exception Unordered

    datatype real_order = LESS | EQUAL | GREATER | UNORDERED

    datatype nan_mode = QUIET | SIGNALLING

    datatype float_class
      = NAN of nan_mode
      | INF
      | ZERO
      | NORMAL
      | SUBNORMAL

    datatype rounding_mode
      = TO_NEAREST
      | TO_NEGINF
      | TO_POSINF
      | TO_ZERO

    val ctlRoundingMode : int option -> int =
	    CInterface.c_function "SMLNJ-Math" "ctlRoundingMode"

    fun intToRM 0 = TO_NEAREST
      | intToRM 1 = TO_ZERO
      | intToRM 2 = TO_POSINF
      | intToRM 3 = TO_NEGINF
      | intToRM _ = raise Match (* shut up compiler *)

    fun setRoundingMode' m = (ctlRoundingMode (SOME m); ())

    fun setRoundingMode TO_NEAREST	= setRoundingMode' 0
      | setRoundingMode TO_ZERO		= setRoundingMode' 1
      | setRoundingMode TO_POSINF	= setRoundingMode' 2
      | setRoundingMode TO_NEGINF	= setRoundingMode' 3

    fun getRoundingMode () = intToRM (ctlRoundingMode NONE)

    type decimal_approx = {
	kind : float_class,
	sign : bool,
	digits : int list,
	exp : int
      }

    fun toString {kind, sign, digits, exp} = let
	  fun fmtExp 0 = []
	    | fmtExp i = ["E", IntImp.toString i]
	  fun fmtDigits ([], tail) = tail
	    | fmtDigits (d::r, tail) =
	      (IntImp.toString d) :: fmtDigits(r, tail)
	  in
	    case (sign, kind, digits)
	     of (true, ZERO, _) => "~0.0"
	      | (false, ZERO, _) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), []) => "~0.0"
	      | (false, (NORMAL|SUBNORMAL), []) => "0.0"
	      | (true, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("~0." :: fmtDigits(digits, fmtExp exp))
	      | (false, (NORMAL|SUBNORMAL), _) =>
		  StringImp.concat("0." :: fmtDigits(digits, fmtExp exp))
	      | (true, INF, _) => "~inf"
	      | (false, INF, _) => "inf"
	      | (_, NAN _, []) => "nan"
	      | (_, NAN _, _) =>
		  StringImp.concat("nan(" :: fmtDigits(digits, [")"]))
	    (* end case *)
	  end

  (* FSM-based implementation of scan: *)
    fun scan gc = let

	val isDigit = Char.isDigit
	val toLower = Char.toLower

	(* check for a literal sequence of case-insensitive chanacters *)
	fun check ([], ss) = SOME ss
	  | check (x :: xs, ss) =
	    (case gc ss of
		 NONE => NONE
	       | SOME (c, ss') =>
		 if toLower c = x then check (xs, ss') else NONE)

	(* return INF or NAN *)
	fun infnan (class, sign, ss) =
	    SOME ({ kind = class,
		    sign = sign,
		    digits = [],
		    exp = 0 },
		  ss)

	(* we have seen "i" (or "I"), now check for "nf(inity)?" *)
	fun check_nf_inity (sign, ss) =
	    case check ([#"n", #"f"], ss) of
		NONE => NONE
	      | SOME ss' =>
		(case check ([#"i", #"n", #"i", #"t", #"y"], ss') of
		     NONE => infnan (INF, sign, ss')
		   | SOME ss'' => infnan (INF, sign, ss''))

	(* we have seen "n" (or "N"), now check for "an" *)
	fun check_an (sign, ss) =
	    case check ([#"a", #"n"], ss) of
		NONE => NONE
	      | SOME ss' => infnan (NAN QUIET, sign, ss')

	(* we have succeeded constructing a normal number,
	 * dl is still reversed and might have trailing zeros... *)
	fun normal (ss, sign, dl, n) = let
	    fun srev ([], r) = r
	      | srev (0 :: l, []) = srev (l, [])
	      | srev (x :: l, r) = srev (l, x :: r)
	in
	    SOME (case srev (dl, []) of
		      [] => { kind = ZERO,
			      sign = sign,
			      digits = [],
			      exp = 0 }
		    | digits => { kind = NORMAL,
				  sign = sign,
				  digits = digits,
				  exp = n },
		  ss)
	end

	(* scanned exponent (e), adjusted by position of decimal point (n) *)
	fun exponent (n, esign, e) = n + (if esign then ~e else e)

	(* scanning the remaining digits of the exponent *)
	fun edigits (ss, sign, dl, n, esign, e) =
	    case gc ss of
		NONE => normal (ss, sign, dl, exponent (n, esign, e))
	      | SOME (dg, ss') =>
		if isDigit dg then
		    edigits (ss', sign, dl, n, esign,
			     10 * e + ord dg - ord #"0")
		else
		    normal (ss, sign, dl, exponent (n, esign, e))

	(* scanning first digit of exponent *)
	fun edigit1 (ss, sign, dl, n, esign) =
	    case gc ss of
		NONE => NONE
	      | SOME (dg, ss') =>
		if isDigit dg then
		    edigits (ss', sign, dl, n, esign, ord dg - ord #"0")
		else NONE

	(* we have seen the "e" (or "E") and are now scanning an exponent *)
	fun exp (ss, sign, dl, n) =
	    case gc ss of
		NONE => NONE
	      | SOME (#"+", ss') => edigit1 (ss', sign, dl, n, false)
	      | SOME ((#"-" | #"~"), ss') => edigit1 (ss', sign, dl, n, true)
	      | SOME _ => edigit1 (ss, sign, dl, n, false)

	(* digits in fractional part *)
	fun fdigits (ss, sign, dl, n) = let
	    fun dig (ss, dg) =
		fdigits (ss, sign, (ord dg - ord #"0") :: dl, n)
	in
	    case gc ss of
		NONE => normal (ss, sign, dl, n)
	      | SOME ((#"e" | #"E"), ss') => exp (ss', sign, dl, n)
	      | SOME (#"0", ss') =>
		(case dl of
		     [] => fdigits (ss', sign, dl, n - 1)
		   | _ => dig (ss', #"0"))
	      | SOME (dg, ss') =>
		if isDigit dg then dig (ss', dg) else normal (ss, sign, dl, n)
	end

	(* digits in integral part *)
	fun idigits (ss, sign, dl, n) = let
	    fun dig (ss', dg) =
		idigits (ss', sign, (ord dg - ord #"0") :: dl, n + 1)
	in
	    case gc ss of
		NONE => normal (ss, sign, dl, n)
	      | SOME (#".", ss') => fdigits (ss', sign, dl, n)
	      | SOME ((#"e" | #"E"), ss') => exp (ss', sign, dl, n)
	      | SOME (#"0", ss') =>
		(case dl of
		     (* ignore leading zeros in integral part *)
		     [] => idigits (ss', sign, dl, n)
		   | _ => dig (ss', #"0"))
	      | SOME (dg, ss') =>
		if isDigit dg then dig (ss', dg) else normal (ss, sign, dl, n)
	end

	(* we know the sign of the mantissa, now let's get it *)
	fun signed (sign, ss) =
	    case gc ss of
		NONE => NONE
	      | SOME ((#"i" | #"I"), ss') => check_nf_inity (sign, ss')
	      | SOME ((#"n" | #"N"), ss') => check_an (sign, ss')
	      | SOME (#".", ss') => fdigits (ss', sign, [], 0)
	      | SOME (dg, _) => if isDigit dg then idigits (ss, sign, [], 0)
				else NONE

	(* start state: check for sign of mantissa *)
	fun start ss =
	    case gc ss of
		NONE => NONE
	      | SOME (#"+", ss') => signed (false, ss')
	      | SOME ((#"-" | #"~"), ss') => signed (true, ss')
	      | SOME _ => signed (false, ss)
    in
	start
    end

  (* use "scan" to implement "fromString" *)
    fun fromString s = StringCvt.scanString scan s

  end;
