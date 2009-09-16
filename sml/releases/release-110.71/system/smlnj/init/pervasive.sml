(* pervasive.sml
 *
 * COPYRIGHT (c) 2009 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * These are the pervasive bindings as defined by the SML'97
 * Basis Library specification (Chapter 3 of Gansner and
 * Reppy, 2004).  Note that this file must be processed before
 * the code in base/system/Basis/Implementation, so we have to
 * reverse the order of the bindings (e.g., Real.round is bound
 * to the top-level round, instead of the other way around).
 *)

infix 7  * / mod div
infix 6 ^ + -
infix 3 := o
infix 4 > < >= <= = <>
infixr 5 :: @
infix 0 before

(* top-level type (we need this one early) *)

datatype bool = datatype PrimTypes.bool

val op o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) = InlineT.compose

local
    structure I31 = InlineT.Int31
    structure I32 = InlineT.Int32
    structure W8 = InlineT.Word8
    structure W31 = InlineT.Word31
    structure W32 = InlineT.Word32
    structure CW64 = CoreWord64
    structure CI64 = CoreInt64
    structure R64 = InlineT.Real64
    structure CV = InlineT.CharVector
    structure PV = InlineT.PolyVector
    structure DI = InlineT.DfltInt

    structure CII = CoreIntInf

    fun w8adapt oper args = W8.andb (oper args, 0wxFF)
    val w8plus = w8adapt W8.+
    val w8minus = w8adapt W8.-
    val w8times = w8adapt W8.*
    val w8neg = w8adapt W8.~

    fun stringlt (a, b) = let
	val al = CV.length a
	val bl = CV.length b
	val ashort = DI.< (al, bl)
	val n = if ashort then al else bl
	fun cmp i =
	    if InlineT.= (i, n) then ashort
	    else let
		val ai = CV.sub (a, i)
		val bi = CV.sub (b, i)
		in
		  InlineT.Char.< (ai, bi) orelse
		  (InlineT.= (ai, bi) andalso cmp (DI.+ (i, 1)))
		end
	in
	  cmp 0
	end
    fun stringle (a, b) = if stringlt (b, a) then false else true
    fun stringgt (a, b) = stringlt (b, a)
    fun stringge (a, b) = stringle (b, a)
in
overload ~ :   ('a -> 'a)
   as  I31.~ and I32.~ and CI64.~ and CII.~
   and w8neg and W31.~ and W32.~ and CW64.~
   and R64.~
overload + :   ('a * 'a -> 'a)
  as  I31.+ and I32.+ and CI64.+ and CII.+
  and w8plus and W31.+ and W32.+ and CW64.+
  and R64.+
overload - :   ('a * 'a -> 'a)
  as  I31.- and I32.- and CI64.- and CII.-
  and w8minus and W31.- and W32.- and CW64.-
  and R64.-
overload * :   ('a * 'a -> 'a)
  as  I31.* and I32.* and CI64.* and CII.*
  and w8times and W31.* and W32.* and CW64.*
  and R64.*
(*
overload / : ('a * 'a -> 'a)
  as R64./
*)
val op / = R64./		(* temporary hack around overloading bug *)
overload div : ('a * 'a -> 'a)
  as  I31.div and I32.div and CI64.div and CII.div
  and W8.div and W31.div and W32.div and CW64.div
overload mod : ('a * 'a -> 'a)
  as  I31.mod and I32.mod and CI64.mod and CII.mod
  and W8.mod and W31.mod and W32.mod and CW64.mod
overload < :   ('a * 'a -> bool)
  as  I31.< and I32.< and CI64.< and CII.<
  and W8.< and W31.< and W32.< and CW64.<
  and R64.<
  and InlineT.Char.<
  and stringlt
overload <= :   ('a * 'a -> bool)
  as  I31.<= and I32.<= and CI64.<= and CII.<=
  and W8.<= and W31.<= and W32.<= and CW64.<=
  and R64.<=
  and InlineT.Char.<=
  and stringle
overload > :   ('a * 'a -> bool)
  as  I31.> and I32.> and CI64.> and CII.>
  and W8.> and W31.> and W32.> and CW64.>
  and R64.>
  and InlineT.Char.>
  and stringgt
overload >= :   ('a * 'a -> bool)
  as  I31.>= and I32.>= and CI64.>= and CII.>=
  and W8.>= and W31.>= and W32.>= and CW64.>=
  and R64.>=
  and InlineT.Char.>=
  and stringge
overload abs : ('a -> 'a)
  as I31.abs and I32.abs and CI64.abs and CII.abs and R64.abs

type unit = PrimTypes.unit
type exn = PrimTypes.exn

exception Bind = Core.Bind
exception Match = Core.Match
exception Subscript = Core.Subscript
exception Size = Core.Size
exception Overflow = Assembly.Overflow
exception Chr = InlineT.Char.Chr
exception Div = Assembly.Div
exception Domain

type string = PrimTypes.string

exception Fail of string

(* exception Span
 * datatype order
 * datatype option
 * exception Option
 * val getOpt
 * val isSome
 * val valOf
 * val op =
 * val op <>
 *)
open PrePervasive

val ! = InlineT.!
val op := = InlineT.:=

val op before : ('a * unit) -> 'a = InlineT.before
val ignore : 'a -> unit = InlineT.ignore

(* top-level types *)

datatype list = datatype PrimTypes.list
datatype ref = datatype PrimTypes.ref

(* top-level value identifiers *)

(* Bool *)
val not = InlineT.inlnot

(* Int *)
type int = PrimTypes.int

(* Word *)
type word = PrimTypes.word

(* Real *)
local
  val w31_r = R64.from_int32 o I32.copy_word31
  val intbound = w31_r 0wx40000000	(* not necessarily the same as rbase *)
  val negintbound = R64.~ intbound
in
type real = PrimTypes.real

val real = R64.from_int31

fun floor x =
      if x < intbound andalso x >= negintbound then Assembly.A.floor x
      else if R64.==(x, x) then raise Overflow (* not a NaN *)
      else raise Domain

fun ceil x = DI.- (~1, floor (R64.~ (x + 1.0)))

fun trunc x = if R64.< (x, 0.0) then ceil x else floor x

fun round x = let
    (* ties go to the nearest even number *)
      val fl = floor(x+0.5)
      val cl = ceil(x-0.5)
      in
	if fl=cl then fl
	else if W31.andb(W31.fromInt fl,0w1) = 0w1 then cl
	else fl
      end

end (* local *)

(* List *)
exception Empty
fun null [] = true
  | null _ = false
fun hd (h :: _) = h
  | hd [] = raise Empty
fun tl (_ :: t) = t
  | tl [] = raise Empty
fun foldl f b l = let
      fun f2 ([], b) = b
	| f2 (a :: r, b) = f2 (r, f (a, b))
      in
	f2 (l, b)
      end
fun length l = let
    (* fast add that avoids the overflow test *)
      fun a + b = W31.copyt_int31 (W31.+(W31.copyf_int31 a, W31.copyf_int31 b))
      fun loop (n, []) = n
	| loop (n, [_]) = n + 1
	| loop (n, _ :: _ :: l) = loop (n + 2, l)
      in
	loop (0, l)
      end
fun rev l = let
      fun loop ([], l) = l
	| loop (x::xs, l) = loop(xs, x::l)
      in
	loop (l, [])
      end
fun foldr f b = let
      fun f2 [] = b
	| f2 (a :: r) = f (a, f2 r)
      in
	f2
      end
fun l1 @ l2 = foldr (op ::) l2 l1
fun app f = let
      fun a2 [] = ()
	| a2 (h :: t) = (f h : unit; a2 t)
      in
	a2
      end
fun map f = let
      fun m [] = []
	| m [a] = [f a]
	| m [a, b] = [f a, f b]
	| m [a, b, c] = [f a, f b, f c]
	| m (a :: b :: c :: d :: r) = f a :: f b :: f c :: f d :: m r
      in
	m
      end

(* Array *)
type 'a array = 'a PrimTypes.array

(* Vector *)
type 'a vector = 'a PrimTypes.vector

fun vector l = let
      val n = length l
      in
	if DI.ltu (Core.max_length, n) then raise Size
	else if (n = 0) then Assembly.vector0
	else Assembly.A.create_v(n, l)
      end

(* Char *)
type char = PrimTypes.char
val ord = InlineT.Char.ord
val chr = InlineT.Char.chr

(* String *)
local
    (* allocate an uninitialized string of given length *)
    fun create n =
	if (DI.ltu (Core.max_length, n)) then raise Size
	else Assembly.A.create_s n

    val unsafeSub = CV.sub
    val unsafeUpdate = CV.update
in

val size = CV.length : string -> int

fun str (c: char) : string = PV.sub (PreString.chars, InlineT.cast c)

(* concatenate a list of strings together *)
fun concat [] = ""
  | concat [s] = s
  | concat (sl : string list) = let
    (* compute total length of result string *)
      fun length (i, []) = i
	| length (i, s::rest) = length(i+size s, rest)
      in
	case length (0, sl)
	 of 0 => ""
	  | 1 => let
	      fun find ("" :: r) = find r
		| find (s :: _) = s
		| find _ = "" (** impossible **)
	      in
		find sl
	      end
	  | totLen => let
	      val ss = create totLen
	      fun copy ([], _) = ()
		| copy (s::r, i) = let
		    val len = size s
		    fun copy' j =
			if (j = len) then ()
			else (unsafeUpdate(ss, i+j, unsafeSub(s, j));
			      copy'(j+1))
		    in
		      copy' 0;
		      copy (r, i+len)
		    end
	      in
		copy (sl, 0);
		ss
	      end
	(* end case *)
      end (* concat *)


(* implode a list of characters into a string *)
fun implode [] = ""
  | implode cl = PreString.implode (length cl, cl)

(* explode a string into a list of characters *)
fun explode s = let
      fun f(l, ~1) = l
	| f(l,  i) = f (unsafeSub(s, i) :: l, i-1)
      in
	f ([], size s - 1)
      end

(* Return the n-character substring of s starting at position i.
 * NOTE: we use words to check the right bound so as to avoid
 * raising overflow.
 *)
local
    structure W = InlineT.DfltWord
in
    fun substring (s, i, n) =
	if ((i < 0) orelse (n < 0)
	  orelse W.<(W.fromInt(size s), W.+(W.fromInt i, W.fromInt n)))
	    then raise Core.Subscript
	else PreString.unsafeSubstring (s, i, n)
end (* local *)

fun "" ^ s = s
  | s ^ "" = s
  | x ^ y = PreString.concat2 (x, y)

end (* local *)

(* Substring *)
type substring = Substring.substring

(* I/O *)
val print = PrintHook.print

(* simple interface to compiler *)
val use = UseHook.use

(* getting info about exceptions *)
val exnName = ExnInfoHook.exnName
val exnMessage = ExnInfoHook.exnMessage

end (* local *)

(* Bind structure _Core.  We use the symbol "xCore", but after parsing
 * is done this will be re-written to "_Core" by the bootstrap compilation
 * machinery.  See file init.cmi for more details. *)
structure xCore = Core
