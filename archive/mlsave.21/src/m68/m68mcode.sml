signature BASICM68 = sig

    datatype JumpKind = Jcc of int | LEA of int | FJcc of int | LABPTR of int
    datatype Size = Byte | Word | Long
    val sizeint : int -> Size
    val sizejump : JumpKind * int * int * int -> int
    val emitword : int -> unit
    val emitlong : int -> unit
    val signedbyte : int -> int
    val emitjump : JumpKind * int * int * int -> unit
    (* from Backpatch *)
    type Label
    val newlabel : unit -> Label
    val emitbyte : int -> unit
    val align : unit -> unit
    val define : Label -> unit
    val jump : JumpKind*Label -> unit
    val mark : unit -> unit
    val finish : ( (JumpKind*int*int*int->int)
	          *(JumpKind*int*int*int->unit)
		  *(int->unit) )
		 -> (int * ((int->unit)->unit))

end (* signature BASICM68 *)

structure BasicM68 : BASICM68 = struct

structure Jumps = struct
    datatype JumpKind = Jcc of int | LEA of int | FJcc of int | LABPTR of int
end (* structure Jumps *)

structure Emitter : BACKPATCH = Backpatch(Jumps)

open Emitter
open Jumps

datatype Size = Byte | Word | Long

fun sizeint i =
	if i < 128 andalso i > ~129 then Byte
	else if i < 32768 andalso i > ~32769 then Word
	else Long

exception TooBig (* pc relative addressing only has 16-bit displacement *)

fun sizejump (LEA _, _,s,d) = (* pc relative addressing *)
	(case sizeint (d - s - 2) of
	      Byte => 4
	    | Word => 4
	    | Long => 8)
  | sizejump (LABPTR _, _, _, _) = 4
  | sizejump (Jcc _, _, s, d) =
	(case sizeint (d - s - 2) of
	      Byte => 2
	    | Word => 4
	    | Long => 6)
  | sizejump (FJcc _, _, s, d) =
	(case sizeint (d - s - 2) of
	      Byte => 4
	    | Word => 4
	    | Long => 6)

fun emitword i =
	if i < 0 then emitword(65536 + i)
	else (emitbyte(i div 256); emitbyte(i mod 256))

fun emitlong i =
	if i < 0 then
	    let val a = ~i
		val b = a mod 65536
		val c = a div 65536
	    in  emitword(~c + (if b = 0 then 0 else ~1));
		emitword(~b)
	    end
	else (emitword(i div 65536); emitword(i mod 65536))

fun signedbyte i = if i < 0 then signedbyte (256 + i) else i

fun emitjump (Jcc(opcode),2,s,d) =
	(case (d-s-2) of
	       0 => emitword(20081) (* nop *)
	     | _ => emitword(opcode + signedbyte (d - s - 2)))
  | emitjump (Jcc(opcode),4,s,d) =
	(emitword(opcode);
	 emitword(d-s-2))
  | emitjump(Jcc(opcode),6,s,d) =
	(emitword(opcode+255);
	 emitlong(d-s-2))
  | emitjump(LABPTR i, _,s,d) = emitlong(d-s+i)
  | emitjump (LEA(opcode),4,s,d) = (* pc relative *)
	(emitword(opcode+58); emitword (d-s-2))
  | emitjump (LEA(opcode),8,s,d) = (* pc relative *)
	(emitword(opcode+59); emitword 368; emitlong (d-s-2))
  | emitjump (FJcc(cond),4,s,d) =
	(emitword(62080+cond); emitword(d-s-2))
  | emitjump (FJcc(cond),6,s,d) =
	(emitword(62144+cond); emitlong(d-s-2))

end (* structure BasicM68*)



structure M68MCode : M68CODER = struct

structure B : BASICM68 = BasicM68
open B

val offset = ref 0

datatype Register = DataReg of int
		  | AddrReg of int
		  | FloatReg of int
		  | PC

datatype EA = Direct of Register
	    | PostInc of Register
	    | PreDec of Register
	    | Displace of Register * int
	    | Index of Register * int * Register * Size
	    | Immedlab of Label
	    | Immed of int
	    | Abs of int
	    | Address of Label

val d0 = DataReg 0
and d1 = DataReg 1
and d2 = DataReg 2
and d3 = DataReg 3
and d4 = DataReg 4
and d5 = DataReg 5
and d6 = DataReg 6
and d7 = DataReg 7
and a0 = AddrReg 0
and a1 = AddrReg 1
and a2 = AddrReg 2
and a3 = AddrReg 3
and a4 = AddrReg 4
and a5 = AddrReg 5
and a6 = AddrReg 6
and sp = AddrReg 7
and fp0 = FloatReg 0
and fp1 = FloatReg 1
and fp2 = FloatReg 2
and fp3 = FloatReg 3
and fp4 = FloatReg 4
and fp5 = FloatReg 5
and fp6 = FloatReg 6
and fp7 = FloatReg 7

exception BadLabelUse

fun reg (Direct(DataReg d)) = d
  | reg (Direct(AddrReg a)) = a
  | reg (PostInc(AddrReg a)) = a
  | reg (PreDec(AddrReg a)) = a
  | reg (Displace(AddrReg a,_)) = a
  | reg (Index(PC,_,_,_)) = 3
  | reg (Index(AddrReg a,_,_,_)) = a
  | reg (Abs i) = (case sizeint i of
			  Byte => 0
			| Word => 0
			| Long => 1)
  | reg (Immed _) = 4
  | reg (Address _) = raise BadLabelUse


fun mode (Direct(DataReg _)) = 0
  | mode (Direct(AddrReg _)) = 1
  | mode (Displace(AddrReg _, 0)) = 2
  | mode (PostInc(AddrReg _)) = 3
  | mode (PreDec(AddrReg _)) = 4
  | mode (Displace(AddrReg _,_)) = 5
  | mode (Index(PC,_,_,_)) = 7
  | mode (Index(AddrReg _,_,_,_)) = 6
  | mode (Abs _) = 7
  | mode (Immed _) = 7
  | mode (Address _) = raise BadLabelUse

fun emitstring ( s  : string  (* bug : overloading of length doesn't work
					without this *) ) = 
	let val len = length s
	    fun emits i = if i< len then (emitbyte(ordof(s,i));
					  emits(i+1))
				    else ()
	in  emits 0
	end

(* Very similar to Vax G_float format, except that the byte order within words
   is different and the bias is different by 2. *)
structure M68PrimReal : PRIMREAL =
struct
open BitOps
val significant = 53 (* 52 + redundant 1 bit *)
fun outofrange s = ErrorMsg.complain("Real constant "^s^" out of range")
(* Convert a portion of a boolean array to the appropriate integer. *)
exception Bits
fun bits(a,start,width) =
    let fun b true = 1
	  | b false = 0
	fun f 0 = b (a sub start)
	  | f n = b (a sub (start+n)) + 2 * f(n-1)
    in  if length a < start+width orelse start < 0 orelse width < 0
	then raise Bits
	else f (width-1)
    end
fun emitreal (sign,frac,exp) =
    let val exponent = exp + 1022
	fun emit () =
	    let val word0 = case frac sub 0 of (* zero? *)
				true => sign<<15 bit_or exponent<<4 bit_or
					     bits(frac,1,4)
			      | false => 0
		val word1 = bits(frac,5,16)
		val word2 = bits(frac,21,16)
		val word3 = bits(frac,37,16)
	    in  B.emitword word0;
		B.emitword word1;
		B.emitword word2;
		B.emitword word3
	    end
    in  if exponent < 1 orelse exponent > 2047
	then outofrange "" (* A hack *)
	else emit()
    end
end
structure M68RealConst = RealConst(M68PrimReal)
open M68RealConst

fun emitext (Immed i) = emitlong i
  | emitext (Abs i) =
	(case sizeint i of
	      Byte => emitword i
	    | Word => emitword i
	    | Long => emitlong i)
  | emitext (Direct _) = ()
  | emitext (Displace(_,0)) = ()
  | emitext (PostInc _) = ()
  | emitext (PreDec _) = ()
  | emitext (Displace (_,i)) = emitword i
  | emitext (Index (_,disp,DataReg d,Word)) =
	emitword(d * 4096 + signedbyte disp)
  | emitext (Index (_,disp,DataReg d,Long)) =
	emitword(d * 4096 + 2048 + signedbyte disp)
  | emitext (Index (_,disp,AddrReg d,Word)) =
	emitword(32768 + d * 4096 + signedbyte disp)
  | emitext (Index (_,disp,AddrReg d,Long)) =
	emitword(32768 + d * 4096 + 2048 + signedbyte disp)
  | emitext (Address _) = raise BadLabelUse

fun emitF (opcode,Direct(FloatReg a),Direct(FloatReg b)) =
	(emitword(61952); emitword(opcode+128*b+1024*a))
  | emitF (opcode,src,Direct(FloatReg f)) =
	(emitword(61952 + 8 * mode src + reg src);
	 emitword(opcode+21504+128*f);
	 emitext src)
  | emitF (opcode,Direct(FloatReg f),dest) =
	(emitword(61952 + 8 * mode dest + reg dest);
	 emitword(opcode+21504+128*f);
	 emitext dest)

exception Illegal

fun rts () = emitword (20085)

fun exg (Direct(AddrReg a),Direct(AddrReg b)) =
	if a = b then ()
	else emitword(49480 + 512 * a + b)
  | exg (Direct(DataReg a),Direct(DataReg b)) =
	if a = b then ()
	else emitword(49472 + 512 * a + b)
  | exg (Direct(AddrReg b),Direct(DataReg a)) =
	emitword(49544 + 512 * a + b)
  | exg (Direct(DataReg a),Direct(AddrReg b)) =
	emitword(49544 + 512 * a + b)

fun pea (Direct _) = raise Illegal
  | pea (PreDec _) = raise Illegal
  | pea (PostInc _) = raise Illegal
  | pea (Immed _) = raise Illegal
  | pea (Address lab) = (jump(LEA(18496),lab))
  | pea (src) =
	(emitword(18496 + 8 * mode src + reg src);
	 emitext src)


fun lea (Direct _,_) = raise Illegal
  | lea (PreDec _,_) = raise Illegal
  | lea (PostInc _,_) = raise Illegal
  | lea (Immed _,_) = raise Illegal
  | lea (Address lab,Direct(AddrReg a)) =
	jump(LEA(16832+512*a),lab)
  | lea (src,Direct(AddrReg a)) =
	(emitword(16832 + 512 * a + 8 * mode src + reg src);
	 emitext src)

fun movl (_,Immed _) = raise Illegal
  (* labels not implemented *)
  | movl (Address _,_) = raise Illegal
  | movl (_,Address _) = raise Illegal
  (* CLR *)
  | movl (Immed 0, dest) = (emitword(17024 + reg dest + 8 * mode dest);
			    emitext dest)
  (* MOVEQ/MOVE *)
  | movl (src as (Immed i),dest as (Direct(DataReg d))) =
	(case sizeint i of
	    Byte => emitword(28672 + 512 * d + signedbyte i)
	  | _ =>
		(emitword(8192 + 512 * reg dest + 64 * mode dest +
			8 * mode src + reg src);
		 emitext src))
  (* MOVEA *)
  | movl (src,Direct(AddrReg a)) =
	(emitword(8256 + a * 512 + mode src * 8 + reg src);
	 emitext src)
  (* general MOVE *)
  | movl (src,dest) =
	(emitword(8192 + 512 * reg dest + 64 * mode dest +
		8 * mode src + reg src);
	 emitext src;
	 emitext dest)

fun addl (_,Immed _) = raise Illegal
  (* ADDQ/ADDA *)
  | addl (src as (Immed i),dest as (Direct(AddrReg a))) =
	if i <= 8 andalso i >= 1 then
   	    emitword(20608 + 512 * (i mod 8) + 8 * mode dest + reg dest)
	else (emitword(53696 + 512 * a + 8 * mode src + reg src);
	      emitext src)
  | addl (src,Direct(AddrReg a)) =
	(emitword(53696 + 512 * a + 8 * mode src + reg src);
	 emitext src)
  (* ADDQ/ADDI *)
  | addl (src as (Immed i),dest) =
	if i <= 8 andalso i >= 1 then
   	    (emitword(20608 + 512 * (i mod 8) + 8 * mode dest + reg dest);
	     emitext dest)
	else (emitword(1664 + 8 * mode dest + reg dest);
	      emitext src;
	      emitext dest)
  (* general ADD *)
  | addl (src,Direct(DataReg d)) = 
	(emitword(53376 + 512 * d + reg src + 8 * mode src);
	 emitext src)
  | addl (Direct(DataReg d),dest) =
	(emitword(53632 + 512 * d  + reg dest + 8 * mode dest);
	 emitext dest)

fun subl (_,Immed _) = raise Illegal
  (* SUBQ/SUBA *)
  | subl (src  as (Immed i),dest as (Direct(AddrReg a))) =
	if i <= 8 andalso i >= 1 then
		emitword(20864 + 512 * (i mod 8) + 8 * mode dest + reg dest)
	else (emitword(37312 + 512 * a + 8 * mode src + reg src);
	      emitext src)
  (* SUBA *)
  | subl (src,Direct(AddrReg a)) =
	(emitword(37312 + 512 * a + 8 * mode src + reg src);
	 emitext src)
  (* SUBQ/SUBI *)
  | subl (src as (Immed i),dest) =
	if i <= 8 andalso i >= 1 then
		(emitword(20864 + 512 * (i mod 8) + 8 * mode dest + reg dest);
		 emitext dest)
	else	(emitword(1152 + reg dest + 8 * mode dest);
		 emitext src;
		 emitext dest)
  (* general SUB *)
  | subl (src,Direct(DataReg d)) = 
	(emitword(36992 + 512 * d + reg src + 8 * mode src);
	 emitext src)
  | subl (Direct(DataReg d),dest) = 
	(emitword(37248 + 512 * d + reg dest + 8 * mode dest);
	 emitext dest)

fun orl (Immed i, Direct(AddrReg _)) = raise Illegal
  | orl (Immed i, dest) =
	if i<65536 then (emitword(64 + reg dest + 8*mode dest); emitword i)
		   else raise Match
  | orl _ = raise Illegal

fun mull (src,Direct(DataReg d)) =
	(emitword(19456 + reg src + 8 * mode src);
	 emitword(2048+4096*d);
	 emitext src)

fun divl (src,Direct(DataReg d)) =
	(emitword(19520 + reg src + 8 * mode src);
         emitword(2048 + 4097 (*yes, 4097!*) * d);
	 emitext src)

fun movb (_,Immed _) = raise Illegal
  (* labels not implemented *)
  | movb (Address _,_) = raise Illegal
  | movb (_,Address _) = raise Illegal
  (* CLR *)
  | movb (Immed 0, dest) = (emitword(16896 + reg dest + 8 * mode dest);
			    emitext dest)
  (* general MOVE *)
  | movb (src,dest) =
	(emitword(4096 + 512 * reg dest + 64 * mode dest +
		8 * mode src + reg src);
	 emitext src;
	 emitext dest)

fun shift dir =
 let val dr = dir * 256
     fun f (Immed 0, _) = ()
       | f (Immed 8, Direct(DataReg d)) = emitword(57472+dr+d)
       | f (Immed i, Direct(DataReg d)) =
	 if  i<1 orelse i>7 then raise Illegal
	   else emitword(57472+dr + 512*i + d)
       | f (Immed 1, Direct _) = raise Illegal
       | f (Immed 1, dest) =
           (emitword(57536 + dr + 8*mode dest + reg dest); emitext dest)
       | f (Direct(DataReg s), Direct(DataReg d)) =
         (emitword(57504+dr + 512*s + d))
       | f _ = raise Illegal
  in f
 end

val asll = shift 1
val asrl = shift 0

fun cmpl (_,Immed _) = raise Illegal
  (* CMP *)
  | cmpl (src,Direct(DataReg d)) =
	(emitword(45184 + 512 * d + 8 * mode src + reg src);
	 emitext src)
  (* CMPA *)
  | cmpl (src,Direct(AddrReg a)) =
	(emitword(45504 + 512 * a + 8 * mode src + reg src);
	 emitext src)
  (* CMPI *)
  | cmpl (Immed i,dest) =
	(emitword(3200 + 8 * mode dest + reg dest);
	 emitext dest)
  (* CMPM *)
  | cmpl (PostInc(AddrReg y),PostInc(AddrReg x)) = emitword(45448 + 512 * x + y)

fun btst (_,Direct(AddrReg _)) = raise Illegal
  | btst (_,Immed _) = raise Illegal
  | btst (Direct(DataReg d),dest) =
	(emitword(256 + 512 * d + 8 * mode dest + reg dest);
	 emitext dest)
  | btst (Immed i,dest) =
	(emitword(2048 + 8 * mode dest + reg dest);
	 emitword i;
	 emitext dest)


fun emitlab (i,lab) = jump(LABPTR i, lab)

fun jne (Address lab) = jump (Jcc(26112),lab)
fun jeq (Address lab) = jump (Jcc(26368),lab)
fun jgt (Address lab) = jump (Jcc(28160),lab)
fun jge (Address lab) = jump (Jcc(27648),lab)
fun jlt (Address lab) = jump (Jcc(27904),lab)
fun jle (Address lab) = jump (Jcc(28416),lab)

fun jra (Address lab) = jump (Jcc(24576),lab)
  | jra (arg as (Displace(AddrReg _,_))) = 
	    (emitword (20160 + 8 * mode arg + reg arg); emitext arg)

fun jbsr (Address lab) = jump (Jcc(24832),lab)
  | jbsr (dest as (Displace(AddrReg _,_))) =
	(emitword(20096 + 8 * mode dest + reg dest);
	 emitext dest)
    	    (* doesn't handle >16bit displacement *)


(* 68881 float operations *)
(* Some src/dest combinations are illegal, but not caught here. *)
fun fjne (Address lab) = jump (FJcc(14),lab)
fun fjeq (Address lab) = jump (FJcc(1),lab)
fun fjgt (Address lab) = jump (FJcc(18),lab)
fun fjge (Address lab) = jump (FJcc(19),lab)
fun fjlt (Address lab) = jump (FJcc(20),lab)
fun fjle (Address lab) = jump (FJcc(21),lab)

fun fcmpd (src,dest) = emitF(56,src,dest)
fun faddd (src,dest) = emitF(34,src,dest)
fun fsubd (src,dest) = emitF(40,src,dest)
fun fmuld (src,dest) = emitF(35,src,dest)
fun fdivd (src,dest) = emitF(32,src,dest)
fun fnegd (src,dest) = emitF(26,src,dest)
fun fmoved (src,dest as Direct(FloatReg f)) = emitF(0,src,dest)
  | fmoved (src as Direct(FloatReg f),dest) = emitF(8192,src,dest)

fun trapv() = emitword(20086)

fun push ea = movl(ea,PreDec sp)

fun pop ea = movl(PostInc sp,ea)

val pusha = pea

fun comment _ = ()

end (* structure MCode *)
