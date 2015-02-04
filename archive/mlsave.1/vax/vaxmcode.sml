signature BASICVAX = sig

    datatype JumpKind = MODE 
		      | WHICH of (int ref * int * int)
		      | COND of (int ref * int * int)
		      | JBR
    val intsize : int -> int
    val emitword : int -> unit
    val emitlong : int -> unit
    val signedbyte : int -> unit
    val sizejump : JumpKind * int * int * int -> int
    val emitjump : JumpKind * int * int * int -> unit
    (* from backpatch *)
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

end (* signature BASICVAX *)


structure BasicVax : BASICVAX =
struct

structure Jumps = struct
    datatype JumpKind = MODE | WHICH of (int ref * int * int)
			| COND of (int ref * int * int) | JBR
 end
structure Emitter : backpatch = backpatch(Jumps)

open Emitter
open Jumps

fun signedbyte i = emitbyte(if i<0 then 256+i else i)
fun emitword i =
	if i<0 then emitword(65536+i)
	else (emitbyte(i mod 256); emitbyte(i div 256));
fun emitlong i =
        if i<0 
	  then let val a = ~i;
		   val b = a mod 65536;
		   val c = a div 65536;
		in emitword(~b);
		   emitword(~c + if b=0 then 0 else ~1)
	       end
	  else (emitword(i mod 65536); emitword(i div 65536))
fun intsize(i) =
        if i >= ~128 andalso i < 128
	    then 1
	else if i >= ~32768 andalso i < 32768
	    then 2
	else 4;

fun sizejump(mode,oldsize,s,d) =
   let fun which (r,a,b) =
            case oldsize of 1 => r := a | _ => r := b
    in case (mode,intsize(d-(s+oldsize)))
	       of  (MODE,i) => i+1
		| (WHICH _, _) => 1
		| (COND x, 1) => (which x; 1)
		| (COND x, 2) => (which x; 4)
		| (COND x, _) => (which x; 7)
		| (JBR,1) => 2
		| (JBR,2) => 3
		| (JBR,_) => 6
   end

fun emitjump(MODE,2,s,d) =
	(emitbyte (10*16+15);
	 signedbyte (d-s-2))
  | emitjump(MODE,3,s,d) =
	(emitbyte (12*16+15);
	 emitword (d-s-3))
  | emitjump(MODE,5,s,d) =
	(emitbyte (14*16+15);
	 emitlong (d-s-5))
  | emitjump(WHICH(ref i,_,_), _,_,_) = emitbyte i
  | emitjump(COND _, 1,s,d) = signedbyte(d-s-1)
  | emitjump(COND _, 4,s,d) = (signedbyte 3; emitbyte(3*16+1); emitword(d-s-4))
  | emitjump(COND _, 7,s,d) = (signedbyte 6;
			    emitbyte (16+7);
			    emitbyte (15*16+15);
		            emitlong (d-s-7))
  | emitjump(JBR,2,s,d) = (*brb(displace(d-s-2,pc))*)
	(emitbyte (16+1);
	 signedbyte (d-s-2))
  | emitjump(JBR,3,s,d) = (*brw(displace(d-s-3,pc))*)
	(emitbyte (3*16+1);
	 emitword (d-s-3))
  | emitjump(JBR,6,s,d) = (*jmp(displace(d-s-6),pc)*)
	(emitbyte (16+7);
	 emitbyte (15*16+15);
	 emitlong (d-s-6))

end (* struct BasicVax *)


structure VaxMCode : VAXCODER = struct

structure B : BASICVAX = BasicVax
open B

val offset = ref 0

datatype Register = reg of int

val r0 = reg 0
val r1 = reg 1
val r2 = reg 2
val r3 = reg 3
val r4 = reg 4
val r5 = reg 5
val r6 = reg 6
val r7 = reg 7
val r8 = reg 8
val r9 = reg 9
val r10 = reg 10
val r11 = reg 11
val r12 = reg 12
val r13 = reg 13
val sp = reg 14
val pc = reg 15

datatype EA = direct of Register
	    | autoinc of Register
	    | autodec of Register
	    | displace of int * Register
	    | deferred of int * Register
	    | immed of int
	    | address of Label
	    | index of EA * Register

fun setmark _ = mark ()

fun emitstring s =
	let val len = String.length s   (* bug .. shouldn't need 'String.' *)
	    fun emits i = if i < len then (emitbyte(ordof(s,i)); emits (i+1))
			  else ()
	in emitlong(len);
	   emits 0
	end

(* This is identical to M68PrimReal except that emitword is different,
   and the bias is off by two. *)
structure VaxPrimReal : PRIMREAL =
struct
open BitOps
val significant = 53 (* 52 + redundant 1/2 bit *)
fun outofrange () = ErrorMsg.Complain "Real constant out of range"
(* Convert a portion of a boolean array to the appropriate integer. *)
exceptionx bits
fun bits(a,start,width) =
    let fun B true = 1
	  | B false = 0
	fun F 0 = B (a sub start)
	  | F n = B (a sub (start+n)) + 2 * F(n-1)
    in
	if length a < start+width orelse start < 0 orelse width < 0
	then raisex bits
	else F (width-1)
    end
fun emitreal (sign,frac,exp) =
    let val exponent = exp + 1024
	fun emit () =
	    let val word0 = case frac sub 0 of (* zero? *)
				true => sign<<15 OR exponent<<4 OR
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
    in
	if exponent < 1 orelse exponent > 2047
	then outofrange()
	else emit()
    end
end
structure VaxRealConst = RealConst(VaxPrimReal)
open VaxRealConst

fun regmode(mode,r) = emitbyte(mode*16+r)

fun emitarg (direct(reg r)) = regmode(5,r)
  | emitarg (autoinc(reg r)) = regmode(8,r)
  | emitarg (autodec(reg r)) = regmode(7,r)
  | emitarg (immed i) = 
	if i>=0 andalso i<64 then emitbyte i
	    else (emitarg(autoinc pc); emitlong i)
  | emitarg (displace(i,reg r)) =
	 if i=0 then regmode(6,r)
	 else (case intsize i 
		 of  1 => (regmode(10,r); signedbyte i)
		   | 2 => (regmode(12,r); emitword i)
		   | 4 => (regmode(14,r); emitlong i))
  | emitarg (deferred(i,reg r)) =
	(case intsize i of
	     1 => (regmode(11,r); signedbyte i)
	   | 2 => (regmode(13,r); emitword i)
	   | 4 => (regmode(15,r); emitlong i))
  | emitarg (index(ea, reg r)) = (regmode(4,r); emitarg ea)

fun emit2arg (arg1,arg2) = (emitarg arg1; emitarg arg2)

fun emit3arg (arg1,arg2,arg3) = (emitarg arg1; emitarg arg2; emitarg arg3)

fun immedbyte(i) =
	if i>=0 andalso i<64 then emitbyte i
	    else (emitarg(autoinc pc); signedbyte i);

fun immedword(i) =
	if i>=0 andalso i<64 then emitbyte i
	    else (emitarg(autoinc pc); emitword i);

fun jne (address L) = let val r = (ref 0,16+2,16+3)
		       in jump(WHICH r, L); jump(COND r, L)
		      end
fun jbr (address L) = jump(JBR,L)
fun bbc (immed 0, arg, address L) =
	    let val r = (ref 0, 14*16+9,14*16+8)
	     in jump(WHICH r, L); emitarg arg; jump(COND r, L)
	    end
  | bbc (arg1, arg2, address L) =
	    let val r = (ref 0, 14*16+1,14*16+0)
	     in jump(WHICH r, L); emitarg arg1; emitarg arg2; jump(COND r, L)
	    end
fun bbs (immed 0, arg, address L) =
	    let val r = (ref 0, 14*16+8,14*16+9)
	     in jump(WHICH r, L); emitarg arg; jump(COND r, L)
	    end
  | bbs (arg1, arg2, address L) =
	    let val r = (ref 0, 14*16+0,14*16+1)
	     in jump(WHICH r, L); emitarg arg1; emitarg arg2; jump(COND r, L)
	    end

fun movb (immed i, arg2) = (emitbyte(9*16); immedbyte i; emitarg arg2)
  | movb args = (emitbyte (9*16); emit2arg args)

fun movzbl args = (emitbyte (9*16+10); emit2arg args)

fun movl (arg, autodec(reg 14)) = (emitbyte(13*16+13); emitarg arg)
  | movl (immed 0, arg) = (emitbyte(13*16+4); emitarg arg)
  | movl args = (emitbyte (13*16); emit2arg args)

fun pushal (address L) = (emitbyte (13*16+15);
			      jump(MODE,L))
  | pushal args = (emitbyte (13*16+15); emitarg args)

fun moval (arg, autodec sp) = pushal arg
  | moval (address L,arg) = (emitbyte (13*16+14);
			     jump(MODE,L);
			     emitarg arg)
  | moval args = (emitbyte (13*16+14); emit2arg args)

fun rsb () = emitbyte 5
fun cmpl args = (emitbyte (13*16+1); emit2arg args)
fun addl2 (immed 1, arg) = (emitbyte(13*16+6); emitarg arg)
  | addl2 args = (emitbyte (12*16); emit2arg args)
fun addl3 args = (emitbyte (12*16+1); emit3arg args)
fun subl2 args = (emitbyte (12*16+2); emit2arg args)
fun subl3 args = (emitbyte (12*16+3); emit3arg args)
fun ashl (immed i,arg2,arg3) =
	(emitbyte (7*16+8);
	 immedbyte i;
	 emitarg arg2;
	 emitarg arg3)
fun mull2 args = (emitbyte (12*16+4); emit2arg args)
fun divl3 args = (emitbyte (12*16+7); emit3arg args)
fun divl2 args = (emitbyte (12*16+6); emit2arg args)
fun jmp arg = (emitbyte (16+7); emitarg arg)
fun brb (displace(i,reg 15)) = (emitbyte (16+1); signedbyte i)
fun brw (displace(i,reg 15)) = (emitbyte (3*16+1); emitword i)
fun beql (displace(i,reg 15)) = (emitbyte (16+3); signedbyte i)
fun bneq (displace(i,reg 15)) = (emitbyte (16+2); signedbyte i)
fun bgeq (displace(i,reg 15)) = (emitbyte (16+8); signedbyte i)
fun bgtr (displace(i,reg 15)) = (emitbyte (16+4); signedbyte i)
fun blss (displace(i,reg 15)) = (emitbyte (16+9); signedbyte i)
fun bleq (displace(i,reg 15)) = (emitbyte (16+5); signedbyte i)

fun movg args = (emitword(20733); emit2arg args)
fun mnegg args = (emitword(21245); emit2arg args)
fun addg3 args = (emitword(16893); emit3arg args)
fun subg3 args = (emitword(17405); emit3arg args)
fun mulg3 args = (emitword(17917); emit3arg args)
fun divg3 args = (emitword(18429); emit3arg args)
fun cmpg args = (emitword(20989); emit2arg args)

fun push arg = movl(arg,autodec sp)
fun pusha arg = pushal arg
fun pop arg = movl(autoinc sp,arg)

end (* structure MCode *)
