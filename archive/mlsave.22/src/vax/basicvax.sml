structure BasicVax : BASICVAX =
struct

structure Jumps = struct
    datatype JumpKind = MODE | WHICH of (int ref * int * int)
		        | BYTEDISPL
			| LABPTR of int
			| COND of (int ref * int * int) | JBR
 end
structure Emitter : BACKPATCH = Backpatch(Jumps)

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
	        | (LABPTR _, _) => 4
		| (BYTEDISPL, _) => 1
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
  | emitjump(BYTEDISPL,1,s,d) =
	(signedbyte(d-s-1))
  | emitjump(LABPTR i, _,s,d) = emitlong(d-s+i)
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
	 emitbyte (14*16+15);
	 emitlong (d-s-6))
  | emitjump _ = ErrorMsg.impossible "emitjump in vax/vaxmcode.sml"
(*
val emitjump = fn x => emitjump x handle Range =>
  (case x of (m,a,b,c) =>
	(print(case m of
		JBR => "JBR" | BYTEDISPL => "BYTEDISPL" 
		| MODE => "MODE" | COND _ => "COND" | WHICH _ => "WHICH" 
		| LABPTR _ => "LABPTR");
	print " "; print a; print " "; print b; print " "; print c; print "\n";
	()))
*)
end (* struct BasicVax *)
