signature BASICM68 = sig

    datatype JumpKind = Jcc of int | LEA of int | FJcc of int | LABPTR of int
			| MODE
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

   exception Illegal

end (* signature BASICM68 *)

structure BasicM68 : BASICM68 = struct

(* DEBUG
fun diag (s : string) f x =
	f x handle e =>
		(print "?exception "; print (System.exn_name e);
		 print " in basicm68."; print s; print "\n";
		 raise e)
*)

structure Jumps = struct
    datatype JumpKind = Jcc of int | LEA of int | FJcc of int | LABPTR of int
			| MODE
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
  | sizejump (MODE,_,_,_) = 2

(* DEBUG val sizejump = diag "sizejump" sizejump *)

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

exception Illegal

fun emitjump (Jcc(opcode),2,s,d) =
	(case (d-s-2) of
	       0 => emitword(20081) (* nop *)
	     | _ => emitword(opcode + signedbyte (d - s - 2)))
  | emitjump (Jcc(opcode),4,s,d) =
	(emitword(opcode); emitword(d-s-2))
  | emitjump(Jcc(opcode),6,s,d) =
	(emitword(opcode+255); emitlong(d-s-2))
  | emitjump(LABPTR i, _,s,d) = emitlong(d-s+i)
  | emitjump (LEA(opcode),4,s,d) = (* pc relative *)
	(emitword(opcode+58); emitword (d-s-2))
  | emitjump (LEA(opcode),8,s,d) = (* pc relative *)
	(emitword(opcode+59); emitword 368; emitlong (d-s-2))
  | emitjump (FJcc(cond),4,s,d) =
	(emitword(62080+cond); emitword(d-s-2))
  | emitjump (FJcc(cond),6,s,d) =
	(emitword(62144+cond); emitlong(d-s-2))
  | emitjump (MODE,2,s,d) =
    let val x = Integer.-(d,s)
    in if x < 32768 andalso ~32768 <= x
       then emitword(d-s) 
       else raise Illegal
    end

(* DEBUG val emitjump = diag "emitjump" emitjump *)

end (* structure BasicM68*)
