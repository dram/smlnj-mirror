(* Copyright 1989 by      Department of Computer Science, 
 *                        The Technical University of Denmak
 *                        DK-2800 Lyngby 
 *
 * 17 Dec. 1991    Yngvi Skaalum Guttesen       (ysg@id.dth.dk)
 *)

structure Jumps386 : JUMPS386 = struct

datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int

datatype Size = Byte | Word | Long

fun sizeint i =
    if i < 128 andalso i > ~129 then Byte
    else if i < 32768 andalso i > ~32769 then Word
    else Long

fun sizejump (LEA _, _, _, _) = 12
  | sizejump (LABPTR _, _, _, _) = 4
  | sizejump (Jcc _, _, s, d)    =
        if sizeint(d-s-2)=Byte then 2 else 6
  | sizejump (JMP, _, s, d)    =
        if sizeint(d-s-2)=Byte then 2 else 5

fun signedbyte i = if i<0 then signedbyte (256+i) else i

exception Ebyte
fun ebyte i = if i>255 then raise Ebyte else chr(signedbyte i)

fun eword i =
    if i<0 then eword(65536+i)
    else ebyte(i mod 256) ^ ebyte(i div 256)

fun elong i =
    if i<0 then
	let val a = ~i
	    val b = a mod 65536
	    val c = a div 65536
	in  eword(~b) ^ eword(~c + (if b=0 then 0 else ~1))
	end
    else eword(i mod 65536) ^ eword(i div 65536)

val emitlong = elong

exception Jump

fun emitjump (Jcc(cond), 2, s, d) =
	ebyte(112 + cond) ^ ebyte(d-s-2)
  | emitjump (Jcc(cond), 6, s, d) =
	ebyte(15) ^ ebyte(128 + cond) ^ elong(d-s-6)
  | emitjump (JMP, 2, s, d) = ebyte(235) ^ ebyte(d-s-2)
  | emitjump (JMP, 5, s, d) = ebyte(233) ^ elong(d-s-5)
  | emitjump (LABPTR i, _, s, d) = elong(d-s+i)
  | emitjump (LEA(r), _, s, d) =
	ebyte(232) ^ elong(0) ^				(* call relative 0   *)
	ebyte(88 + r) ^ 				(* pop r             *)
	ebyte(129) ^ ebyte(192 + r) ^ elong(d-s-5) 	(* add r,(d-s-5)     *)
  | emitjump _ = raise Jump

end (* structure Jumps386 *)





























