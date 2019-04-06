(* Copyright 1989 by      Department of Computer Science, 
 *                        The Technical University of Denmak
 *                        DK-2800 Lyngby 
 *
 * 17 Dec. 1991    Yngvi Skaalum Guttesen       (ysg@id.dth.dk)
 *)

functor MCode386 (Jumps : JUMPS386) : CODER386 = struct

structure Emitter : BACKPATCH = Backptch(Jumps)

open Emitter Jumps

val emitbyte = fn i => emitstring(ebyte i)
val emitlong = fn i => emitstring(elong i)
fun realconst s = emitstring(implode(rev(explode(IEEEReal.realconst s))))

datatype EA = Direct of int
	    | Displace of int * int
	    | Index of int * int * int * Size
	    | Immed of int
	    | Immedlab of Label

(*************************** The 80386 registers ******************************)

val eax = 0
and ebx = 3
and ecx = 1
and edx = 2
and esi = 6
and edi = 7
and ebp = 5
and esp = 4

(*********************** Emit then addr. and data extension *******************)

(* Emit the Scaled/Index/Base byte *)
exception Emitsib
fun emitsib(Index(base, _, indx, size)) =
        let val ss = if indx=4 then 0
                     else (case size of Byte => 0 | Word => 1 | Long => 2)
	in  ebyte(ss*64 + indx*8 + base) end
  | emitsib _ = raise Emitsib

(* Emit the mod-reg-r/m byte and addr. and data 
 * extension for binary operations 
 *)
exception Emitext
fun emitext(Direct s, Direct d) = ebyte(3*64 + 8*d + s)
  | emitext(Displace(s, 0), b as Direct d) =
	if s=esp
	then emitext(Index(s,0,4,Byte), b)
	else	if s=ebp 
		then (ebyte(1*64 + d*8 + ebp) ^ ebyte(0))
		else  ebyte(d*8 + s)
  | emitext(Displace(s,i), b as Direct d) =
	if s=esp
	then emitext(Index(s,i,4,Byte), b)
	else	if sizeint(i)=Byte
		then (ebyte(1*64 + d*8 + s) ^ ebyte(i))
		else (ebyte(2*64 + d*8 + s) ^ elong(i))
  | emitext(src as Index(s, 0,_,_), Direct d) =
	if s=ebp
	then (ebyte(1*64 + 8*d + 4) ^ emitsib(src) ^ ebyte(0))
	else (ebyte(8*d + 4) ^ emitsib(src))
  | emitext(src as Index(_,i,_,_), Direct d) =
	if sizeint(i)=Byte
	then (ebyte(1*64 + d*8 + 4) ^ emitsib(src) ^ ebyte(i))
	else (ebyte(2*64 + d*8 + 4) ^ emitsib(src) ^ elong(i))
  | emitext(a as Direct _, b as Displace _) = emitext(b,a)
  | emitext(a as Direct _, b as Index _) = emitext(b,a)
  | emitext _ = raise Emitext

fun emitimm i = if sizeint(i)=Byte then ebyte(i) else elong(i)

(* Emit the mod-reg-r/m byte and addr. and data  extension for 
 * immediate operations. This is also used in unary operations
 *)
exception EmitImmext
fun emitImmext(opcode, Direct r) = ebyte(3*64 + opcode*8 +r)
  | emitImmext(opcode, Displace(r, 0)) =
	if r=esp 
	then emitImmext(opcode, Index(r,0,4,Byte))
	else	if r=ebp
		then (ebyte(1*64 + opcode*8 + 5) ^ ebyte(0))
		else ebyte(opcode*8 + r)
  | emitImmext(opcode, Displace(r, j)) =
	if r=esp
	then emitImmext(opcode, Index(r,j,4,Byte))
	else	let val mode = (if (sizeint(j) = Byte) then 1 else 2)
		in
		    (ebyte(mode*64 + opcode*8 + r) ^ emitimm(j))
		end
  | emitImmext(opcode, dest as Index(r, 0, _, _)) =
	if r=ebp
	then (ebyte(1*64 + opcode*8 + 4) ^ emitsib(dest) ^ ebyte(0))
	else (ebyte(opcode*8 + 4) ^ emitsib(dest))
  | emitImmext(opcode, dest as Index(b, j, _, _)) =
	let val mode = (if (sizeint(j) = Byte) then 1 else 2)
	in (ebyte(mode*64 + opcode*8 + 4) ^ emitsib(dest) ^ emitimm(j))
	end
  | emitImmext _ = raise EmitImmext

(* Generate code for binary operations *)
exception Gen2
fun gen2(frst,nxt, src, dest) =
        (case (src,dest) of
	    (Immed i, _)  =>  if ~128<=i andalso i<128
			      then (ebyte(131) ^
				    emitImmext(nxt,dest) ^
				    ebyte(i))
			      else (ebyte(129) ^
				    emitImmext(nxt,dest) ^
				    elong(i))
	  | (_, Direct _) => (ebyte(frst+3) ^ emitext(src, dest))
          | (Direct _, _) => (ebyte(frst+1) ^ emitext(src, dest))
          | _ => raise Gen2)

exception Inc
fun inc(x as Direct d)   = emitstring(ebyte(64+d))
  | inc(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(0,x))
  | inc(x as Index _)    = emitstring(ebyte(255) ^ emitImmext(0,x))
  | inc _ = raise Inc

exception Dec
fun dec(x as Direct d)   = emitstring(ebyte(72+d))
  | dec(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(1,x))
  | dec(x as Index _)    = emitstring(ebyte(255) ^ emitImmext(1,x))
  | dec _ = raise Dec

fun addl(Immed 1, dest) = inc(dest)
  | addl(src, dest)     = emitstring(gen2(  0, 0, src, dest))

exception Addl2
fun addl2(Immed i, Direct r) = 
	emitstring(ebyte(129) ^ ebyte(192+r) ^ elong(i))
  | addl2 _ = raise Addl2

fun subl(Immed 1, dest) = dec(dest)
  | subl(src, dest)     = emitstring(gen2( 40, 5, src, dest))

fun orl (src, dest) = emitstring(gen2(  8, 1, src, dest))
fun xorl(src, dest) = emitstring(gen2( 48, 6, src, dest))
fun andl(src, dest) = emitstring(gen2( 32, 4, src, dest))
fun cmpl(src, dest) = emitstring(gen2( 56, 7, src, dest))

fun xchg(x, y) = emitstring(ebyte(135) ^ emitext(x,y))


exception Notl
fun notl(x as Direct _) = emitstring(ebyte(247) ^ emitImmext(2,x))
  | notl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(2,x))
  | notl _ = raise Notl

exception Negl
fun negl(x as Direct _) = emitstring(ebyte(247) ^ emitImmext(3,x))
  | negl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(3,x))
  | negl _ = raise Negl

fun movl(Immed i, Direct r) = 
	emitstring(ebyte(184+r) ^ elong(i))
  | movl(Immed i, dest) = 
	emitstring(ebyte(199) ^ emitImmext(0,dest) ^ elong(i))
  | movl(src, dest) = emitstring(gen2(136, 0, src, dest))

exception Movb
fun movb(x, y as Direct _)  = emitstring(ebyte(138) ^ emitext(x,y))
  | movb(x as Direct _, y) = emitstring(ebyte(136) ^ emitext(x,y))
  | movb _ = raise Movb

exception Movzx
fun movzx(x, y as Direct _) = emitstring(ebyte(15) ^ ebyte(182) ^ emitext(x,y))
  | movzx _ = raise Movzx

exception Stos
fun stos(Direct 0) = emitstring(ebyte(171))
  | stos _ = raise Stos

exception Push
fun push(Direct d) = emitstring(ebyte(80 + d))
  | push _ = raise Push

exception Pop 
fun pop(Direct d) = emitstring(ebyte(88 + d)) 
  | pop _ = raise Pop

exception Shift
fun shift(_,Immed 0, _) = ()
  | shift(TTT, Immed 1, dest) = 
	emitstring(ebyte(209) ^ emitImmext(TTT,dest))
  | shift(TTT, cnt as Immed i, dest) = 
	emitstring(ebyte(193) ^ emitImmext(TTT,dest) ^ ebyte(i))
  | shift(TTT, cnt as Direct 1, dest) = 
	emitstring(ebyte(211) ^ emitImmext(TTT,dest))
  | shift _ = raise Shift

fun asll(cnt, dest) = shift(4, cnt, dest)
fun asrl(cnt, dest) = shift(7, cnt, dest)

exception Lea
fun lea(Displace(s, 0),Direct r) =
	emitstring(ebyte(139) ^ ebyte(3*64 + 8*r + s))
  | lea(Displace(s, i),Direct r) = emitstring(
	ebyte(141) ^
        (case sizeint(i) of
	    Byte => (ebyte(1*64 + 8*r + s) ^ ebyte(i))
          | _    => (ebyte(2*64 + 8*r + s) ^ elong(i))))
  | lea(Immedlab l, Direct r) = jump(LEA(r), l)
  | lea _ = raise Lea

exception Btst
fun btst(src as Immed i, dst as Direct _) = emitstring(
        ebyte(15) ^
        ebyte(186) ^ 
	emitImmext(4,dst) ^
        ebyte(i) )
  | btst(src as Immed i, dst as Displace _) = emitstring(
        ebyte(15) ^
        ebyte(186) ^
	emitImmext(4,dst) ^
        ebyte(i) )
  | btst _ = raise Btst

fun emitlab(i,lab) = jump(LABPTR(i), lab)

exception Jne
exception Jeq
exception Jgt
exception Jge
exception Jlt
exception Jle
exception Jb
exception Jbe
exception Ja
exception Jae
exception Jc
exception Jnc


fun jne(Immedlab lab) = jump(Jcc(5), lab)  | jne _ = raise Jne
fun jeq(Immedlab lab) = jump(Jcc(4), lab)  | jeq _ = raise Jeq
fun jgt(Immedlab lab) = jump(Jcc(15), lab) | jgt _ = raise Jgt
fun jge(Immedlab lab) = jump(Jcc(13), lab) | jge _ = raise Jge
fun jlt(Immedlab lab) = jump(Jcc(12), lab) | jlt _ = raise Jlt
fun jle(Immedlab lab) = jump(Jcc(14), lab) | jle _ = raise Jle
fun jb (Immedlab lab) = jump(Jcc(2), lab)  | jb  _ = raise Jb
fun jbe(Immedlab lab) = jump(Jcc(6), lab)  | jbe _ = raise Jbe
fun ja (Immedlab lab) = jump(Jcc(7), lab)  | ja  _ = raise Ja
fun jae(Immedlab lab) = jump(Jcc(3), lab)  | jae _ = raise Jae
fun jc (Immedlab lab) = jump(Jcc(2), lab)  | jc  _ = raise Jc
fun jnc(Immedlab lab) = jump(Jcc(3), lab)  | jnc _ = raise Jnc

exception Jra
fun jra(arg as Immedlab lab) = jump(JMP, lab)
  | jra _ = raise Jra

exception Jmp
fun jmp(x as Displace _) = emitstring(ebyte(255) ^ emitImmext(4,x))
  | jmp(x as Direct _)   = emitstring(ebyte(255) ^ emitImmext(4,x))
  | jmp _ = raise Jmp

exception Mull
fun mull(x as Direct _, y as Direct _) = emitstring(
	ebyte(15) ^
	ebyte(175) ^
	emitext(x,y))
  | mull _ = raise Mull

exception Divl
fun divl(x as Direct r) =  emitstring(ebyte(247) ^ emitImmext(7,x)) 
  | divl(x as Displace _) = emitstring(ebyte(247) ^ emitImmext(7,x))
  | divl _ = raise Divl

fun cdq() = emitstring(ebyte(153))

(******************** Floating point operations *******************)
exception Fadd
fun fadd(Direct r) = if r=ebp
                     then emitstring(ebyte(220) ^ ebyte(69) ^ ebyte(0))
                     else emitstring(ebyte(220) ^ ebyte(r))
  | fadd _ = raise Fadd

exception Fsub
fun fsub(Direct r) = if r=ebp
                     then emitstring(ebyte(220) ^ ebyte(69) ^ ebyte(0))
                     else emitstring(ebyte(220) ^ ebyte(32+r))
  | fsub _ = raise Fsub

exception Fdiv
fun fdiv(Direct r) = if r=ebp
                     then emitstring(ebyte(220) ^ ebyte(117) ^ ebyte(0))
                     else emitstring(ebyte(220) ^ ebyte(48+r))
  | fdiv _ = raise Fdiv

exception Fmul
fun fmul(Direct r) = if r=ebp
                     then emitstring(ebyte(220) ^ ebyte(77) ^ ebyte(0))
                     else emitstring(ebyte(220) ^ ebyte(8+r))
  | fmul _ = raise Fmul

exception Fcomp
fun fcomp(Direct r) = if r=ebp
                      then emitstring(ebyte(220) ^ ebyte(93) ^ ebyte(0))
                      else emitstring(ebyte(220) ^ ebyte(24+r))
  | fcomp _ = raise Fcomp

exception Fstp
fun fstp(Direct r) = if r=ebp
                     then emitstring(ebyte(221) ^ ebyte(93) ^ ebyte(0))
                     else emitstring(ebyte(221) ^ ebyte(24+r))
  | fstp _ = raise Fstp

exception Fld
fun fld(Direct r)  = if r=ebp
                     then emitstring(ebyte(221) ^ ebyte(69) ^ ebyte(0))
                     else emitstring(ebyte(221) ^ ebyte(r))
  | fld _ = raise Fld

exception Fnstsw
fun fnstsw(Direct 0) = emitstring(ebyte(223) ^ ebyte(224))
  | fnstsw _ = raise Fnstsw

fun sahf() = emitstring(ebyte(158))

fun comment _ = ()

exception Trapv
fun trapv(Displace(4,r))  = emitstring(ebyte(113) ^ ebyte(4) ^ 
			    ebyte(255) ^ ebyte(84) ^ ebyte(36) ^ ebyte(r))
  | trapv _ = raise Trapv

exception Trapmi
fun trapmi(Displace(4,r)) = emitstring(ebyte(121) ^ ebyte(4) ^ 
			    ebyte(255) ^ ebyte(84) ^ ebyte(36) ^ ebyte(r))
  | trapmi _ = raise Trapmi

val finish = Emitter.finish

end (* functor MCode386 *)

















