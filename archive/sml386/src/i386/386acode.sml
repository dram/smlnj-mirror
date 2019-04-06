(* Copyright 1989 by      Department of Computer Science, 
 *                        The Technical University of Denmak
 *                        DK-2800 Lyngby 
 *
 * 17 Dec. 1991    Yngvi Skaalum Guttesen      (ysg@id.dth.dk)
 *)

structure Ass386 = struct val outfile = ref std_out end

functor ACode386() : CODER386 = struct

open Ass386

type Label = string

datatype Size = Byte | Word | Long

datatype EA = Direct of int
	    | Displace of int * int
	    | Index of int * int * int * Size
	    | Immedlab of Label
	    | Immed of int

val eax = 0
and ebx = 3
and ecx = 1
and edx = 2
and esi = 6
and edi = 7
and ebp = 5
and esp = 4

val offset = ref 0

local val i = ref 0 in
fun newlabel () = (i := !i + 1; "L" ^ makestring (!i))
end

fun itoa (i:int) = if i < 0 then "-" ^ makestring (~i)
		   else makestring i


fun emit s = outputc (!outfile) s

fun emitreg (0) = emit "eax"
  | emitreg (1) = emit "ecx"
  | emitreg (2) = emit "edx"
  | emitreg (3) = emit "ebx"
  | emitreg (4) = emit "esp"
  | emitreg (5) = emit "ebp"
  | emitreg (6) = emit "esi"
  | emitreg (7) = emit "edi"

fun sizeint i =
	if i < 128 andalso i > ~129 then Byte
	else if i < 32768 andalso i > ~32769 then Word
	else Long

fun emitarg (Immed i) = emit (itoa i)
  | emitarg (Direct r) = emitreg r
  | emitarg (Displace (r,i)) =
        (emit "dword ptr ["; emitreg r;
         emit "+";
         emit (itoa i);
         emit "]")
  | emitarg (Index (ra,disp,r,s)) =
        (emit "dword ptr [";
         emitreg ra;
         emit "+";
	 emit (itoa disp);
         emit "+";
	 emitreg r;
         emit "*";
	 emit (case s of Byte => "1" | Word => "2" | Long => "4");    
         emit "]")
  | emitarg (Immedlab l) = emit l

fun emit2arg (a,b) = (emitarg b; emit ","; emitarg a; emit "\n")
fun emit1arg a = (emitarg a; emit "\n")

fun emitargb (Immed i) = emit (itoa i)
  | emitargb (Direct r) = emitreg r
  | emitargb (Displace (r,i)) =
        (emit "byte ptr ["; emitreg r;
         emit "+";
         emit (itoa i);
         emit "]")
  | emitargb (Index (ra,disp,r,s)) =
        (emit "byte ptr [";
         emitreg ra;
         emit "+";
	 emit (itoa disp);
         emit "+";
	 emitreg r;
         emit "*";
	 emit (case s of Byte => "1" | Word => "2" | Long => "4");    
         emit "]")
  | emitargb (Immedlab l) = emit l

fun emit2argb (a,b) = (emitarg a; emit ","; emitarg b; emit "\n")

fun oct i = let val m = Integer.makestring
	    in  m(i div 64)^m((i div 8)mod 8)^m(i mod 8) end
fun c_char "\n" = "\\n"
  | c_char "\t" = "\\t"
  | c_char "\\" = "\\\\"
  | c_char "\"" = "\\\""
  | c_char c = if ord c < 32 then "\\"^oct(ord c) else c
fun a_str s = implode(map c_char (explode s))

(**************************** Misc. functions ********************************)

fun align () = emit "ALIGN 2\n"

fun mark () = let val lab = newlabel()
	      in  emit lab;
                  emit ": DD MAKE_DESC((";
		  emit lab;
		  emit "-base)/4+1,tag_backptr)\n"   (* STRING dependency *)
	      end

fun define lab = (emit lab; emit ":\n")
fun comment s  = (emit "; "; emit s)
fun finish ()  = ""; (* this function is never called because we doen't 
                      * need to backpatch assembly code. But we have to 
                      * satisfy the I386CODER signature
                      *)

(******************************** Emitters ***********************************)

fun emitstring s = (emit "DB \""; emit(a_str s); emit "\"\n")
fun realconst s = (emit "DQ "; emit s; emit "r\n")
fun emitlong (i : int) = (emit "DD "; emit(makestring i); emit "\n")

fun emitlab (offset,l2) =
        (emit "@@: DD "; emit l2; emit "-@B";
	 if offset < 0 then (emit "-"; emit (makestring (~offset)))
	               else (emit "+"; emit (makestring offset));
	 emit "\n")

exception Illegal

(**************************** Memory functions *******************************)

fun movl  args = (emit "mov   "; emit2arg args)
fun movb  args = (emit "movb  "; emit2argb args)
fun movzx args = (emit "movzx "; emit2arg args)
fun stos  arg  = (emit "stos  "; emit1arg arg)
fun lea   args = (emit "lea   "; emit2arg args)
fun push  arg  = (emit "push  "; emit1arg arg)
fun pop   arg  = (emit "pop   "; emit1arg arg)
fun xchg  args = (emit "xchg  "; emit2arg args)

(************************ Bitwise operations *********************************)

fun xorl args = (emit "xor   "; emit2arg args)
fun orl  args = (emit "or    "; emit2arg args)
fun notl arg  = (emit "not   "; emit1arg arg)
fun andl args = (emit "and   "; emit2arg args)
fun btst args = (emit "bt    "; emit2arg args)


(***************************** Arithmetic ************************************)

fun addl  args = (emit "add   "; emit2arg args)
fun subl  args = (emit "sub   "; emit2arg args)
fun mull  args = (emit "imul  "; emit2arg args)
fun asll  args = (emit "sal   "; emit2arg args)
fun asrl  args = (emit "sar   "; emit2arg args)
fun cmpl  args = (emit "cmp   "; emit2arg args)
fun addl2 args = (emit "add   "; emit2arg args)
fun negl  arg  = (emit "neg   "; emit1arg arg )
fun divl  arg  = (emit "idiv  "; emit1arg arg)
fun cdq   ()   =  emit "cdq\n" 

(******************************* Jumps ***************************************)

fun jne arg = (emit "jne   "; emit1arg arg)
fun jeq arg = (emit "jeq   "; emit1arg arg)
fun jgt arg = (emit "jgt   "; emit1arg arg)
fun jge arg = (emit "jge   "; emit1arg arg)
fun jlt arg = (emit "jlt   "; emit1arg arg)
fun jle arg = (emit "jle   "; emit1arg arg)
fun jc  arg = (emit "jc    "; emit1arg arg)
fun jnc arg = (emit "jnc   "; emit1arg arg)
fun jls arg = (emit "jls   "; emit1arg arg)
fun ja  arg = (emit "ja    "; emit1arg arg)
fun jae arg = (emit "jea   "; emit1arg arg)
fun jb  arg = (emit "jb    "; emit1arg arg)
fun jbe arg = (emit "jbe   "; emit1arg arg)

fun jra (arg as (Immedlab lab)) = (emit "jmp   "; emit1arg arg)

fun jmp (arg as Displace _) = (emit "jmp   "; emit1arg arg)
  | jmp (arg as Direct _  ) = (emit "jmp   "; emit1arg arg)

(********************** Floating point functions  ****************************)

(* 80387 float operations *)
(* Some src/dest combinations are illegal, but not caught here. *)

fun fcomp  arg = (emit "fcompp  "; emit1arg arg)
fun fadd   arg = (emit "faddp   "; emit1arg arg)
fun fsub   arg = (emit "fsubp   "; emit1arg arg)
fun fmul   arg = (emit "fmulp   "; emit1arg arg)
fun fdiv   arg = (emit "fdivp   "; emit1arg arg)
fun fld    arg = (emit "fld     "; emit1arg arg)
fun fstp   arg = (emit "fstp    "; emit1arg arg)
fun fnstsw arg = (emit "fnstsw  "; emit1arg arg)
fun sahf   ()  = (emit "sahf\n")

(******************************* Traps ***************************************)

fun trapv(arg as Displace(4,r))   = (emit "jno @f\n"; 
                                     emit "call    "; emit1arg arg;
                                     emit "@@:\n")
fun trapmi(arg as Displace(4,r)) = (emit "jns @f\n"; 
                                     emit "call    "; emit1arg arg;
                                     emit "@@:\n")

end (* structure AsCode *)
