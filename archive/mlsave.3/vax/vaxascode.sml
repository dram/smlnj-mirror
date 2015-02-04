structure VaxAssem = struct val outfile = ref std_out end

structure VaxAsCode : VAXCODER = struct

open Boot.tags VaxAssem

val offset = ref 0

type Label = string

local val i = ref 0 in
fun newlabel () = (i := !i + 1; "L" ^ makestring (!i))
end

fun itoa (i:int) = if i < 0 then "-" ^ makestring (~i)
		   else makestring i

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

fun emit (s:string) = (print s; output(!outfile,s))

fun newline () = (emit "\n"   (*  ; emit(makestring(!offset)); emit "\t" *) )

fun emitreg (reg 15) = emit "pc"
  | emitreg (reg 14) = emit "sp"
  | emitreg (reg r) = emit ("r" ^ itoa r)

fun emitarg (direct(r)) = emitreg r
  | emitarg (autoinc(r)) = (emit "("; emitreg r; emit ")+")
  | emitarg (autodec(r)) = (emit "-("; emitreg r; emit ")")
  | emitarg (immed i) = emit ("$" ^ itoa i)
  | emitarg (displace(0,r)) = (emit "("; emitreg r; emit ")")
  | emitarg (displace(i,r)) = (emit (itoa i); emit "("; emitreg r; emit ")")
  | emitarg (deferred(i,r)) = (emit ("*"^itoa i); emit "("; emitreg r; emit ")")
  | emitarg (address L) = emit L
  | emitarg (index(ea,r)) = (emitarg ea; emit "["; emitreg r; emit "]")

fun emit1arg (a) = (emitarg a; newline())

fun emit2arg (a,b) = (emitarg a; emit ","; emitarg b; newline())

fun emit3arg (a,b,c) =
	(emitarg a; emit ","; emitarg b; emit ","; emitarg c; newline())

fun emitbarg (displace(n,reg 15)) = (emit "$"; emit(makestring n); newline())
  | emitbarg a = emit1arg a

fun align () = emit ".align 2\n"

local val p = makestring power_tags
      val t = makestring tag_backptr
in
fun mark () = let val L = newlabel()
	      in  emit L;
		  emit ": .long ((";
		  emit L;
		  emit "-base)/4+1)*";   (* STRING dependency *)
		  emit p;
		  emit "+";
		  emit t;
		  emit "\n"
	      end
end

fun define L = (emit L; emit ":\n")
fun oct i = let val m = Integer.makestring
	    in  m(i div 64)^m((i div 8)mod 8)^m(i mod 8) end
fun c_char "\n" = "\\n"
  | c_char "\t" = "\\t"
  | c_char "\\" = "\\\\"
  | c_char "\"" = "\\\""
  | c_char c = if ord c < 32 then "\\"^oct(ord c) else c
fun a_str s = implode(map c_char (explode s))
fun emitstring s = (emit ".long "; emit(itoa(length s)); newline();
		    emit ".ascii \""; emit(a_str s); emit "\"\n")
fun realconst s = (emit ".gfloat "; emit s; emit "\n")
fun emitlong (i : int) = (emit ".long "; emit(makestring i); emit "\n")

fun jne arg = (emit "jneq "; emit1arg arg)
fun jbr arg = (emit "jbr "; emit1arg arg)
fun bbc (immed 0, arg1, arg2) = (emit "blbc "; emit2arg(arg1,arg2))
  | bbc args = (emit "bbc "; emit3arg args)
fun bbs (immed 0, arg1, arg2) = (emit "blbs "; emit2arg(arg1,arg2))
  | bbs args = (emit "bbs "; emit3arg args)

fun movb args = (emit "movb "; emit2arg args)
fun movzbl args = (emit "movzbl "; emit2arg args)

fun movl (arg, autodec sp) = (emit "pushl "; emit1arg arg)
  | movl (immed 0, arg) = (emit "clrl "; emit1arg arg)
  | movl args = (emit "movl "; emit2arg args)

fun pushal arg = (emit "pushal "; emit1arg arg)
fun moval (arg, autodec sp) = pushal arg
  | moval args = (emit "moval "; emit2arg args)
fun rsb () = emit "rsb\n"
fun cmpl args = (emit "cmpl "; emit2arg args)
fun addl2 args = (emit "addl2 "; emit2arg args)
fun addl3 args = (emit "addl3 "; emit3arg args)
fun subl2 args = (emit "subl2 "; emit2arg args)
fun subl3 args = (emit "subl3 "; emit3arg args)
fun ashl (immed i,arg2,arg3) = (emit "ashl "; emit3arg (immed i,arg2,arg3))
fun mull2 args = (emit "mull2 "; emit2arg args)
fun divl3 args = (emit "divl3 "; emit3arg args)
fun divl2 args = (emit "divl2 "; emit2arg args)
fun jmp arg = (emit "jmp "; emit1arg arg)
fun brb arg = (emit "brb "; emitbarg arg)
fun brw arg = (emit "brw "; emitbarg arg)
fun beql arg = (emit "beql "; emitbarg arg)
fun bneq arg = (emit "bneq "; emitbarg arg)
fun bgeq arg = (emit "bgeq "; emitbarg arg)
fun bgtr arg = (emit "bgtr "; emitbarg arg)
fun blss arg = (emit "blss "; emitbarg arg)
fun bleq arg = (emit "bleq "; emitbarg arg)

fun movg args = (emit "movg "; emit2arg args)
fun mnegg args = (emit "mnegg "; emit2arg args)
fun addg3 args = (emit "addg3 "; emit3arg args)
fun subg3 args = (emit "subg3 "; emit3arg args)
fun mulg3 args = (emit "mulg3 "; emit3arg args)
fun divg3 args = (emit "divg3 "; emit3arg args)
fun cmpg args = (emit "cmpg "; emit2arg args)

fun push arg = movl(arg,autodec sp)
fun pusha arg = moval(arg,autodec sp)
fun pop arg = movl(autoinc sp,arg)

end (* structure AsCode *)
