functor M68CM(V : M68CODER) : CMACHINE = struct

structure V' : sig 
			datatype Register = DataReg of int
					  | AddrReg of int
					  | FloatReg of int
					  | PC
			
		        type Label sharing type Label = V.Label
			datatype Size = Byte | Word | Long
			
			datatype EA = Direct of Register
				    | PostInc of Register
				    | PreDec of Register
				    | Displace of Register * int
				    | Index of Register * int * Register * Size
				    | Immed of int
				    | Immedlab of Label
				    | Abs of int
				    | Address of Label

		end = V
open V'

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

(* +DEBUG *)
fun diag (s : string) f x =
	f x handle e =>
		(print "?exception "; print (System.exn_name e);
		 print " in m68."; print s; print "\n";
		 raise e)
(* -DEBUG *)

fun defer(Direct r) = Displace(r,0)
  | defer(Immedlab lab) = Address lab
  | defer _ = ErrorMsg.impossible "defer in cpsm68"

(* DEBUG *) val defer = diag "defer" defer

val numregs = 5

val exnptr = Direct(DataReg 7)
val dataptr as Direct dataptr' = Direct(AddrReg 6)
val arithtemp as Direct arithtemp' = Direct(DataReg 1)
val arithtemp2 = Direct(DataReg 2)
val storeptr = Direct(DataReg 6)
val standardclosure = Direct(AddrReg 2)
val standardarg = Direct(AddrReg 0)
val standardcont = Direct(AddrReg 1)
val miscregs = map (Direct o AddrReg) [3,4,5];

val ptrtemp2 = Direct(DataReg 3)
val ptrtemp = Direct(AddrReg 5) (* note that this is in miscregs, and
				   so it must be restored if it is creamed. *)

fun reg(Direct r) = r

fun newlabel() = Immedlab(V.newlabel())
(* DEBUG *) val newlabel = diag "newlabel" newlabel
fun emitlab(i,Immedlab lab) = V.emitlab(i,lab)
fun define (Immedlab lab) = V.define lab
val align = V.align
val mark = V.mark
fun move(Immedlab l, dest as Direct(AddrReg x)) = V.lea(Address l, dest)
  | move(Immedlab l, dest as Direct(DataReg x)) = 
	    (V.exg(dest,ptrtemp);
	     V.lea(Address l, ptrtemp);
	     V.exg(dest,ptrtemp))
  | move(Immedlab l, dest) = 
	    (V.exg(ptrtemp2,ptrtemp); 
	     V.lea(Address l, ptrtemp);
	     V.movl(ptrtemp,dest);
	     V.exg(ptrtemp2,ptrtemp))
  | move(Displace(DataReg(d), i), dest) =
	    (V.exg(ptrtemp2,ptrtemp); 
	     V.movl(Direct(DataReg(d)), ptrtemp);
	     V.movl(Displace(reg(ptrtemp), i), dest);
	     V.exg(ptrtemp2,ptrtemp))
  | move x = V.movl x
(* DEBUG *) val move = diag "move" move

val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun ashl(Immed k, r, d as Direct(DataReg _)) =
    (if r<>d then move(r,d) else ();
     if k>0 then V.asll(Immed k,d) else V.asrl(Immed(~k),d))
(* DEBUG *) val ashl = diag "ashl" ashl

fun jmpindexb lab = V.jra(Index(PC,2,arithtemp',Byte))
(* DEBUG *) val jmpindexb = diag "jmpindexb" jmpindexb

fun storefield(i, v) = move(v, Displace(dataptr',i*4))
(* DEBUG *) val storefield = diag "storefield" storefield

fun addtodataptr i = V.lea(Displace(dataptr',4*i), dataptr)
(* DEBUG *) val addtodataptr = diag "addtodataptr" addtodataptr

fun select(i, Direct r, s) = move(Displace(r,i*4),s)
  | select(0, a, s) = move(defer a,s)

fun offset(i, Direct r, s) = V.lea(Displace(r,i*4),s)
(* DEBUG *) val select = diag "select" select
(* DEBUG *) val offset = diag "offset" offset

fun sceql(Direct x', Direct y', len, Immedlab lab) =
    let val loop = V.newlabel()
     in case len
	 of Immed k => move(Immed(k div 2), arithtemp)
	  | len => (move(len, arithtemp); V.asrl(Immed 1, arithtemp));
	V.define loop;
	V.cmpl(Index(x',0,arithtemp',Long),Index(y',0,arithtemp',Long));
	V.jne(Address lab);
	V.subl(Immed 1, arithtemp);
	V.jge(Address loop)
    end
(* DEBUG *) val sceql = diag "sceql" sceql


fun three opcode (a,b,c) = 
	    if b=c then opcode(a,b) else (move(b,c); opcode(a,c))
(* DEBUG *) val three = diag "three" three

val addl3 = three V.addl
val subl3 = three V.subl
val mull2 = V.mull
val divl2 = V.divl
val bisl3 = three V.orl
fun store(x, y) = move(x, defer y)
(* DEBUG *) val store = diag "store" store
exception Fetchindexb
fun fetchindexb(Direct x,y) = (if y=arithtemp then raise Fetchindexb else ();
			       move(Immed 0,y); 
			       V.movb(Index(x,0,arithtemp',Byte),y))
(* DEBUG *) val fetchindexb = diag "fetchindexb" fetchindexb
fun storeindexb(x, Direct y) = V.movb(x,Index(y,0,arithtemp',Byte))
(* DEBUG *) val storeindexb = diag "storeindexb" storeindexb
fun fetchindexl(Direct x,y) = move(Index(x,0,arithtemp',Long),y)
  | fetchindexl(Immedlab lab, y) = 
		    move(Index(PC,6,arithtemp',Long), y);
(* DEBUG *) val fetchindexl = diag "fetchindexl" fetchindexl
fun storeindexl(x, Direct y) = move(x,Index(y,0,arithtemp',Long))
(* DEBUG *) val storeindexl = diag "storeindexl" storeindexl

val fp0 = FloatReg 0

fun float f (a,b,c) =
    (V.fmoved(defer a, Direct fp0);
     f(defer b, Direct fp0);
     V.fmoved(Direct fp0, defer c))

fun mnegg (a,c) = (V.fnegd(a, Direct fp0);
		   V.fmoved(Direct fp0, defer c))
val mulg3 = float V.fmuld
val divg3 = float V.fdivd
val addg3 = float V.faddd
val subg3 = float V.fsubd

fun cbranch NEQ = V.jne
  | cbranch EQL = V.jeq
  | cbranch LEQ = V.jle
  | cbranch GEQ = V.jge
  | cbranch LSS = V.jlt
  | cbranch GTR = V.jgt

fun rev LEQ = GEQ
  | rev GEQ = LEQ
  | rev LSS = GTR
  | rev GTR = LSS
  | rev NEQ = NEQ
  | rev EQL = EQL

fun ibranch (cond, op1, op2 as Immed _, label) =
	(V.cmpl(op2, op1); cbranch (rev cond) (defer label))
  | ibranch (cond, op1, op2, label) =
	(V.cmpl(op1, op2); cbranch cond (defer label))

fun gbranch (cond, op1, op2, label) =
	(V.fcmpd(defer op1, defer op2); cbranch cond (defer label))

fun defer' j = fn x => j(defer x)
val jmp = defer' V.jra
fun bbs (x,dest as Direct(AddrReg _) ,l) = (move(dest,ptrtemp2);
					    bbs(x,ptrtemp2,l))
  | bbs (x,y,l) = (V.btst(x,y); V.jne(defer l))
(* DEBUG *) val bbs = diag "bbs" bbs

val immed = Immed
fun isimmed(Immed i) = SOME i
  | isimmed _ = NONE

fun isreg(Direct(AddrReg i)) = SOME i
  | isreg(Direct(DataReg i)) = SOME(i+8)
  | isreg _		= NONE

fun eqreg (a: EA) b = a=b
(* DEBUG *) val eqreg = diag "eqreg" eqreg

fun profile(index,incr) = V.addl(Immed incr, Displace(V.sp,4*index))

val comment = V.comment
end
