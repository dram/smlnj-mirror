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

fun defer(Direct r) = Displace(r,0)
  | defer(Immedlab lab) = Address lab
  | defer _ = ErrorMsg.impossible "defer in cpsm68"

val numregs = 5

val exnptr = Direct(DataReg 7)
val dataptr as Direct dataptr' = Direct(AddrReg 6)
val arithtemp as Direct arithtemp' = Direct(DataReg 1)
val arithtemp2 = Direct(DataReg 2)
val storeptr = Direct(DataReg 6)
val standardclosure = Direct(AddrReg 2)
val standardarg = Direct(AddrReg 0)
val standardcont = Direct(AddrReg 1)
val miscregs = map (Direct o AddrReg) [3,4];

val ptrtemp2 = Direct(DataReg 3)
val ptrtemp = Direct(AddrReg 5)

fun newlabel() = Immedlab(V.newlabel())
fun emitlab(i,Immedlab lab) = V.emitlab(i,lab)
fun define (Immedlab lab) = V.define lab
val align = V.align
val mark = V.mark
fun move(Immedlab l, dest as Direct(AddrReg x)) = V.lea(Address l, dest)
  | move(Immedlab l, dest) = (V.lea(Address l, ptrtemp); V.movl(ptrtemp,dest))
  | move x = V.movl x
val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun ashl(Immed k, r, d as Direct(DataReg _)) =
    (if r<>d then move(r,d) else ();
     if k>0 then V.asll(Immed k,d) else V.asrl(Immed(~k),d))

fun jmpindexb lab = V.jra(Index(PC,2,arithtemp',Byte))

fun storefield(i, v) = move(v, Displace(dataptr',i*4))

fun addtodataptr i = V.lea(Displace(dataptr',4*i), dataptr)

fun select(i, Direct r, s) = move(Displace(r,i*4),s)
fun offset(i, Direct r, s) = V.lea(Displace(r,i*4),s)

fun sceql(Direct x', Direct y', Immed k, Immedlab lab) =
    let val loop = V.newlabel()
     in move(Immed(k div 2), arithtemp);
	V.define loop;
	V.cmpl(Index(x',0,arithtemp',Long),Index(y',0,arithtemp',Long));
	V.jne(Address lab);
	V.subl(Immed 1, arithtemp);
	V.jge(Address loop)
    end


fun three opcode (a,b,c) = 
	    if b=c then opcode(a,b) else (move(b,c); opcode(a,c))

val addl3 = three V.addl
val subl3 = three V.subl
val mull2 = V.mull
val divl2 = V.divl
val bisl3 = three V.orl
fun store(x, y) = move(x, defer y)
fun fetchindexb(Direct x,y) = (move(Immed 0,y); 
			       V.movb(Index(x,0,arithtemp',Byte),y))
fun storeindexb(x, Direct y) = V.movb(x,Index(y,0,arithtemp',Byte))
fun fetchindexl(Direct x,y) = move(Index(x,0,arithtemp',Long),y)
  | fetchindexl(Immedlab lab, y) = 
		    move(Index(PC,6,arithtemp',Long), y);
fun storeindexl(x, Direct y) = move(x,Index(y,0,arithtemp',Long))

exception UnimplementedM68
fun unimp x = raise  UnimplementedM68

val mnegg = unimp
val mulg3 = unimp
val divg3 = unimp
val addg3 = unimp
val subg3 = unimp

val cmpl = V.cmpl

val cmpg = unimp

fun defer' j = fn x => j(defer x)
val jmp = defer' V.jra
val bneq = defer' V.jne
val beql = defer' V.jeq
val bleq = defer' V.jle
val bgeq = defer' V.jge
val blss = defer' V.jlt
val bgtr = defer' V.jgt
fun bbs (x,dest as Direct(AddrReg _) ,l) = (move(dest,ptrtemp2);
					    bbs(x,ptrtemp2,l))
  | bbs (x,y,l) = (V.btst(x,y); bneq l)

val immed = Immed
fun isimmed(Immed i) = SOME i
  | isimmed _ = NONE

fun isreg(Direct(AddrReg i)) = SOME i
  | isreg(Direct(DataReg i)) = SOME(i+8)
  | isreg _ = NONE
fun eqreg (a: EA) b = a=b

fun profile(index,incr) = addl(Immed incr, Displace(sp,4*index))

val comment = V.comment
end
