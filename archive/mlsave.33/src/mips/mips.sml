functor MipsCM(MipsC : MIPSCODER) : CMACHINE = struct

open MipsC System.Tags

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

val exnptr = Direct(Reg 30)
val dataptr as Direct dataptr' = Direct(Reg 23)
val arithtemp as Direct arithtemp' = Direct(Reg 24)
val arithtemp2 as Direct arithtemp2'= Direct(Reg 25)
val arithtemp3 as Direct arithtemp3'= Direct(Reg 20)
val storeptr = Direct(Reg 22)
val standardclosure = Direct(Reg 1)
val standardarg = Direct(Reg 2)
val standardcont = Direct(Reg 3)
val miscregs = map (Direct o Reg) [4,5,6,7,8,9,10,11,12,13,14,
				   15,16,17,18,19]
val ptrtemp = Direct(Reg 22)
val ptrtemp2 = Direct(Reg 21)

fun newlabel() = Immedlab(M.newlabel())
fun emitlab(i,Immedlab lab) = M.emitlab(i,lab)
fun define (Immedlab lab) = M.define lab
val align = M.align
val mark = M.mark

val emitlong = M.emitlong
val realconst = M.realconst
val emitstring = M.emitstring

fun jmpindexb lab = (add(arithtemp',lab,arithtemp');
		     jump(arithtemp'))

fun record(vl, Direct z) =
    let open CPS
	val len = List.length vl
	fun f(i,nil) = ()
	  | f(i,(r, SELp(j,p))::rest) = 
		(M.lw(ptrtemp', r, j*4); f(i,(ptrtemp,p)::rest))
	  | f(i,(Direct r,OFFp 0)::rest) = 
		(M.sw(r, dataptr, i*4); f(i-1,rest))
	  | f(i,(Direct r, OFFp j)::rest) = 
		(M.add(r, Immed(4*j), ptrtemp); f(i,(ptrtemp,OFFp0)::rest))
	  | f(i,(ea,p)::rest) = 
		(M.add(Reg 0, ea, ptrtemp'); f(i,(ptrtemp,p)::rest))
      in f(len - 2, rev vl);
	 M.add(dataptr', Immed 0, z);
         M.add(dataptr', Immed(4*len), dataptr')
     end

fun select(i, r, Direct s) = M.lw(s, r, i*4)

fun offset(i, Direct r, Direct s) = M.add(r,Immed(i*4), s)

fun three f (Direct x, ea, Direct y) = f(x,ea,y)
  | three f (ea, Direct x, Direct y) = f(x,ea,y)

val addl3 = three M.add

fun subl3(Immed k, x, y) = addl3(x, Immed(~k), y)
  | subl3(Direct x, Direct y, Direct z) = M.sub(y,x,z)
  | subl3(x, Immed k, dest) = 
	    (M.add(Reg 0, Immed k, arithtemp3');
	     subl3(x, arithtemp3, dest))

fun ashr(ea, Direct x, Direct y) = M.sra(ea,x,y)
fun ashl(ea, Direct x, Direct y) = M.sll(ea,x,y)

fun mull2(Direct x, Direct y) = M.mult(x,y,y)
fun divl2(Direct x, Direct y) = M.div(y,x,y)

val orb = three M.or
fun andb = three M.and'
fun notb (a,b) = subl3(a, Immed ~1, b)
val xorb = three M.xor

fun mstoreindexl(v,w,Immed 1) = 
    (M.sw(storeptr', dataptr,0);
     M.sw(v, w,0);
     M.sw(w, dataptr,~4);
     M.add(Reg 0, dataptr,storeptr');
     M.addl(dataptr',Immed(8),dataptr'))
  | mstoreindexl(v,w,Direct i) = 
    (M.add(i, Immed ~1, arithtemp');
     M.sll(Immed 1,arithemp',arithtemp');
     M.sw(storeptr, dataptr,0);
     M.add(arithtemp',w,arithtemp');
     M.sw(v, arithtemp,0);
     M.sw(arithtemp', dataptr,~4);
     M.add(Reg 0, dataptr,storeptr');
     M.addl(dataptr',Immed(8),dataptr'))

fun fetchindexl(v,Direct w, Direct i) = 
    (M.add(i, Immed ~1, arithtemp');
     M.sll(Immed 1,arithemp',arithtemp');
     M.add(arithtemp',v,arithtemp');
     M.lw(w, arithtemp,0))

fun storeindexl(v,w, Immed 1) = M.sw(w,0, v)
  | storeindexl(v,w,i) = 
    (M.add(i, Immed ~1, arithtemp');
     M.sll(Immed 1,arithemp',arithtemp');
     M.add(arithtemp',w,arithtemp');
     M.sw(v, arithtemp,0))

fun fetchindexb(v,w) =
    (M.sra(Immed 1,arithemp',arithtemp');
     M.add(arithtemp',v,arithtemp');
     M.lbu(w,arithtemp,0))
fun storeindexb(v,w) =
    (M.sra(Immed 1,arithemp',arithtemp');
     M.add(arithtemp',w,arithtemp');
     M.sb(v,arithtemp,0))

fun finishreal(y) = ()

fun mnegg(x,y) = ()
val mulg3 = ()
val divg3 = ()
val addg3 = ()
val subg3 = ()

fun rev LEQ = GEQ
  | rev GEQ = LEQ
  | rev LSS = GTR
  | rev GTR = LSS
  | rev NEQ = NEQ
  | rev EQL = EQL

fun ibranch (cond, Immed a, Immed b, Immedlab label) =
	    if (case cond of EQL => a=b | NEQ => a<>b | LSS => a<b |
			     LEQ => a<=b | GTR => a>b | GEQ => a>=b)
		then M.beq(true,Reg 0, Reg 0, label) else ()
  | ibranch (NEQ, Direct r, Direct s, Immedlab label) =
		    M.beq(false, r, s, label)
  | ibranch (NEQ, Direct r, x, Immedlab label) =
		    (M.add(Reg 0, x, arithtemp3');
		     M.beq(false, r, arithtemp3', label))
  | ibranch (EQL, Direct r, Direct s, Immedlab label) =
		    M.beq(true, r, s, label)
  | ibranch (EQL, Direct r, x, Immedlab label) =
		    (M.add(Reg 0, x, arithtemp3');
		     M.beq(true, r, arithtemp3', label))
  | ibranch (LSS, Direct r, x, Immedlab lab) =
		(M.slt(r,x,arithtemp3'); M.beq(false,Reg 0, arithtemp3',lab))
  | ibranch (GEQ, Direct r, x, Immedlab lab) =
		(M.slt(r,x,arithtemp3'); M.beq(true,Reg 0, arithtemp3',lab))
  | ibranch (GTR, x, Direct r, Immedlab lab) =
		(M.slt(r,x,arithtemp3'); M.beq(false,Reg 0, arithtemp3',lab))
  | ibranch (LEQ, x, Direct r, Immedlab lab) =
		(M.slt(r,x,arithtemp3'); M.beq(true,Reg 0, arithtemp3',lab))
  | ibranch (_, Immedlab _, Immedlab _, _) => 
		ErrorMsg.Impossible "4455 in mips"
  | ibranch (cond, x, y, l) = ibranch(rev cond, y,x,l)

subl3(op2, op1, arithtemp3); branch (cond, arithtemp3', label))

fun gbranch (cond, op1, op2, label) = ()

fun jmp(Direct x) = jump x

val bbs = fn(Immed k,Direct y,Immedlab label) => 
	(andb(Bits.lshift(1,k),y,arithtemp3);
	 branch(NEQ,arithtemp3,label))

fun isimmed(Immed i) = SOME i
  | isimmed _ = NONE

fun isreg(Direct(Reg i)) = SOME i | isreg _ = NONE
fun eqreg (a: EA) b = a=b

fun profile(i,incr) = ()

val comment = M.comment
end
