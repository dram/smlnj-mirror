(* Copyright 1989 by AT&T Bell Laboratories *)
functor NS32CM(V : NS32CODER) : CMACHINE = struct

structure V' : sig datatype Register = Genreg of int
				     | FloatReg of int
				     | FP
				     | SP
				     | SB
				     | PC

		   eqtype Label sharing type Label = V.Label
		   datatype Size = Byte | Word | Long
			
		   datatype EA = Direct of Register
			       | Topofstack
			       | Displace of int * Register
			       | Immed of int
			       | Immedlab of Label
			       | Abs of int
			       | OffAddress of Label * int
			       | Index of EA * Register * Size

		   exception BadReal of string

		end = V
open V' System.Tags

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

fun address lab = OffAddress(lab,0)	(* old Address style *)

fun defer(Direct r) = Displace(0,r)
  | defer(Immedlab lab) = address lab
  | defer(Displace z) = ErrorMsg.impossible "defer of displace in cpsns32"
  | defer _ = ErrorMsg.impossible "defer in cpsns32"

val sp' = SP
val exnptr = Displace(0, SB)
val dataptr as Direct dataptr' = Direct(FP)
val arithtemps = []
val storeptr = Displace(4, SB)
val globalvar = NONE
val standardclosure = Direct(Genreg 2)
val standardarg = Direct(Genreg 0)
val standardcont = Direct(Genreg 1)
val miscregs : EA list = map (Direct o Genreg) [3,4,6,7]
val floatregs = map (Direct o FloatReg) [0, 2, 4, 6]
val savedfpregs = [] : EA list
val ptrtemp = Direct(Genreg 5)

fun newlabel() = Immedlab(V.newlabel())
fun emitlab(i,Immedlab lab) = V.emitlab(i,lab)
fun define (Immedlab lab) = V.define lab

fun beginStdFn ()  = ()

val align = V.align
val mark = V.mark

fun move(src as Direct(FloatReg _), dst as Direct(FloatReg _)) = V.movg(src,dst)
  | move(Immedlab l, dest as Direct(Genreg x)) = V.lea(address l, dest)
  | move(Immedlab l, dest) = V.lea(address l, dest)
(* Only runtime knows about SB and MOD registers.
   We use SB as a base for our pseudo-registers.
  | move(src as Direct(SB), dest) = V.sprl(src, dest)
  | move(src, dest as Direct(SB)) = V.lprl(src, dest)
 *)
  | move(src as Direct(FP), dest) = V.sprl(src, dest)
  | move(src, dest as Direct(FP)) = V.lprl(src, dest)
  | move x = V.movl x

(* checkLimit (n):
 * Generate code to check the heap limit to see if there is enough free space
 * to allocate n bytes.
 *)
fun checkLimit maxAllocSize = (
      V.comment ("begin fun, max alloc = "^(makestring maxAllocSize)^"\n");
    (* Check the heap limit by writing to the dataptr+maxAllocSize-4 *)
      move(Immed 0, Displace(maxAllocSize-4, dataptr')))

val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun jmpindexb(lab, Direct y) = V.jump(Index(defer lab, y, Byte))

fun record(vl, z) =
    let open CPS
	val len = List.length vl
	fun f(i,nil) = ()
	  | f(i,(Direct r, SELp(j,p))::rest) = f(i,(Displace(j*4,r),p)::rest)
	  | f(i,(Immedlab l, p)::rest) = (V.lea(address l, ptrtemp);
				          f(i, (ptrtemp,p)::rest))
	  | f(i,(x,OFFp 0)::rest) = (move(x,Displace(i*4,dataptr'));
				     f(i-1,rest))
	  | f(i,(Direct r, OFFp j)::rest) = (V.lea(Displace(j*4,r),
						   Displace(i*4,dataptr'));
						 f(i-1,rest))
	  | f(i,(x,p)::rest) = (move(x,ptrtemp); f(i,(ptrtemp,p)::rest))
      in
	 f(len - 2, rev vl);
	 move(dataptr,z);
         V.lea(Displace(4*len, dataptr'), ptrtemp);
         move(ptrtemp, dataptr)
(*         V.addl(Immed(4*len), dataptr)	*)
     end

fun select(i, Direct r, s) = move(Displace(i*4,r),s)
  | select(0, a, s) = move(defer a, s)	(* BUGBUG *)

fun offset(i, Direct r, s) = V.lea(Displace(i*4,r),s)

exception Three
fun three opcode (a,b,c) = 
	    if b=c then opcode(a,b) 
	    else if a=c then (move(a,ptrtemp); three opcode(ptrtemp,b,c))
	    else (move(b,c); opcode(a,c))

(* opcode is commutative *)
fun threec opcode (a,b,c) = 
	     if b=c then opcode(a,c)
	     else if a=c then opcode(b,c)
	     else (move(b,c); opcode(a,c))

(* opcode(x,y) is bop(y,anti(x)) *)
fun threeab opcode anti bop (a,b,c) = 
	     if b=c then opcode(a,c)
	     else if a=c then (anti(c,c); bop(b,c))
	     else (move(b,c); opcode(a,c))

(* The use of three here is silly.
   If opcode is commutative, then only one move should ever
   be necessary, not two, as in the middle case of above.
   Also note that we have few registers, so folding is likely.
   The only non-commutative 3-operation here is sub.
   Subtraction may require a move, but in this case we should
   do a negate instead of a move, then add.
 *)

val add = threec V.addl
val addt = add
val op sub = threeab V.subl V.negl V.addl
val subt = op sub
fun ashl(s, r, d as Direct(Genreg _)) =
        (if r<>d then move(r,d) else ();
         V.ashl(s,d))
fun ashr(Immed i, b, c) = ashl(Immed (~i), b, c)
  | ashr(a,b,c) = (V.negl(a,ptrtemp);
		   ashl(ptrtemp,b,c))
val mul = V.mull
val mult = mul
val divt = V.divl
val andb = threec V.andl
val orb = threec V.orl
val xorb = threec V.xorl
fun notb (a,b) = V.coml(a,b)

fun fetchindexl(Direct x,y,Immed k) = move(Displace(k+k-2,x),y)
  | fetchindexl(Direct x,y,Direct z) = move(Index(Displace(~2,x),z,Word),y)
  | fetchindexl(Immedlab lab, y, Direct z) =
	move(Index(OffAddress(lab,~2), z, Word), y)
fun storeindexl(x,y, Immed 1) = move(x, defer y)
  | storeindexl(x, Direct y, Immed k) = move(x, Displace(k+k-2,y))
  | storeindexl(x, Direct y, Direct z) = move(x,Index(Displace(~2,y),z,Word))

(* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], where y is not x or z *)
fun fetchindexb (x, y, Direct indx) = V.movzbl(Index(defer x, indx, Byte), y)
  | fetchindexb (Direct x, y, Immed indx) = V.movzbl(Displace(indx, x), y)
(* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
fun storeindexb (x, y, Direct indx) = V.movb(x, Index(defer y, indx, Byte))
  | storeindexb (x, Direct y, Immed indx) = V.movb(x, Displace(indx, y))

fun loadfloat(src,dst as Direct(FloatReg _)) = V.movg(defer src,dst)
  | loadfloat _ = ErrorMsg.impossible "ns32/ns32/loadfloat - bad registers"

fun storefloat(src as Direct(FloatReg _), dst) = 
    (V.movg(src, defer dataptr);
     V.movl(Immed desc_real, Displace(~4,dataptr'));
     move(dataptr, dst);
     V.lea(Displace(4*3, dataptr'), ptrtemp);
     move(ptrtemp, dataptr))

fun float f (fp1,fp2,fp3) = (V.movg(fp1,fp3); f(fp2,fp3))

(* fun mnegg(x,y) = (V.negg(defer x, defer dataptr); finishreal y) *)

val mulf = float V.mulg
val divf = float V.divg
val addf = float V.addg
val subf = float V.subg

fun cbranch NEQ = V.bne
  | cbranch EQL = V.beq
  | cbranch LEQ = V.ble
  | cbranch GEQ = V.bge
  | cbranch LSS = V.blt
  | cbranch GTR = V.bgt

fun ibranch (cond, op1, op2, label) =
	(V.cmpl(op1, op2); cbranch cond (defer label))

(* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
fun rangeChk (op1, op2, label) = (V.cmpl(op1, op2); V.bls (defer label))

fun gbranch (cond, op1, op2, label) =
	(V.cmpg(defer op1, defer op2); cbranch cond (defer label))

fun defer' j = fn x => j(defer x)
val jmp = defer' V.jump
val bbs = fn(x,y,l) => (V.tbit(x,y); V.bfs(defer l))

val immed = Immed

val comment = V.comment
end
