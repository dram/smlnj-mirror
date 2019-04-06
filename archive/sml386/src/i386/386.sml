(* Copyright 1989 by      Department of Computer Science, 
 *                        The Technical University of Denmak
 *                        DK-2800 Lyngby 
 *)

(*******************************************************************************
* 25 Oct. 1991    Yngvi Skaalum Guttesen     (ysg@id.dth.dk)
*
* This is the code generator for the 80386
*
*
* Contents : Register definitions
*            Immediate functions
*            Alignment, Marks, and Constants
*            Labels
*            Move
*            Memory check
*            Record manipulation
*            Indexed fetch and store (byte)
*            Indexed fetch and store (word = 4 byte)
*            Utility functions
*            Arithmetic
*            Bitwise operations
*            Shifts
*            Branches
*            Floating point operations
*******************************************************************************)
functor CM386(V : CODER386) : CMACHINE = struct

structure V' :
    sig

	type Label sharing type Label = V.Label

	datatype Size = Byte | Word | Long

	datatype EA = Direct of int
		    | Displace of int * int
		    | Index of int * int * int * Size
		    | Immed of int
		    | Immedlab of Label

	val eax : int	(* = 0 *)
	val ebx : int	(* = 3 *)
	val ecx : int	(* = 1 *)
	val edx : int	(* = 2 *)
	val esi : int	(* = 6 *)
	val edi : int	(* = 7 *)
	val ebp : int	(* = 5 *)
	val esp : int	(* = 4 *)

    end = V

open V'

(************************** Register definitions ******************************
* The 80386 only have 7 registers to be used here. The stack is used to
* hold memory variables when running ML code. Be careful with pushing 
* anything onto the stack because it will invalidate the offsets of the
* stack variables. The runtime system and this code generator must agree on
* the layout of these stack variables.
*)

val exnptr          = Displace(esp, 36)
val storeptr        = Displace(esp, 32)
val standardclosure = Displace(esp, 48)
val standardcont    = Displace(esp, 44) 
val standardarg     = Displace(esp, 40)
val datalimit       = Displace(esp,  0) (* actually a copy of the dataptr *)
                                        (* in the MLState vector          *)
val globalvar       = NONE

val dataptr as Direct dataptr' = Direct(edi)
val arithtemps                 = []
val miscregs                   = map Direct [ebx, edx, esi, ebp]

val temp1 as Direct temp1' = Direct(eax)
val temp2 as Direct temp2' = Direct(ecx)

val trapmi1ptr = Displace(esp,12)
val trapmi2ptr = Displace(esp,16)
val trapvptr   = Displace(esp,20)

val comment = V.comment

(*************************** Immediate functions *****************************)

val immed = Immed     (* make the immediate integer mode *)

(******************** Alignment, marks, and constants ************************)

val align = V.align   (* ensure that next code is on 4-byte boudary *)
val mark  = V.mark    (* insert a gc-tag in the code so that next
                         address may be moved into a record         *)
exception BadReal of string
val emitlong   = V.emitlong   (* put a 4-byte integer literal into the code *)
val realconst  = V.realconst  (* put a floating literal into the code       *)
val emitstring = V.emitstring (* put a literal string into the code
                                 (just the chars , no descriptor or length) *)

(****************************** Labels ***************************************)

exception Emitlab
exception Define

fun newlabel() = Immedlab(V.newlabel()) 
             (* create a new label (but don't define it) *) 

fun emitlab(i,Immedlab lab) = V.emitlab(i,lab)
  | emitlab _ = raise Emitlab
             (* L3: emitlab(k,L2) is equivalent to L3: emitlong(L2+k-L3) *)

fun define (Immedlab lab) = V.define lab
  | define _ = raise Define
             (* Associate a label with a point in the code *)

(******************************* Move ****************************************)
(* move(a,b)       a -> b *)
exception Move
fun move(_, Immed _)    = raise Move
  | move(_, Immedlab _) = raise Move
  | move(lab as Immedlab _, dest as Direct _) = V.lea(lab, dest)
  | move(src as Immed _   , dest)             = V.movl(src, dest)
  | move(src as Direct _  , dest)             = V.movl(src, dest)
  | move(src              , dest as Direct _) = V.movl(src, dest)
  | move(src              , dest)             = (move(src, temp1) ;
                                                 move(temp1, dest))

(***************************** Memory check **********************************)
(* checklimit(n):
 *      generate code to check the heap limit to see if there 
 *      is enough free space to allocate n bytees.
 *)
fun checkLimit maxAllocSize = (if (maxAllocSize <= 4096)
                               then (V.cmpl(dataptr, datalimit);
                                     V.trapmi(trapmi1ptr))
                               else (V.movl(dataptr, temp1);
	                             V.addl2(Immed(maxAllocSize-4096), temp1);
	                             V.cmpl(temp1,datalimit);
	                             V.trapmi(trapmi2ptr)))

(************************* Record manipulation *******************************)

(* record : (EA * CPS.accesspath) list * EA -> unit
 *	makes a new record, puts address of it into the destination
 *	specified by the second arg. The contents are numbered from
 *	~1 and up
 *)
fun record(vl, z) =
    let open CPS
	fun f(Direct r, SELp(j,p)) = f (Displace(r, j*4), p)
	  | f(Immedlab l, p)       = (V.lea(Immedlab l, temp1) ; f(temp1,p))
          | f(x, OFFp 0)           = if x=temp1
		                     then V.stos(x)
		                     else (move(x,temp1); V.stos(temp1))
	  | f(Direct r, OFFp j)    = (V.lea(Displace(r, j*4), temp1) ;
		                      f(temp1, OFFp 0))
	  | f(x,p)                 = (move(x, temp1) ; f(temp1,p))
    in
	app f vl ;
	(case z of
	   (Direct _) => V.lea(Displace(dataptr', ~4*(List.length(vl)-1)), z)
	|  _ => (V.lea(Displace(dataptr', ~4*(List.length(vl)-1)), temp1);
	         V.movl(temp1,z)))
    end

(* select(i, x, y)         mem[x+4*i] -> y *)
exception Select
fun select(i, Direct s, y)          =  move(Displace(s, i*4), y)
  | select(i, x as Displace _, y)   = (move(x,temp1); select(i, temp1, y))
  | select(i, lab as Immedlab _, y) = (V.lea(lab, temp1); select(i,temp1,y))
  | select _ = raise Select

(* offset(i, x, y)         x+4*i -> y     *)
exception Offset
fun offset(i,Direct s,y as Direct _)        = V.lea(Displace(s,i*4),y)
  | offset(i,Direct s,y)                    = (V.lea(Displace(s,i*4),temp1);
                                               move(temp1, y))
  | offset(i,x as Displace _,y as Direct _) = (move(x,temp1); 
                                               offset(i, temp1, y))
  | offset(i,x as Displace _,y)             = (move(x, temp1);
                                               offset(i, temp1, temp1);
                                               move(temp1, y))
  | offset _ = raise Offset

(****************** Indexed fetch and store (byte) ***************************)
(*
 * fetchindexb(x:EA, y:EA, z:EA) fetches a byte:   mem[x+z] -> y
 *                               y CAN be x or z 
 * 
 * storeindexb(x:EA, y:EA, z:EA) stores a byte:    x -> mem[y+z]
 *)
exception Fetchindexb
exception Fidxb
fun fetchindexb(x, y as Direct r, z)   = fidxb(x,y,z)
  | fetchindexb(x, y as Displace _, z) = (fidxb(x,temp1,z); move(temp1,y))
  | fetchindexb _ = raise Fetchindexb

(* don't use temp1 in fidxb *)
and fidxb(x as Direct r  , y, z as Direct s)   = 
					(V.movzx(Index(r,0,s,Byte),y))
  | fidxb(x as Direct r  , y, z as Immed i)    = 
					(V.movzx(Displace(r, i),y))
  | fidxb(x as Direct r  , y, z as Displace _) = 
					(V.movl(z, temp2);
					V.movzx(Index(r,0,temp2',Byte),y))  
  | fidxb(x as Displace _, y, z as Direct r)   = 
					(V.movl(x, temp2); 
					V.movzx(Index(temp2',0,r,Byte),y))
  | fidxb(x as Displace _, y, z as Immed i)    = 
					(V.movl(x, temp2);
					V.movzx(Displace(temp2', i), y))
  | fidxb(x as Displace _, y, z as Displace _) = 
					(V.movl(x,temp2);
					V.addl(z, temp2);
					V.movzx(Displace(temp2',0),y))
  | fidxb _ = raise Fidxb

(* storeindexb(x,y,z) stores a byte:      x -> mem[y+z]
 * The 80386 can only perform byte operations on the al,bl,cl,dl,
 * ah,bh,ch, and dh. When doing byte operations on ebp, esi, and edi 
 * (Direct(i) where i>3) we must use a temporary register.
 *)
exception Storeindexb
exception Sidxb
fun storeindexb(x as Displace _, y, z) = (V.movl(x, temp1); 
                                          sidxb(temp1,y,z))
  | storeindexb(x as Immed _   , y, z) =  sidxb(x,y,z)
  | storeindexb(x as Direct r  , y, z) =  if (r>3)
                                          then (move(x, temp1); 
                                                sidxb(temp1,y,z))
	                                  else sidxb(x,y,z)
  | storeindexb _ = raise Storeindexb

(* Don't use temp1 in sidxb *)
and sidxb(x, y as Displace _, z as Displace _) = 
					(V.movl(y, temp2);
					V.addl(z, temp2);
					V.movb(x, Displace(temp2',0)))
  | sidxb(x, y as Displace _, z as Immed i)    = 
					(V.movl(y, temp2);
					V.movb(x, Displace(temp2', i)))
  | sidxb(x, y as Displace _, z as Direct r)   = 
					(V.movl(y, temp2);
					V.movb(x, Index(temp2',0,r,Byte)))
  | sidxb(x, y as Direct r  , z as Displace _) = 
					(V.movl(z, temp2);
					V.movb(x, Index(r,0,temp2',Byte)))
  | sidxb(x, y as Direct r  , z as Immed i)    =  
					V.movb(x, Displace(r,i))
  | sidxb(x, y as Direct r  , z as Direct s)   =  
					V.movb(x, Index(r,0,s,Byte))
  | sidxb _ = raise Sidxb


(************ Indexed fetch and store (word = 4 byte) ************************)
(* fetchindexl(x,y,z) fetches a word:       mem[x+2*(z-1)] -> y  
 *
 * storeindexl(x,y,z) stores a word:        x -> mem[y+2*(z-1)]
 *)

exception Fetchindexl
exception Fidxl
fun fetchindexl(x, y as Direct _, z)   =  fidxl(x,y,z)
  | fetchindexl(x, y as Displace _, z) = (fidxl(x,temp1,z); move(temp1,y))
  | fetchindexl _ = raise Fetchindexl

(* y is always a Direct register *)
and fidxl(x as Direct r,y,z as Direct s)     = 
				V.movl(Index(r, ~2, s, Word),y)
  | fidxl(x as Direct r,y,z as Immed i)      = 
				V.movl(Displace(r, i+i-2),y)
  | fidxl(x as Direct r,y,z as Displace _)   = 
				(V.movl(z, temp2);
				V.movl(Index(r, ~2, temp2', Word),y)) 
  | fidxl(x as Displace _,y,z as Direct s)   = 
				(V.movl(x, temp2);
				V.movl(Index(temp2', ~2, s, Word),y))
  | fidxl(x as Displace _,y,z as Immed i)    = 
				(V.movl(x, temp2);
				V.movl(Displace(temp2', i+i-2),y))
  | fidxl(x as Displace _,y,z as Displace _) = 
				(V.movl(z,temp2);
				V.movl(x, temp1);
				V.movl(Index(temp1',~2,temp2',Word),y))
  | fidxl(x as Immedlab _,y,z as Direct s)   = 
				(move(x,temp2);
				V.movl(Index(temp2', ~2, s, Word),y))
  | fidxl(x as Immedlab _,y,z as Immed i)    = 
				(move(x, temp2);
				V.movl(Displace(temp2', i+i-2), y))
  | fidxl(x as Immedlab _,y,z as Displace _) = 
				(move(x, temp1);
				V.movl(z,temp2);
				V.movl(Index(temp1',~2,temp2',Word),y))
  | fidxl _ = raise Fidxl

(* storeindexl(x,y,z) stores a word:      x -> mem[y+2*(z-1)]    *)
exception Storeindexl
exception Sidxl
fun storeindexl(x as Displace _, y, z) = (move(x, temp1); sidxl(temp1,y,z))
  | storeindexl(x as Immed _, y, z)    = sidxl(x,y,z)
  | storeindexl(x as Direct _, y, z)   = sidxl(x,y,z)
  | storeindexl _ = ErrorMsg.impossible "bad args to storeindexl in 386.sml"

(* Don't use temp1 in storeidxl !!! *)
and sidxl(x, y as Direct r, z as Direct s)     = 
				V.movl(x, Index(r,~2,s,Word))
  | sidxl(x, y as Direct r, z as Immed i)      = 
				V.movl(x, Displace(r,i+i-2))
  | sidxl(x, y as Direct r, z as Displace _)   = 
				(V.movl(z, temp2);
				V.movl(x, Index(r,~2,temp2',Word)))
  | sidxl(x, y as Displace _, z as Direct r)   = 
				(V.movl(y,temp2);
				V.movl(x, Index(temp2',~2,r,Word)))
  | sidxl(x, y as Displace _, z as Immed i)    = 
				(V.movl(y, temp2);
				V.movl(x, Displace(temp2', i+i-2)))
  | sidxl(x, y as Displace _, z as Displace _) = 
				(V.movl(z, temp2);
				V.asll(Immed 1, temp2);
				V.addl(y,temp2);
				V.movl(x,Displace(temp2',~2)))
  | sidxl _ = raise Sidxl

(******************************** Shifts *************************************)
(* Remember that only ECX can be used at counter 
 * The 80386 only shifts modulo 32 so it is possible that this function  
 * will lead to an error. Only constants are tested here. 
 * ecx = 1 = temp2 
 *)


fun ashl(cnt as Immed k, src, dest) = if k>31 
                                      then ashl'(Immed 31, src, dest)
                                      else ashl'(cnt, src, dest)
  | ashl(cnt as Direct 1, src, dest) = ashl'(cnt, src, dest)
  | ashl(cnt, src, dest) = (move(cnt, temp2); ashl'(temp2, src, dest))

and ashl'(cnt, s, d) = (if s<>d then move(s,d) else (); V.asll(cnt,d) )


fun ashr(cnt as Immed k, src, dest) = if k > 31
				      then ashr'(Immed 31, src, dest)
				      else ashr'(cnt, src, dest)
  | ashr(cnt as Direct 1, src, dest) = ashr'(cnt, src, dest)
  | ashr(cnt, src, dest) = (move(cnt, temp2) ; ashr'(temp2, src, dest))

and ashr'(cnt, s, d) = (if s<>d then move(s,d) else (); V.asrl(cnt,d) )

(*************************** Utility functions *******************************)
(* three opcode (x,y,z) performs the operation: x opcode y -> z for 
 *                      COMMUTATIVE opcodes.
 * three' opcode cmps (x,y,z) performs the same except now it compensates
 * the result for commutativity.
 *)
exception Three
fun three opcode (x, y, z as Direct _) = 
            if x=z then opcode(y,z)
            else if y=z then opcode(x,z)
            else (move(y,z); opcode(x,z))
  | three opcode (x as Displace _, y as Displace _, z as Displace _) =
           if x=z then (move(y,temp1); opcode(temp1,z))
           else if y=z then (move(x,temp1); opcode(temp1,z))
           else (move(y,temp1); opcode(x,temp1); move(temp1,z))
  | three opcode (x as Displace _, y, z as Displace _) =
           if x=z then opcode(y,z) else (move(x,z); opcode(y,z))
  | three opcode (x, y as Displace _, z as Displace _) =
           if y=z then opcode(x,z) else (move(y,z); opcode(x,z))
  | three opcode (x, y, z as Displace _) =
           (move(y,z); opcode(x,z))
  | three _ _ = raise Three

exception Three'
fun three' opcode cmps (x, y, z as Direct _) = 
            if x=z then (opcode(y,z); cmps(z))
            else if y=z then opcode(x,z)
            else (move(y,z); opcode(x,z))
  | three' opcode cmps (x as Displace _, y as Displace _, z as Displace _) =
           if x=z then (move(y,temp1); opcode(temp1,z); cmps(z))
           else if y=z then (move(x,temp1); opcode(temp1,z))
           else (move(y,temp1); opcode(x,temp1); move(temp1,z))
  | three' opcode cmps (x as Displace _, y, z as Displace _) =
           if x=z then (opcode(y,z); cmps(z))
           else (move(x,z); opcode(y,z); cmps(z))
  | three' opcode _ (x, y as Displace _, z as Displace _) =
           if y=z then opcode(x,z) else (move(y,z); opcode(x,z))
  | three' opcode _ (x, y, z as Displace _) =
           (move(y,z); opcode(x,z))
  | three' _ _ _ = raise Three'

(*************************** Arithmetic **************************************)

(* Because we use memory registers we often must perform addl(x,x,x) where
 * x is a memory register. We cannot do memory to memory addition but this
 * is the same as multiplying with 2, that is shifting one to the left. This
 * knowledge can save som moveing to temporary registers.
 *)
fun addl3 (x as Displace _, y as Displace _ , z as Displace _) = 
		if x=y
		then ashl(Immed 1, x, z)
		else three V.addl (x,y,z)
  | addl3 args = three V.addl args

fun subl3 args  = three' V.subl V.negl args

fun addl3t x = (addl3 x ; V.trapv(trapvptr))
fun subl3t x = (subl3 x ; V.trapv(trapvptr))

fun mull2(x as Direct _, y as Direct _) = V.mull(x,y)
  | mull2(x            , y as Direct _) = (move(x, temp2);
	                                   V.mull(temp2,y))
  | mull2(x as Direct _, y)             = (move(y, temp2);
	                                   V.mull(x, temp2);
	                                   move(temp2, y))
  | mull2(x            , y)             = (move(y, temp2) ;
	                                   move(x, temp1) ;
	                                   V.mull(temp1,temp2);
                                           move(temp2, y))

fun mull2t x = (mull2 x ; V.trapv(trapvptr))

(* On the 80386 signed integer division is done with the IDIV instruction.
 *         IDIV src       ; where src is a register or a memory operand
 * The EAX register is the implied destination operand and will hold the
 * result of the division. But EDX will hold the reminder of the division !!!
 * The action taken is : EDX:EAX/src ==> quotien -> eax ; remainder -> EDX
 * That is EDX IS DESTROYED BY THE DIVISION. We must therefor save EDX's  
 * contents if it not is the intendent destination (in wich case it will 
 * by overwritten anyway). Notice also that we must signextend EAX to the
 * EDX:EAX register pair with the CDQ instruction.
 *)
 
fun divl2(x as Immed _, y)  = (V.movl(x,temp2); divl2(temp2,y))
  | divl2(x, y as Direct 2) = (V.movl(y,temp1); 
                               V.cdq(); 
                               V.divl(x); 
                               V.movl(temp1,y))
  | divl2(x,y)              = (V.xchg(y, Direct 2);
                               divl2(x, Direct 2);
                               V.xchg(y,Direct 2))

(************************** Bitwise operations *******************************)

fun notb(a,b) = (move(a,b) ; V.notl(b))
val orb    = three  V.orl  
val xorb   = three  V.xorl 
val andb   = three  V.andl 

(*************************** Branches ***************************************)
exception Jmp
fun jmp(lab as Immedlab _) = V.jra(lab)
  | jmp(x   as Direct _)   = V.jmp(x)
  | jmp(x   as Displace _) = V.jmp(x)
  | jmp _ = raise Jmp

(* jmpindexb(x,y)     (x+y) -> PC     *)
exception Jmpindexb
fun jmpindexb(lab as Immedlab _, indx as Direct _)   = jmpidx(lab, indx)
  | jmpindexb(lab as Immedlab _, indx as Displace _) = jmpidx(lab, indx)
  | jmpindexb _ = raise Jmpindexb

and jmpidx(lab, indx) = ( move(lab, temp1);
                          V.addl(indx, temp1);
                          V.jmp(temp1))

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

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

(* if op1 <cond> op2 then label -> PC else () 
 * Note that cmpl(op1,op2) is equivalent to flags = op2-op1
 * that is if we want to see if op1 <= op2 we have to make 
 * the test cmpl(op2,op1) (op1-op2) and jump on the condition leq
 *)
fun ibranch(cond, op1 as Displace _, op2 as Displace _, label) =
			(move(op1, temp1); ibranch(cond, temp1, op2, label))
  | ibranch(cond, op1 as Immed _, op2, label)  = 
			(V.cmpl(op1, op2) ; cbranch (rev cond) label)
  | ibranch(cond, op1, op2, label)  = 
			(V.cmpl(op2,op1) ; cbranch cond label)

(* if ((a < 0) or (b <= a)) then lab -> PC   *)
exception RangeChk
fun rangeChk(a as Immed _   , b, lab)  = (V.cmpl(a,b);
                                          V.jbe(lab))
  | rangeChk(a as Direct _  , b, lab)  = (V.cmpl(b,a);
                                          V.jae(lab))
  | rangeChk(a as Displace _, b, lab)  = (move(a, temp1);
                                          rangeChk(temp1 ,b, lab))
  | rangeChk _ = raise RangeChk

(* bbs(i, dst, lab): test the i'th bit of dst and jump to lab if it is set.
 * This funktion is only called from one place in GENERIC.SML, and that is 
 * as: bbs(immed 0, regbind x, lab); gen a; genlab(lab, b)
 *)
exception Bbs
fun bbs(x as Immed _, y as Direct _  , l) = (V.btst(x,y) ;
                                             V.jc(l))
  | bbs(x as Immed _, y as Displace _, l) = (V.btst(x,y);
                                             V.jc(l))
  | bbs _ = raise Bbs

(************************** Floating point instructions *********************)
(*
 * These instructions take ML real values as arguments (ie. addresses of
 * heap objects) and store their results on the heap.
 *)

fun finishreal c = (V.movl(Immed(System.Tags.desc_real), temp1);
                    V.stos(temp1) ;
                    move(dataptr,c) ;
                    V.fstp(dataptr) ;
                    V.addl(Immed 8, dataptr))

fun float f (a as Direct _, b as Direct _, c) = ( V.fld(a);
                                                  f(b);
                                                  finishreal(c))
  | float f (a as Direct _, b, c)             = ( move(b,temp2);
                                                  float f (a,temp2,c) )
  | float f (a, b as Direct _, c)             = ( move(a, temp1);
                                                  float f (temp1,b,c) )
  | float f (a,b,c)                           = ( move(a,temp1);
                                                  move(b,temp2);
                                                  float f (temp1,temp2,c) )

val mulg3 = float V.fmul
val divg3 = float V.fdiv
val addg3 = float V.fadd
val subg3 = float V.fsub

fun fcom(a as Direct _, b as Direct _) = ( V.fld(a); 
                                           V.fcomp(b);
                                           V.fnstsw(Direct eax);
                                           V.sahf() )
  | fcom(a as Direct _, b)             = ( move(b,temp2);
                                           fcom(a,temp2) )
  | fcom(a, b as Direct _)             = ( move(a,temp2);
                                           fcom(temp2,b))
  | fcom(a,b)                          = ( move(a,temp1);
                                           move(b,temp2);
                                           fcom(temp1,temp2) )

fun fcbranch NEQ = V.jne
  | fcbranch EQL = V.jeq
  | fcbranch LEQ = V.jbe
  | fcbranch GEQ = V.jae
  | fcbranch LSS = V.jb
  | fcbranch GTR = V.ja

fun gbranch(cond, op1, op2, label) = ( fcom(op1,op2); 
                                       fcbranch cond label)

(*********************** Omitted functions ***********************************)
(* stuff for simulating PC-relative addressing. Not used on 80386 although 
 * we don't have PC-relative addressing on this machine, because we don't
 * have enough registers to save one just for this purpose. Although many
 * functions don't need relative addressing, addressing is always Relative
 * in this version of SML. (se generic)
 *)

datatype addressing = PositionIndependent | Relative

val beginStdFn = NONE

end (* functor I386CM *)







































