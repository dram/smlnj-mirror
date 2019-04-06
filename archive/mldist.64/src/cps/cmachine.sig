(* Copyright 1989 by AT&T Bell Laboratories *)
signature CMACHINE = sig

type EA
(* the following kinds of effective address are assumed:
    register;
    immediate label:  
	This mode doesn't in fact exist on the Vax or the MC68020,
        but it can be simulated (e.g. by "move address" instructions).
    immediate integer literal;  *)

val immed : int -> EA		(* makes the immediate integer mode *)

(* DEDICATED REGISTERS *)
(* The following registers can hold pointers or properly tagged integers *)
val exnptr : EA	    (* the current exception-handler *)
val storeptr : EA   (* the head of the store-list of modified words *)
(* val dataptr : EA    the boundary between allocated and unused *)

(* The following register may not hold pointers, and may hold untagged ints *)
val arithtemps : EA list
(* If arithtemps are present, then every attempt must be made to do 
   "arithmetic" operations in arithtemp and arithtemp2 rather than
   general registers.  This is all for the 68020.  Note that arithtemps
   on that machine are not capable of all the operations
   that "general" registers can do, and vice versa.  
*)

(* The following optional register holds a pointer to the instruction-count 
								array *)
val globalvar : EA option

(* The following registers are not dedicated, and must be all disjoint *)
val standardclosure : EA
val standardarg : EA
val standardcont : EA
val miscregs : EA list

val move : EA * EA -> unit  (* move(a,b)    a -> b *)

val align : unit -> unit  (* ensure that next code is on 4-byte boundary *)
val mark: unit -> unit    (* insert a gc-tag in the code so that next address
			     may be moved into a record *)
val emitlong : int -> unit (* put an 4-byte integer literal into the code *)
exception BadReal of string
val realconst : string -> unit  (* put a floating literal into the code *)
val emitstring : string -> unit (* put a literal string into the code
				   (just the chars, no descriptor or length) *)
val emitlab : int * EA -> unit  (* L3: emitlab(k,L2) is equivalent to
				   L3: emitlong(k+L2-L3) *)

val newlabel : unit -> EA	(* create a new label (but don't define it) *)
val define : EA -> unit  (* Associate a label with a point in the code *)

(* checkLimit (n):
 * Generate code to check the heap limit to see if there is enough free space
 * to allocate n bytes.
 *)
val checkLimit : int -> unit

(* Some machines (eg., SPARC) don't have PC relative addressing of data, so we
 * need to simulate it by dynamically loading a register with a base address
 * for relative addressing.  Since many functions don't need relative addressing,
 * we want to avoid loading the base address when possible.  We do this by keeping
 * track of the addressing requirements of each function.  A function's addressing
 * requirements are either PositionIndependent or Relative.
 *)
datatype addressing = PositionIndependent | Relative
val curFnAddr : addressing ref ref (* the current function addressing needs *)

(* beginStdFn (cl, lab, addr):
 * Note the beginning of a standard function with entry label lab, and
 * register cl containing its closure.  Only architectures that need to simulate
 * PC relative addressing require this.
 *)
val beginStdFn : ((EA * EA * addressing ref) -> unit) option

val jmp : EA -> unit	  (* unconditional jump to the address specified *)

val record : (EA * CPS.accesspath) list * EA -> unit
		 (* makes a new record, puts address of it
		    into the destination specified by the second arg.
		    The contents are numbered from ~1 and up. *)

val select : int * EA * EA -> unit  (* select(i,x,y) = y <- mem[x+4*i] *)
val offset : int * EA * EA -> unit  (* offset(i,x,y) = y <- x+4*i *)

(* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z], where y is not x or z *)
val fetchindexb : EA * EA * EA -> unit
(* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
val storeindexb : EA * EA * EA -> unit

val jmpindexb : EA*EA -> unit	    (* jmpindexb(x,y)    pc <- (x+y) *)
val fetchindexl : EA * EA * EA -> unit   (* fetchindexl(x,y,z) fetches a word:
					   y <- mem[x+2*(z-1)] *)
val storeindexl : EA * EA * EA -> unit   (* storeindexl(x,y,z) stores a word:
	    					mem[y+2*(z-1)] <- x *)

val ashl : EA * EA * EA -> unit  (* shift left: count, src, dest;
				     shift count is non-negative *)
val ashr : EA * EA * EA -> unit  (* shift right: count, src, dest;
				     shift count is non-negative *)
	   
val orb :  EA * EA * EA -> unit  (* bitwise or *)
val andb :  EA * EA * EA -> unit  (* bitwise and *)
val xorb :  EA * EA * EA -> unit  (* bitwise xor *)
val notb :  EA * EA -> unit  (* bitwise complement *)

(* the following instructions are really just vax instructions *)
val addl3 : EA * EA * EA -> unit
val subl3 : EA * EA * EA -> unit	(* subl3(a,b,c):  c <- (b - a) *)

val divl2 : EA * EA -> unit		(* divl2(a,b):  b <- (b div a) *)

(* integer arithmetic with overflow trapping *)
val addl3t : EA * EA * EA -> unit
val subl3t : EA * EA * EA -> unit	(* subl3t(a,b,c):  c <- (b - a) *)
val mull2t : EA * EA -> unit

val bbs	  : EA * EA * EA -> unit

datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR

(* ibranch (cond, a, b, lab):  pc <- lab if (a <cond> b). *)
val ibranch : condition * EA * EA * EA -> unit

(* rangeChk (a, b, lab):  pc <- lab if ((a < 0) or (b <= a)) *)
val rangeChk : EA * EA * EA -> unit

(* these are almost vax instructions, except that in each case
   the EA's are the addresses of the operands, not the values *)
val mulg3 : EA * EA * EA -> unit
val divg3 : EA * EA * EA -> unit
val addg3 : EA * EA * EA -> unit
val subg3 : EA * EA * EA -> unit
val gbranch : condition * EA * EA * EA -> unit

val comment : string -> unit
end
