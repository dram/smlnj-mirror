signature CMACHINE = sig

type EA
(* the following kinds of effective address are assumed:
    register;
    immediate label:  
	This mode doesn't in fact exist on the Vax or the MC68020,
		    but it can be simulated (e.g. by "move address" 
		    instructions).
    immediate integer literal;
*)

val immed : int -> EA		(* makes the immediate integer mode *)
val isimmed : EA -> int option	(* if a is an immediate integer mode, extracts
				    the integer *)
val isreg : EA -> int option
val eqreg : EA -> EA -> bool

(* DEDICATED REGISTERS *)
(* The following registers can hold pointers or properly tagged integers *)
val exnptr : EA	    (* the current exception-handler *)
val dataptr : EA    (* the boundary between allocated and unused *)
val storeptr : EA   (* the linked list of recently-assigned references *)

(* The following registers may not hold pointers, and may hold untagged ints *)
val arithtemp : EA
val arithtemp2 : EA

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
val realconst : string -> unit  (* put a floating literal into the code *)
val emitstring : string -> unit (* put a literal string into the code
				   (just the chars, no descriptor or length) *)
val emitlab : int * EA -> unit  (* L3: emitlab(k,L2) is equivalent to
				   L3: emitlong(k+L2-L3) *)

val newlabel : unit -> EA	(* create a new label (but don't define it) *)
val define : EA -> unit  (* Associate a label with a point in the code *)

val jmp : EA -> unit	  (* unconditional jump to the address specified *)

val storefield : int * EA -> unit
				(* storefield(i,x) stores  x
				    into the i'th field (counting by 4-byte
				    words, starting at 0) of the new record
				    pointed to by dataptr.  i can range
				    from ~1 on up. *)

val addtodataptr : int -> unit  (* adds 4*arg to the dataptr register *)
val select : int * EA * EA -> unit  (* select(i,x,y) = y <- mem[x+4*i] *)
val offset : int * EA * EA -> unit  (* offset(i,x,y) = y <- x+4*i *)
val sceql : EA * EA * EA * EA -> unit
		(* sceql(x,y,len,lab)
		   x and y each point to strings in memory
		   sceql compares string x with string y up to 4*len bytes
		    and jumps to lab if not equal; doesn't destroy x or y *)


val store : EA * EA -> unit	    (* store(x,y)  mem[y] <- x *)
val fetchindexb : EA * EA -> unit   (* fetchindexb(x,y) fetches a byte:
					mem[y+arithtemp] <- x *)
val storeindexb : EA * EA -> unit   (* storeindexb(x,y) stores a byte:
				        y <- mem[x+arithtemp] *)
val jmpindexb : EA -> unit	    (* jmpindexb(x)    pc <- (x+arithtemp) *)
val fetchindexl : EA * EA -> unit   (* fetchindexb(x,y) fetches a word:
					mem[y+4*arithtemp] <- x *)
val storeindexl : EA * EA -> unit   (* storeindexb(x,y) stores a word:
					   y <- mem[x+4*arithtemp] *)

(* the following instructions are really just vax instructions *)
val ashl : EA * EA * EA -> unit
val addl3 : EA * EA * EA -> unit
val subl3 : EA * EA * EA -> unit
val mull2 : EA * EA -> unit
val divl2 : EA * EA -> unit
val bisl3 : EA * EA * EA -> unit
val bbs	  : EA * EA * EA -> unit
val cmpl : EA * EA -> unit
val bneq : EA -> unit
val beql : EA -> unit
val bleq : EA -> unit
val bgeq : EA -> unit
val blss : EA -> unit
val bgtr : EA -> unit

(* these are almost vax instructions, except that in each case
   the EA's are the addresses of the operands, not the values *)
val mnegg  : EA * EA -> unit
val mulg3 : EA * EA * EA -> unit
val divg3 : EA * EA * EA -> unit
val addg3 : EA * EA * EA -> unit
val subg3 : EA * EA * EA -> unit
val cmpg : EA * EA -> unit

val profile : int * int -> unit

val comment : string -> unit

end
