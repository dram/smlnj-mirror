functor MipsCM(MipsC : MIPSCODER) : CMACHINE = struct

    val globalvar = NONE;	(* ??? *)

    open MipsC System.Tags

    (* The function "three" makes commutative three-operand               *)
    (* instructions easier to call.                                         *)
    (* All three operands become EAs, and it is enough if either of the     *)
    (* first two operands is a register.                                    *)
    
    fun three f (Direct x, ea, Direct y) = f(x,ea,y)
      | three f (ea, Direct x, Direct y) = f(x,ea,y)
      | three f _ =ErrorMsg.impossible 
				"neither arg to three f is register in mips"


    val immed = Immed

    (* Here's what our register conventions are:
	Reg	G.C.?   Description
	0		zero
	1		reserved for assembler (temporary)
	2	yes	standard argument (saved_ptrs[0])
	3	yes	standard continuation (saved_ptrs[1])
	4	yes	standard closure (saved_ptrs[2])
	5--18,20,25 yes	miscellaneous registers
	19	special	data limit; contains MAXINT less the address at
				which to stop allocating, plus 4096
	21		my_temp (internal to CMACHINE)
	22	special	store pointer (list of stored locations)
				used to track mutable (assigned) cells for
				garbage collection
	23	special	data pointer (next available word on heap)
				this points to the first available word in the 
				garbage-collectible area, the word that will 
				be the descriptor of a newly-created object.
	24		arithmetic temporary (exported by CMACHINE)
	26--27		reserved for operating system kernel
	28		global pointer (used by C)
	29		stack pointer (used by C)
	30	yes	points to current exception handler
	31	special	reserved for the assembler (to hold program counter) *)

    val standardarg = Direct(Reg 2)
    val standardcont = Direct(Reg 3)
    val standardclosure = Direct(Reg 4)
    val miscregs = map (Direct o Reg) [5,6,7,8,9,10,11,12,13,14,
                                       15,16,17,18,20,24,25]
    val storeptr  as Direct storeptr'  = Direct(Reg 22)
    val dataptr   as Direct dataptr'   = Direct(Reg 23)
    val limit     as Direct limit'     = Direct(Reg 19)
    val exnptr = Direct(Reg 30)
    
      (* internal use only *)
    val my_temp as Direct my_temp' = Direct(Reg 21)
    
      (* exported for external use *)
    val arithtemps = nil

    fun move (src,Direct dest) = M.move(src, dest)
      | move _ = ErrorMsg.impossible "destination of move not register in mips"

    val align = M.align
    val mark = M.mark
    
    val emitlong = M.emitlong
    exception BadReal = M.BadReal
    val realconst = M.realconst
    val emitstring = M.emitstring

    fun emitlab(i,Immedlab lab) = M.emitlab(i,lab)
      | emitlab _ = ErrorMsg.impossible "bad emitlab arg in mips"
    fun newlabel() = Immedlab(M.newlabel())
    fun define (Immedlab lab) = M.define lab
      | define _ = ErrorMsg.impossible "bad define arg in mips"

    (* Record manipulation:
       We only ever put the address of a newly created record into a register.
       If I make this out correctly, the first word on the list of 
       values vl is actually a descriptor.
       What needs to go into z is the address of the first word in the record.
       We can get this by adding 4 to the dataptr'.                         *)
    
    fun record(vl, Direct z) =
        let open CPS
            val len = List.length vl
            fun f(i,nil) = ()
              | f(i,(r, SELp(j,p))::rest) = (* follow ptrs to get the item *)
                    (M.lw(my_temp', r, j*4); f(i,(my_temp,p)::rest))
              | f(i,(Direct r,OFFp 0)::rest) =  (* simple store, last first *) 
                    (M.sw(r, dataptr, i*4); f(i-1,rest))
              | f(i,(Direct r, OFFp j)::rest) = 
                    (M.add(r, Immed(4*j), my_temp'); 
                                    f(i,(my_temp,OFFp 0)::rest))
              | f(i,(ea,p)::rest) = (* convert to register-based *)
                    (M.move(ea, my_temp'); f(i,(my_temp,p)::rest))
          in f(len - 1, rev vl); (* store first word in 0(dataptr') *)
             M.add(dataptr', Immed 4, z);
             M.add(dataptr', Immed(4*len), dataptr')
         end
       | record _ = ErrorMsg.impossible "result of record not register in mips"
    
    fun select(i, r, Direct s) = M.lw(s, r, i*4)
      | select _ = ErrorMsg.impossible "result of select not register in mips"
    
    fun offset(i, Direct r, Direct s) = M.add(r,Immed(i*4), s)
      | offset _ = ErrorMsg.impossible "nonregister arg to offset in mips"

    (* For the indexed fetch and store, the index is not tagged--the
       tags are removed at a higher level (in generic.sml).
       These could be made faster for the case when they're called with
       immediate constants as x. *)
    
    (* fetchindexb(x,y,z) fetches a byte: y <- mem[x+z]  *)
    fun fetchindexb (x, Direct y, Immed indx) = M.lbu(y, x, indx)
      | fetchindexb (x, Direct y, Direct indx) = (
          M.add(indx, x, my_temp'); M.lbu(y, my_temp, 0))
      | fetchindexb _ = ErrorMsg.impossible "fetchindexb"

    (* storeindexb(x,y,z) stores a byte: mem[y+z] <- x. *)
    fun storeindexb (x as (Immed _), y, z) = (
	  M.move(x, my_temp'); storeindexb(my_temp, y, z))
      | storeindexb (Direct x, Direct y, indx as (Direct _)) = (
	  M.add(y, indx, my_temp'); M.sb(x, my_temp, 0))
      | storeindexb (Direct x, y, Immed indx) = M.sb(x, y, indx)
      | storeindexb _ = ErrorMsg.impossible "storeindexb"

    (* jmpindexb(x,y)    pc <- (x+y) *)
    fun jmpindexb(x,Direct y) = (M.add(y,x,my_temp'); M.jump(my_temp'))
      | jmpindexb(x,y) = (M.move(y,my_temp');
			  M.add(my_temp',x,my_temp'); M.jump(my_temp'))

    (* Here it looks like z is a tagged integer number of words,            *)
    (* so that 2*(z-1) converts to the appropriate byte offset.             *)
    (* But I'm just guessing.                                                   *)
    (* In any case, it saves an instruction to compute 2*z (actually z+z) *)
    (* and load (or store) with offset ~2. *)

    (* Anything stored with storeindexl is being put into an array, so it   *)
    (* is safe to treat it as a pointer.                                    *)
    
       (* fetchindexl(x,y,z) fetches a word:   y <- mem[x+2*(z-1)] *)
       (* storeindexl(x,y,z) stores a word:    mem[y+2*(z-1)] <- x *)
    
    fun fetchindexl(x,Direct y, Direct z) = 
          (M.sll(Immed 1,z,my_temp');
           M.add(my_temp',x,my_temp');
           M.lw(y, my_temp,~2))
      | fetchindexl(x,Direct y, Immed z) = M.lw(y, x, z+z-2)
      | fetchindexl _ = ErrorMsg.impossible "fetchl result not register in mips"
    
    fun storeindexl(Direct x,y, Immed 1) = M.sw(x,y,0)
      | storeindexl(Direct x,y,Direct z) = 
        (M.sll(Immed 1,z,my_temp');
         M.add(my_temp',y,my_temp');
         M.sw(x, my_temp,~2))
      | storeindexl(Direct x,y,Immed z) = M.sw(x,y,z+z-2)
      | storeindexl(Direct _,_,Immedlab _) =
    	ErrorMsg.impossible "storeindexl(Direct _,_,Immedlab _) in mips"
      | storeindexl(Immedlab label,y,z) =
        (M.move(Immedlab label,my_temp');
         storeindexl(my_temp,y,z))
      | storeindexl(Immed constant,y,offset) =
    	(M.move(Immed constant,my_temp');
    	 storeindexl(my_temp,y,offset))

    (* Subtraction may appear a bit odd.                                    *)
    (* The MIPS machine instruction and  MIPSCODER.sub both subtract        *)
    (* their second operand from their first operand.                       *)
    (* The VAX machine instruction and CMACHINE.subl3 both subtract         *)
    (* their first operand from their second operand.                       *)
    (* This will certainly lead to endless confusion.                       *)
    
    val addl3 = three M.add
    
    fun subl3(Immed k, x, y) = addl3(x, Immed(~k), y)
      | subl3(Direct x, Direct y, Direct z) = M.sub(y,x,z)
      | subl3(x, Immed 0, dest) = subl3(x, Direct(Reg 0), dest)
      | subl3(x, Immed k, dest) = 
                (M.move(Immed k, my_temp');
                 subl3(x, my_temp, dest))
      | subl3 _ = ErrorMsg.impossible "subl3 args don't match in mips"
    
    fun mull2(Direct x, Direct y) = (M.mult(y,x); M.mflo y)
      | mull2(Immed x, Direct y) = (M.move(Immed x,my_temp');
				    M.mult(y,my_temp'); M.mflo y)
      | mull2 _ = ErrorMsg.impossible "mull2 args don't match in mips"
    fun divl2(Direct x, Direct y) = M.div(y,x,y)
      | divl2(Immed x, Direct y) = (M.move(Immed x,my_temp');
    				M.div(y,my_temp',y))
      | divl2 _ = ErrorMsg.impossible "divl2 args don't match in mips"

    fun ashr(shamt, Direct op1, Direct result) = M.sra(shamt,op1,result)
      | ashr(shamt, Immed op1, Direct result) = 
    	(M.move(Immed op1,my_temp'); M.sra(shamt,my_temp',result))
      | ashr _ = ErrorMsg.impossible "ashr args don't match in mips"
    fun ashl(shamt, Direct op1, Direct result) = M.sll(shamt,op1,result)
      | ashl(shamt, Immed op1, Direct result) = 
    	(M.move(Immed op1,my_temp'); M.sll(shamt,my_temp',result))
      | ashl _ = ErrorMsg.impossible "ashl args don't match in mips"

    (* Arithmetic and shifts with overflow detection:                       *)
    (* The Mips hardware detects two's complement integer overflow on       *)
    (* add and subtract instructions only.                                  *)
    (* The exception is not maskable (see the Mips book, page 5-18).        *)
    (* We must also deal with the case of an addl3t with two Immediate 
	operands, which can be constructed as a hacked-up way of dealing 
        with large constants. *)
    
        fun addl3t (a as Immed _, b as Immed _, d as Direct dst) =
         (* This should only occur when we need to build a constant larger than
               2^29. *)
                                          (M.move(a, dst); addl3(b,d,d))
          | addl3t args = addl3 args
    val subl3t = subl3

    (* The Mips multiplies two 32-bit quantities to get a 64-bit result.     *)
    (* That result fits in 32 bits if and only if the high-order word is zero*)
    (* or negative one, and it has the same sign as the low order word.      *)
    (* Thus, we can add the sign bit of the low order word to the high order *)
    (* word, and we have overflow if and only if the result is nonzero.      *)
    
    fun mull2t(Immed x, y) = (M.move(Immed x, my_temp'); mull2t(my_temp,y))
      | mull2t(Direct x, Direct y) = 
        let val ok = M.newlabel()
        in  M.mult(x,y);
	    M.mflo(y);
	    M.slt(y,Direct (Reg 0),y);
	    M.mfhi(my_temp');
	    M.add(my_temp',Direct y,my_temp');
	    M.mflo(y);
	    M.beq(true,my_temp',Reg 0,ok);    (* OK if not overflow *)
	    M.lui(my_temp',32767);
	    M.add(my_temp',my_temp,my_temp');
	    M.define(ok)
        end
      | mull2t _ = ErrorMsg.impossible "result of mull2t not register in mips"

    val orb = three M.or
    val andb = three M.and'
    fun notb (a,b) = subl3(a, Immed ~1, b) (* ~1 - a == one's complement *)
    val xorb = three M.xor

    (* We hack ibranch to make sure it will only reverse once.              *)
    (* It's easier than thinking.                                           *)
    
    datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR
    local 
    fun makeibranch reverse = 
    let
    fun ibranch (cond, Immed a, Immed b, Immedlab label) =
                if (case cond of EQL => a=b | NEQ => a<>b | LSS => a<b |
                                 LEQ => a<=b | GTR => a>b | GEQ => a>=b)
                    then M.beq(true,Reg 0, Reg 0, label) else ()
      | ibranch (NEQ, Direct r, Direct s, Immedlab label) =
                        M.beq(false, r, s, label)
      | ibranch (NEQ, Direct r, x, Immedlab label) =
                        (M.move(x, my_temp');
                         M.beq(false, r, my_temp', label))
      | ibranch (EQL, Direct r, Direct s, Immedlab label) =
                        M.beq(true, r, s, label)
      | ibranch (EQL, Direct r, x, Immedlab label) =
                        (M.move(x, my_temp');
                         M.beq(true, r, my_temp', label))
      | ibranch (LSS, Direct r, x, Immedlab lab) =
                    (M.slt(r,x,my_temp');
                     M.beq(false,Reg 0, my_temp',lab))
      | ibranch (GEQ, Direct r, x, Immedlab lab) =
                    (M.slt(r,x,my_temp'); 
                     M.beq(true,Reg 0, my_temp',lab))
      | ibranch (GTR, x, Direct r, Immedlab lab) =
                    (M.slt(r,x,my_temp'); 
                     M.beq(false,Reg 0, my_temp',lab))
      | ibranch (LEQ, x, Direct r, Immedlab lab) =
                    (M.slt(r,x,my_temp'); 
                     M.beq(true,Reg 0, my_temp',lab))
    (* These two cases added to prevent infinite reversal *)
      | ibranch (GTR, Direct r, x, Immedlab lab) =
    		(M.move(x, my_temp');
    		 M.slt(my_temp',Direct r,my_temp');
    		 M.beq(false,Reg 0,my_temp',lab))
      | ibranch (LEQ, Direct r, x, Immedlab lab) =
    		(M.move(x, my_temp');
    		 M.slt(my_temp',Direct r,my_temp');
    		 M.beq(true,Reg 0,my_temp',lab))
      | ibranch (_, Immedlab _, Immedlab _, _) = 
                    ErrorMsg.impossible "bad ibranch args 1 in mips"
      | ibranch (_, Immedlab _, _, _) = 
                    ErrorMsg.impossible "bad ibranch args 1a in mips"
      | ibranch (_, _, Immedlab _, _) = 
                    ErrorMsg.impossible "bad ibranch args 1b in mips"
      | ibranch (_, _, _, Direct _) = 
                    ErrorMsg.impossible "bad ibranch args 2 in mips"
      | ibranch (_, _, _, Immed _) = 
                    ErrorMsg.impossible "bad ibranch args 3 in mips"
      | ibranch (cond, x, y, l) = 
            let fun rev LEQ = GEQ
                  | rev GEQ = LEQ
                  | rev LSS = GTR
                  | rev GTR = LSS
                  | rev NEQ = NEQ
                  | rev EQL = EQL
            in  if reverse then (makeibranch false) (rev cond, y,x,l) 
    	    else ErrorMsg.impossible "infinite ibranch reversal in mips"
    	
            end
    in ibranch
    end
    in
    val ibranch = makeibranch true
    end
        
    fun rangeChk(Immed a, Immed b, Immedlab lab) =
	if a<0 orelse a>=b then M.beq(true,Reg 0, Reg 0, lab) else ()
      | rangeChk(Immed a, b, Immedlab lab) =
	if a<0 then M.beq(true,Reg 0, Reg 0, lab) 
	       else ibranch(GEQ,Immed a,b, Immedlab lab)
      | rangeChk(Direct a, b, Immedlab lab) =
	  (M.sltu(a,b,my_temp');
	   M.beq(true,Reg 0, my_temp',lab))

    fun jmp (Direct r) = M.jump(r)
      | jmp (Immedlab lab) = M.beq(true,Reg 0,Reg 0,lab)
      | jmp (Immed i) = ErrorMsg.impossible "jmp (Immed i) in mips"
    
    
            (* branch on bit set *)
    fun bbs (Immed k, Direct y, Immedlab label) =
            (M.and'(y,Immed (Bits.lshift(1,k)),my_temp');
             M.beq(false,my_temp',Reg 0,label))
      | bbs _ = ErrorMsg.impossible "bbs args don't match in mips"


    (* Floating point:                                                      *)
    (* We decided not to include floating point registers in our galaxy of   *)
    (* effective addresses.                                                  *)
    (* This means that floating point registers are used only at this level, *)
    (* and only to contain intermediate results.                             *)
    (* All operands and final results will be stored in memory, in the usual *)
    (* ML format (i.e. as 8-byte strings).                                   *)

    (* In fact, we can be much more strict than that, and claim that         *)
    (* all floating point operands will live in FPR0 and FPR2, and that all  *)
    (* results will appear in FPR0.                                          *)

    (* We don't make a distinction between general-purpose and floating point*)
    (* registers; it's up to the instructions to know the difference.        *)
    
    val floatop1 = Reg 0
    val floatop2 = Reg 2
    val floatresult = Reg 0
    
    (* One very common operation is to take the result of a floating point   *)
    (* operation and put it into a fresh record, newly allocated on the heap.*)
    (* This operation is traditionally called finish_real, and it takes one *)
    (* argument, the destination register for the new value.                 *)
    (* All real values on the heap are labelled as 8-byte strings.           *)
    (* To store a floating point, we store the least significant             *)
    (* word in the lower address, but we store the most significant word     *)
    (* first, in case that triggers a garbage collection.                    *)
    
    val real_tag = Immed System.Tags.desc_real
    
    fun store_float(Reg n,ea,offset) = 
        if n mod 2 <> 0 then ErrorMsg.impossible "bad float reg in mips"
        else (M.swc1(Reg(n+1-M.low_order_offset),ea,offset+4);
    	  M.swc1(Reg(n+M.low_order_offset),ea,offset))
    
    fun finish_real (Direct result) = (
        store_float(floatresult,dataptr,4);
        M.move(real_tag,my_temp');
        M.sw(my_temp',dataptr,0);
        M.add(dataptr',Immed 4,result);
        M.add(dataptr',Immed 12,dataptr'))
      | finish_real _ = 
         ErrorMsg.impossible "ptr to result of real operation not register in mips"
    
    (* Loading a floating point quantity is analogous.                       *)
    
    fun load_float(Reg dest,src,offset) =
        if dest mod 2 <> 0 then ErrorMsg.impossible "bad float reg in mips"
        else (M.lwc1(Reg(dest+M.low_order_offset),src,offset);
              M.lwc1(Reg(dest+1-M.low_order_offset),src,offset+4))
    
    (* Now we can do a general two- and three-operand floating point 
	operation. *)
    (* The only parameter is the function in MipsCoder that                 *)
    (* emits the floating point register operation.                         *)
    
    fun two_float instruction (op1,result) = (
        load_float(floatop1,op1,0);
        instruction(floatop1,floatresult);
        finish_real(result))
    
    fun three_float instruction (op1,op2,result) = (
        load_float(floatop1,op1,0);
        load_float(floatop2,op2,0);
        instruction(floatop1,floatop2,floatresult);
        finish_real(result))
    
    val mnegg = two_float M.neg_double
    val mulg3 = three_float M.mul_double
    val divg3 = three_float M.div_double
    val addg3 = three_float M.add_double
    val subg3 = three_float M.sub_double
    
    
    local
        (* Floating point compare:                                          *)
        (* The Mips doesn't provide all six comparisons in hardware, so the *)
        (* next function does the comparison using only less than and equal.*)
        (* The result tells bcop1 whether to branch on condition true       *)
        (* or condition false.                                              *)
        
        fun compare(LSS,op1,op2) = (M.slt_double(op1,op2); true)
          | compare(GEQ,op1,op2) = (M.slt_double(op1,op2); false)
          | compare(EQL,op1,op2) = (M.seq_double(op1,op2); true)
          | compare(NEQ,op1,op2) = (M.seq_double(op1,op2); false)
          | compare(LEQ,op1,op2) = compare(GEQ,op2,op1)
          | compare(GTR,op1,op2) = compare(LSS,op2,op1)
    in
        fun gbranch (cond, op1, op2, Immedlab label) = (
                load_float(floatop1,op1,0);
                load_float(floatop2,op2,0);
                M.bcop1(compare(cond,floatop1,floatop2),label))
          | gbranch _ = ErrorMsg.impossible "insane gbranch target in mips.nw"
    end
    	


    (* Memory check:                                                        *)
    (* When a function begins execution, it checks to make sure there is    *)
    (* sufficient memory available that it can do all its allocation.       *)
    (* generic does this by calling checkLimit : int -> unit.               *)
    (* This is implemented by adding the data pointer to the data limit,    *)
    (* which will cause overflow if there's not enough room.                *)
    
    fun checkLimit max_allocation = 
        if max_allocation <= 4096 then
            M.add(dataptr', limit, Reg 0) (* overflow if out of room *)
        else (
    	M.add(dataptr', Immed(max_allocation-4096), my_temp');
            M.add(my_temp', limit, Reg 0) (* overflow if out of room *)
        )

  (* stuff for simulating PC relative addressing (not used on Mips (yet!)) *)
    datatype addressing = PositionIndependent | Relative
    val beginStdFn = NONE

    val comment = M.comment

(* +DEBUG 
  (* The debugging code replaces possibly offensive functions with functions *)
  (* that diagnose their own exceptions.                                     *)
    
    fun diag (s : string) f x =
    	f x handle e =>
    		(print "?exception "; print (System.exn_name e);
    		 print " in mips."; print s; print "\n";
    		 raise e)

    val emitlab = diag "emitlab" emitlab
    val define = diag "define" define
    val record = diag "record" record
    val select = diag "select" select
    val offset = diag "offset" offset
    val fetchindexb = diag "fetchindexb" fetchindexb
    val storeindexb = diag "storeindexb" storeindexb
    val jmpindexb = diag "jmpindexb" jmpindexb
    val fetchindexl = diag "fetchindexl" fetchindexl
    val storeindexl = diag "storeindexl" storeindexl
    val ashr = diag "ashr" ashr
    val ashl = diag "ashl" ashl
    val orb = diag "orb" orb
    val andb = diag "andb" andb
    val notb = diag "notb" notb
    val xorb = diag "xorb" xorb
    val addl3 = diag "addl3" addl3
    val subl3 = diag "subl3" subl3
    val mull2 = diag "mull2" mull2
    val divl2 = diag "divl2" divl2
    val addl3t = diag "addl3t" addl3t
    val subl3t = diag "subl3t" subl3t
    val mull2t = diag "mull2t" mull2t
    val ibranch = diag "ibranch" ibranch
    val jmp = diag "jmp" jmp
    val bbs = diag "bbs" bbs

 -DEBUG *)

end (* MipsCM *)

