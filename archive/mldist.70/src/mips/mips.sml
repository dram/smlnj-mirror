functor MipsCM(MipsC : MIPSCODER) : CMACHINE = struct

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
	5--18   yes	miscellaneous registers
	20	yes	varptr
	19	special	data limit; contains MAXINT less the address at
				which to stop allocating, plus 4096
        24      no	instruction counter (if enabled)
	25	no	my_temp2 (internal to CMACHINE)
	21	no	my_temp (internal to CMACHINE)
	22	special	store pointer (list of stored locations)
				used to track mutable (assigned) cells for
				garbage collection
	23	special	data pointer (next available word on heap)
				this points to the first available word in the 
				garbage-collectible area, the word that will 
				be the descriptor of a newly-created object.
	26--27		reserved for operating system kernel
	28		global pointer (used by C)
	29		stack pointer (used by C)
	30	yes	points to current exception handler
	31	special	reserved for the assembler (to hold program counter) *)

    val standardarg = Direct(Reg 2)
    val standardcont = Direct(Reg 3)
    val standardclosure = Direct(Reg 4)
    val miscregs = map (Direct o Reg) [5,6,7,8,9,10,11,12,13,14,
                                       15,16,17,18]
    val floatregs = 
	let fun f n m = if n > m then [] else n :: f (n+2) m
	in Direct(FloatReg 30) :: map (Direct o FloatReg) (f 0 18)
	end 
    val savedfpregs = map (Direct o FloatReg) [20,22,24,26,28] 
    val storeptr  as Direct storeptr'  = Direct(Reg 22)
    val dataptr   as Direct dataptr'   = Direct(Reg 23)
    val limit     as Direct limit'     = Direct(Reg 19)
    val exnptr = Direct(Reg 30)
    val varptr = Direct(Reg 20)
    val varptr_indexable = true
    
      (* internal use only *)
    val my_temp as Direct my_temp' = Direct(Reg 21)
    val my_temp2 as Direct my_temp2' = Direct(Reg 25)
    
      (* exported for external use *)
    val arithtemps = nil

    fun move (Direct(FloatReg fp1), Direct (FloatReg fp2)) =
	M.mov_double(FloatReg fp1, FloatReg fp2)
      | move (Direct(FloatReg _), _) =
	ErrorMsg.impossible "mips/mips/move: destination not a float register"
      | move (_, Direct(FloatReg _)) =
	ErrorMsg.impossible "mips/mips/move: source not a float register" 
      | move (src, Direct dest) = M.move(src, dest)
      | move _ = ErrorMsg.impossible "mips/mips/move"

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
       We can get this by adding 4 to the dataptr'.                   
       We use two different temporaries to aid in instruction scheduling   *)
    
    fun record(vl, Direct z) =
        let open CPS
            val len = List.length vl
            fun f(_,i,nil) = ()
              | f((t1,t2),i,(r, SELp(j,p))::rest) = 
		   (* follow ptrs to get the item *)
                    (M.lw(t1, r, j*4); f((t2,t1),i,(Direct t1,p)::rest))
              | f(t,i,(Direct r,OFFp 0)::rest) = 
                       (* simple store, last first *) 
                    (M.sw(r, dataptr, i*4); f(t,i-1,rest))
              | f((t1,t2),i,(Direct r, OFFp j)::rest) = 
                    (M.add(r, Immed(4*j), t1); 
                                    f((t2,t1),i,(Direct t1,OFFp 0)::rest))
              | f((t1,t2),i,(ea,p)::rest) = (* convert to register-based *)
                    (M.move(ea, t1); f((t2,t1),i,(Direct t1,p)::rest))
          in f((my_temp',my_temp2'),
               len - 1, rev vl); (* store first word in 0(dataptr') *)
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
	  M.move(x, my_temp2'); storeindexb(my_temp2, y, z))
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
        (M.move(Immedlab label,my_temp2');
         storeindexl(my_temp2,y,z))
      | storeindexl(Immed constant,y,offset) =
    	(M.move(Immed constant,my_temp2');
    	 storeindexl(my_temp2,y,offset))

    (* Subtraction may appear a bit odd.                                    *)
    (* The MIPS machine instruction and  MIPSCODER.sub both subtract        *)
    (* their second operand from their first operand.                       *)
    (* The VAX machine instruction and CMACHINE.sub both subtract         *)
    (* their first operand from their second operand.                       *)
    (* This will certainly lead to endless confusion.                       *)
    
    val add = three M.add
    
    fun op sub(Immed k, x, y) = add(x, Immed(~k), y)
      | op sub(Direct x, Direct y, Direct z) = M.sub(y,x,z)
      | op sub(x, Immed 0, dest) = op sub(x, Direct(Reg 0), dest)
      | op sub(x, Immed k, dest) = 
                (M.move(Immed k, my_temp');
                 op sub(x, my_temp, dest))
      | op sub _ = ErrorMsg.impossible "sub args don't match in mips"
    
(*
    fun mul(Direct x, Direct y) = (M.mult(y,x); M.mflo y)
      | mul(Immed x, Direct y) = (M.move(Immed x,my_temp');
				    M.mult(y,my_temp'); M.mflo y)
      | mul _ = ErrorMsg.impossible "mul args don't match in mips"
*)

    fun divt(Direct x, Direct y) = M.div(y,x,y)
      | divt(Immed x, Direct y) = (M.move(Immed x,my_temp');
    				M.div(y,my_temp',y))
      | divt _ = ErrorMsg.impossible "divt args don't match in mips"

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
    (* We must also deal with the case of an addt with two Immediate 
	operands, which can be constructed as a hacked-up way of dealing 
        with large constants. *)
    
        fun addt (a as Immed _, b as Immed _, d as Direct dst) =
         (* This should only occur when we need to build a constant larger than
               2^29. *)
                                          (M.move(a, dst); add(b,d,d))
          | addt args = add args
    val subt = op sub

    (* The Mips multiplies two 32-bit quantities to get a 64-bit result.     *)
    (* That result fits in 32 bits if and only if the high-order word is zero*)
    (* or negative one, and it has the same sign as the low order word.      *)
    (* Thus, we can add the sign bit of the low order word to the high order *)
    (* word, and we have overflow if and only if the result is nonzero.      *)
    
    fun mult(Immed x, y) = (M.move(Immed x, my_temp'); mult(my_temp,y))
      | mult(Direct x, Direct y) = 
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
      | mult _ = ErrorMsg.impossible "result of mult not register in mips"

    val orb = three M.or
    val andb = three M.and'
    fun notb (a,b) = op sub(a, Immed ~1, b) (* ~1 - a == one's complement *)
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
    (* We don't make a distinction between general-purpose and floating point*)
    (* registers; it's up to the instructions to know the difference.        *)

    val real_tag = Immed System.Tags.desc_real
    fun store_float(Reg n,ea,offset) = 
        if n mod 2 <> 0 then ErrorMsg.impossible "bad float reg in mips"
        else (M.swc1(Reg(n+1-M.low_order_offset),ea,offset+4);
	      M.swc1(Reg(n+M.low_order_offset),ea,offset))
    fun load_float(Reg dest,src,offset) =
        if dest mod 2 <> 0 then ErrorMsg.impossible "bad float reg in mips"
        else (M.lwc1(Reg(dest+M.low_order_offset),src,offset);
              M.lwc1(Reg(dest+1-M.low_order_offset),src,offset+4))
    fun storefloat(src,dst) =
	case (src,dst)
	  of (Direct(FloatReg fpr),Direct (Reg result)) => 
	      (store_float(Reg fpr, dataptr, 4);
	       M.move(real_tag,my_temp');
	       M.sw(my_temp',dataptr,0);
	       M.add(dataptr',Immed 4,Reg result);
	       M.add(dataptr',Immed 12,dataptr'))
	   | (Direct(Reg _),_) => 
		 ErrorMsg.impossible "mips: Source of storefloat is a GPReg"
	   | (_, Direct(FloatReg _))=> 
		 ErrorMsg.impossible "mips: Destination of storefloat is a FPReg"
	   | _ => ErrorMsg.impossible "mips: Bad arguments to storefloat"
    fun loadfloat(src, dst) =
	case dst of Direct(FloatReg fpr) => load_float(Reg fpr, src, 0)
                  | _ => ErrorMsg.impossible 
			"mips: Bad destination register for loadfloat"
    fun cast_fp_EA(Direct(FloatReg fp)) = Reg fp
      | cast_fp_EA _ = 
	ErrorMsg.impossible "mips: Did not find a FPR in cast_fp_EA"
    fun cast3instr inst (op1,op2,result) = 
	inst(cast_fp_EA op1, cast_fp_EA op2, cast_fp_EA result)
    fun mnegg(op1,result) = M.neg_double(cast_fp_EA op1, cast_fp_EA result)
    val mulf = cast3instr M.mul_double
    val divf = cast3instr M.div_double
    val addf = cast3instr M.add_double
    val subf = cast3instr M.sub_double
    local
        fun compare(LSS,op1,op2) = (M.slt_double(op1,op2); true)
          | compare(GEQ,op1,op2) = (M.slt_double(op1,op2); false)
          | compare(EQL,op1,op2) = (M.seq_double(op1,op2); true)
          | compare(NEQ,op1,op2) = (M.seq_double(op1,op2); false)
          | compare(LEQ,op1,op2) = compare(GEQ,op2,op1)
          | compare(GTR,op1,op2) = compare(LSS,op2,op1)
    in
    fun gbranch (cond, op1, op2, Immedlab label) = 
	let val regop1 = cast_fp_EA op1
	    val regop2 = cast_fp_EA op2
	in  M.bcop1(compare(cond,regop1,regop2),label)
	end
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

    fun beginStdFn () = ()

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
    val add = diag "add" add
    val op sub = diag "sub" op sub
    val mult = diag "mull2" mult
    val divt = diag "divl2" divt
    val addt = diag "addt" addt
    val subt = diag "subt" subt
    val mull2t = diag "mull2t" mull2t
    val ibranch = diag "ibranch" ibranch
    val jmp = diag "jmp" jmp
    val bbs = diag "bbs" bbs

 -DEBUG *)

end (* MipsCM *)

