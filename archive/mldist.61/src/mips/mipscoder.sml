functor MipsCoder(Emitter: EMITTER) : MIPSCODER = struct

  open Emitter
  (* For now, labels are just pointers to integers.                         *)
  (* During code generation, those integers will be set to positions        *)
  (* in the instruction stream, and then they'll be useful as addresses     *)
  (* relative to the program counter pointer (to be held in Reg pcreg).     *)
  
    type Label = int ref

  datatype Register = Reg of int

  datatype EA = Direct of Register
              | Immed of int
              | Immedlab of Label

  (* Here are the instructions that exist.                                  *)
  (* We list them in more or less the order of the MIPSCODER signature.     *)
  
  datatype size = Byte | Word | Floating
  datatype floatop = MULf | DIVf | ADDf | SUBf
  datatype compare = EQf | LTf
  datatype arith = ADD | AND | OR | XOR | SLTU | SLT | SLL | SRA
  datatype multop = MULT | DIV
  datatype multreg = LO | HI

  
  datatype instr = 
      STRINGCONST of string               (* constants *)
    | EMITLONG of int
    | MARK                                (* a backpointer *)
    | EMITLAB of int * Label

    | DEFINE of Label                     (* labels *)
  
    | BEQ of bool * Register * Register * Label (* control flow *)
    | JUMP of Register 
    | BCOP1 of bool * Label
    | BREAK of int			(* break instruction *)
  
    | NOP (* no-op for delay slot *)
  
    | ARITH of arith * Register * EA * Register     (* arithmetic *)
    | SUB of Register * Register * Register

    | MULTOP of multop * Register * Register
    | MF of multreg * Register    (* used with 64-bit multiply and divide *)
  
    | SETFLOAT of compare * Register * Register
    | FLOAT of floatop * Register * Register * Register
  
	   (* We offer a separate MOVE instruction because of large
	      immediate constants. It is always possible to do 
	      move(src,dest) by doing add(Reg 0,src,dest), but the
	      general form add(Reg i, Immed c, dest)  takes three
	      instructions when c is a large constant (more than
	      16 bits).  Rather than clutter up the code for add 
	      (and or and xor) by trying to recognize register 0,
	      we provide move explicitly. *)

    | MOVE of EA * Register    (* put something into a register *)
    | LUI of Register * int    (* Mips lui instruction *)
  
    | LOAD of size * Register * EA * int  (* load and store *)
    | STORE  of size * Register * EA * int
  
  

fun showea(Direct(Reg i)) = "r" ^ makestring i
  | showea(Immed k) = makestring k
  | showea(Immedlab(ref l)) = "L" ^ makestring l

val showinstr =
 fn STRINGCONST s => [".ascii \"", s, "\""]
  | EMITLONG i =>    [".word ", makestring i]
  | MARK => [".mark"]
  | EMITLAB(i,ref j) => [".word L", makestring(j), "+", makestring i]
  | DEFINE(ref j) => ["L", makestring j, ":"]
  | BEQ(b,Reg i,Reg j,ref l) =>
	[(if b then "beq r" else "bneq r"),
		makestring i, ",r", makestring j, ",L", makestring l]
  | JUMP(Reg i) => ["jr r", makestring i]
  | BCOP1(b,ref l) =>
	[(if b then "bc1t L" else "bc1f L"), makestring l]
  | BREAK i => ["break ", makestring i]
  | NOP => ["nop"]
 | ARITH(SRA,Reg s,ea,Reg d) => 
		["sra r",makestring d,",",showea ea,",r",makestring s]
  | ARITH(SLL,Reg s,ea,Reg d) => 
		["sll r",makestring d,",",showea ea,",r",makestring s]
  | ARITH(p,Reg s,ea,Reg d) =>
	[case p of ADD => "add r" | AND => "and r" | OR => "or  r"
		 | XOR => "xor r" | SLT => "slt r" | SLTU => "sltu r",
	  makestring d, ",r", makestring s, ",", showea ea]
  | SUB(Reg s, Reg t, Reg d) =>
	["sub r", makestring s, ",r", makestring t,",r", makestring d]
  | MULTOP(MULT,Reg i,Reg j) => ["mult r",makestring i,",r",makestring j]
  | MULTOP(DIV,Reg i,Reg j) => ["div r",makestring i,",r",makestring j]
  | MF(LO,Reg j) => ["mflo r", makestring j]
  | MF(HI,Reg j) => ["mfhi r", makestring j]
  | SETFLOAT(EQf,Reg i,Reg j) => ["c.eq.d f", makestring i,",f",makestring j]
  | SETFLOAT(LTf,Reg i,Reg j) => ["c.lt.d f", makestring i,",f",makestring j]
  | FLOAT(f,Reg i,Reg j,Reg k) =>
	[(case f of ADDf => "add.d f" | SUBf => "sub.d f"
		  | MULf => "div.d f" | DIVf => "div.d f"),
	 makestring i, ",f", makestring j, ",f", makestring k]
  | MOVE(ea,Reg i) => ["move ", showea ea, ",r", makestring i]
  | LUI(Reg i, v) => ["lui r",makestring i, ",", makestring v]
  | LOAD(f,Reg i,ea,k) =>
	[(case f of Byte => "lbu r" | Word => "lw  r" | Floating => "lwc1 f"),
	 makestring i, ",", showea ea, if k=0 then "" else "+" ^ makestring k]
  | STORE(f,Reg i,ea,k) =>
	[(case f of Byte => "sb  r" | Word => "sw  r" | Floating => "swc1 f"),
	 makestring i, ",", showea ea, if k=0 then "" else "+" ^ makestring k]

val showinstr = implode o showinstr
	
  (* Here is the code that handles the generated stream, kept.          *)
  (* It begins life as nil and returns to nil every time code is    *)
  (* generated.                                                             *)
  (* The function keep is a convenient way of adding a single instr to *)
  (* the list; it's very terse.                                             *)

  (* The basic strategy of the implementation is to hold on, via the kept *)
  (* pointer, to the list of instructions generated so far.                 *)
  (* We use instr for the type of an instruction, so                    *)
  (* kept has type instr list ref.                                  *)
  (*                                                                        *)
  (* The instructions will be executed in the following order: the          *)
  (* instruction at the head of the !kept is executed last.             *)
  (* This enables us to accept calls in the order of execution but          *)
  (* add the new instruction(s) to the list in constant time.               *)

  (* Sometimes we have to add multiple instrs; then we use keeplist.*)
  (* We also define a function delay that is just like a keep but   *)
  (* it adds a NOP in the delay slot.                                       *)
  
    val kept = ref nil : instr list ref
    fun keep f a = kept := f a :: !kept
    fun delay f a = kept := (* NOP :: *) f a :: !kept
    fun keeplist nil = ()
      | keeplist (a::rest) = (kept := a :: !kept; keeplist rest)
  
  structure M = struct
      val emitstring = keep STRINGCONST     (* literals *)
      exception BadReal = IEEEReal.BadReal
      val low_order_offset = Emitter.low_order_offset
      val realconst = keep (STRINGCONST o order_real o IEEEReal.realconst)
      val emitlong = keep EMITLONG
    
        fun newlabel () = ref 0
        val define = keep DEFINE
        val emitlab = keep EMITLAB                   (* labels *)
    
      val beq = delay BEQ
      val jump = delay JUMP
      fun slt_double(a,b) = delay SETFLOAT (LTf,a,b)
      fun seq_double(a,b) = delay SETFLOAT (EQf,a,b)
      val bcop1 = delay BCOP1
    
      fun keeparith i (a,b,c) = keep ARITH (i,a,b,c)

      val add = keeparith ADD                    (* arithmetic *)
      val and' = keeparith AND
      val or = keeparith OR
      val xor = keeparith XOR
      val slt = keeparith SLT
      val sltu = keeparith SLTU

      val op sub = keep SUB

     (* Multiplication has a minor complication; the                         *)
     (* result has to be fetched from the LO register.                       *)
      fun mult (op1, op2, result) = keeplist [MULTOP (MULT,op1, op2),
					      MF(LO,result)]
      fun mfhi r = keep MF (HI,r)

     (* Division has a major complication; I must test for divide by zero *)
     (* since the hardware does not. Remember to interpret this in reverse! *)
      fun op div (op1, op2, result) =
        let val next = newlabel()
        in  keeplist [
              MULTOP(DIV, op1, op2),		(* divide; can be done
						   in delay slot *)
              BEQ (false, Reg 0, op2, next),	(* skip if divisor nonzero *)
	(*    NOP, *)
	      BREAK 7,				(* signals zerodivide *)
              DEFINE next,			(* skip to here if nonzero *)
              MF(LO,result)			(* get the result *)
            ]
        end
    
      fun neg_double _ = ErrorMsg.impossible "mnegg unimplemented"
      fun keepf i (a,b,c) = keep FLOAT (i,a,b,c)

      val mul_double = keepf MULf
      val div_double = keepf DIVf
      val add_double = keepf ADDf
      val sub_double = keepf SUBf
    
      val move = keep MOVE
    
      fun lbu (a,b,c) = delay LOAD (Byte,a,b,c) (* load and store *)
      fun lw (a,b,c)  = delay LOAD (Word,a,b,c)
      fun lwc1 (a,b,c)  = delay LOAD (Floating,a,b,c)
      fun sb (a,b,c)  = keep STORE (Byte,a,b,c)
      fun sw (a,b,c)  = keep STORE (Word,a,b,c)
      fun swc1 (a,b,c)  = keep STORE (Floating,a,b,c)
      val lui = keep LUI
    
      fun sll (a,b,c) = keep ARITH (SLL,b,a,c)
      fun sra (a,b,c) = keep ARITH (SRA,b,a,c)
    
      fun align() = ()                      (* never need to align on MIPS *)
      val mark = keep (fn () => MARK)
      fun comment _ = ()
  end

  open M

  (* Functions that assemble instrs into code *)

  fun get (SOME x) = x 
    | get NONE = ErrorMsg.impossible "missing pcptr in mipscoder"
  
  (* The program counter pointer is a device that enables us to do addressing *)
  (* relative to the pcp register, register 31.                             *)
  (* The need for it arises when we want to access a data element which we know *)
  (* only by its label.                                                     *)
  (* The labels give us addresses relative to the beginning of the function,*)
  (* but we can only use addresses relative to some register.               *)
  (* The answer is to set register~31 with a bltzal instruction,        *)
  (* then use that for addressing.                                          *)
  (*                                                                        *)
  (* The function needs_a_pcptr determines when it is necessary         *)
  (* to have a known value in register~31.                                  *)
  (* That is, we need the program counter pointer                           *)
  (* 1. at any operation that uses an effective address that refers to a  *)
  (* label (since all labels have to be relative to the program counter).   *)
  (* 2. BEQ's and BCOP1's to very far away,                                 *)
  (* 	since we have to compute the address for a JUMP                     *)
  (*    knowing the value of the program counter pointer.                   *)
  
  local val shortrange = 32768 - 20 (* allow some slop *)
       (* can't use absolute value, because of most-negative-integer problem *)
    in fun is_short x = x > ~shortrange  andalso x < shortrange
   end


  fun needs_a_pcptr(_,ARITH(_,_,Immedlab _,_)) = true
    | needs_a_pcptr(_,MOVE(Immedlab _,_)) = true
    | needs_a_pcptr(_,LOAD(_,_,Immedlab _,_)) = true
    | needs_a_pcptr(_,STORE(_,_,Immedlab _,_)) = true
    | needs_a_pcptr(1, BEQ _) = false  (* small BEQ's dont need pcptr *)
    | needs_a_pcptr(_, BEQ _) = true   (* but large ones do *)
    | needs_a_pcptr(1, BCOP1 _) = false  (* small BCOP1's dont need pcptr *)
    | needs_a_pcptr(_, BCOP1 _) = true   (* but large ones do *)
    | needs_a_pcptr _ = false

 fun isbranch(JUMP _) = true
   | isbranch(BEQ _ ) = true
   | isbranch(BCOP1 _) = true
   | isbranch _ = false

	(* avoid putting a BLTZAL in the delay slot of a branch;
	   partly because of the no-branch-in-branch rule, but mostly
	   because the branch scheduler carefully put an instruction
	   there that must be executed even if the branch succeeds.
	     By the way, we are allowing a branch in the delay
	   slot of a failing BLTZAL, but not vice versa. *)	   
 fun need_bltzal(NONE, (ref s, i)::(ref s',i')::_) = 
	needs_a_pcptr(s,i) orelse isbranch i' andalso needs_a_pcptr(s',i')
   | need_bltzal(NONE, (ref size, instr)::_) = needs_a_pcptr(size,instr)
   | need_bltzal _ = false

  (* Creating the program counter pointer once, with a bltzal, is not   *)
  (* enough; we have to invalidate the program counter pointer at every     *)
  (* label, since control could arrive at the label from God knows where, and *)
  (* therefore we don't know what the program counter pointer is.           *)
  (*                                                                        *)
  (* We use create a new program counter pointer *)
  (* ``on the fly'' while generating code for other instrs.             *)
  (* (I chose not to create a special instr for bltzal, which I     *)
  (* could have inserted at appropriate points in the instruction stream.)  *)
  (*  We're allowing a branch in the "delay" slot of a bltzal, since    *)
  (* the bltzal always fails.  We don't know if this is strictly legal. *)
  (*                                                                        *)
  (* The function gen, which generates the instructions (or computes    *)
  (* their size), takes three arguments.                                    *)
  (* Third: the list of instructions to be generated (paired with pointers  *)
  (* to their sizes); first: the position (in words) at which to generate   *)
  (* those instructions;  second: the current value of the program counter  *)
  (* pointer (register~31), if known.                                       *)

       fun hexstring(_,0) = ()
         | hexstring(i,k) = (hexstring(i quot 16, k-1);
			     print(substring("0123456789abcdef",(i rem 16),1)))

fun prinstrs(_,_,nil) = ()
  | prinstrs(i,_,(_,DEFINE _)::rest) = prinstrs(i,NONE,rest)
  | prinstrs(i,pcptr,x as (ref size, inst)::rest) = 
	if need_bltzal(pcptr,x)
	then (hexstring(i*4,8);
	      print "  bltzal\n";
	      prinstrs(i+1,SOME(i+2),x))
	else	(hexstring(i*4,8);
		 print "  "; print(showinstr inst); 
		if isbranch inst then print "         XXXXXXXXXXXXX\n" 
				  else 
		 print "\n";
		 prinstrs(i+size,pcptr,rest))


 fun adjust(i,_,nil) = i
   | adjust(i,_,(_,DEFINE lab)::rest) = (lab:=i; adjust(i,NONE,rest))
   | adjust(pos,pcptr,x as ((sizeref as ref size, inst) :: rest)) =
       if need_bltzal(pcptr,x)
       then adjust(pos+1,SOME(pos+2),x)
       else let 
	      (* Register operations take one instruction. 
		Immediate operations take one instruction for
		    16 bit constants,  and 3 for larger constants
		    (since it costs two instructions to load a
		     big immediate constant into a register).
		An immediate instruction with Immedlab l means that
		the operand is intended to be the machine address
		associated with that label.  To compute that address, 
		we need to add 4*(l-pcptr) to the contents of 
		register pcreg (which holds 4*pcptr), put the results
		in a register, and operate on that register.  *)
	   
		fun easize (Direct _) = 1
		  | easize (Immed i) = if is_short i then 1 else 3
		  | easize (Immedlab(ref lab)) = 
				  1 + easize(Immed (4*(lab-(get pcptr))))
	   
		fun adrsize(_, Reg _, Direct _, offset) = 
			     if is_short offset then 1 else 3
		  | adrsize(_, Reg _, Immed address, offset) = 
		    	     if is_short(address+offset) then 1 else 2
		  | adrsize(x, Reg dest, Immedlab (ref lab), offset) =
			      adrsize(x, Reg dest, Direct (Reg 31), 
					 offset+4*(lab-(get pcptr)))
	        val newsize = case inst
		      of DEFINE _ => 0
		       (* String constants are padded with nulls out 
			  to a word boundary.           *)
		       | STRINGCONST s => Integer.div(String.length(s)+3,4)
	   
		       | EMITLONG _ => 1
			(* EMITLAB is almost like an EMITLONG.  *)
		       | EMITLAB _ => 1
		 
		       (* Now we have to start worrying about instructions
			  with EA in them. The real difficulty these things
			  present is that they may have an immediate
			  operand that won't fit in 16 bits.  So we'll need
			  to get this large immediate operand into a
			  register, sixteen bits at a time, and then do the
			  operation on the register. 
			     Since all of the arithmetic instructions have
			  this difficulty, and since we can use them to 
			  implement the others, we'll start with those and
			  catch up with the control-flow instructions later.
			   SUB, MULT, DIV, and MFLO all use registers only, 
			  so they are easy. 
			    The other arithmetic operations get treated
			  exactly the same, so we'll use a function to 
			  compute the size.              *) 
		       | ARITH(_,_, ea, _) => easize ea
		       | SUB _ => 1
		       | MULTOP _ => 1 
		       | MF _ => 1
		       (* Floating point arithmetic is pretty easy because
			  we always do it in registers. 
			  We also support only double precision. *)
		       | FLOAT _ => 1
		       | MOVE(Direct _,_) => 1
		       | MOVE(Immed i, _) => if is_short i then 1 else 2
		       | MOVE(Immedlab(ref lab),_) => 
					 easize(Immed (4*(lab-(get pcptr))))
		       | LUI _ => 1
		       (* Now that we've done arithmetic, we can see how to
			  do control flow without too much trouble.  
			  BEQ is simple if the address to which
			  we branch is close enough.
			  Otherwise we use the following sequence for 
			  BEQ(Reg op1, Reg op2, ref dest):
			       bne op1,op2,L
			       ARITH(ADD, Reg pcreg, Immed (4*(dest-pcptr)), 
				 Reg tempreg)
			       jr tempreg
			    L: ... 
			  Notice we don't have to put a NOP in the delay
			  slot of the bne.  We don't need one after the 
			  jump unless we needed one after the original BEQ,
			  in which case one will be there. 
			  If the branch is taken, we're doing as well as we
			  can. If the branch is not taken, we will have
			  executed an add or lui in the delay slot of the
			  bne, but the results just get thrown away.  *)
		       
		       | BEQ(_,_,_,ref dest) => 
			       if is_short((pos+1)-dest) then 1
			       else 2+easize (Immed (4*(dest-(get pcptr))))
		       | JUMP _ => 1
		       | SETFLOAT _ => 1
		       | BCOP1(_,ref dest) => 
			       if is_short((pos+1)-dest) then 1
			       else 2+easize (Immed (4*(dest-(get pcptr))))
		       | NOP => 1
		       | LOAD  x => adrsize x
		       | STORE x => adrsize x
		 
		       | MARK => 1 
		       | BREAK _ => 1	
	       val newsize = max(size,newsize)
	    in sizeref := newsize;
       	       adjust(pos+newsize,pcptr,rest)
	   end
		 
 fun gen(pos,_,nil) = pos
   | gen(pos, _, (_,DEFINE lab) :: rest) = (lab := pos; gen(pos,NONE, rest))
                          (* invalidate the pc pointer at labels *)
   | gen(pos, pcptr, x as (ref size, inst) :: rest) =
      if need_bltzal(pcptr,x)
      then (emit(Opcodes.bltzal(0,0));
	    gen(pos+1,SOME(pos+2),x))
      else let
           fun gen1() = gen(pos+size,pcptr,rest) 
	   open Bits Opcodes
	   (* When we get around to generating code, we may need to
	      use a temporary register. For example, if we want to
	      load into a register an immediate constant that won't
	      fit into 16 bits, we will have to load the high-order
	      part of the constant with lui, then use addi to add 
	      then the low-order part. The MIPS assembler has a 
	      similar problem, and on page D-2 of the MIPS book we
	      notice that register 1 is reserved for the use of the
	      assembler.  So we do the same.
		   We need to reserve a second register for use in
	      pointing to the program counter.  We will use 
	      register 31 because the bltzal instruction 
	      automatically sets register 31 to the PC.      *)

	   val tempreg = 1
	   val pcreg = 31

	   (* We should point out that we have two different ways
	      of emitting a long word. emitlong just splits the 
	      bits into two pieces for those cases when it's 
	      desirable to put a word into the memory image. 
	      split gives something that will load correctly
	      when the high-order piece is loaded into a high-order
	      halfword  (using lui), and the low-order piece is 
	      sign-extended and then added to the high-order piece.
	      This is the way we load immediate constants of more 
	      than sixteen bits.  It is also useful for generating
	      load or store instructions with offsets of more than
	      sixteen bits: we lui the hi part and add it to the
	      base register, then use the lo part as an offset. *)

	   fun emitlong i = emit(rshift(i,16), andb(i,65535))
		  (* emit one long word (no sign fiddling) *)
	   fun split i = let val hi = rshift(i,16) 
			     and lo = andb(i,65535)
			  in if lo<32768 then (hi,lo) 
					 else (hi+1, lo-65536)
			 end

	   (* Our next problem is to tackle load and store.  The 
	      major difficulty is if the offset is too large to
	      fit in sixteen bits; if so, we have to create a new
	      base register. If we have Immedlab, we do it as an 
	      offset from pcreg.           *)

	   fun memop(rform,Reg dest, Direct (Reg base), offset) =
		 (case size
		  of 1 => gen1(emit(rform(dest,offset,base)))
		   | 3 => let val (hi,lo) = split offset
			  in  gen1(emit(lui(tempreg,hi));       (* tempreg = hi << 16 *)
				   emit(add(tempreg,base,tempreg));(* tempreg += base *)
				   emit(rform(dest,lo,tempreg)) (* load dest,lo(tempreg) *)
				  )
			  end
		   | _ => gen1(ErrorMsg.impossible "bad size in memop Direct in mipscoder")
		  )
	     | memop(rform,Reg dest, Immed address, offset) =
		 (case size
		  of 1 => gen1(emit(rform(dest,offset+address,0)))
		   | 2 => let val (hi,lo) = split (offset+address)
			  in  gen1(emit(lui(tempreg,hi)); 
				   emit(rform(dest,lo,tempreg))
				  )
			  end
		   | _ => gen1(ErrorMsg.impossible "bad size in memop Immed in mipscoder")
		  )
	     | memop(rform,Reg dest, Immedlab (ref lab), offset) =
		 memop(rform, Reg dest, Direct (Reg pcreg), 
			offset+4*(lab - get pcptr))
	   fun rform ADD = add
	     | rform AND = and' 
	     | rform OR = or 
	     | rform XOR = xor 
	     | rform SLT = slt
	     | rform SLTU = sltu

	   fun iform ADD = addi
	     | iform AND = andi 
	     | iform OR = ori
	     | iform XOR = xori
	     | iform SLT = slti
	     | iform SLTU = sltiu

       in  case inst of
	     STRINGCONST s => 
		   let val s' = s ^ "\000\000\000\000"
		   in  gen1(emit_string (4*size) s')
						 (* doesn't know Big vs Little-Endian *)
		   end
	   | EMITLONG i => gen1(emitlong i)
	   | DEFINE _ => gen1(ErrorMsg.impossible "generate code for DEFINE in mipscoder")
	   | EMITLAB(i, ref d) => gen1(emitlong((d-pos)*4+i))

       (* For the shift instructions, only register and 
	  immediate operands make sense. 
	  Immediate operands make sense if and only if
	  they are representable in five bits. 
	  If everything is right, these are single
	  instructions. *)
	   | ARITH (SLL, Reg op1, Immed shamt, Reg result) => gen1(
		   if (shamt >= 0 andalso shamt < 32) then emit(sll(result,op1,shamt))
		   else ErrorMsg.impossible ("bad sll shamt "
		     ^ (Integer.makestring shamt) ^ " in mipscoder"))
	   | ARITH(SLL, Reg op1, Direct(Reg shamt), Reg result) => 
		   gen1(emit(sllv(result,op1,shamt)))
	   | ARITH(SLL,_,_,_) => ErrorMsg.impossible "sll shamt is Immedlab in mipscoder"
	   | ARITH(SRA,Reg op1, Immed shamt, Reg result) => gen1(
		   if (shamt >= 0 andalso shamt < 32) then emit(sra(result,op1,shamt))
		   else ErrorMsg.impossible ("bad sra shamt "
		     ^ (Integer.makestring shamt) ^ " in mipscoder"))
	   | ARITH(SRA, Reg op1, Direct(Reg shamt), Reg result) =>
		   gen1(emit(srav(result,op1,shamt)))
	   | ARITH(SRA,_,_,_) => ErrorMsg.impossible "sra shamt is Immedlab in mipscoder"
	   | ARITH(opr,Reg s,Direct(Reg t),Reg d) =>
			 gen1(emit(rform opr (d,s,t)))
	   | ARITH(opr,Reg s,Immed a,Reg d) =>
		     if size=1
		     then gen1(emit(iform opr(d,s,a)))
		     else gen(pos,pcptr,
			      (ref 2, MOVE(Immed a, Reg tempreg))::
			      (ref 1, ARITH(opr,Reg s,Direct(Reg tempreg), Reg d)) ::
			      rest)
	   | ARITH(opr,Reg s,Immedlab a,Reg d) =>
		     gen(pos,pcptr,
			 (ref (size-1),
			     ARITH(ADD,Reg pcreg,Immed(4*(!a-(get pcptr))), Reg tempreg)) ::
			 (ref 1, ARITH(opr,Reg s,Direct(Reg tempreg),Reg d))::
			 rest)

	   | SUB (Reg op1, Reg op2, Reg result) => gen1(emit(sub(result,op1,op2)))
	   | MULTOP(DIV,Reg op1, Reg op2) => 
				     gen1(emit(div(op1,op2)))
	   | MULTOP(MULT,Reg op1, Reg op2) => 
				     gen1(emit(mult(op1,op2)))
	   | MF(LO,Reg result) => gen1(emit(mflo(result)))
	   | MF(HI,Reg result) => gen1(emit(mfhi(result)))
	   (* When emitting instructions we have to remember the
	      Mips instructions use result on the left, but the 
	      MIPSCODER signature requires result  on the right. *)

	   | FLOAT (i,Reg op1,Reg op2,Reg result) =>
		     let val i' = case i of MULf => mul_fmt 
					  | ADDf => add_fmt
					  | SUBf => sub_fmt
					  | DIVf => div_fmt
		      in gen1(emit(i'(D_fmt,result,op1,op2)))
		     end

	   | MOVE(Direct (Reg src), Reg dest) =>
			gen1(emit(add(dest,src,0)))
	   | MOVE(Immed src, Reg dest) =>
		    if size=1
		    then (* 16 bits *) gen1(emit(addi(dest,0,src)))
		    else (* 32 bits *)
			    let val (hi,lo) = split src
			    in emit(lui(dest,hi)); emit(addi(dest,dest,lo));
			       gen1()
			    end 
	   | MOVE(Immedlab (ref src), Reg dest) =>
		   gen(pos, pcptr, 
		       (ref size, 
		        ARITH(ADD,Reg pcreg,Immed(4*(src-(get pcptr))), 
			      Reg dest))
		       ::rest)

	   | LUI (Reg dest,immed16) => gen1(emit(lui(dest,immed16)))
	   | BEQ(b, Reg op1, Reg op2, ref dest) =>
	       if size = 1 then 
		    gen1(emit((if b then beq else bne)(op1,op2,dest-(pos+1))))
	       else gen(pos,pcptr,
			     (ref 1, BEQ(not b, Reg op1, Reg op2, ref(pos+size)))
			     ::(ref (size-2), 
				 ARITH(ADD,Reg pcreg, Immed(4*(dest-(get pcptr))), Reg tempreg))
			     ::(ref 1, JUMP(Reg tempreg))
			     ::rest)
	   | JUMP(Reg dest) => gen1(emit(jr(dest)))
	   | SETFLOAT (LTf,Reg op1, Reg op2) => 
		gen1(emit(c_lt(D_fmt,op1,op2)))
	   | SETFLOAT (EQf,Reg op1, Reg op2) => 
		gen1(emit(c_seq(D_fmt,op1,op2)))
	   | BCOP1(b, ref dest) =>
	       let fun bc1f offset = cop1(8,0,offset)
	     fun bc1t offset = cop1(8,1,offset)
	       in  if size = 1 then 
			gen1(emit((if b then bc1t else bc1f)(dest-(pos+1))))
		   else gen(pos,pcptr,
			     (ref 1, BCOP1(not b, ref(pos+size)))
			     ::(ref (size-2), 
				 ARITH(ADD,Reg pcreg, Immed(4*(dest-(get pcptr))), Reg tempreg))
			     ::(ref 1, JUMP(Reg tempreg))
			     ::rest)
	       end

	   | NOP => gen1(emit(add(0,0,0)))  (* one of the many MIPS no-ops *)

	   | LOAD  (Byte,dest,address,offset) => memop(lbu,dest,address,offset)
	   | LOAD  (Word,dest,address,offset) => memop(lw,dest,address,offset)
	   | LOAD  (Floating,dest,address,offset) => memop(lwc1,dest,address,offset)
	   | STORE (Byte,dest,address,offset) => memop(sb,dest,address,offset)
	   | STORE (Word,dest,address,offset) => memop(sw,dest,address,offset)
	   | STORE (Floating,dest,address,offset) => memop(swc1,dest,address,offset)


	   (* Just for the record, here's the description of what a
	      mark (backpointer) is.  See also "A Runtime System"
	      by Andrew W. Appel.
	      ``Take the byte address at which the mark resides and
	      add 4, giving the byte address of the object 
	      following the mark. That object is the marked object.
	      Subtract the byte address of the initial word that 
	      marks the start of this instruction stream.
	      Now divide by 4, giving the distance in words between
	      the beginning of the block and the marked object. 
	      Take that quantity and shift it left by multiplying
	      by power_tags, and indicate the result is a mark by
	      adding the tag bits tag_backptr into the low order
	      part.'' 
	      pos+1 is exactly the required distance in words.  *)

	   | MARK => gen1(
	       let open System.Tags
	       in  emitlong((pos+1) * power_tags + tag_backptr)
	       end)
	   | BREAK n => gen1(
	       if n < 0 orelse n > 32 then ErrorMsg.impossible "bad break code"
	       else emit(break n))
       end

  
infix 5 --> -#> -?>

datatype depend = OK | ZERO | ONE | MANY

  fun reads_or_writes(i,instr) =
    case instr
     of BEQ(_,Reg j,Reg k,_) =>  i=j orelse i=k
      | JUMP(Reg j) => i=j
      | BCOP1 _ => false
      | ARITH(_,Reg j, Direct(Reg k),Reg d) => i=j orelse i=k orelse i=d
      | ARITH(_,Reg j, _,Reg d) => i=j orelse i=d
      | SUB(Reg s, Reg t, Reg d) => i=s orelse i=t orelse i=d 
      | MULTOP(_,Reg j,Reg k) => i=j orelse i=k
      | MF(_,Reg j) => i=j
      | MOVE(Direct(Reg j),Reg d) => i=j orelse i=d
      | MOVE(_, Reg d) => i=d
      | LUI(Reg j,_) => i=j
      | LOAD(Floating,_,Direct(Reg j),_) => i=j
      | LOAD(Floating,_,_,_) => false
      | LOAD(_,Reg d,Direct(Reg s),_) => i=d orelse i=s
      | LOAD(_,Reg d,_,_) => i=d
      | STORE(Floating,_,Direct(Reg j),_) => i=j
      | STORE(_,Reg d,Direct(Reg s),_) => i=d orelse i=s
      | STORE(_,Reg d,_,_) => i=d
      | BREAK _ => true  (* hack! *)
      | SETFLOAT _ => false
      | FLOAT _ => false
      | _ => ErrorMsg.impossible "8765 in mipscoder"

  fun target(i,delay,instr as BEQ _) =
		(case (reads_or_writes(i,instr),delay)
		  of (true,ZERO) => ONE
		   | (true,ONE) => MANY
		   | (false,_) => delay)
    | target(i,delay,instr as JUMP _) =
		(case (reads_or_writes(i,instr),delay)
		  of (true,ZERO) => ONE
		   | (true,ONE) => MANY
		   | (false,_) => delay)
    | target(i,delay,instr) = if reads_or_writes(i,instr) then delay else OK

val depend = 
 fn (STRINGCONST _, _) => ZERO
  | (EMITLONG _, _ ) => ZERO
  | (DEFINE _, _ ) => ZERO
  | (EMITLAB _, _ ) => ZERO
  | (MARK, _) => ZERO
  | (BREAK _, _) => ZERO
  | (_, STRINGCONST _ ) => ZERO
  | (_, EMITLONG _ ) => ZERO
  | (_, DEFINE _ ) => ZERO
  | (_, EMITLAB _ ) => ZERO
  | (_, MARK) => ZERO
  | (_, BREAK _) => ZERO
  | (BEQ _, _) => ZERO
  | (JUMP _, _) => ZERO
  | (BCOP1 _, _) => ZERO
  | (NOP, _) => OK
  | (_,NOP) => OK
  | (MULTOP _, MULTOP _) => ZERO
  | (MULTOP _, MF _) => MANY
  | (MULTOP _, _) => OK
	(* these next two are ONE because we aren't smart enough
	   yet to put a load in a branch-delay slot *)
  | (LOAD(Floating,_,_,_), BEQ _) => ONE
  | (LOAD(Floating,_,_,_), BCOP1 _) => ONE
  | (LOAD(Floating,_,_,_), JUMP _) => ZERO
  | (LOAD(Floating,Reg i,_,_), FLOAT(_,Reg j,Reg k,Reg d)) => 
			    if i=j orelse i=j+1 orelse i=k orelse i=k+1
					orelse i=d orelse i=d+1
					then ONE else OK
  | (LOAD(Floating,Reg i,_,_), SETFLOAT(_,Reg j,Reg k)) => 
			    if i=j orelse i=j+1 orelse i=k orelse i=k+1
					then ONE else OK
  | (LOAD(Floating,Reg i,_,_), LOAD(Floating,Reg j,_,_)) =>
					if i=j then ONE else OK
  | (LOAD(Floating,Reg i,_,_), STORE(Floating,Reg j,_,_)) =>
					if i=j then ONE else OK
  | (LOAD(Floating,_,_,_), _) => OK
  | (LOAD(_,Reg i,_,_), x ) =>  target(i,ONE,x)
  | (ARITH(_,_,_,Reg t), x) => target(t,ZERO,x)
  | (SUB(_,_,Reg t), x) => target(t,ZERO,x)
  | (SETFLOAT _, BCOP1 _) => MANY
  | (SETFLOAT _, SETFLOAT _) => ZERO
  | (SETFLOAT _, _) => OK
  | (MF _, MULTOP _) => ZERO
  | (MF(z1,Reg r1), MF(z2,Reg r2)) => if z1=z2 orelse r1<>r2 then OK else ZERO
  | (MF(_,Reg t), x) => target(t,ZERO,x)
  | (FLOAT(_,_,_,Reg i), SETFLOAT(_,Reg j, Reg k)) => 
					if i=j orelse i=k then MANY else OK
  | (FLOAT(_,_,_,Reg i), FLOAT(_,Reg j, Reg k,Reg d)) => 
				if i=j orelse i=k orelse i=d then MANY else OK
  | (FLOAT(_,_,_,Reg i), LOAD(Floating,Reg j,_,_)) =>
				if i=j then MANY else OK
  | (FLOAT(_,_,_,Reg i), STORE(Floating,Reg j,_,_)) =>
				if i=j then MANY else OK
  | (FLOAT _, _) => OK
  | (MOVE(_,Reg i), x) => target(i,ZERO,x)
  | (LUI(Reg i,_), x) => target(i,ZERO,x)
  | (STORE(_,_,Direct(Reg 23),_), ARITH(ADD,Reg 23,_,_)) => ZERO
		(* this serves as a roadblock for later loads that
		might interfere with this store *)
  | (STORE(_,_,Direct(Reg 23),_), _) => OK  (* data-pointer register *)
  | (STORE _, STORE(Word,_,Direct(Reg 23),_)) => OK  (* data-pointer register *)
  | (STORE _, STORE _) => ZERO
  | (STORE _, LOAD _) => ZERO
  | (STORE _, _) => OK


fun a --> b = not(depend(a,b)=OK andalso depend(b,a)=OK)
fun a -#> b = case depend(a,b) of OK => 0 | ZERO => 0 | ONE => 1 | MANY => 2

fun (MULTOP _) -?> (MULTOP _) = false
  | (MULTOP _) -?> _ = true
  | (LOAD _) -?> (LOAD _) = false
  | (LOAD _) -?> _ = true
  | (MF _) -?> (MF _) = false
  | _ -?> (MF _) = true
  | _ -?> _ = false

val nopb0 = ref 0
val nopb1 = ref 0
val nop1 = ref 0
val nop0 = ref 0

  fun addnopb() = nopb1 := !nopb1+1
  fun addnop1() = nop1 := !nop1+1
  fun addnop0() = nop0 := !nop0+1

  fun schedule(revinstrs) =
 let fun f(instrs,change,nochange) =
   case instrs 
    of MARK::(a as DEFINE _)::(rest as ARITH(ADD,_,_,Reg 0)::_) => 
			let fun g(rest) = nochange(MARK::a::rest)
			 in f(rest,g,g) end
     | MARK::(a as DEFINE _)::b::(rest as ARITH(ADD,_,_,Reg 0)::_) =>
			let fun g(rest) = nochange(MARK::a::b::rest)
			 in f(rest,g,g) end
     | (a as MARK)::rest =>
			let fun g(rest) = nochange(MARK::rest)
			 in f(rest,g,g) end
     | (a as DEFINE _)::rest =>
			let fun g(rest) = nochange(a::rest)
			 in f(rest,g,g) end
     | (a as STRINGCONST _)::rest =>
			let fun g(rest) = nochange(a::rest)
			 in f(rest,g,g) end
     | (a as EMITLONG _)::rest =>
			let fun g(rest) = nochange(a::rest)
			 in f(rest,g,g) end
     | (a as EMITLAB _)::rest =>
			let fun g(rest) = nochange(a::rest)
			 in f(rest,g,g) end
     | (a::(bcr as b::c::(rest as d::_))) => 
	if not(b-->c)
	  then let val oldcount = (a-#>b) + (c-#>d)
		   val newcount = (a-#>c) + (b-#>d)
		in if oldcount > newcount
			orelse oldcount=newcount andalso c-?>b
			then let fun g cbd' = f(a::cbd',change,change)
			      in (*print(showinstr c); print "\n"; 
				print(showinstr b);
				print "\n\n"; *)
				 f(c::b::rest,g,g)
			     end
			else f(bcr,
			       fn bcr' => f(a::bcr',change,nochange),
			       fn bcr' => nochange(a::bcr'))
		end
	  else f(bcr, fn bcr' => f(a::bcr',change,nochange),
		      fn bcr' => nochange(a::bcr'))
     | (a::rest) => let fun g r' = nochange(a::r')
		     in f(rest,g,g) end
     | nil => nochange nil

    fun id x = x

  fun addnop(a as LOAD(Floating,_,_,_), rest as (_,b)::_) =
	   if depend(a,b)=ONE
		then (addnop1(); (ref 0,NOP)::rest)
		else (addnop0(); rest)
    | addnop(a as LOAD(_,Reg i,_,_), rest as (_,b)::_) =
	   if reads_or_writes(i,b) 
		then (addnop1(); (ref 0,NOP)::rest)
		else (addnop0(); rest)
    | addnop(a as SETFLOAT _, rest as (_,BCOP1 _)::_) = 
			(addnop1(); (ref 0,NOP)::rest)
    | addnop(a as SETFLOAT _, rest) = (addnop0(); rest)
    | addnop(a as BEQ _, rest) = (addnopb(); (ref 0,NOP)::rest)
    | addnop(a as JUMP _, rest) = (addnopb(); (ref 0,NOP)::rest)
    | addnop(a as BCOP1 _, rest) = (addnopb(); (ref 0,NOP)::rest)
    | addnop(a,rest) = rest

    fun finish(instr::rest,done) =
			(ref 0,instr)::addnop(instr,finish(rest,done))
      | finish(nil,done) = done

    fun reverse(done,new,nil) = finish(f(new,id,id),done)
      | reverse(done,new,(a as DEFINE _)::MARK::rest) =
			reverse(finish(f(MARK::a::new,id,id),done),nil,rest)
      | reverse(done,new,(a as DEFINE _)::rest) =
			reverse(finish(f(a::new,id,id),done),nil,rest)
      | reverse(done,new,a::rest) = reverse(done,a::new,rest)
  in reverse(nil,nil,revinstrs)
 end

fun schedule_branches instrs =
let fun click() = (nopb0 := !nopb0+1; nopb1 := !nopb1-1)
   fun f(instrs) = 
   case instrs
    of (a as (_,MARK))::(b as(_,DEFINE _))::
				(c0 as(_,ARITH(ADD,_,_,Reg 0))::_)=>
			a::b::f(c0)
     | (a as (_,MARK))::(b as(_,DEFINE _))::c::
				(d0 as(_,ARITH(ADD,_,_,Reg 0))::_) =>
			a::b::c::f(d0)
     | (a as (_,BEQ _))::rest => a::f(rest)
     | (a as (_,JUMP _))::rest => a::f(rest)
     | (a as (_,BCOP1 _))::rest => a::f(rest)

	(* we can't put a LOAD in the delay slot of most branches
	   because we don't know whether the branched-to instruction
	   needs the register loaded; however, a JUMP is always to a 
	   "standard" function whose first instruction can't depend on
	     the LOAD because it is the heap limit check.
	  This is slightly conservative for Floating loads (since
	  register j is really a Float register), but we don't
	  expect these to be right before JUMPs anyway.

	Someday, fix this for unconditional BEQ's to known functions
	(but beware of non-folded BEQ's from conditionals) *)
     |   (a as (_,ai))::(b0 as (b as (ref size,bi))::
		 (c as (_,ci))::(_,NOP)::(e0 as (_,ei)::_)) => 
	if (case (bi,ci) of (BCOP1 _,_) => false
			  | (BEQ _,_) => false
			  | (JUMP _, _) => false
			  | (_,JUMP _) => true
			  | (LOAD _, BEQ _) => false
			  | (LOAD _, BCOP1 _) => false
			  | (_, BEQ _) => true
			  | (_, BCOP1 _) => true
			  | _ => false)
	  andalso size=1 andalso bi-#>ci=0
          andalso ai-#>ci <= 1 andalso bi-#>ei=0
	then (click(); a::c::f(b::e0))
	else a::f(b0)

     | a::rest => a::f(rest)
     | nil => nil
 in f(instrs)
end

  (* Sizes of instructions                                       
   Now let's consider the correspondence between our instr type and the 
   actual MIPS instructions we intend to emit.                             
   One important problem to solve is figuring out how big things are,      
   so that we know what addresses to generate for the various labels.      
   We will also want to know what address is currently stored in the  
   program counter register (pcreg),                                       
   because we'll need to know when something is close                      
   enough that we can use a sixteen-bit address relative to that register. 
   The kind of address we can use will determine how big things are.       
                                                                           
   We'll rearrange the code so that we have a list of ref int*instr pairs, 
   where the ref int stores the position in the list.                      
   (Positions start at zero.)                                              
   Since in the MIPS all instructions are the same size, we measure        
   position as number of instructions.                                     
   While we're at it, we reverse the list so that the head will execute    
   first, then the rest of the list.                                       

   We begin with each position set to zero, and make a pass over the list  
   trying to set the value of each position.                               
   We do this by estimating the size of (number of MIPS instructions       
   generated for) each instr.                                              
   Since there are forward references, we may not have all the distances right 
   the first time, so we have to make a second pass.                       
   But during this second pass we could find that something is further away    
   than we thought, and we have to switch from using a pc-relative mode to 
   something else (or maybe grab the new pc?), which changes the size again, 
   and moves things even further away.                                     
   Because we can't control this process, we just keep making passes over the 
   list until the process quiesces (we get the same size twice).           

   In order to guarantee termination, we have to make sure later passes only  
   increase the sizes of things.                                            
   This is sufficient since there is a maximum number of MIPS instructions  
   we can generate for each instr.                                      

   While we're at it, we might want to complicate things by making the
   function that does the passes also emit code.                                     
   For a single pass we hand an optional triple of emitters, the initial 
   position, an int option for the program counter pointer (if known), and the
   instructions.                                                            

   I'm not sure what explains the use of the ref int to track the position, 
   instead of just an int---it might be a desire to avoid the           
   overhead of creating a bunch of new objects, or it might be really hard  
   to do the passes cheaply.                                                
   It should think a variation on map would do the job, but maybe I'm   
   missing something.                                                       
                                                                            
   emit : int * int -> unit emits one instruction,                     
   and emit_string : int -> string -> unit emits a string constant.     
   emit_string could be specified as a function of emit,            
   but the nature of the function would depend on whether the target        
   machine was little-endian or big-endian, and we don't want to have       
   that dependency built in.                                                
                                                                            
    instrs is the list of instructions (in execute-head-last order).
                                                                            
         prepare produces two results: the instruction stream with    
   size pointers added, and the total size of code to be generated.         
   We add the total size because that is the only way to find the number    
   of bltzals, which are implicit in the instruction stream.            *)
  
  fun prepare instrs =
   let fun add_positions(done, (inst as (LOAD _))::rest) =
                   add_positions( (ref 0, inst) :: (ref 0, NOP) :: done, rest)
         | add_positions(done, (inst as (SETFLOAT _))::rest) =
                   add_positions( (ref 0, inst) :: (ref 0, NOP) :: done, rest)
         | add_positions(done, (inst as (BEQ _))::rest) =
                   add_positions( (ref 0, inst) :: (ref 0, NOP) :: done, rest)
         | add_positions(done, (inst as (JUMP _))::rest) =
                   add_positions( (ref 0, inst) :: (ref 0, NOP) :: done, rest)
         | add_positions(done, (inst as (BCOP1 _))::rest) =
                   add_positions( (ref 0, inst) :: (ref 0, NOP) :: done, rest)
         | add_positions(done, inst::rest) =
                   add_positions( (ref 0, inst) :: done, rest)
         | add_positions(done, nil) = done
  
       val _ = (nopb1 := 0; nopb0 := 0; nop1 := 0; nop0 := 0)
       val instrs' = if !System.Control.CG.machdep
			then (if !System.Control.debugging orelse !System.Control.CG.printit
				then print "instruction scheduling\n" else();
			      schedule instrs)
			else add_positions(nil, instrs)
  
       fun passes(instrs,oldsize) = 
                  (* make passes with no emission until size is stable*)
          let val size = adjust (0,NONE,instrs)
          in  if size=oldsize then size
              else passes(instrs,size)
          end

       val _ = if !System.Control.debugging orelse !System.Control.CG.printit
				then print "instruction sizing\n" else()

       val size = passes(instrs',0)

       val (instrs',size) = if !System.Control.CG.misc3 = 1
				then (if !System.Control.debugging orelse !System.Control.CG.printit
					then print "branch scheduling\n" else();
				     let val i' = schedule_branches instrs'
				         val s' = adjust(0,NONE,i')
				      in (i',s')
				     end)
				else (instrs',size)

    in if !System.Control.debugging orelse !System.Control.CG.printit
	   then (print "size="; print size;
	         print ", branchUnfilled="; print(!nopb1);
	         print ", branchFilled="; print(!nopb0);
	         print ", loadUnFilled="; print(!nop1);
	         print ", loadFilled="; print (!nop0); print "\n")
	   else ();
       if !System.Control.CG.printit then prinstrs(0,NONE,instrs') else ();
	{size = size, stream = instrs'}
    end
  
  fun assemble instrs = gen (0,NONE,#stream (prepare instrs))
  
  fun codegen () = (if !System.Control.debugging orelse !System.Control.CG.printit
				then print "assembling\n" else();
		    assemble (!kept); kept := nil)

  (* Optimization                                                *)
  (* The first step towards optimization is to take statistics.             *)
  (* We will count: instrs, Mips words, NOPs in load and branch delays,   *)
  (* and bltzals.                                                         *)
  (* In the current implementation the bltzals are implicit, so there     *)
  (* is no way to count them or optimize them.                            *)
  
  fun printstats stream
   {inst : int, code : int, data : int, 
    load : int, branch : int, compare : int, size : int} =
      let val print = outputc stream
  	val nop = load+branch+compare
  	val bltzal = size - (code + data)
  	val code = code + bltzal
	  val I = Integer.makestring
	  val R = Real.makestring
	  exception Printf
	  fun sprintf format values =
	      let fun merge([x],nil) = [x]
	            | merge(nil,nil) = nil
	            | merge(x::y,z::w) = x::z:: merge(y,w)
	            | merge _ = raise Printf
	      in  implode(merge(format,values))
	      end
  
  	fun P x = substring(makestring(100.0 * x),0,4)  (* percent *)
  	fun printf f d = print (sprintf f d)
      in  printf ["Counted "," instrs in "," words (",
  				" code, "," data)\n" ^
  		"Used "," NOPs ("," load, "," branch,"," compare) and "," bltzals\n" ^
  		"","% of code words were NOPs; ","% were bltzals\n" ^
  		"","% of all words were code; ","% of all words were NOPs\n"]
  	       [I inst, I size, I code, I data, 
                  I nop, I load, I branch,  I compare, I bltzal,
  		P (real nop / real code), P (real bltzal / real code),
  		P (real code / real size), P (real nop / real size)]
  	handle Overflow => print "[Overflow in computing Mips stats]\n"
      end
  		
  val iscode = fn
      STRINGCONST _ => false
    | EMITLONG _ => false
    | DEFINE _ => false
    | EMITLAB _ => false
  
    | BEQ _ => true
    | JUMP _ => true
    | NOP => true
    | SETFLOAT _ => true
    | BCOP1 _ => true
  
    | ARITH _ => true
    | SUB _ => true
    | MULTOP _ => true
    | MF _ => true
  
    | FLOAT _ => true
  
    | MOVE _ => true
    | LUI _ => true
  
    | LOAD _ => true
    | STORE  _ => true
  
    | MARK => false
    | BREAK _ => true
  
  fun addstats (counts as {inst,code,data,load,branch,compare}) =
    fn nil => counts
     | (sizeref,first)::(_,NOP)::rest => addstats
            {inst=inst+2, code=code+(!sizeref)+1, data=data,
             load=load+ (case first of LOAD _ => 1 | _ => 0),
             branch=branch +(case first of BEQ _ => 1 | JUMP _ => 1 
  				       | BCOP1 _ => 1 | _ => 0),
  	   compare=compare+(case first of SETFLOAT _ => 1 | _ => 0)
            } rest
     | (sizeref,first)::rest => addstats
            {inst=inst+1, 
             code = code + if iscode(first) then !sizeref else 0,
             data = data + if not (iscode first) then !sizeref else 0,
             load=load,
             branch=branch,
  	   compare=compare
            } rest
  
  
  fun codestats outfile =
      let val {size,stream=instrs} = prepare (!kept)
  	val zero = {inst=0, code=0, data=0, load=0, branch=0, compare=0}
          val counts as {inst,code,data,load,branch,compare} = 
  						addstats zero instrs
      in  printstats outfile 
  	    {inst=inst,code=code,data=data,
  	     load=load,branch=branch,compare=compare,size=size}
      end
  	
end (* MipsInstr *)
