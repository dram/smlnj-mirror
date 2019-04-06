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
  
  datatype instr = 
      STRINGCONST of string               (* constants *)
    | EMITLONG of int
  
    | DEFINE of Label                     (* labels *)
    | EMITLAB of int * Label
  
    | SLT of Register * EA * Register     (* control flow *)
    | BEQ of bool * Register * Register * Label
    | JUMP of Register 
    | SLT_D of Register * Register
    | SEQ_D of Register * Register
    | BCOP1 of bool * Label
  
    | NOP (* no-op for delay slot *)
  
    | ADD of Register * EA * Register     (* arithmetic *)
    | AND of Register * EA * Register
    | OR  of Register * EA * Register
    | XOR of Register * EA * Register
    | SUB of Register * Register * Register
    | MULT of Register * Register
    | DIV of Register * Register
    | MFLO of Register    (* used with 64-bit multiply and divide *)
    | MFHI of Register
  
    | NEG_D of Register * Register
    | MUL_D of Register * Register * Register
    | DIV_D of Register * Register * Register
    | ADD_D of Register * Register * Register
    | SUB_D of Register * Register * Register
  
    | MOVE of EA * Register    (* put something into a register *)
    | LDI_32 of int * Register (* load in a big immediate constant (>16 bits)*)
    | LUI of Register * int    (* Mips lui instruction *)
  
  (* We structure the instruction stream a little bit by factoring          *)
  (* out the different load and store instructions that can occur:          *)
  (* we have load byte, load word, and load to coprocessor (floating point). *)
    | LOAD of size * Register * EA * int  (* load and store *)
    | STORE  of size * Register * EA * int
  
    | SLL of EA * Register * Register     (* shift *)
    | SRA of EA * Register * Register
  
    | COMMENT of string                   (* generates nothing *)
    | MARK                                (* a backpointer *)
  
    | BREAK of int			(* break instruction *)

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
    fun delay f a = kept := NOP :: f a :: !kept
    fun keeplist l = kept := l @ !kept
  
  structure M = struct
      val emitstring = keep STRINGCONST     (* literals *)
      exception BadReal = IEEEReal.BadReal
      val low_order_offset = Emitter.low_order_offset
      val realconst = keep (STRINGCONST o order_real o IEEEReal.realconst)
      val emitlong = keep EMITLONG
    
        fun newlabel () = ref 0
        val define = keep DEFINE
        val emitlab = keep EMITLAB                   (* labels *)
    
      val slt = keep SLT                    (* control flow *)
      val beq = delay BEQ
      val jump = delay JUMP
      val slt_double = delay SLT_D
      val seq_double = delay SEQ_D
      val bcop1 = delay BCOP1
    
      val add = keep ADD                    (* arithmetic *)
      val and' = keep AND
      val or = keep OR
      val xor = keep XOR
      val op sub = keep SUB

     (* Multiplication has a minor complication; the                         *)
     (* result has to be fetched from the LO register.                       *)
      fun mult (op1, op2, result) = keeplist [MFLO result, MULT (op1, op2)]
      val mfhi = keep MFHI

     (* Division has a major complication; I must test for divide by zero *)
     (* since the hardware does not. Remember to interpret this in reverse! *)
      fun op div (op1, op2, result) =
        let val next = newlabel()
        in  keeplist [
              MFLO result,			(* get the result *)
              DEFINE next,			(* skip to here if nonzero *)
	      BREAK 7,				(* signals zerodivide *)
              DIV (op1, op2),			(* divide in delay slot *)
              BEQ (false, Reg 0, op2, next)	(* skip if divisor nonzero *)
            ]
        end
    
      val neg_double = keep NEG_D
      val mul_double = keep MUL_D
      val div_double = keep DIV_D
      val add_double = keep ADD_D
      val sub_double = keep SUB_D
    
      val move = keep MOVE
    
      fun lbu (a,b,c) = delay LOAD (Byte,a,b,c) (* load and store *)
      fun lw (a,b,c)  = delay LOAD (Word,a,b,c)
      fun lwc1 (a,b,c)  = delay LOAD (Floating,a,b,c)
      fun sb (a,b,c)  = keep STORE (Byte,a,b,c)
      fun sw (a,b,c)  = keep STORE (Word,a,b,c)
      fun swc1 (a,b,c)  = delay STORE (Floating,a,b,c)
      val lui = keep LUI
    
      val sll = keep SLL                    (* shift *)
      val sra = keep SRA
    
      fun align() = ()                      (* never need to align on MIPS *)
      val mark = keep (fn () => MARK)
      val comment = keep COMMENT
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
  
  fun needs_a_pcptr(_,SLT(_,Immedlab _,_)) = true
    | needs_a_pcptr(_,ADD(_,Immedlab _,_)) = true
    | needs_a_pcptr(_,AND(_,Immedlab _,_)) = true
    | needs_a_pcptr(_,OR(_,Immedlab _,_)) = true
    | needs_a_pcptr(_,XOR(_,Immedlab _,_)) = true
    | needs_a_pcptr(_,MOVE(Immedlab _,_)) = true
    | needs_a_pcptr(_,LOAD(_,_,Immedlab _,_)) = true
    | needs_a_pcptr(_,STORE(_,_,Immedlab _,_)) = true
    | needs_a_pcptr(_,SLL(Immedlab _,_,_)) = true
    | needs_a_pcptr(_,SRA(Immedlab _,_,_)) = true
    | needs_a_pcptr(1, BEQ _) = false  (* small BEQ's dont need pcptr *)
    | needs_a_pcptr(_, BEQ _) = true   (* but large ones do *)
    | needs_a_pcptr(1, BCOP1 _) = false  (* small BCOP1's dont need pcptr *)
    | needs_a_pcptr(_, BCOP1 _) = true   (* but large ones do *)
    | needs_a_pcptr _ = false

  (* Creating the program counter pointer once, with a bltzal, is not   *)
  (* enough; we have to invalidate the program counter pointer at every     *)
  (* label, since control could arrive at the label from God knows where, and *)
  (* therefore we don't know what the program counter pointer is.           *)
  (*                                                                        *)
  (* We use the function makepcptr to create a new program counter pointer *)
  (* ``on the fly'' while generating code for other instrs.             *)
  (* (I chose not to create a special instr for bltzal, which I     *)
  (* could have inserted at appropriate points in the instruction stream.)  *)
  (* To try and find an odd bug, I'm adding no-ops after each bltzal.   *)
  (* I don't really believe they're necessary.                              *)
  (*                                                                        *)
  (* The function gen, which generates the instructions (or computes    *)
  (* their size), takes three arguments.                                    *)
  (* Third: the list of instructions to be generated (paired with pointers  *)
  (* to their sizes); first: the position (in words) at which to generate   *)
  (* those instructions;  second: the current value of the program counter  *)
  (* pointer (register~31), if known.                                       *)
  (*                                                                        *)
  (* The mutual recursion between gen and makepcptr maintains       *)
  (* the program counter pointer.                                           *)
  (* gen invalidates it at labels, and calls makepcptr to create a valid *)
  (* one when necessary (as determined by needs_a_pcptr).               *)
  
  fun pass emit_now =
  let fun makepcptr(i,x) = 
           (* may need to emit NOP for delay slot if next instr is branch *)
    let val size = case x of ((_,BEQ _)::rest) => 2 
  			 | ((_,BCOP1 _)::rest) => 2 
  			 | _ => 1
    in  if emit_now then (emit(Opcodes.bltzal(0,0));
  			if size=2 then emit(Opcodes.add(0,0,0)) else ())
  		  else ();
        gen(i+size, SOME (i+2), x)
    end
  and gen(i,_,nil) = i
    | gen(i, _, (_,DEFINE lab) :: rest) = (lab := i; gen(i,NONE, rest))
                          (* invalidate the pc pointer at labels *)
    (* may want to do special fiddling with NOPs *)
    | gen(pos, pcptr, x as ((sizeref as ref size, inst) :: rest)) =
         if (pcptr=NONE andalso needs_a_pcptr(size, inst)) then makepcptr(pos,x)
         else if emit_now
  	    then let fun gen1() = gen(pos+size,pcptr,rest) 
		                                  (* generate the rest of the instrs *)
		      open Bits
		      open Opcodes
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
			 base regsiter, then use the lo part as an offset. *)
		      
		      fun emitlong i = emit(rshift(i,16), andb(i,65535))
	                     (* emit one long word (no sign fiddling) *)
		      fun split i = let val hi = rshift(i,16) 
					and lo = andb(i,65535)
		                     in if lo<32768 then (hi,lo) 
						    else (hi+1, lo-65536)
		                    end
		      
		      (* To implement any arithmetic operation, we need to
			 know the register form and the sixteen-bit immediate
			 form.  We will also want the operator from instr,
			 since we do the large immediate via a recursive call.
		         We'll set up a function, arith, that does the job. *)
		      
		      fun arith (opr, rform, iform) =
		         let fun ar (Reg op1, Direct (Reg op2), Reg result) = 
		                      gen1(emit(rform(result,op1,op2)))
		               | ar (Reg op1, Immed op2, Reg result) =
		                      (case size of
		                         1 => gen1(emit(iform(result,op1,op2)))
		                       | 3 => 
		                           gen(pos,pcptr,
		                                (ref 2, LDI_32(op2, Reg tempreg))::
		                                (ref 1, opr(Reg op1, Direct(Reg tempreg), Reg result))::
		                                rest)
		                        | _ => gen(ErrorMsg.impossible 
		                                      "bad size in arith Immed in mipscoder")
		                      )
		               | ar (Reg op1, Immedlab (ref op2), Reg result) =
		                      gen(pos, pcptr, 
		                            (ref (size-1), 
		                                  ADD(Reg pcreg,Immed(4*(op2-(get pcptr))), Reg tempreg))::
		                            (ref 1, opr(Reg op1, Direct(Reg tempreg), Reg result))::
		                            rest)
		         in  ar
		         end

		      fun float3double instruction (Reg op1,Reg op2,Reg result) =
		         gen1(emit(instruction(D_fmt,result,op1,op2)))

		      (* We offer a separate MOVE instruction because of large
			 immediate constants. It is always possible to do 
			 move(src,dest) by doing add(Reg 0,src,dest), but the
			 general form add(Reg i, Immed c, dest)  takes three
			 instructions when c is a large constant (more than
			 16 bits).  Rather than clutter up the code for add 
			 (and or and xor) by trying to recognize register 0,
			 we provide move explicitly. *)

		      (* LDI_32 takes care of the particular case in which we
			 are loading a 32-bit immediate constant into a
			 register. It dates from the bad old days before MOVE,
			 and it might be a good idea  to remove it sometime. *)
		      
		      fun domove (Direct (Reg src), Reg dest) = gen1(emit(add(dest,src,0)))
		        | domove (Immed src, Reg dest) =
		              (case size of
		                  1 (* 16 bits *) => gen1(emit(addi(dest,0,src)))
		                | 2 (* 32 bits *) => 
		      			gen(pos,pcptr,(ref 2, LDI_32(src, Reg dest))::rest)
		                | _ => gen(ErrorMsg.impossible "bad size in domove Immed in mipscoder")
		              )
		        | domove (Immedlab (ref src), Reg dest) =
		              gen(pos, pcptr, 
		                    (ref size, 
		                          ADD(Reg pcreg,Immed(4*(src-(get pcptr))), Reg dest))::rest)

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
		            memop(rform, Reg dest, Direct (Reg pcreg), offset+4*(lab - get pcptr))
		  in  case inst of
		        STRINGCONST s => 
		              let val s' = s ^ "\000\000\000\000"
		              in  gen1(emit_string (4*size) s')
		                                            (* doesn't know Big vs Little-Endian *)
		              end
		      | EMITLONG i => gen1(emitlong i)
		      | DEFINE _ => gen1(ErrorMsg.impossible "generate code for DEFINE in mipscoder")
		      | EMITLAB(i, ref d) => gen1(emitlong((d-pos)*4+i))
		      | ADD stuff => arith (ADD,add,addi) stuff
		      | AND stuff => arith (AND,and',andi) stuff
		      | OR  stuff => arith (OR,or,ori) stuff
		      | XOR stuff => arith (XOR,xor,xori) stuff
		      | SUB (Reg op1, Reg op2, Reg result) => gen1(emit(sub(result,op1,op2)))
		      | DIV (Reg op1, Reg op2) => gen1(emit(div(op1,op2)))
		      | MULT(Reg op1, Reg op2) => gen1(emit(mult(op1,op2)))
		      | MFLO(Reg result) => gen1(emit(mflo(result)))
		      | MFHI(Reg result) => gen1(emit(mfhi(result)))
		      (* When emitting instructions we have to remember the
			 Mips instructions use result on the left, but the 
			 MIPSCODER signature requires result  on the right. *)
		      
		      | NEG_D (Reg op1,Reg result) => gen1(emit(neg_fmt(D_fmt,result,op1)))
		      | MUL_D x => float3double mul_fmt x
		      | DIV_D x => float3double div_fmt x
		      | ADD_D x => float3double add_fmt x
		      | SUB_D x => float3double sub_fmt x
		      | MOVE stuff => domove stuff
		      | LDI_32 (immedconst, Reg dest) =>
		               let val (hi,lo) = split immedconst
		               in  gen1(emit(lui(dest,hi));emit(addi(dest,dest,lo)))
		               end 
		      | LUI (Reg dest,immed16) => gen1(emit(lui(dest,immed16)))
		      (* We use a non-standard nop. There are many Mips 
			 instructions that have no effect, and the standard 
			 one is the word with all zeroes (sll 0,0,0).
			 We use add,  adding 0 to 0 and store the result in 0,
			 because it will be easy to distinguish from a data 
			 word that happens to be zero. (?)   *)
		      
		      | SLT stuff => arith (SLT,slt,slti) stuff
		      | BEQ(b, Reg op1, Reg op2, ref dest) =>
		          if size = 1 then 
		               gen1(emit((if b then beq else bne)(op1,op2,dest-(pos+1))))
		          else gen(pos,pcptr,
		                        (ref 1, BEQ(not b, Reg op1, Reg op2, ref(pos+size)))
		                        ::(ref (size-2), 
		                            ADD(Reg pcreg, Immed(4*(dest-(get pcptr))), Reg tempreg))
		                        ::(ref 1, JUMP(Reg tempreg))
		                        ::rest)
		      | JUMP(Reg dest) => gen1(emit(jr(dest)))
		      | SLT_D (Reg op1, Reg op2) => 
		           gen1(emit(c_lt(D_fmt,op1,op2)))
		      | SEQ_D (Reg op1, Reg op2) => 
		           gen1(emit(c_seq(D_fmt,op1,op2)))
		      | BCOP1(b, ref dest) =>
		          let fun bc1f offset = cop1(8,0,offset)
		      	fun bc1t offset = cop1(8,1,offset)
		          in  if size = 1 then 
		                   gen1(emit((if b then bc1t else bc1f)(dest-(pos+1))))
		              else gen(pos,pcptr,
		                        (ref 1, BCOP1(not b, ref(pos+size)))
		                        ::(ref (size-2), 
		                            ADD(Reg pcreg, Immed(4*(dest-(get pcptr))), Reg tempreg))
		                        ::(ref 1, JUMP(Reg tempreg))
		                        ::rest)
		          end
		      | NOP => gen1(emit(add(0,0,0)))         (* one of the many MIPS no-ops *)
		      | LOAD  (Byte,dest,address,offset) => memop(lbu,dest,address,offset)
		      | LOAD  (Word,dest,address,offset) => memop(lw,dest,address,offset)
		      | LOAD  (Floating,dest,address,offset) => memop(lwc1,dest,address,offset)
		      | STORE (Byte,dest,address,offset) => memop(sb,dest,address,offset)
		      | STORE (Word,dest,address,offset) => memop(sw,dest,address,offset)
		      | STORE (Floating,dest,address,offset) => memop(swc1,dest,address,offset)
		      | SLL (Immed shamt, Reg op1, Reg result) => gen1(
		              if (shamt >= 0 andalso shamt < 32) then emit(sll(result,op1,shamt))
		              else ErrorMsg.impossible ("bad sll shamt "
		      		^ (Integer.makestring shamt) ^ " in mipscoder"))
		      | SLL (Direct(Reg shamt), Reg op1, Reg result) => 
		              gen1(emit(sllv(result,op1,shamt)))
		      | SLL (Immedlab _,_,_) => ErrorMsg.impossible "sll shamt is Immedlab in mipscoder"
		      | SRA (Immed shamt, Reg op1, Reg result) => gen1(
		              if (shamt >= 0 andalso shamt < 32) then emit(sra(result,op1,shamt))
		              else ErrorMsg.impossible ("bad sra shamt "
		      		^ (Integer.makestring shamt) ^ " in mipscoder"))
		      | SRA (Direct(Reg shamt), Reg op1, Reg result) =>
		              gen1(emit(srav(result,op1,shamt)))
		      | SRA (Immedlab _,_,_) => ErrorMsg.impossible "sra shamt is Immedlab in mipscoder"

		      | COMMENT _ => gen1()

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
  	    else
		  (* In  early passes, we'll just need to know how many
		     instructions are required (and that number may change
		     from pass to pass, so it must be recomputed). 
		     In the last pass, the sizes are stable (by definition),
		     so we can look at the sizes to see what instructions 
		     to generate. *)
		  let (* Register operations take one instruction. 
			 Immediate operations take one instruction for
				 16 bit constants,  and 3 for larger constants
				 (since it costs two instructions to load a
				  big immediate constant into a register).
		         An immediate instruction with Immedlab l means that
			 the operand is intended to be the machine address				associated with that label.  To compute that address, 
			 we need to add 4*(l-pcptr) to the contents of 
			 register~pcreg (which holds 4*pcptr), put the results
			 in a register, and operate on that register.  *)
		      
		      fun easize (Direct _) = 1
		        | easize (Immed i) = if abs(i)<32768 then 1 else 3
		        | easize (Immedlab(ref lab)) = 1 + easize(Immed (4*(lab-(get pcptr))))
		      (* Notice we use easize and not movesize in the third 
			 clause because when we reach this point the treatment
			 of a MOVE is the same as that of an ADD.  *)
		      
		      fun movesize (Direct _) = 1
		        | movesize (Immed i) = if abs(i)<32768 then 1 else 2
		        | movesize (Immedlab(ref lab)) = easize(Immed (4*(lab-(get pcptr))))
		      
		      (* The actual registers don't matter for computing sizes,
			 and in fact the value of pcreg is not visible here,
			 so we use an arbitrary register (Reg 0) to compute
			 the size.                                *)
		      
		      fun adrsize(_, Reg _, Direct _, offset) = 
		                  if abs(offset)<32768 then 1 else 3
		        | adrsize(_, Reg _, Immed address, offset) = 
		                  if abs(address+offset) < 32768 then 1 else 2
		        | adrsize(x, Reg dest, Immedlab (ref lab), offset) =
		                  adrsize(x, Reg dest, Direct (Reg 0  (* pcreg in code *) ), 
		                          offset+4*(lab-(get pcptr)))
		      val newsize = case inst of
			  (* String constants are padded with nulls out 
			     to a word boundary.           *)
			    STRINGCONST s => Integer.div(String.length(s)+3,4)
			  | EMITLONG _ => 1

			  (* A DEFINE should never reach this far, 
			     and EMITLAB is almost like an EMITLONG.  *)
			  
			  | DEFINE _ => ErrorMsg.impossible "generate code for DEFINE in mipscoder"
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
			  | ADD(_, ea, _) => easize ea
			  | AND(_, ea, _) => easize ea
			  | OR (_, ea, _) => easize ea
			  | XOR(_, ea, _) => easize ea
			  | SUB _ => 1
			  | DIV (_,_) => 1
			  | MULT (_,_) => 1 
			  | MFLO _ => 1
			  | MFHI _ => 1
			  (* Floating point arithmetic is pretty easy because
			     we always do it in registers. 
			     We also support only double precision. *)
			  | NEG_D _ => 1
			  | MUL_D _ => 1
			  | DIV_D _ => 1
			  | ADD_D _ => 1
			  | SUB_D _ => 1
			  | MOVE (src,_) => movesize src
			  | LDI_32 _ => 2
			  | LUI _ => 1
			  (* Now that we've done arithmetic, we can see how to
			     do control flow without too much trouble.  
			     SLT can be treated just like an arithmetic
			     operator.  BEQ is simple if the address to which
			     we branch is close enough.
			     Otherwise we use the following sequence for 
			     BEQ(Reg op1, Reg op2, ref dest):
			          bne op1,op2,L
			          ADD (Reg pcreg, Immed (4*(dest-pcptr)), 
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
			  
			  | SLT(_, ea, _) => easize ea
			  | BEQ(_,_,_,ref dest) => 
			          if abs((pos+1)-dest) < 32768 then 1
			          else 2+easize (Immed (4*(dest-(get pcptr))))
			  | JUMP _ => 1
			  | SLT_D _ => 1
			  | SEQ_D _ => 1
			  | BCOP1(_,ref dest) => 
			          if abs((pos+1)-dest) < 32768 then 1
			          else 2+easize (Immed (4*(dest-(get pcptr))))
			  | NOP => 1
			  | LOAD  x => adrsize x
			  | STORE x => adrsize x
			  (* For the shift instructions, only register and 
			     immediate operands make sense. 
			     Immediate operands make sense if and only if
			     they are representable in five bits. 
			     If everything is right, these are single
			     instructions. *)
			  | SLL _ => 1  
			  | SRA _ => 1
			  (* Finally, comments are ignored, and marks 
			     (backpointers) are written into the instruction 
			     stream. 
			     Comments are used by the front end to give 
			     diagnostics. In the bad old days we would have
			     had two different MIPSCODERs, one which generated
			     machine code (and ignored comments), and one
			     which wrote out assembly code (and copied
			     comments).  Today we have just one, which means
			     the rerouting of comments takes place 
			     at a much higher level.  Look in mipsglue.sml. *)
			  | COMMENT _ => 0
			  | MARK => 1 
			  | BREAK _ => 1	
		  in  if newsize > size then sizeref := newsize else ();
		      gen(pos+(!sizeref) (* BUGS -- was pos+size*),pcptr,rest)
		  end
  in  gen
  end
  
  (* Sizes of instructions                                       
   Now let's consider the correspondence between our instr type and the 
   actual MIPS instructions we intend to emit.                             
   One important problem to solve is figuring out how big things are,      
   so that we know what addresses to generate for the various labels.      
   We will also want to know what address is currently stored in the  
   program counter regsiter (pcreg),                                       
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
                                                                            
   The second argument to pass indicates for what instructions code     
   is to be generated.                                                      
   It is a record (position of next instruction, program counter pointer 
   if any, remaining instructions to generate [with positions]).                    
         prepare produces two results: the instruction stream with    
   size pointers added, and the total size of code to be generated.         
   We add the total size because that is the only way to find the number    
   of bltzals, which are implicit in the instruction stream.            *)
  
  fun prepare instrs =
   let fun add_positions(done, inst::rest) =  
                   add_positions( (ref 0, inst) :: done, rest)
         | add_positions(done, nil) = done
  
       val instrs' = add_positions(nil, instrs) (* reverse and add ref ints*)
  
       fun passes(oldsize) = 
                  (* make passes with no emission until size is stable*)
          let val size = pass false (0,NONE,instrs')
          in  if size=oldsize then size
              else passes size
          end
    in {size = passes 0, stream = instrs'}
    end
  
  fun assemble instrs =
          pass true (0,NONE,#stream (prepare instrs))
  
  
  fun codegen () = (
      assemble (!kept);
        kept := nil
      )

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
  
    | SLT _ => true
    | BEQ _ => true
    | JUMP _ => true
    | NOP => true
    | SLT_D _ => true
    | SEQ_D _ => true
    | BCOP1 _ => true
  
    | ADD _ => true
    | AND _ => true
    | OR  _ => true
    | XOR _ => true
    | SUB _ => true
    | MULT _ => true
    | DIV _ => true
    | MFLO _ => true
    | MFHI _ => true
  
    | NEG_D _ => true
    | MUL_D _ => true
    | DIV_D _ => true
    | ADD_D _ => true
    | SUB_D _ => true
  
    | MOVE _ => true
    | LDI_32 _ => true
    | LUI _ => true
  
    | LOAD _ => true
    | STORE  _ => true
  
    | SLL _ => true
    | SRA _ => true
  
    | COMMENT _ => false
    | MARK => false
    | BREAK _ => true
  
  fun addstats (counts as {inst,code,data,load,branch,compare}) =
    fn nil => counts
     | (sizeref,first)::(_,NOP)::rest => addstats
            {inst=inst+2, code=code+(!sizeref)+1, data=data,
             load=load+ (case first of LOAD _ => 1 | _ => 0),
             branch=branch +(case first of BEQ _ => 1 | JUMP _ => 1 
  				       | BCOP1 _ => 1 | _ => 0),
  	   compare=compare+(case first of SLT_D _ => 1 | SEQ_D _ => 1 
  					| _ => 0)
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
