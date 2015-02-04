structure MipsInstr = struct

type Label = int ref
fun newlabel () = ref 0

datatype Register = Reg of int

datatype EA = Direct of Register
	    | Immed of int
	    | Immedlab of Label

datatype size = Byte | Word
datatype mulop = MULT | DIV

datatype instr = 
    MARK | DEFINE of Label | EMITLAB of int * Label
  | EMITLONG of int | REALCONST of string | STRINGCONST of string
  | SLT of Register * EA * Register
  | BEQ of bool * Register * Register * Label
  | JUMP of Register 
  | ADD of Register * EA * Register
  | AND of Register * EA * Register
  | OR  of Register * EA * Register
  | XOR of Register * EA * Register
  | SUB of Register * Register * Register
  | MULOP of mulop * Register * Register
  | MFLO of Register
  | SLL of EA * Register * Register
  | SRA of EA * Register * Register
  | LOAD of size * Register * EA * int
  | STORE  of size * Register * EA * int
  | COMMENT of string
  | NOP

  val newlabel = newlabel

  val kept = ref [EMITLONG 0];

  fun keep f a = kept := f a :: !kept
  val mark = keep MARK
  fun align() = ()
  val define = keep DEFINE
  val emitlab = keep EMITLAB
  val emitlong = keep EMITLONG
  val realconst = keep REALCONST
  val stringconst = keep STRINGCONST
  val slt = keep SLT
  val beq = bubble BEQ
  val jump = bubble JUMP
  val add = keep ADD
  val and' = keep AND
  val or = keep OR
  val xor = keep XOR
  val op sub = keep SUB
  fun mulop f (a,b,c) = kept := MULOP(f,a,b) :: MFLO c :: !kept
  val op div = mulop DIV
  val mult = mulop MULT
  val sll = keep SLL
  val sra = keep SRA
  fun lbu (a,b,c) = kept := LOAD(Byte,a,b,c)::NOP:: !kept;
  fun lw (a,b,c) = kept := LOAD(Word,a,b,c)::NOP:: !kept;
  fun sb (a,b,c) = kept := STORE(Byte,a,b,c):: !kept;
  fun sw (a,b,c) = kept := STORE(Word,a,b,c):: !kept;
  val comment = keep COMMENT

val rec squeeze =

 fn (x as LOAD(_,Reg d, m, i))::NOP::instr::rest =>
	if use(instr,d) then ??
	else squeeze(x::instr::rest)
  | (x as STORE _)::(y as LOAD _)::rest => 
	x :: squeeze(y::rest)
  | instr::(x as LOAD(_,Reg d, Direct(Reg s), i))::NOP::rest =>
	if use(instr, d) orelse gen(instr, s) then ??
	else squeeze(x::instr::rest)
  | instr::(x as LOAD(_,Reg d, _, i))::NOP::rest =>
	if use(instr,d) then ??
	else squeeze(x::instr::rest)
  | (x as MFLO _):: (y as MULOP _) :: rest =>
	x :: squeeze (y::rest)
  | (x as MFLO(Reg d))::instr::rest =>
	if (use(instr,d) orelse gen(instr,d) then ??
	else squeeze(instr::x::rest)
  | instr :: (x as MULOP(Reg a, Reg b)) :: rest =>
	if gen(instr,a) orelse gen(instr,b) then ??
	else squeeze(x::instr::rest)

val rec final =
 fn
  | instr::(x as LOAD(_,Reg d, Direct(Reg s), i))::NOP::rest =>
	if gen(instr, s) then instr::final(x::NOP::rest)
	else x::instr::(final rest)
  | instr :: (x as JUMP _) :: NOP :: rest =>
	x :: instr :: final rest
  | instr :: (x as BEQ(_,Reg a, Reg b, _)) :: NOP :: rest =>
	if gen(instr,a) orelse gen(instr,b) then instr::x::NOP::(final rest)
	else x::instr::(final rest)


fun get (SOME x) = x | get NONE = ErrorMsg.impossible "4342 in mips"

fun pass emit
let fun makepcp(i,x) = 
  (case emit of NONE => () | SOME emit => emit(16384, 0);
   gen(i+1, SOME (i+2), x))
and gen(i,_,nil) = i
  | gen(i, _, (_,DEFINE lab) :: rest) = (lab := i; gen(i,NONE, rest))
  | gen(i, _, (_,NOP)::rest) = makepcp(i,rest)
  | gen(i,NONE, x as (_,SLT(_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,ADD(_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,AND(_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,OR(_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,XOR(_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,LOAD(_,_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (_,STORE(_,_,Immedlab _,_))::_) = makepcp(i,x)
  | gen(i,NONE, x as (ref 5, BEQ _)::_) = makepcp(i,x)
  | gen(pos, pcp, (sizeref as ref size, inst) :: rest) =
 case emit
  of NONE =>
   let fun easize (Reg _) = 1
         | easize (Immed i) = if abs(i)<32768 then 1 else 3
         | easize (Immedlab(ref lab)) =
		   if 4*abs(lab-(get pcp)) < 32768 then 2 else 4
       fun adrsize(Reg _, i, _) = if abs(i)<32768 then 1 else 3
	 | adrsize(Immedlab(ref lab), i, _) =
		   if abs(i+4*(lab-(get pcp))) < 32768 then 1 else 3
	 | adrsize(Immed i, j, _) =
		if abs(i+j) < 32768 then 1 else 2
			    
       val newsize = case inst
    of MARK => 1
     | EMITLAB _ => 1
     | EMITLONG _ => 1
     | REALCONST _ => 2
     | STRINGCONST s => (length(s)+3) div 4
     | BEQ(_,_,_,ref lab) => if abs((pos+1)-lab) < 32768 then 1 else 4
     | JUMP _ => 1
     | SLT(_, Reg _, _) => 1
     | SLT(_, Immed i, _) => if abs(i) < 32768 then 1 else 3
     | SLT(_, Immedlab(ref lab), _) =>
		  if (lab-(get pcp))*4 < 32768 then 2 else 3
     | ADD(_, ea, _) => easize ea
     | AND(_, ea, _) => easize ea
     | OR (_, ea, _) => easize ea
     | XOR(_, ea, _) => easize ea
     | LOAD x => adrsize x
     | STORE x  => adrsize x
     | SUB _ => 1
     | MULOP _ => 1 | MFLO _ => 1
     | SLL _ => 1  | SRA _ => 1
     | COMMENT _ => 0
   in if newsize > size then sizeref := newsize else ();
      gen(pos+size,pcp,rest)
  end
 | SOME (emit : int*int -> unit) =>
  let open Bits
      fun gen1() = gen(pos+size,pcp,rest)
      fun emitlong i = emit(rshift(i,16), andb(i,65535))
      fun i_type(opcode,rs,rt,i) = emit(lshift(opcode,10)+lshift(rs,5)+rt, i)
      fun r_type(opcode,rs,rt,rd,shamt,funct) =
		i_type(opcode,rs,rt, lshift(rd,11)+lshift(shamt,6)+funct)

      fun split i = let val hi = rshift(i,16) and lo = andb(i,65535)
		     in if lo<32768 then (hi,lo) else (hi+1, lo-65536)
		    end

      fun arithop(opr, code, (Reg i, Reg j, Reg k)) = 
	    gen1(r_type(0,i,j,k,0,32+code)
	| arithop(opr, code, (Reg i, Immed j, Reg k)) =
	    case size
	     of 1 => gen1(i_type(8+code,i,k,j))
	      | 3 => gen(pos,pcp,(ref 2, ADD(Reg 0, Immed j, Reg temp))::
				 (ref 1, opr(Reg i, Reg temp, Reg k)))
	| arithop(opr, code, (Reg i, Immedlab (ref j), Reg k))
	    gen(pos,pcp, (ref (size-1), ADD(Reg 31, 
				     Immed(4*(j-(get pcp))), Reg temp))::
				  (ref 1, opr(Reg i, Reg temp, Reg k)))
      fun memop(opr,code,(s, Reg base, i, Reg t)) =
	    case size
	     of 1 => gen1(i_type(code,base,t,i))
	      | 3 => let val (hi,lo) = split i
		      in i_type(15,0,temp,hi);
			 gen(pos+1,pcp, 
			      (ref 1, ADD(Reg base, Reg temp, Reg temp))::
			      (ref 1, opr(s, Reg temp, Immed lo, Reg t))::rest)
		     end
	
   in case inst
    of MARK => gen1(emitlong(pos * power_tags + tag_backptr))
     | EMITLAB(i, ref d) => gen1(emitlong((d-pos)*4+i))
     | EMITLONG i => gen1(emitlong i)
     | REALCONST s => gen1(emitlong 0; emitlong 0)
     | STRINGCONST s => 
		let val s' = s ^ "\000\000\000\000"
		    fun emits i = if i < size*4
				  then (emit(lshift(ordif(s,i),8)+ordof(s,i+1),
				         lshift(ordif(s,i+2),8)+ordof(s,i+3));
					emits(i+4))
				  else ()
		 in gen1(emits 0)
		end
     | BEQ(b,Reg i,Reg j, ref dest) =>
	case size
         of 1 => gen1(i_type(if b then 4 else 5, i, j, dest-(pos+1)))
	  | 5 => gen(pos,pcp,
		      (ref 1, BEQ(not b, Reg i, Reg j, ref(pos+5)))::
		      (ref 3, ADD(Reg 31, Immed(4*dest-(get pcp)), Reg temp))::
		      (ref 1, JUMP(Reg temp))::rest)
     | JUMP(Reg j) => gen1(r_type(0,j,0,0,0,8))
     | SLT(Reg i, Reg j, Reg k) => gen1(r_type(0,i,j,k,0,42))
     | SLT(Reg i, Immed j, Reg k) =>
	case size
	 of 1 => gen1(i_type(10,i,j,k))
	  | 3 => gen(pos,pcp,(ref 2, ADD(Reg 0, Immed j, Reg temp))::
		             (ref 1, SLT(Reg i, Reg temp, Reg k))::rest)
     | ADD(stuff as (Reg 0, Immed j, Reg dest)) => 
	(case size
	 of 2 => let val (hi,lo) = split j
		  in i_type(15,0,dest,hi);
		     gen(pos+1,pcp,(ref 1, 
			    ADD(Reg dest,Immed lo,Reg dest))::rest)
		 end
	  | _ => arithop(ADD,0,stuff))
     | ADD stuff => arithop(ADD,0,stuff)
     | AND stuff => arithop(AND,4,stuff)
     | OR  stuff => arithop(OR,5,stuff)
     | XOR stuff => arithop(XOR,6,stuff)
     | LOAD x as (Byte,_,_,_) => memop(LOAD,36,x)
     | LOAD x as (Word,_,_,_) => memop(LOAD,35,x)
     | STORE x as (Byte,_,_,_) => memop(STORE,40,x)
     | STORE x as (Word,_,_,_) => memop(STORE,43,x)
     | SUB stuff => arithop(SUB,2,stuff)
     | MULOP(DIV, Reg i, Reg j) => gen1(r_type(0,i,j,0,0,26))
     | MULOP(MULT,Reg i, Reg j) => gen1(r_type(0,i,j,0,0,24))
     | MFLO(Reg k) => gen1(r_type(0,0,0,k,0,18))
     | SLL (Reg d, Reg t, Immed k) => gen1(r_type(0,0,t,d,k,0))
     | SLL (Reg d, Reg t, Reg k) =>   gen1(r_type(0,k,t,d,0,4))
     | SRA (Reg d, Reg t, Immed k) => gen1(r_type(0,0,t,d,k,3))
     | SRA (Reg d, Reg t, Reg k) =>   gen1(r_type(0,k,t,d,0,7))
     | COMMENT _ => 0
 in gen
end


fun showinstr(pos,hi,lo) =
 let val opr = andb(rshift(hi,10),63)
     and rs = andb(rshift(hi,5),31) and rt = andb(hi,31)
     and rd = andb(rshift(lo,11),31) and shamt = andb(rshift(lo,6),31)
     and funct = andb(lo,63)
     fun sext x = if x>32767 then x-65536 else x
     val int = Integer.makestring
     val opcode::pieces =  case (opr,funct)
      of (0,8) => ["jr    ", reg rs]
       | (4,_) => ["beq   ", reg rs, reg rt, int(sext lo)]
       | (5,_) => ["bne   ", reg rs, reg rt, int(sext lo)]
       | (0,42)=> ["slt   ", reg rs, reg rt, reg rd]
       | (10,_)=> ["slt   ", reg rs, int (sext lo), reg rt]
       | (15,_)=> ["sui   ", reg rt, int (Bits.lshift(lo,16))]
       | (0,32)=> ["add   ", reg rs, reg rt, reg rd]
       | (0,34)=> ["sub   ", reg rs, reg rt, reg rd]
       | (0,36)=> ["and   ", reg rs, reg rt, reg rd]
       | (0,37)=> ["or    ", reg rs, reg rt, reg rd]
       | (0,38)=> ["xor   ", reg rs, reg rt, reg rd]
       | (8,_) => ["add   ", reg rs, int(sext lo), reg rt]
       | (12,_)=> ["and   ", reg rs, int lo, reg rt]
       | (13,_)=> ["or    ", reg rs, int lo, reg rt]
       | (14,_)=> ["xor   ", reg rs, int lo, reg rt]
       | (35,_)=> ["lw    ", reg rt, int lo ^ "(" reg rs ^ ")"]
       | (36,_)=> ["lbu   ", reg rt, int lo ^ "(" reg rs ^ ")"]
       | (40,_)=> ["sb    ", reg rt, int lo ^ "(" reg rs ^ ")"]
       | (43,_)=> ["sw    ", reg rt, int lo ^ "(" reg rs ^ ")"]
       | (0,26)=> ["div   ", reg rs, reg rt]
       | (0,24)=> ["mult  ", reg rs, reg rt]
       | (0,18)=> ["mflo  ", reg rd]
       | (0,0) => ["sll   ", reg rd, reg rt, int shamt]
       | (0,4) => ["sll   ", reg rd, reg rt, reg rs]
       | (0,3) => ["sra   ", reg rd, reg rt, int shamt]
       | (0,7) => ["sra   ", reg rd, reg rt, reg rs]
       | _     => [".word ", int(Bits.orb(Bits.lshift(hi,16),lo))]
     fun show nil = ()
       | show [x] = say x
       | show a::r = (say a; say ","; say r)
    in say(int (pos*4)); say ":  "; say opcode; show pieces; say "\n";
   end
end


fun disassemble(instrs) =
 let fun enhance(done, inst::rest) = enhance( (ref 0, inst) :: done, rest)
       | enhance(done, nil) = done

     val instrs' = enhance(nil, instrs)
     fun passes(oldsize) =
	let val size = pass NONE (0,NONE,instrs')
         in if size=oldsize then size
	    else passes size
	end
     val pos = ref 0
     fun emit(hi,lo) = (showinstr(!pos,hi,lo); inc pos)
  in pass (SOME emit) (0,NONE,instrs');
     kept := [EMITLONG 0]
 end

end
       
