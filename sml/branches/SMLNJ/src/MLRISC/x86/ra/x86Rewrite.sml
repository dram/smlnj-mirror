(* alpha32Rewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)
functor X86Rewrite(Instr : X86INSTR) = struct
  structure I=Instr

  fun operand (mapr,rs,rt) opnd =
    (case opnd
     of I.Direct r => if mapr r=rs then I.Direct rt else opnd
      | I.Displace{base, disp} => 
	  if mapr base = rs then I.Displace{base=rt, disp=disp} else opnd
      | I.Indexed{base as SOME b, index, scale, disp} => let
	  val base'= if mapr b = rs then SOME rt else base
	  val index'=if mapr index=rs then rt else index
	in I.Indexed{base=base', index=index', scale=scale, disp=disp}
	end
      | I.Indexed{base, index, scale, disp}  => 
	if mapr index=rs then 
	  I.Indexed{base=base, index=rt, scale=scale, disp=disp}
	else opnd
      | _ => opnd
    (*esac*))

  fun rewriteUse(mapr, instr, rs, rt) = let
    val operand = operand (mapr, rs, rt)
    fun replace r = if mapr r = rs then rt else r
  in
    case instr
    of I.JMP(opnd, labs) => I.JMP(operand opnd, labs)
     | I.JCC{cond, opnd} => I.JCC{cond=cond, opnd = operand opnd}
     | I.CALL(opnd, defs, (ru,fu,cu)) => 
         I.CALL(operand opnd, defs, (map replace ru, fu, cu))
     | I.MOVE{mvOp, src, dst as I.Direct _} => I.MOVE{mvOp=mvOp, src=operand src, dst=dst}
     | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=operand src, dst=operand dst}
     | I.LEA{r32, addr} => I.LEA{r32=r32, addr=operand addr}
     | I.CMP{lsrc, rsrc} => I.CMP{lsrc=operand lsrc, rsrc=operand rsrc}
     | I.BINARY{binOp, src, dst} => 
	I.BINARY{binOp=binOp, src=operand src, dst=operand dst}
     | I.MULTDIV{multDivOp, src} => 
	I.MULTDIV{multDivOp=multDivOp, src=operand src}
     | I.MUL3{dst, src1, src2 as NONE} => 
	I.MUL3{dst=if mapr dst=rs then rt else rs, src1=operand src1, src2=NONE}
     | I.MUL3{dst, src1, src2} => 
	I.MUL3{dst=dst, src1=operand src1, src2=src2}
     | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
     | I.PUSH opnd => I.PUSH(operand opnd)
     | I.POP opnd  => I.POP(operand opnd)
     | I.COPY{dst, src, tmp} => 
	I.COPY{dst=dst, src=map replace src, tmp=tmp}
     | I.FSTP opnd => I.FSTP(operand opnd)
     | I.FLD opnd => I.FLD(operand opnd)
     | I.FBINARY{binOp, src, dst} => 
	I.FBINARY{binOp=binOp, src=operand src, dst=dst}
     | _ => instr
  end (* rewriteUse *)

  fun rewriteDef(mapr, instr, rs, rt) = let
    fun operand(opnd as I.Direct r) = if mapr r=rs then I.Direct rt else opnd
    fun replace r = if mapr r=rs then rt else r
  in
    case instr 
    of I.CALL(opnd, (dr,df,dc), uses) => I.CALL(opnd, (map replace dr, df, dc), uses)
     | I.MOVE{mvOp, src, dst} => I.MOVE{mvOp=mvOp, src=src, dst=operand dst}
     | I.LEA{r32, addr} => I.LEA{r32=replace r32, addr=addr}
     | I.BINARY{binOp, src, dst} => I.BINARY{binOp=binOp, src=src, dst=operand dst}
     | I.MUL3{dst, src1, src2} => I.MUL3{dst=replace dst, src1=src1, src2=src2}
     | I.UNARY{unOp, opnd} => I.UNARY{unOp=unOp, opnd=operand opnd}
     | I.COPY{dst, src, tmp} => I.COPY{dst=map replace dst, src=src, tmp=tmp}
     | _ => instr
  end


  fun frewriteUse(mapr, instr, fs, ft) = let
    fun foperand(opnd as I.FDirect f) = 
         if f=fs then I.FDirect ft else opnd
      | foperand opnd = opnd

    fun replace f = if mapr f=fs then ft else f
  in
    case instr
    of I.FCOPY{dst, src, tmp,...} => I.FCOPY{dst=dst, src=map replace src, tmp=tmp}
     | I.FLD opnd => I.FLD(foperand opnd)
     | I.CALL(opnd, defs, (ur, uf, uc)) => I.CALL(opnd, defs, (ur, map replace uf, uc))
     | I.FBINARY{binOp, src, dst} => 
	 I.FBINARY{binOp=binOp, src=foperand src, dst=foperand dst}
     | _ => instr
  end

  fun frewriteDef(mapr, instr, fs, ft) = let
    fun foperand(opnd as I.FDirect r) = 
         if r=fs then I.FDirect ft else opnd
      | foperand opnd = opnd
    fun replace f = if mapr f = fs then ft else f
  in
    case instr
    of I.FCOPY{dst, src, tmp, ...} => I.FCOPY{dst=map replace dst, src=src, tmp=tmp}
     | I.FSTP opnd => I.FSTP(foperand opnd)
     | I.CALL(opnd, (dr,df,dc), uses) => I.CALL(opnd, (dr, map replace df, dc), uses)
     | I.FBINARY{binOp, src, dst} => I.FBINARY{binOp=binOp, src=src, dst=foperand dst}
     | _  => instr
  end
end

(*
 * $Log: X86Rewrite.sml,v $
 * Revision 1.2  1998/08/27 14:12:21  george
 *   used sethi-ullman number for floating point stack
 *
 * Revision 1.1.1.1  1998/07/22 18:10:32  george
 *   X86.1
 *
 *)


