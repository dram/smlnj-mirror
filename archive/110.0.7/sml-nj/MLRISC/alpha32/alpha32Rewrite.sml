(* alpha32Rewrite.sml -- rewrite an alpha instruction 
 *
 * COPYRIGHT (c) 1997 Bell Labs
 *)

functor Alpha32Rewrite(Instr : ALPHA32INSTR) = struct
  structure I=Instr

  fun rewriteUse(instr, rs, rt) = let
    fun isRegOp (opnd as I.REGop r) = r=rs
      | isRegOp _ = false
    fun rwOperand(opnd as I.REGop r) = if r=rs then I.REGop rt else opnd
      | rwOperand opnd = opnd
    fun replace r = if r=rs then rt else r
    fun load(ldClass, {ldOp, r, b, d, mem}) =
       if b=rs then ldClass{ldOp=ldOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun fstore(stClass, {stOp, r, b, d, mem}) =
       if b=rs then stClass{stOp=stOp, r=r, b=rt, d=d, mem=mem}
       else instr
    fun store{stOp, r, b, d, mem} = 
      if r=rs then
	if b=rs then
	  I.STORE{stOp=stOp, r=rt, b=rt, d=d, mem=mem}
	else
	  I.STORE{stOp=stOp, r=rt, b=b, d=d, mem=mem}
      else if b=rs then
	I.STORE{stOp=stOp, r=r, b=rt, d=d, mem=mem}
      else instr
    fun operate(opClass, {oper, ra, rb, rc}) = 
      if ra=rs then
	if isRegOp rb then 
	  opClass{oper=oper, ra=rt, rb=I.REGop rt, rc=rc}
	else opClass{oper=oper, ra=rt, rb=rb, rc=rc}
      else if isRegOp rb then
	opClass{oper=oper, ra=ra, rb=I.REGop rt, rc=rc}
      else instr
  in
    case instr
    of I.LDA{r, b, d} => if b=rs then I.LDA{r=r, b=rt, d=d} else instr
     | I.LDAH{r, b, d} => if b=rs then I.LDAH{r=r, b=rt, d=d} else instr
     | I.LOAD arg => load(I.LOAD, arg)
     | I.FLOAD farg => load(I.FLOAD, farg)
     | I.STORE arg => store arg
     | I.FSTORE farg => fstore(I.FSTORE, farg)
     | I.JMPL({r, b, d}, labs) =>
       if b=rs then I.JMPL({r=r, b=rt, d=d}, labs) else instr
     | I.JSR({r,b,d}, defs, uses) =>
       if b=rs then I.JSR({r=r,b=rt,d=d}, defs, uses) else instr
     | I.BRANCH(I.BR, _, _) => instr
     | I.BRANCH(br, r, lab) => if r=rs then I.BRANCH(br, rt, lab) else instr
     | I.OPERATE arg => operate(I.OPERATE, arg)
     | I.OPERATEV arg => operate(I.OPERATEV, arg)
     | I.COPY(rds, rss, l) => I.COPY(rds, map replace rss, l)
     | _ => instr
  end

  fun frewriteUse(instr, fs, ft) = let
    fun replace f = if f=fs then ft else f
    fun foperate(opClass, {oper, fa, fb, fc}) = 
      if fa=fs then 
	opClass{oper=oper, fa=ft, fc=fc, fb=replace fb}
      else if fb=fs then opClass{oper=oper, fa=fa, fb=ft, fc=fc}
      else instr
  in
    case instr
    of I.FBRANCH(br, f, lab) =>
       if f=fs then I.FBRANCH(br, ft, lab) else instr
     | I.FCOPY(fss, fds, l) => 
        I.FCOPY(fds, map (fn f => if f=fs then ft else f) fss, l)
     | I.FSTORE{stOp, r, b, d, mem} => 
	if r=fs then I.FSTORE{stOp=stOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE arg => foperate(I.FOPERATE, arg)
     | I.FOPERATEV arg => foperate(I.FOPERATEV, arg)
     | _ => instr
  end

  fun rewriteDef(instr, rs, rt) = 
    case instr
    of I.LDA{r, b, d} => if r=rs then I.LDA{r=rt, b=b, d=d} else instr
     | I.LDAH{r, b, d} => if r=rs then I.LDAH{r=rt, b=b, d=d} else instr
     | I.LOAD{ldOp, r, b, d, mem} => 
       if r=rs then I.LOAD{ldOp=ldOp, r=rt, b=b, d=d, mem=mem} else instr
     | I.JMPL({r, b, d}, labs) =>
       if r=rs then I.JMPL({r=rt, b=b, d=d}, labs) else instr
     | I.JSR({r, b, d}, defs, uses) =>
       if r=rs then I.JSR({r=rt, b=b, d=d}, defs, uses) else instr
     | I.BRANCH(I.BR, r, lab) => 
       if r=rs then I.BRANCH(I.BR, rt, lab) else instr
     | I.OPERATE{oper, ra, rb, rc} => 
       if rc=rs then I.OPERATE{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.OPERATEV{oper, ra, rb, rc} =>
       if rc=rs then I.OPERATEV{oper=oper, ra=ra, rb=rb, rc=rt} else instr
     | I.COPY(rds, rss, l) => 
	I.COPY(map (fn r => if r=rs then rt else r) rds, rss, l)
     | _ => instr

  fun frewriteDef(instr, fs, ft) = 
    case instr
    of I.DEFFREG f => if f=fs then I.DEFFREG ft else instr
     | I.FLOAD{ldOp, r, b, d, mem} => 
        if r=fs then I.FLOAD{ldOp=ldOp, r=ft, b=b, d=d, mem=mem} else instr
     | I.FOPERATE{oper, fa, fb, fc} =>
	if fc=fs then I.FOPERATE{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FOPERATEV{oper, fa, fb, fc} =>
	if fc=fs then I.FOPERATEV{oper=oper, fa=fa, fb=fb, fc=ft} else instr
     | I.FCOPY(fds, fss, l) =>
	I.FCOPY(map (fn f => if f=fs then ft else f) fds, fss, l)
     | _  => instr
end

(*
 * $Log: alpha32Rewrite.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:34  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/08/29 11:00:14  george
 *   Added code to handle the new LDS, CVTLQ, DIVL and DIVLU instructions.
 *
# Revision 1.2  1997/07/28  20:04:10  george
#   Added support for regions
#
# Revision 1.1  1997/07/03  13:57:41  george
#    Version 109.30
#
 *)

