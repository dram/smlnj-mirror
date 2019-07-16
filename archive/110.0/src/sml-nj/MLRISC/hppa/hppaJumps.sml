(* hppaJumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor HppaJumps(Instr:HPPAINSTR) : SDI_JUMPS = struct
  structure I = Instr
  structure C = Instr.C
  structure LE = LabelExp
  structure Const = I.Constant
  structure Shuffle = Shuffle(C)

  fun error msg = MLRiscErrorMsg.impossible ("HppaJumps." ^ msg)

  val branchDelayedArch = false

  fun minSize(I.COPY _)  = 0
    | minSize(I.FCOPY _) = 0
    | minSize _          = 4

  fun maxSize (I.BCOND _)  = 20
    | maxSize (I.BCONDI _) = 20
    | maxSize (I.B _)	   = 16
    | maxSize (I.FBCC _)   = 16
    | maxSize (I.COPY _)   = error "maxSize:COPY"
    | maxSize (I.FCOPY _)  = error "maxSize:FCOPY"
    | maxSize _		   = 4

  fun mayNeedNops _ = 0
  fun needsNop _ = 0

  fun isSdi instr = let
    fun opnd (I.LabExp _) = true
      | opnd (I.ConstOp _) = true
      | opnd _ = false
  in
    case instr
    of I.BCOND _		=> true
     | I.BCONDI _		=> true
     | I.B _			=> true
     | I.FBCC _			=> true
     | I.LDO{i, ...}		=> opnd i
     | I.STORE{d, ...}		=> opnd d
     | I.ARITHI{i, ...}		=> opnd i
     | I.LOADI{i, ...}          => opnd i
     | I.FCOPY _		=> true
     | I.COPY _			=> true
     | _			=> false
  end

  fun im11 n = ~1024 <= n andalso n < 1024
  fun im12 n = ~2048 <= n andalso n < 2048
  fun im14 n = ~8192 <= n andalso n < 8192
  fun im17 n = ~65536 <= n andalso n < 65536

  fun sdiSize(instr, regmap, labMap, loc) = let
    fun branchOffset lab = ((labMap lab) - loc - 8) div 4
    fun branch lab = let
      val offset = branchOffset lab
    in
      if im12 offset then 4 else if im17 offset then 8 else 20
    end
    fun memDisp(c, short, long) = if im14(c) then short else long
    val lookup = Intmap.map regmap
  in
    case instr 
     of I.LDO{i=I.LabExp(lexp, _), ...} => memDisp(LE.valueOf lexp, 4, 12)
      | I.LDO{i=I.ConstOp c, ...} => memDisp(Const.valueOf c, 4, 8)
      | I.LOADI{i=I.ConstOp c, ...} => memDisp(Const.valueOf c, 4, 12)
      | I.LOADI{i=I.LabExp(lexp, _), ...} => memDisp(LE.valueOf lexp, 4, 12)
      | I.STORE{d=I.ConstOp c, ...} => memDisp(Const.valueOf c, 4, 12)
      | I.ARITHI{ai, i=I.ConstOp c, ...} => let
	  fun arithImmed() = if im11(Const.valueOf c) then 4 else 12 
	in
	  case ai
	  of I.ADDI => arithImmed()
	   | I.ADDIO => arithImmed()
	   | I.SUBI => arithImmed()
	   | I.SUBIO => arithImmed()
	   | _ => error "sdiSize: ARITHI LabelExp"
	  (*esac*)
	end
      | I.ARITHI{ai, i=I.LabExp(lexp,_), ...} => let
	  fun arithImmed() = if im11(LabelExp.valueOf lexp) then 4 else 12
	in
	  case ai
	  of I.ADDI => arithImmed()
	   | I.ADDIO => arithImmed()
	   | I.SUBI => arithImmed()
	   | I.SUBIO => arithImmed()
	   | _ => error "sdiSize: ARITHI LabelExp"
	 (*esac*)
        end
      | I.BCOND{t, ...}  => branch t
      | I.BCONDI{t, ...} => branch t
      | I.B(lab)         => if im17 (branchOffset lab) then 4 else 16
      | I.FBCC{t, ...}   => if im17 (branchOffset t) then 4 else 16
      | I.COPY(_, _, ref(SOME l)) => 4 * length l
      | I.FCOPY(_, _, ref(SOME l)) => 4 * length l
      | I.COPY(rds, rss, exp) => let
	  fun mvInstr(rd, rs) = I.ARITH{a=I.OR, r1=rs, r2=0, t=rd}
	  val instrs = Shuffle.shuffle(mvInstr, lookup, C.asmTmpR, rds, rss)
        in
	  exp := SOME(instrs);  4 * length instrs
        end
      | I.FCOPY(fds, fss, exp) => let
	  fun fmvInstr(fd, fs) = I.FUNARY{fu=I.FCPY, f=fs, t=fd}
	  val instrs = Shuffle.shuffle(fmvInstr, lookup, C.fasmTmp, fds, fss)
        in
	  exp := SOME instrs;  4 * length instrs
        end
      | _		 => error "sdiSize"
  end

  fun longJump lab = let
    val baseDisp =  LE.MINUS(LE.LABEL lab, LE.CONST 8192)
    val labOpnd = (baseDisp, I.T)
    val baseptrR = 8
  in
   (* Note: A better sequence would be to use ADDIL, however
    * the expansion is done after register allocation and
    * ADDIL defines %r1. 
    *)
    [I.LDIL{i=I.HILabExp labOpnd, t=C.asmTmpR},
     I.LDO{i=I.LOLabExp labOpnd, b=C.asmTmpR, t=C.asmTmpR},
     I.ARITH{a=I.ADD, r1=baseptrR, r2=C.asmTmpR, t=C.asmTmpR},
     I.BV{x=0, labs=[lab], b=C.asmTmpR}]
  end

  fun split11 n = let
    val w = Word.fromInt(n)
  in (Word.toIntX(Word.~>>(w, 0w11)), Word.toIntX(Word.andb(w, 0wx7ff)))
  end

  fun split11X n = let
    val w = Word.fromInt(n)
    val hi' = Word.~>>(w, 0w11)
    val lo' = Word.andb(w, 0wx7ff)
    val (hi,lo) = 
      if Word.<=(lo', 0wx3ff) then (hi', lo') else (hi'+0w1, lo'-0wx800)
  in (Word.toIntX hi, Word.toIntX lo)
  end

  fun loadIndexed I.LDW = I.LDWX
    | loadIndexed I.LDH = I.LDHX
    | loadIndexed I.LDB = I.LDBX

  fun expand(I.LDO{i=I.LabExp lexp, t, b}, size) = 
      (case size 
        of 4 => [I.LDO{i=I.LabExp lexp, b=b, t=t}]
         | 12 => [I.LDIL{i=I.HILabExp lexp, t=C.asmTmpR},
	          I.LDO{i=I.LOLabExp lexp, b=C.asmTmpR, t=C.asmTmpR},
		  I.ARITH{a=I.ADD, r1=C.asmTmpR, r2=b, t=t}]
      (*esac*))
    | expand(I.LDO{i=I.ConstOp c, t, b}, size) = 
      (case size 
       of 4 => [I.LDO{i=I.IMMED(Const.valueOf c), t=t, b=b}]
        | 8 => let
	    val (hi, lo) = split11(Const.valueOf c)
	  in
	    [I.LDIL{i=I.IMMED hi, t=C.asmTmpR},
	     I.LDO{i=I.IMMED lo, b=C.asmTmpR, t=t}]
          end
      (*esac*))
    | expand(I.STORE{st, d=I.ConstOp c, b, r, mem}, size) = 
      (case size 
       of 4 => [I.STORE{st=st, d=I.IMMED(Const.valueOf c), b=b, r=r, mem=mem}]
        | 12 => let
	    val (hi, lo) = split11(Const.valueOf c)
	  in
	    [I.LDIL{i=I.IMMED hi, t=C.asmTmpR},
	     I.ARITH{a=I.ADD, r1=C.asmTmpR, r2=b, t=C.asmTmpR},
	     I.STORE{st=st, b=C.asmTmpR, d=I.IMMED lo, r=r, mem=mem}]
	  end
      (*esac*))
    | expand(I.STORE _, _) = error "expand:STORE" 
    | expand(I.ARITHI{ai, r, i=I.ConstOp c, t}, size) = 
      (case size
       of 4 => [I.ARITHI{ai=ai, r=r, i=I.IMMED(Const.valueOf c), t=t}]
        | 12 => let
	    val (hi, lo) = 
	      (case ai
	       of I.ADDI => split11X(Const.valueOf c)
		| I.ADDIO => split11X(Const.valueOf c)
		| I.SUBI => split11X(Const.valueOf c)
		| I.SUBIO => split11X(Const.valueOf c)
		| _ => error "expandd: ARITHI"
	       (*esac*))
	  in
	    [I.LDIL{i=I.IMMED hi, t=C.asmTmpR},
	     I.ARITH{a=I.ADD, r1=C.asmTmpR, r2=r, t=C.asmTmpR},
	     I.ARITHI{ai=ai, r=C.asmTmpR, i=I.IMMED lo, t=t}]
	  end
      (*esac*))
    | expand(instr as I.LOADI{li, r, i=I.ConstOp c, t, mem} , size) = 
      (case size
       of 4  => [I.LOADI{li=li, r=r, i=I.IMMED(Const.valueOf c), t=t, mem=mem}]
        | 12 => let
	    val (hi, lo) = split11(Const.valueOf c)
	  in
	    [I.LDIL{i=I.IMMED hi, t=C.asmTmpR},
	     I.ARITH{a=I.ADD, r1=C.asmTmpR, r2=r, t=C.asmTmpR},
	     I.LOADI{li=li, r=C.asmTmpR, i=I.IMMED lo, t=t, mem=mem}]
	  end
      (*esac*))
    | expand(instr as I.LOADI{li, r, i=I.LabExp lexp, t, mem} , size) = 
      (case size
       of 4  => [instr]
        | 12 => [I.LDIL{i=I.HILabExp lexp, t=C.asmTmpR},
		 I.ARITH{a=I.ADD, r1=C.asmTmpR, r2=r, t=C.asmTmpR},
		 I.LOADI{li=li, r=C.asmTmpR, i=I.LOLabExp lexp, t=t, mem=mem}]
      (*esac*))
    | expand(instr as I.ARITHI{ai, r, i=I.LabExp lexp, t} , size) = 
      (case size
       of 4  => [instr]
        | 12 => 
	 (* Note: A better sequence would be to use ADDIL, however
	  * the expansion is done after register allocation and
	  * ADDIL defines %r1. 
	  *)
	    [I.LDIL{i=I.HILabExp lexp, t=C.asmTmpR},
	     I.LDO{i=I.LOLabExp lexp, b=C.asmTmpR, t=C.asmTmpR},
	     I.ARITH{
		a = case ai of I.ADDI => I.ADD | I.ADDIO => I.ADDO
		             | I.SUBI => I.SUB | I.SUBIO => I.SUBO
			     | _ => error "expand: I.ARITHI LabExp",
		t=t,
		r1=C.asmTmpR,
		r2=r}]
      (*esac*))
    | expand(instr as I.BCOND{cmp,bc, t, f, r1, r2}, size) = let
	fun rev I.COMBT = I.BCOND{cmp=I.COMBF, bc=bc, t=f, f=f, r1=r1, r2=r2}
	  | rev I.COMBF = I.BCOND{cmp=I.COMBT, bc=bc, t=f, f=f, r1=r1, r2=r2}
      in
	case size 
	of 4 => [instr]
	 | 8 => [rev cmp, I.B(t)]
	 | 20 => rev cmp :: longJump t
        (*esac*)
      end
    | expand(instr as I.BCONDI{cmpi, bc, t, f, i, r2}, size) = let
        fun rev I.COMIBT = I.BCONDI{cmpi=I.COMIBF, bc=bc, i=i, r2=r2, t=f, f=f}
	  | rev I.COMIBF = I.BCONDI{cmpi=I.COMIBT, bc=bc, i=i, r2=r2, t=f, f=f}
      in
	(case size 
	  of 4 => [instr]
	   | 8 => [rev cmpi, I.B(t)]
	   | 20 => rev cmpi :: longJump t
	(*esac*))
      end
    | expand(instr as I.B(lab), size) =
      (case size 
	of 4 => [instr]
         | 16 => longJump lab
      (*esac*))
    | expand(instr as I.FBCC{t, f}, size) =
      (case size 
	of 4 => [I.B(t)]
         | 16 => 
	     (* lets hope this sequence never gets generated sequence:
			FTEST
			allways trapping instruction
			B (f)
			longJmp
	      *)
	        error "FBCC"
      (*esac*))
    | expand(I.COPY(_, _, ref(SOME instrs)), _) = instrs
    | expand(I.FCOPY(_, _, ref(SOME instrs)), _) = instrs
    | expand _ = error "expand"

end

(*
 * $Log: hppaJumps.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:34  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.7  1998/01/30 14:21:47  george
 *   missing case for expansion of span dependent loads
 *
 * Revision 1.6  1997/09/29 20:58:33  george
 *   Propagate region information through instruction set
 *
# Revision 1.4  1997/07/17  12:27:35  george
#   The regmap is now represented as an int map rather than using arrays.
#
# Revision 1.3  1997/07/03  13:54:29  george
#   Added support for FCOPY.
#
# Revision 1.2  1997/04/19  18:39:10  george
# Version 109.27
#
# Revision 1.1.1.1  1997/04/19  18:14:22  george
#   Version 109.27
#
 *)
