functor MLTreeSize
    (structure T : MLTREE
     val intTy : T.ty (* size of integer word *)
    ) : MLTREE_SIZE =
struct
   structure T = T

   val intTy = intTy

   fun size(T.REG(ty,_)) = ty
     | size(T.LI _) = intTy
     | size(T.LI32 _) = intTy
     | size(T.LI64 _) = intTy
     | size(T.LABEL _) = intTy
     | size(T.CONST _) = intTy
     | size(T.NEG(ty,_)) = ty
     | size(T.ADD(ty,_,_)) = ty
     | size(T.SUB(ty,_,_)) = ty
     | size(T.MULS(ty,_,_)) = ty
     | size(T.DIVS(ty,_,_)) = ty
     | size(T.QUOTS(ty,_,_)) = ty
     | size(T.REMS(ty,_,_)) = ty
     | size(T.MULU(ty,_,_)) = ty
     | size(T.DIVU(ty,_,_)) = ty
     | size(T.REMU(ty,_,_)) = ty
     | size(T.NEGT(ty,_)) = ty
     | size(T.ADDT(ty,_,_)) = ty
     | size(T.SUBT(ty,_,_)) = ty
     | size(T.MULT(ty,_,_)) = ty
     | size(T.DIVT(ty,_,_)) = ty
     | size(T.QUOTT(ty,_,_)) = ty
     | size(T.REMT(ty,_,_)) = ty
     | size(T.ANDB(ty,_,_)) = ty
     | size(T.ORB(ty,_,_)) = ty
     | size(T.XORB(ty,_,_)) = ty
     | size(T.NOTB(ty,_)) = ty
     | size(T.SRA(ty,_,_)) = ty
     | size(T.SRL(ty,_,_)) = ty
     | size(T.SLL(ty,_,_)) = ty
     | size(T.COND(ty,_,_,_)) = ty
     | size(T.LOAD(ty,_,_)) = ty
     | size(T.CVTI2I(ty,_,_,_)) = ty
     | size(T.CVTF2I(ty,_,_,_)) = ty
     | size(T.LET(_,e)) = size e
     | size(T.PRED(e,_)) = size e
     | size(T.REXT(ty,_)) = ty
     | size(T.MARK(e,_)) = size e

   fun fsize(T.FREG(ty,_)) = ty
     | fsize(T.FLOAD(ty,_,_)) = ty
     | fsize(T.FADD(ty,_,_)) = ty
     | fsize(T.FSUB(ty,_,_)) = ty
     | fsize(T.FMUL(ty,_,_)) = ty
     | fsize(T.FDIV(ty,_,_)) = ty
     | fsize(T.FABS(ty,_)) = ty
     | fsize(T.FNEG(ty,_)) = ty
     | fsize(T.FSQRT(ty,_)) = ty
     | fsize(T.FCOND(ty,_,_,_)) = ty
     | fsize(T.CVTI2F(ty,_,_)) = ty
     | fsize(T.CVTF2F(ty,_,_)) = ty
     | fsize(T.FCOPYSIGN(ty,_,_)) = ty
     | fsize(T.FPRED(e,_)) = fsize e
     | fsize(T.FEXT(ty,_)) = ty
     | fsize(T.FMARK(e,_)) = fsize e

end
