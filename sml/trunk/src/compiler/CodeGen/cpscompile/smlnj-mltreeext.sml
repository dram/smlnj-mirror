structure SMLNJMLTreeExt : SMLNJ_MLTREE_EXT =
struct
   type ('s,'r,'f,'c) sx = unit
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) ccx = unit
   datatype ('s,'r,'f,'c) fx = 
       FSINE of 'f
     | FCOSINE of 'f
     | FTANGENT of 'f

end

(* This is the default extension compilation module 
 * used for all architectures except the x86.
 *)
functor SMLNJMLTreeExtComp
   (structure T   : MLTREE
    structure I   : INSTRUCTIONS
    structure CFG : CONTROL_FLOW_GRAPH where I = I and P = T.PseudoOp
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure CFG = CFG
   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) T.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SMLNJMLTreeExtComp" 

   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
end
