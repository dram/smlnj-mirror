(* hppaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor HppaProps 
  (structure HppaInstr : HPPAINSTR
   val exnptrR : int list) : INSN_PROPERTIES = 
struct
  structure I = HppaInstr
  structure C = I.C
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.impossible ("HppaProps." ^ msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  (* Note: BLE and BL are not view as branches *)
  fun instrKind(I.BCOND _)  = IK_JUMP
    | instrKind(I.BCONDI _) = IK_JUMP
    | instrKind(I.B _)      = IK_JUMP
    | instrKind(I.FBCC _)   = IK_JUMP
    | instrKind(I.BV _)     = IK_JUMP
    | instrKind(I.NOP)      = IK_NOP
    | instrKind _	    = IK_INSTR

  fun branchTargets(I.BCOND{t, ...})    = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BCONDI{t, ...})   = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.B(lab))           = [LABELLED lab]
    | branchTargets(I.FBCC{t,...})      = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BV{labs=[],...})  = [ESCAPES]
    | branchTargets(I.BV{labs,...})     = map LABELLED labs
    | branchTargets _ = error "branchTargets"

  fun nop() = I.NOP

  fun moveInstr(I.COPY _)   = true
    | moveInstr(I.FCOPY _)  = true
    | moveInstr _ = false

  fun defUseR instr = let
    fun trap((I.ADDO | I.SUBO | I.SH1ADDO), d, u) = (d, exnptrR @ u)
      | trap(_, d, u) = (d, u)
    fun trapi((I.ADDIO | I.SUBIO), d, u) = (d, exnptrR @ u)
      | trapi(_, d, u) = (d, u)
  in
    case instr
     of I.STORE {b, r,...}          => ([],  [b,r])
      | I.LOAD {l, r1, r2, t, ...}  => ([t], [r1,r2])
      | I.LOADI {li, r, t, ...}     => ([t], [r])
      | I.ARITH {a, r1, r2, t, ...} => trap(a, [t], [r1,r2])
      | I.ARITHI {ai, r, t, ...}    => trapi(ai, [t], [r])
      | I.COMCLR{r1, r2, t, ...}    => ([t], [r1, r2])
      | I.SHIFTV {r, t, ...}        => ([t], [r])
      | I.SHIFT {r, t, ...}         => ([t], [r])
      | I.BCOND {r1, r2, ...}       => ([],  [r1,r2])
      | I.BCONDI {r2, ...} 	    => ([],  [r2])
      | I.BV {x, b, ...}	    => ([],  [x,b])
      | I.BL{defs, uses, ...}       => (#1 defs, #1 uses)
      | I.BLE{t, b, defs, uses, ...}=> (31 :: t :: #1 defs, b :: #1 uses)
      | I.LDIL{i, t}		    => ([t], [])
      | I.LDO{b, t, ...}	    => ([t], [b])
      | I.COPY(rds, rss, _)         => (rds, rss)
      | I.MTCTL{r, t}		    => ([],  [r])
      | I.FSTORE {b, ...}	    => ([],  [b])
      | I.FSTOREX {b, x, ...}  	    => ([],  [b,x])
      | I.FLOAD {b, ...}	    => ([],  [b])
      | I.FLOADX{b, x, ...} 	    => ([],  [b,x])
      | _   => ([],[])
  end

  fun defUseF instr = 
    case instr
      of I.FSTORE {r, ...}  	   => ([],  [r])
       | I.FSTOREX{r, ...}	   => ([],  [r])
       | I.FLOAD{t, ...}	   => ([t], [])
       | I.FLOADX{t, ...}	   => ([t], [])
       | I.FARITH {r1, r2, t, ...} => ([t], [r1,r2])
       | I.FUNARY {f, t, ...}      => ([t], [f])
       | I.FCMP  (_, f1, f2)	   => ([],  [f1, f2])
       | I.BL{defs, uses, ...}     => (#2 defs, #2 uses)
       | I.BLE{defs, uses, ...}   => (#2 defs, #2 uses)
       | I.FCOPY(fds, fss, _)      => (fds, fss)
       | _ => ([],[])


  fun latency _ = 1
end



(*
 * $Log: hppaProps.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:34  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.3  1997/09/29 20:58:36  george
 *   Propagate region information through instruction set
 *
# Revision 1.2  1997/07/02  13:22:04  george
# *** empty log message ***
#
# Revision 1.1.1.1  1997/04/19  18:14:22  george
#   Version 109.27
#
 *)
