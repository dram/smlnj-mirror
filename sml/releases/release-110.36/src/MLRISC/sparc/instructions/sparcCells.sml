(*
 * WARNING: This file was automatically generated by MDLGen (v3.0)
 * from the machine description file "sparc/sparc.mdl".
 * DO NOT EDIT this file directly
 *)


signature SPARCCELLS =
sig
   include CELLS
   val Y : CellsBasis.cellkind
   val PSR : CellsBasis.cellkind
   val FSR : CellsBasis.cellkind
   val CELLSET : CellsBasis.cellkind
   val showGP : CellsBasis.register_id -> string
   val showFP : CellsBasis.register_id -> string
   val showY : CellsBasis.register_id -> string
   val showPSR : CellsBasis.register_id -> string
   val showFSR : CellsBasis.register_id -> string
   val showCC : CellsBasis.register_id -> string
   val showMEM : CellsBasis.register_id -> string
   val showCTRL : CellsBasis.register_id -> string
   val showCELLSET : CellsBasis.register_id -> string
   val showGPWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showFPWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showYWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showPSRWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showFSRWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showCCWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showMEMWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showCTRLWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val showCELLSETWithSize : (CellsBasis.register_id * CellsBasis.sz) -> string
   val frameptrR : CellsBasis.cell
   val linkReg : CellsBasis.cell
   val y : CellsBasis.cell
   val psr : CellsBasis.cell
   val fsr : CellsBasis.cell
   val r0 : CellsBasis.cell
   val addGP : (CellsBasis.cell * cellset) -> cellset
   val addFP : (CellsBasis.cell * cellset) -> cellset
   val addY : (CellsBasis.cell * cellset) -> cellset
   val addPSR : (CellsBasis.cell * cellset) -> cellset
   val addFSR : (CellsBasis.cell * cellset) -> cellset
   val addCC : (CellsBasis.cell * cellset) -> cellset
   val addMEM : (CellsBasis.cell * cellset) -> cellset
   val addCTRL : (CellsBasis.cell * cellset) -> cellset
   val addCELLSET : (CellsBasis.cell * cellset) -> cellset
end

structure SparcCells : SPARCCELLS =
struct
   exception SparcCells
   fun error msg = MLRiscErrorMsg.error("SparcCells",msg)
   open CellsBasis
   fun showGPWithSize (r, ty) = (fn (r, _) => (if (r < 8)
                                       then ("%g" ^ (Int.toString r))
                                       else (if (r = 14)
                                          then "%sp"
                                          else (if (r < 16)
                                             then ("%o" ^ (Int.toString (r - 8)))
                                             else (if (r < 24)
                                                then ("%l" ^ (Int.toString (r - 16)))
                                                else (if (r = 30)
                                                   then "%fp"
                                                   else (if (r < 32)
                                                      then ("%i" ^ (Int.toString (r - 24)))
                                                      else ("%r" ^ (Int.toString r))))))))
                                ) (r, ty)
   and showFPWithSize (r, ty) = (fn (f, _) => "%f" ^ (Int.toString f)
                                ) (r, ty)
   and showYWithSize (r, ty) = (fn _ => "%y"
                               ) (r, ty)
   and showPSRWithSize (r, ty) = (fn (0, _) => "%psr"
                                   | (n, _) => "%psr" ^ (Int.toString n)
                                 ) (r, ty)
   and showFSRWithSize (r, ty) = (fn (0, _) => "%fsr"
                                   | (n, _) => "%fsr" ^ (Int.toString n)
                                 ) (r, ty)
   and showCCWithSize (r, ty) = (fn _ => "%cc"
                                ) (r, ty)
   and showMEMWithSize (r, ty) = (fn (r, _) => "m" ^ (Int.toString r)
                                 ) (r, ty)
   and showCTRLWithSize (r, ty) = (fn (r, _) => "ctrl" ^ (Int.toString r)
                                  ) (r, ty)
   and showCELLSETWithSize (r, ty) = (fn _ => "CELLSET"
                                     ) (r, ty)
   fun showGP r = showGPWithSize (r, 64)
   fun showFP r = showFPWithSize (r, 32)
   fun showY r = showYWithSize (r, 64)
   fun showPSR r = showPSRWithSize (r, 64)
   fun showFSR r = showFSRWithSize (r, 64)
   fun showCC r = showCCWithSize (r, 64)
   fun showMEM r = showMEMWithSize (r, 8)
   fun showCTRL r = showCTRLWithSize (r, 0)
   fun showCELLSET r = showCELLSETWithSize (r, 0)
   val Y = CellsBasis.newCellKind {name="Y", nickname="y"}
   and PSR = CellsBasis.newCellKind {name="PSR", nickname="psr"}
   and FSR = CellsBasis.newCellKind {name="FSR", nickname="fsr"}
   and CELLSET = CellsBasis.newCellKind {name="CELLSET", nickname="cellset"}
   structure MyCells = Cells
      (exception Cells = SparcCells
       val firstPseudo = 256
       val desc_GP = CellsBasis.DESC {low=0, high=31, kind=CellsBasis.GP, defaultValues=[(0, 
              0)], zeroReg=SOME 0, toString=showGP, toStringWithSize=showGPWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_FP = CellsBasis.DESC {low=32, high=63, kind=CellsBasis.FP, 
              defaultValues=[], zeroReg=NONE, toString=showFP, toStringWithSize=showFPWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_Y = CellsBasis.DESC {low=64, high=64, kind=Y, defaultValues=[], 
              zeroReg=NONE, toString=showY, toStringWithSize=showYWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_PSR = CellsBasis.DESC {low=65, high=65, kind=PSR, defaultValues=[], 
              zeroReg=NONE, toString=showPSR, toStringWithSize=showPSRWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_FSR = CellsBasis.DESC {low=66, high=66, kind=FSR, defaultValues=[], 
              zeroReg=NONE, toString=showFSR, toStringWithSize=showFSRWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_MEM = CellsBasis.DESC {low=67, high=66, kind=CellsBasis.MEM, 
              defaultValues=[], zeroReg=NONE, toString=showMEM, toStringWithSize=showMEMWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CTRL = CellsBasis.DESC {low=67, high=66, kind=CellsBasis.CTRL, 
              defaultValues=[], zeroReg=NONE, toString=showCTRL, toStringWithSize=showCTRLWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       and desc_CELLSET = CellsBasis.DESC {low=67, high=66, kind=CELLSET, defaultValues=[], 
              zeroReg=NONE, toString=showCELLSET, toStringWithSize=showCELLSETWithSize, 
              counter=ref 0, dedicated=ref 0, physicalRegs=ref CellsBasis.array0}
       val cellKindDescs = [(CellsBasis.GP, desc_GP), (CellsBasis.FP, desc_FP), 
              (Y, desc_Y), (PSR, desc_PSR), (FSR, desc_FSR), (CellsBasis.CC, 
              desc_GP), (CellsBasis.MEM, desc_MEM), (CellsBasis.CTRL, desc_CTRL), 
              (CELLSET, desc_CELLSET)]
      )

   open MyCells
   val addGP = CellSet.add
   and addFP = CellSet.add
   and addY = CellSet.add
   and addPSR = CellSet.add
   and addFSR = CellSet.add
   and addCC = CellSet.add
   and addMEM = CellSet.add
   and addCTRL = CellSet.add
   and addCELLSET = CellSet.add
   val RegGP = Reg GP
   and RegFP = Reg FP
   and RegY = Reg Y
   and RegPSR = Reg PSR
   and RegFSR = Reg FSR
   and RegCC = Reg CC
   and RegMEM = Reg MEM
   and RegCTRL = Reg CTRL
   and RegCELLSET = Reg CELLSET
   val stackptrR = RegGP 14
   val frameptrR = RegGP 30
   val asmTmpR = RegGP 10
   val linkReg = RegGP 15
   val fasmTmp = RegFP 30
   val y = RegY 0
   val psr = RegPSR 0
   val fsr = RegFSR 0
   val r0 = RegGP 0
end

