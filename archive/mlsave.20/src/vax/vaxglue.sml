structure VaxCodes =
struct

  structure Machm : MACHINECODE = 
  struct
      structure MOpt = VaxPeephole(VaxMCode)
      structure M : MACHINE = VaxGen(MOpt)
      structure B : BASICVAX = BasicVax

      fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);

  end (* structure Machm *)

  structure Macha : ASSEMBLYCODE = 
  struct
      structure AsOpt = VaxPeephole(VaxAsCode)
      structure M : MACHINE = VaxGen(AsOpt)
      open VaxAssem
  end (* structure Macha *)

end  (* structure VaxCodes *)

structure VaxGlue = Invoke(VaxCodes)
