structure VaxGlue =
struct
  structure Codes =
  struct
    structure machm : machineCode = 
    struct
        structure MOpt = VaxPeephole(VaxMCode)
        structure M : machine = VaxGen(MOpt)
        structure B : BASICVAX = BasicVax

        fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);

    end (* structure vaxm *)

    structure macha : assemblyCode = 
    struct
	structure AsOpt = VaxPeephole(VaxAsCode)
	structure M : machine = VaxGen(AsOpt)
	open VaxAssem
    end (* structure vaxa *)
  end  (* structure Codes *)

val _ = print("3\n")
   structure g = invoke(Codes)
val _ = print("4\n")
   open g

end (* structure glue *)
