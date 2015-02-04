structure VaxGlue =
struct


    structure vaxm : machineCode = 
    struct
        structure MOpt = VaxPeephole(VaxMCode)
        structure M : machine = VaxGen(MOpt)
        structure B : BASICVAX = BasicVax

        fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);

    end (* structure vaxm *)

    structure vaxa = 
    struct
	structure AsOpt = VaxPeephole(VaxAsCode)
	structure M : machine = VaxGen(AsOpt)
	open VaxAssem
    end (* structure vaxa *)

val _ = print("3\n")
   structure g = invoke(vaxm,vaxa)
val _ = print("4\n")
   open g

(*   fun NewML () =
	ExportML ("vax/comp.exp", "New vaxML Compiler", []) *)

end (* structure glue *)
