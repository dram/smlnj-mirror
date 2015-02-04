structure M68Glue =
struct
  structure Codes = 
  struct
    structure Machm : MACHINECODE = 
    struct
	structure MOpt = M68Peephole(M68MCode)
        structure M : MACHINE = M68Gen(MOpt)
        structure B : BASICM68 = BasicM68

        fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong)
    end (* structure Machm *)

    structure Macha : ASSEMBLYCODE = 
    struct
	structure AsOpt = M68Peephole(M68AsCode)
	structure M : MACHINE = M68Gen(AsOpt)
	open M68Assem
    end (* structure Macha *)
  end (* structure Codes *)

  structure Glue = Invoke(Codes)

  open Glue

end (* structure M68Glue *)
