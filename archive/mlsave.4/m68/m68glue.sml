structure M68Glue =
struct
  structure Codes = 
  struct
    structure machm : machineCode = 
    struct
	structure MOpt = M68Peephole(M68MCode)
        structure M : machine = M68Gen(MOpt)
        structure B : BASICM68 = BasicM68

        fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);

    end

    structure macha : assemblyCode = 
    struct
	structure AsOpt = M68Peephole(M68AsCode)
	structure M : machine = M68Gen(AsOpt)
	open M68Assem
    end

  end  (* structure Codes *)

  structure g = invoke(Codes)

  open g

end (* structure glue *)
