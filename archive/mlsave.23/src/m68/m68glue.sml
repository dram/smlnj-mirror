structure M68MO : CODEGENERATOR = 
  struct
      structure MOpt = M68Peephole(M68MCode)
      structure M : MACHINE = M68Gen(MOpt)
      structure B : BASICM68 = BasicM68

      fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);
      structure C = Codegen(M)
      fun generate lexp = (C.generate lexp; assemble())

  end

structure M68AO : ASSEMBLER = 
  struct
      structure AsOpt = M68Peephole(M68AsCode)
      structure M : MACHINE = M68Gen(AsOpt)
      structure C = Codegen(M)
      fun generate(lexp,stream) = (M68Assem.outfile := stream;
			           C.generate lexp)
  end
