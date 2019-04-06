structure VaxMO : CODEGENERATOR = 
  struct
      structure MOpt = VaxPeephole(VaxMCode)
      structure M : MACHINE = VaxGen(MOpt)
      structure B : BASICVAX = BasicVax

      fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);
      structure C = Codegen(M)
      fun generate lexp = (C.generate lexp; assemble())

  end

structure VaxAO : ASSEMBLER = 
  struct
      structure AsOpt = VaxPeephole(VaxAsCode)
      structure M : MACHINE = VaxGen(AsOpt)
      structure C = Codegen(M)
      fun generate(lexp,stream) = (VaxAssem.outfile := stream;
			           C.generate lexp)
  end
