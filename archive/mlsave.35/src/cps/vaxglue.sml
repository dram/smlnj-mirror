structure VaxMC : CODEGENERATOR = 
struct
  structure CM = VaxCM(VaxMCode.Coder)
  structure G = CPScomp(CM)
  fun generate lexp = (G.compile lexp; VaxMCode.finish())
end

structure VaxAC : ASSEMBLER =
struct
  structure CM = VaxCM(VaxAsCode)
  structure AssemGen = CPScomp(CM)
  fun generate(lexp,stream) = (VaxAssem.outfile := stream;
			       AssemGen.compile lexp)
end

structure IntVax = IntShare(VaxMC);
structure CompVax = Batch(structure M=VaxMC and A=VaxAC)
