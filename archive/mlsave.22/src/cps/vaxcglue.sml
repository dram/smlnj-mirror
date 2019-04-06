structure VaxMC : CODEGENERATOR = 
struct
  structure CM = VaxCM(VaxMCode)
  structure G = CPScomp(CM)
  structure B = BasicVax
  fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);
  fun generate lexp =
	(G.compile lexp; assemble())
end

structure VaxAC : ASSEMBLER =
struct
  structure CM = VaxCM(VaxAsCode)
  structure AssemGen = CPScomp(CM)
  fun generate(lexp,stream) = (VaxAssem.outfile := stream;
			       AssemGen.compile lexp)
end
	   
