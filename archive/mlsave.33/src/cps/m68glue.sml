structure M68MC : CODEGENERATOR = 
struct
  structure CM = M68CM(M68MCode)
  structure G = CPScomp(CM)
  structure B = BasicM68
  fun assemble () = B.finish(B.sizejump,B.emitjump,B.emitlong);
  fun generate lexp =
	(G.compile lexp; assemble())
end

structure M68AC : ASSEMBLER =
struct
  structure CM = M68CM(M68AsCode)
  structure AssemGen = CPScomp(CM)
  fun generate(lexp,stream) = (M68Assem.outfile := stream;
			       AssemGen.compile lexp)
end

structure IntM68 = IntShare(M68MC);
structure CompM68 = Batch(structure M=M68MC and A=M68AC)
