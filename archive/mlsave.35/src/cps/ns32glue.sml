structure Ns32MC : CODEGENERATOR = 
struct
  structure CM = Ns32CM(Ns32MCode.Coder)
  structure G = CPScomp(CM)
  fun generate lexp =
	(G.compile lexp; Ns32MCode.finish())
end

structure Ns32AC : ASSEMBLER =
struct
  structure CM = Ns32CM(Ns32AsCode)
  structure AssemGen = CPScomp(CM)
  fun generate(lexp,stream) = (Ns32Assem.outfile := stream;
			       AssemGen.compile lexp)
end

structure IntNs32 = IntShare(Ns32MC);
structure CompNs32 = Batch(structure M=Ns32MC and A=Ns32AC)
