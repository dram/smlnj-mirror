(* sparcglue.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

structure SparcMC : CODEGENERATOR = 
struct
  structure G = CPScomp(SparcCM(SparcCoder(SparcMCode.Coder)))
  fun generate lexp =
    (G.compile lexp; SparcMCode.finish())
end

structure SparcAC : ASSEMBLER =
struct
  structure CM = SparcCM(SparcCoder(SparcAsCode))
  structure AssemGen = CPScomp(CM)
  fun generate(lexp, stream) = (SparcAssem.outfile := stream;
		   AssemGen.compile lexp)
end

structure IntSparc = IntShare(structure Machm = SparcMC
			      val fileExtension = ".spa"
			     )
structure CompSparc = Batch(structure M=SparcMC and A=SparcAC)
