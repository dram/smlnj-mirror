(* mipsgen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(*functor MipsAssembler(structure Endian : ENDIAN) : ASSEMBLER = 
  struct
    structure MipsAssemblyEmitter = MipsAsCode()
    structure CoderInstr = MipsInstr(structure E=Endian)
    structure MipsCoder = Coder(structure M=CoderInstr 
				      and E=MipsAssemblyEmitter)
    structure CMachineAssembler = MipsCM(structure C=MipsCoder
					       and E=Endian)
    structure MLAssembler = CPScomp(CMachineAssembler)

    fun generate(lexp,stream) =	(MipsAsmStream.asmStream := stream;
				 MLAssembler.compile lexp;
				 MipsCoder.finish())
  end
*)
functor MipsCodeGen(Endian : ENDIAN) : CODEGENERATOR =  
 CPScodeGenerator(
    structure CoderInstr = MipsInstr(structure E=Endian)
    structure MachSpec = MipsSpec(Endian)
    structure MipsCoder = Coder(structure M=CoderInstr
				      and E=MipsMCode(structure E=Endian))
    structure Gen = CPSgen(structure M = MipsCM(structure C=MipsCoder
						and E=Endian
						and MachSpec=MachSpec)
			   structure MachSpec = MachSpec)
    fun collect () = (MipsCoder.finish(); KeepMipsMCode.getCodeString())
)


(*
 * $Log: mipsgen.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:46  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:39  george
 *   Version 109.24
 *
 *)
