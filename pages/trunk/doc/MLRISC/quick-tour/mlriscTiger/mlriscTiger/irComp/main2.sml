functor Main(structure Frame : FRAME
	     structure BackEnd : 
	       sig
		 val codegen : Frame.frag -> unit
	       end) =
struct
  structure Tr = Translate(Frame)
  structure Semant = Semant(Tr)

  fun emitFrags(frag::frags) = (BackEnd.codegen frag; emitFrags(frags))
    | emitFrags [] = ()

  fun withOpenFile fname frags = let
    val outStrm = TextIO.openOut fname
  in 
    AsmStream.asmOutStream := outStrm;
    (emitFrags frags before TextIO.closeOut outStrm) 
       handle e => (TextIO.closeOut outStrm; raise e)
  end 

  fun compile filename = let
    val absyn = Parse.parse filename
    val frags = (FindEscape.prog absyn; Semant.transProg absyn)
    val outFile = 
      OS.Path.joinBaseExt{base= #base(OS.Path.splitBaseExt filename),
			  ext=SOME"s"}
  in withOpenFile outFile frags
  end
end