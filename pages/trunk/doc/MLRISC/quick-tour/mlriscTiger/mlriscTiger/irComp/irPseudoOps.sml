functor IrPseudoOps(structure AsmIO : ASM_IO
		    structure Imports : IMPORTS) = 
struct
  structure IO = AsmIO

  fun error msg = MLRiscErrorMsg.impossible ("IRPseudoOps." ^ msg)
 
  datatype pseudo_op =
      PROLOGUE of string
    | EPILOGUE of string
    | STRING of Label.label * string 

 fun transchar #"\n" = "\\n"
   | transchar #"\t" = "\\t"
   | transchar #"\000" = "\\0"
   | transchar #"\"" = "\\\""
   | transchar #"'" = "\\'"
   | transchar #"\\" = "\\\\"
   | transchar c = if c >= #" " andalso ord(c) < 127 then String.str c
         else let val c' = ord c
		  val i1 = c' div 64
	          val i2 = (c' mod 64) div 8
                  val i3 = c' mod 8
               in implode[#"\\",chr(i1 + ord #"0"),
			  chr(i2 + ord #"0"),chr(i3 + ord #"0")]
	      end

  fun toString(PROLOGUE sym) =	IO.dotText() ^ IO.globl(sym) ^	IO.enter(sym) 
    | toString(EPILOGUE sym) =	(IO.endf sym ^ Imports.output())
    | toString(STRING(lab, str)) = let
        val string = String.concat(map transchar (explode str))
      in
        IO.dotData() ^ IO.label lab ^ IO.dotWord(size str) ^ IO.ascii string 
      end
  fun emitValue _ = error "emitValue"
  fun sizeOf _ = error "sizeOf"
  fun adjustLabels _ = error "adjustLabels"

end
