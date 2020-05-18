(*
 * An ``intelligent'' pretty printer
 *)
signature PP =
sig

   type pp

   val ++       : pp * pp -> pp
   val !        : string -> pp
   val !!       : string -> pp
   val int      : int -> pp
   val char     : char -> pp
   val word     : Word32.word -> pp
   val real     : real -> pp
   val string   : string -> pp
   val bool     : bool -> pp
   val nop      : pp
   val indent   : pp
   val settab   : pp
   val unindent : pp
   val tab      : pp
   val tab'     : int -> pp
   val setmode  : string -> pp
   val unsetmode : pp
   val select   : (string -> pp) -> pp
   val sp       : pp
   val nl       : pp
   val nl'      : int -> pp
   val block    : pp -> pp
   val line     : pp -> pp
   val seq      : (pp * pp * pp) -> pp list -> pp
   val paren    : pp -> pp
   val group    : string * string -> pp -> pp
   val concat   : pp list -> pp
   val text     : pp -> string

end
