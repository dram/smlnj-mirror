(* 
 * Maps strings to unique symbols. 
 * This saves space and makes equality testing and hashing quicker
 *)
signature SYMBOL =
sig

   datatype symbol = SYMBOL of string ref * word

   val eqSymbol   : symbol * symbol -> bool
   val hashSymbol : symbol -> word
   val new        : string -> symbol  
   val toString   : symbol -> string

end
