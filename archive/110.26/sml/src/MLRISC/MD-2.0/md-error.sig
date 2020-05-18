(*
 * Module for simple error handling with filenames/line numbers 
 *)
signature MD_ERROR =
sig

   val setLoc     : SourceMap.location -> unit
   val errorCount : int ref
   val init       : unit -> unit
   val log        : string -> unit
   val fail       : string -> 'a
   val error      : string -> unit
   val errorPos   : SourceMap.location * string -> unit
   val warning    : string -> unit
   val warningPos : SourceMap.location * string -> unit
   val withLoc    : SourceMap.location -> ('a -> 'b) -> 'a -> 'b
end
