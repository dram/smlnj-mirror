(* omit the frame pointer based by rewriting to use the stack pointer. *)

signature OMIT_FRAME_POINTER = sig
  structure I : INSTRUCTIONS
  structure CFG : CONTROL_FLOW_GRAPH where I = I
  
  (* idelta is the intial displacement between the fp and sp. *)
  val omitframeptr : {vfp:CellsBasis.cell, idelta:Int32.int option, cfg:CFG.cfg} -> unit
end
