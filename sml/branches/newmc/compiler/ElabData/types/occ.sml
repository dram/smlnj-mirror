  (*
   * 8/18/92: cleaned up occ "state machine" some and fixed bug #612.
   * Known behaviour of the attributes about the context that are kept:
   * lamd = # of Abstr's seen so far.  Starts at 0 with Root.
   * top = true iff haven't seen a LetDef yet.
   *)
structure Occurence :> OCCURENCE =
struct

  datatype occ = OCC of {lamd: int, top: bool}

  val Root = OCC{lamd=0, top=true}

  fun LetDef(OCC{lamd,...}) = OCC{lamd=lamd, top=false}

  fun Abstr(OCC{lamd,top})  = OCC{lamd=lamd+1, top=top}

  fun lamdepth (OCC{lamd,...}) = lamd

  fun toplevel (OCC{top,...})  = top

end (* structure Occurrence *)
