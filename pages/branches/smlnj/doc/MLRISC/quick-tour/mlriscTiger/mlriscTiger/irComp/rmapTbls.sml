(* rmapTbls.sml --- A mapping of Tiger labels and temps to MLRISC
 *		    labels and temps 
 *)
functor RMapTbls(structure C:CELLS) : REGIGER_MAP_TABLES  = 
struct

 (* mapping of Tiger labels to MLRISC labels *)
  val labelTbl : Label.label Symbol.table ref = ref Symbol.empty
  fun lookupLabel lab =
    case Symbol.look(!labelTbl, lab)
     of SOME label => label
      | NONE => let
          val name = Symbol.name lab
	  val lab' = Label.newLabel name
        in
          labelTbl:= Symbol.enter(!labelTbl, lab, lab');
	  lab'
	end

  (* Mapping of Tiger temporaries to MLRISC temporaries *)
  val tempTbl : int Temp.Table.table ref = ref Temp.Table.empty

  fun lookupTemp tmp = 
    if tmp < C.firstPseudo then tmp
    else (case Temp.Table.look(!tempTbl, tmp)
      of SOME reg => reg
       | NONE => let
           val reg = C.newReg()
         in
	   tempTbl := Temp.Table.enter(!tempTbl, tmp, reg);
	    reg
	 end
      (*esac*))

  fun reset() = 
    (tempTbl :=  Temp.Table.empty;   labelTbl := Symbol.empty)
end