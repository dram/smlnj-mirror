(* Copyright 1989 by AT&T Bell Laboratories *)
(* coreinfo.sml *)

(* info extracted from Core structure *)

structure CoreInfo : COREINFO =
struct

  open Access Basics EnvAccess
  val bogusCON = DATACON{name=Symbol.varSymbol "bogus",const=true,
			 typ=BasicTypes.exnTy,
			 rep=CONSTANT 0,sign=[]}

  val exnBind = ref(bogusCON)
  val exnMatch = ref(bogusCON)
  val stringequalPath = ref[0]
  val polyequalPath = ref[0]
  val currentPath = ref[0]
  val toplevelPath = ref[0]
  val forcerPath = ref[0]
  val getDebugVar = ref(mkVALvar (Symbol.varSymbol "getDebug"))

  fun resetCore () = 
      (exnBind := bogusCON; exnMatch := bogusCON;
       stringequalPath := [0];       polyequalPath := [0];
       currentPath := [0];       toplevelPath := [0];
       forcerPath := [0])

  fun setCore(STRvar{access=PATH p,binding,...}) =
      let fun extractPath name = 
	      let val sym = Symbol.varSymbol name
		  val VARbind(VALvar{access=PATH p,...}) =
			lookVARCONinStr(binding, sym, p, [sym],
					ErrorMsg.impossible)
	       in p
	      end
	  fun coreCon name = 
	      let val CONbind c = lookVARCONinStr(binding,name,p,[name],
						   ErrorMsg.impossible)
	       in c
	      end
       in exnBind := coreCon(Symbol.varSymbol "Bind");
	  exnMatch := coreCon(Symbol.varSymbol "Match");
	  stringequalPath := extractPath "stringequal";
	  forcerPath := extractPath "forcer_p";
	  polyequalPath := extractPath "polyequal";
	  currentPath := extractPath "current";
	  toplevelPath := extractPath "toplevel";
	  getDebugVar := let val name = Symbol.varSymbol "getDebug"
			     val VARbind x = lookVARCONinStr(binding,name,p,
						[name],ErrorMsg.impossible)
			  in x
			 end
      end
    | setCore _ = ErrorMsg.impossible "CoreInfo.setCore"

end (* CoreInfo *)
