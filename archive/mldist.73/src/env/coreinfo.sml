(* Copyright 1989 by AT&T Bell Laboratories *)
(* coreinfo.sml *)

(* info extracted from Core structure *)

structure CoreInfo : COREINFO =
struct

  open Access Types Variables Modules ModuleUtil

  val exnBind = ref bogusEXN
  val exnMatch = ref bogusEXN
  val exnOrd = ref bogusEXN
  val exnRange = ref bogusEXN
  val exnSubscript = ref bogusEXN
  val exnRealSubscript = ref bogusEXN
  val stringequalPath = ref[0]
  val polyequalPath = ref[0]
  val currentPath = ref[0]
  val toplevelPath = ref[0]
  val forcerPath = ref[0]
  val getDebugVar = ref(mkVALvar (Symbol.varSymbol "getDebug"))

  fun resetCore () = 
      (exnBind := bogusEXN; exnMatch := bogusEXN;
       exnOrd := bogusEXN; exnRange := bogusEXN; exnSubscript := bogusEXN;
       exnRealSubscript := bogusEXN;
       stringequalPath := [0]; polyequalPath := [0];
       currentPath := [0]; toplevelPath := [0];
       forcerPath := [0])

  fun setCore(env, path) =
      let fun extractPath name = 
	      let val spath' = path @ [Symbol.varSymbol name]
		  val VARbind(VALvar{access=PATH p,...}) = 
		        lookVARCON (env,spath',fn _ => ErrorMsg.impossible)
	       in p
	      end
	  fun coreExn name = 
	      let val spath' = path @ [Symbol.varSymbol name]
	      in lookEXN (env,spath',fn _ => ErrorMsg.impossible)
	      end
       in exnBind := coreExn "Bind";
	  exnMatch := coreExn "Match";
          exnOrd := coreExn "Ord";
          exnRange := coreExn "Range";
          exnSubscript := coreExn "Subscript";
  	  exnRealSubscript := coreExn "RealSubscript";
	  stringequalPath := extractPath "stringequal";
	  forcerPath := extractPath "forcer_p";
	  polyequalPath := extractPath "polyequal";
	  currentPath := extractPath "current";
	  toplevelPath := extractPath "toplevel";
	  getDebugVar := let val name = Symbol.varSymbol "getDebug"
			     val VARbind x =
				 lookVARCON (env,path @ [name],
						 fn _ => ErrorMsg.impossible)
			  in x
			 end
      end

end (* CoreInfo *)
