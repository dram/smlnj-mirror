(* DebugEnv
 
   Support for special looker function; intended use is to add
   SPECIAL(looker) to environment at appropriate spot in interactive system.
*)


signature DEBUG_ENV =
sig
  type time
  val useSpecial: bool ref
      (* true if special looker function enabled *)
  val blookup: int -> System.Unsafe.object option
      (* look up lvars associated with the bindings we return. *)
  val bclear: unit -> unit
      (* clear out all our bindings. *)
  val looker: Symbol.symbol -> Modules.binding
      (* the SPECIAL lookup function. *)
  val setLookerTimeF: (unit->time) -> unit
      (* set time function used by looker. *)
end

structure DebugEnv: DEBUG_ENV =
struct
  val useSpecial = ref true

  exception RunDebugBind
  val btable = (ref (Intmap.new(32,RunDebugBind))):System.Unsafe.object Intmap.intmap ref

  fun bclear () = btable := Intmap.new(32,RunDebugBind)
  val bind = Intmap.add (!btable)
  val lookup = Intmap.map (!btable)
  fun blookup lv = if !useSpecial then
                     (SOME (lookup lv)) handle RunDebugBind => NONE
		   else NONE

  open DebugUtil DebugRun DebugBindings DebugMotions Types Variables
         Access Absyn Modules PrintAbsyn PrintBasics PrintUtil CalcType

  exception Unbound = Env.Unbound

  (* look up symbol in a given namespace, and return new binding *)
  fun lookVARCON (time:time) (symbol:Symbol.symbol) : binding  =
	let val n = Symbol.name symbol
	    val _ = dbgprint ("lookVARCON " ^ n ^ "\n")
	    val (t,c,(i,binding)) = findVARCONBind ([n],time,0) 
	in case binding of
	     VARbind(v as VALvar _) => 
	       let val (evn,args) = evnArgsAt t
		   val bv = nth (nthArgs(evn,c,args),i)
		   val ty = chaseType (t,c) (VARexp(ref v))
		   val lv = namedLvar symbol
		   val _ = dbgprint "gotVALVAR\n"
	       in bind(lv,bv);
		  VARbind(VALvar{access=PATH[lv],
				 name=[symbol],
				 typ=ref ty})
	       end
	   | VARbind(OVLDvar _) =>
	       (dbgprint "gotOVLDVAR\n";
		binding)
	   | CONbind(dc as (DATACON{const,rep,sign,...}))  =>
	       let val typ = chaseType(t,c) (CONexp dc)
		   val rep = case rep of
			       VARIABLE _ => 
				  let val (evn,args) = evnArgsAt t
				      val ev = nth(nthArgs(evn,c,args),i)
				      val lv = namedLvar symbol
				  in bind (lv,ev);
				     VARIABLE (PATH[lv])
				  end
			     | VARIABLEc _ => 
				  let val (evn,args) = evnArgsAt t
				      val ev = nth(nthArgs(evn,c,args),i)
				      val lv = namedLvar symbol
				  in bind (lv,ev);
				     VARIABLEc (PATH[lv])
				  end
			     | _ => rep
		   val _ = dbgprint "gotCON\n"
	       in CONbind(DATACON{name=symbol,const=const,
				  typ=typ,rep=rep,sign=sign})
	       end
	end
  
  fun lookSTR (time:time) (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookSTR " ^ n ^ "\n")
	      val (t,c,(i,STRbind(STRvar{binding=s,...}))) = 
			     findSTRBind ([n],time,0)
	      val s = chaseStr t s
	      val sobj =
		  if i >= 0 then 
		    let val (evn,args) = evnArgsAt t
		    in nth(nthArgs(evn,c,args),i+1)
		    end
		  else let val args = argsAt (t-1)
		       in hd args
		       end
	      val lv = namedLvar symbol
	      val _ = dbgprint "gotSTR\n"
	  in bind (lv,sobj);
	     STRbind(STRvar{name=symbol,access=PATH[lv],binding=s})
	  end
  
  fun lookFCT (time:time) (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookFCT " ^ n ^ "\n")
	      val (t,c,(i,FCTbind(FCTvar{binding=f,...}))) = 
		  findFCTBind ([n], time,0)
	      val (evn,args) = evnArgsAt t
	      val fobj = nth(nthArgs(evn,c,args),i)
	      val lv = namedLvar symbol
	      val _ = dbgprint "gotFCT\n"
	  in bind (lv,fobj);
	     FCTbind(FCTvar{name=symbol,access=PATH[lv],binding=f})
	  end
  
  fun lookSIG (time:time) (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookSIG " ^ n ^ "\n")
	      val (t,c,(i,SIGbind sv)) = findSIGBind ([n],time,0)
	      val _ = dbgprint "gotSIG\n"
	  in SIGbind(sv)
	  end
  
  fun lookTYC (time:time) (symbol:Symbol.symbol) : binding = 
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookTYC " ^ n ^ "\n")
	      val (t,c,(i,TYCbind tycon)) = findTYCBind ([n],time,0)
	      val tycon = chaseTycon t tycon
	      val _ = dbgprint "gotTYC\n"
	  in TYCbind(tycon)
	  end
  
  fun lookFIX (time:time) (symbol:Symbol.symbol) : binding =
	  let val n = Symbol.name symbol
	      val _ = dbgprint ("lookFIX " ^ n ^ "\n")
	      val (t,c,(i,FIXbind fv)) = findFIXBind ([n],time,0)
	      val _ = dbgprint "gotFIX\n"
	  in FIXbind(fv)
	  end
  
  val lookerTimeF:(unit->time) ref = ref (DebugExec.currentTime)

  fun setLookerTimeF (timeF:unit->time) = lookerTimeF := timeF

  fun looker' (symbol:Symbol.symbol) :binding =
	let val space = Symbol.nameSpace symbol      
	in case (withEstablishedTime (fn _ => 
                  let val time = (!lookerTimeF)()
		  in	      
		   SOME (case space of
			   Symbol.VARspace => lookVARCON time symbol
			 | Symbol.TYCspace => lookTYC time symbol
			 | Symbol.SIGspace => lookSIG time symbol 
			 | Symbol.STRspace => lookSTR time symbol
			 | Symbol.FCTspace => lookFCT time symbol
			 | Symbol.FIXspace => lookFIX time symbol
			 | _ => raise Unbound)
		  end handle Unbound => NONE
		           | QueryInterrupted => NONE)) of
	     NOTRUNNING => raise Unbound
           | COMPLETED(SOME binding) => binding
           | COMPLETED NONE => raise Unbound
	   | INTERRUPTED _ => raise Unbound  (* somewhat superfluously *)
	end

   fun looker (symbol:Symbol.symbol) : binding =
     if !useSpecial then
       looker' symbol
     else raise Unbound

end




