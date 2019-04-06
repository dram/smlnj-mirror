signature LINKAGE = 
	sig val getvars: Absyn.dec -> Access.lvar list
	    val bind: Access.lvar * System.Unsafe.object -> unit
	    val unbind: Access.lvar -> unit
	    val lookup: Access.lvar -> System.Unsafe.object
	    val bindLvars : Access.lvar list * System.Unsafe.object array
				 -> unit
	end

structure Linkage : LINKAGE = struct

 (* functions for retrieving new bound lvars from declaration abstract syntax *)

  open Access Basics BareAbsyn ErrorMsg

  fun smash f l = fold (fn (a,c) => f a @ c) l []

  fun patvars (VARpat(VALvar{access=PATH[v],...})) = [v]
    | patvars (VARpat(VALvar{access=INLINE _,...})) = []
    | patvars (VARpat _ ) = impossible "non-PATH in translate.patvars"
    | patvars (RECORDpat{fields,...}) = smash (fn (_,p) => patvars p) fields
    | patvars (APPpat(_,p)) = patvars p
    | patvars (CONSTRAINTpat(p,_)) = patvars p
    | patvars (LAYEREDpat(p,q)) = patvars p @ patvars q
    | patvars _ = []

  fun getvars (VALdec vbl) = smash (fn VB{pat,...} => patvars pat) vbl
    | getvars (a as VALRECdec rvbl) =
	smash (fn RVB{var=VALvar{access=PATH[var],...},exp,...} => [var]
	        | _ => impossible "#738 in translate")
	      rvbl
    | getvars (LOCALdec (localdec,visibledec)) = 
	(* it's necessary to "getvars localdec" in case the visibledec
		contains an "open".  Yuck. *)
		(getvars localdec @ getvars visibledec)
    | getvars (EXCEPTIONdec ebl) =
	map (fn EBgen{exn=DATACON{rep=VARIABLE(PATH[v]),...},...} => v
	      | EBdef{exn=DATACON{rep=VARIABLE(PATH[v]),...},...} => v
	      | _ => impossible "in getvars EXCEPTIONdec")
	    ebl
    | getvars (SEQdec decl) = smash getvars decl
    | getvars (DATATYPEdec _) = []
    | getvars (ABSTYPEdec{body,...}) = getvars body
    | getvars (TYPEdec _) = []
    | getvars (STRdec sbl) =
	map (fn STRB{strvar=STRvar{access=PATH[v],...},...} => v
	      | _ => impossible "getvars(STRdec)/fn"
	    ) sbl
    | getvars (ABSdec sbl) =
	map (fn STRB{strvar=STRvar{access=PATH[v],...},...} => v
	      | _ => impossible "getvars(ABSdec)/fn"
	    ) sbl
    | getvars (FCTdec fbl) =
	map (fn FCTB{fctvar=FCTvar{name,access=PATH[v],...},...} => v
	      | _ => impossible "getvars(FCTdec)/fn"
	    ) fbl
    | getvars (FIXdec _) = []
    | getvars (OVLDdec _) = []
    | getvars (OPENdec _) = []
    | getvars (SIGdec _) = []
    | getvars (IMPORTdec _) = impossible "getvars(IMPORTdec)"
    | getvars (MARKdec (dec,_,_)) = getvars dec

    (* set up top-level runtime environment, represented as intmap *)
    exception Runbind
    val t = Intmap.new(32, Runbind) : System.Unsafe.object Intmap.intmap
    val bind = Intmap.add t   (* add runtime binding *)
    val unbind = Intmap.rmv t (* remove runtime binding *)
    val _ = System.Unsafe.lookup_r := Intmap.map t
    val lookup = System.Unsafe.lookup

    fun bindLvars(newlvars,result) =
	let fun f(i,v::r) = (bind(v, result sub i); f(i+1,r))
	      | f(_,nil) = ()
	 in f(0,newlvars)
	end

end
