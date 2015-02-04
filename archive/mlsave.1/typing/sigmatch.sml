(* sigmatch.sml *)

structure SigMatch = struct

local 
  open List2 PrintUtil ErrorMsg Access Basics BareAbsyn BasicTypes
       StrAccess EnvAccess TypesUtil
  open PrintType
  val symName = Symbol.Name
in

exceptionx compareTypes

fun tyconInStr(tycon,str) : tycon =
    case tycon
      of VARtyc{name,context=SIGctx,...} => !(lookTYCinStr(str,name))
       | DATAtyc{name,context=SIGctx,...} => !(lookTYCinStr(str,name))
       | SYMtyc(spath,name) =>
	  (case str
	    of PARAM _ => !(evalSYMtyc(spath,name,str))
	     | _ => let val (str',_) = getStr(str,spath,contextOf(str),[])
		     in !(lookTYCinStr(str',name))
		    end)
       | _ => tycon

fun typeInStr(ty,str) : ty =
    case ty
      of CONty(ref tycon, args) =>
	   CONty(ref(tyconInStr(tycon,str)),
	         map (fn ty' => typeInStr(ty',str)) args)
       | _ => ty

fun compareTypes(spec: ty, actual: ty, str: Structure, name: Symbol.symbol) : ty =
    let val env = ref []
	exceptionx lookup : unit
	fun lookup(tyv,env) =
	    case env
	      of [] => raisex lookup
	       | (tyv',ty)::rest => 
		   if eqTyvar(tyv,tyv') then ty else lookup(tyv,rest)
        fun bind(tyv,ty) = env := (tyv,ty)::(!env)
	fun comp(spec,actual) =
	    case (spec,actual)
	      of (_, VARty(TYVAR{status=ref(INSTANTIATED(ty)),...})) =>
		   comp(spec,ty)
	       | (_, VARty(tyv)) =>
		   compareTypes(spec, lookup(tyv,!env), str, name)
		   handlex lookup => 
		   let val ty = typeInStr(spec,str)
		    in bind(tyv, ty); 
		       ty
		   end
	       | (CONty(ref tycon, args), CONty(ref tycon', args')) =>
		   let val tycon = tyconInStr(tycon,str)
		    in if eqTycon(tycon,tycon')
		       then CONty(ref tycon', map2 comp (args,args'))
		       else let val spec = CONty(ref tycon,args)
			     in (case (tycon, tycon')
			          of (TYPEtyc _, _) => comp(expandTy spec, actual)
			           | (_,TYPEtyc _) => comp(spec, expandTy actual)
				   | _ => raisex compareTypes)
			    end
		   end
	        | (UNKNOWNty, _) => UNKNOWNty  (* propagate error *)
	        | (_, UNKNOWNty) => UNKNOWNty  (* propagate error *)
		| (_,_) => raisex compareTypes
     in comp(spec,actual)
	handlex compareTypes =>
		    (Complain "Type in structure doesn't match signature";
		     print ("name = " ^ symName name ^ "\nspec = ");
		     PrintType.printType(spec); print "\nactual = ";
		     PrintType.printType(actual); newline();
		     UNKNOWNty)		    
    end

fun sigMatch(SIG{stamp,elements,env},str: Structure) : Structure * thinning =
    let val resultTable = Table.new()
	and resultTrans = ref nil
        fun checkSpec(spec) =
	    case spec
	      of STRbind(STRvar{access=SLOT(new),name,binding=SPEC(sign),...}) =>
		   let val STRvar{access=PATH[old],binding,sign=sign',...} =
			     lookSTRinStr(str,name,[])
		       val (newStr,trans) = 
			   case sign'
			     of NONE => sigMatch(sign,binding)
			      | SOME(sign'') => 
			          if eqSignature(sign,sign'')
				    then (binding,NOTHIN)
				    else sigMatch(sign,binding)
		    in Table.add(resultTable,
		         (name, STRbind(
			  STRvar{name=name,
			  	 access=SLOT(new),
				 binding=newStr,
				 sign=SOME sign})));
		       resultTrans := STRtrans(old,trans) :: !resultTrans
		   end
		   handlex Table.notfound =>
		     Complain("sigMatch: missing structure "^symName(name))
	       | TYCbind(ref(VARtyc{name,arity,...})) =>
		   let val tyconRef as ref tycon = lookTYCinStr(str,name)
		    in if arity = tyconArity(tycon)
		         then (Table.add(resultTable,(name,TYCbind(tyconRef))); ())
			 else Complain "sigMatch: tycon arity"
		   end
		   handlex Table.notfound =>
		     Complain ("sigMatch: missing type "^symName(name))
	       | TYCbind(ref(DATAtyc{name,params,dcons,...})) =>
		  (case lookTYCinStr(str,name)
		   of tyconRef as ref(DATAtyc{params=params',dcons=dcons',...})
		      => if length params = length params'
			  andalso length(!dcons) = length(!dcons')
			 then (Table.add(resultTable,(name,TYCbind(tyconRef)));
			       ())
			 else Complain "sigMatch: datatype mismatch"
		   | _ => Complain ("sigMatch: datatype expected - "^symName(name)))
		   handlex Table.notfound =>
			   Complain ("sigMatch: missing datatype "^symName(name))
	       | CONbind(DATACON{name,vtype,rep=ref(VARIABLE(SLOT(new))),const,...}) =>
		   let val DATACON{vtype=vtype',rep=ref(VARIABLE(PATH[old])),...} =
			     lookEXNinStr(str,name,[])
		       val newty = compareTypes(vtype,vtype',str,name)
		    in Table.add(resultTable,(name,
		         CONbind(DATACON{name=name,const=const,vtype=newty,
			 	         rep=ref(VARIABLE(SLOT(new))),
					 tycon= !BasicTypes.exnTycon})));
		       resultTrans := VALtrans(old) :: !resultTrans
		   end
		   handlex Table.notfound =>
		     Complain ("sigMatch: missing exception "^symName(name))
	       | CONbind(DATACON{name,const,vtype,rep,tycon}) =>
		   let val DATACON{vtype=vtype',rep=rep',tycon=tycon',...} =
			     lookCONinStr(str,name,[])
		       val newty = compareTypes(vtype,vtype',str,name)
		    in Table.add(resultTable,(name,
		         CONbind(DATACON{name=name,const=const,vtype=newty,
			 	         rep=rep',
					 tycon=tycon'})));
		       ()
		   end
		   handlex Table.notfound =>
		     Complain ("sigMatch: missing data constructor "^symName(name))
	       | VARbind(VALvar{access=SLOT(new),name,vtype}) =>
		   (case lookVARCONinStr(str,name,[~30])
		     of VARbind(VALvar{access,vtype=vtype',...}) =>
			  (Table.add(resultTable,(name,
			    VARbind(VALvar{name=name,
			    		   access=(case access
						    of INLINE _ => access
						     | _ => SLOT(new)),
			 	vtype=ref(compareTypes(!vtype,!vtype',str,name))})));
		           resultTrans := 
 			     (case access of INLINE n => INLtrans n
					   | PATH[old,~30] => VALtrans(old)
					   | p => (PrintBasics.printAccess p;
					      Impossible "sigmatch.1"))
			     :: !resultTrans)
		      | CONbind(dcon as DATACON{vtype=ty,...}) =>
			  (Table.add(resultTable,(name,
			    VARbind(VALvar{name=name,access=SLOT(new),
			 		vtype=ref(compareTypes(ty,!vtype,str,name))})));
		           resultTrans := CONtrans(dcon) :: !resultTrans)
		      | _ => Impossible "sigmatch.476")
		   handlex Table.notfound =>
		     Complain ("sigMatch: missing val "^symName(name))
	       | _ => (printBinding(spec); newline();
		       Impossible "sigMatch--unexpected spec. ")
     in app checkSpec elements;
	(mkDIRECT(TOPctx,resultTable), THIN(rev(!resultTrans)))
    end


end (* local open Basics *)

end (* structure SigMatch *)
