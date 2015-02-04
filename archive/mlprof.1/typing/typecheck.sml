structure Typecheck  : TYPECHECK = struct

structure BareAbsyn = BareAbsyn

open Basics BasicTypes TypesUtil BareAbsyn Overload ErrorMsg
     PrintUtil PrintType PrintAbsyn

val printDepth = System.Control.Print.printDepth

val lambdaDepth = ref 0;  (* doesn't get reset with error recovery *)

fun reset() = (lambdaDepth := 0)

(* sorting numbered fields, used in typing record expressions *)
local
  val maxFieldNum = 100
  val buffer = array(maxFieldNum,
	             (Symbols.stringToSymbol("bogus"), UNDEFty))
in
  fun sortNumbered(numberedFields) =
      (app (fn (n,idty) => update(buffer,n,idty)) numberedFields;
       let fun collect(i,l) = 
	       if i<0 then l else collect(i-1,(buffer sub i)::l)
	in collect(length(numberedFields)-1,nil)
       end)
end (* local *)

fun applyType(operType: ty, randType: ty) : ty =
    let val resultType = VARty(mkTyvar METAARG)
     in unifyTy(operType, (randType --> resultType));
	resultType
    end;

exception NotThere

fun generalizeTy(refty as ref ty: ty ref, userbound: tyvar list) : unit =
    let val index = ref 0  (* counts no of type variables bound *)
	val uenv = array(length userbound, UNDEFty)
	fun pos(tv,tv'::rest) = if eqTyvar(tv,tv') then 0 else pos(tv,rest)+1
	  | pos(_,[]) = raise NotThere
	val menv = ref([]: (tyvar*ty) list)
	fun lookup tv =
	    let fun find [] = raise NotThere
		  | find((tv',ty)::rest) = if eqTyvar(tv,tv') then ty else find rest
	     in find(!menv)
	    end
	fun bind(b as (_,ty)) = (menv := b::(!menv); ty)
	fun gen(ty) =     
	    case ty
	      of VARty(tv) =>
		   (case !tv
		     of METALAM n =>
			  if n > !lambdaDepth
			  then lookup tv
			       handle NotThere =>
			         bind(tv,VARty(ref(IBOUND(!index before inc index))))
			  else ty (* raise SHARE *)
		      | METAARG =>
			  (lookup tv
			   handle NotThere =>
			    bind(tv,VARty(ref(IBOUND(!index before inc index)))))
		      | UBOUND _ =>
			 (let val i = pos(tv,userbound)
			   in case (uenv sub i)
			        of UNDEFty =>
				    let val new =
					 VARty(ref(IBOUND(!index before inc index)))
				     in update(uenv,i,new);
					new
				    end
			         | ty => ty  (* raise SHARE *)
			  end
			  handle NotThere => ty)  (* raise SHARE *)
		      | INSTANTIATED ty => gen ty
		      | IBOUND n => impossible("generalizeTy--IBOUND "^makestring n))
	       | CONty(reftyc,args) => CONty(reftyc, map gen args) (* shareMap *)
	       | FLEXRECORDty(ref(CLOSED ty)) => gen ty
	       | FLEXRECORDty _ =>
		   (complain "unresolved flex record in let pattern:";
		    PrintType.resetPrintType();
		    printType ty; ERRORty)
	       | ERRORty => ERRORty
	       | _ => impossible "generalizeTy -- bad arg"
        val ty = gen ty
     in if !index > 0 then refty := POLYty(TYFUN{arity=(!index),body=ty}) else ()
    end

fun generalizePat(pat: pat, userbound: tyvar list) : unit =
    let fun gen(pat) =
	    case pat
	      of VARpat(VALvar{name,vtype,...}) => generalizeTy(vtype,userbound)
	       | RECORDpat{fields,...} =>
		   app (fn (_,pat) => gen(pat)) fields
	       | APPpat(_,arg) => gen(arg)
	       | CONSTRAINTpat(pat',_) => gen(pat')
	       | LAYEREDpat(varPat,pat') =>
		   (gen(varPat); gen(pat'))
	       | _ => ()
     in gen(pat)
    end

fun patType(pat: pat, kind: tvkind) : ty =
    case pat
      of WILDpat => VARty(mkTyvar(kind))
       | VARpat(VALvar{vtype = refty,...}) =>
	   (refty := VARty(mkTyvar(kind)); !refty)
       | INTpat _ => intTy
       | REALpat _ => realTy
       | STRINGpat _ => stringTy
       | CONpat(DATACON{typ = ty,...}) => applyPoly ty
       | RECORDpat{fields,flex,typ,...} =>
	   (* fields assumed already sorted by label *)
	   let val labtys =
		   map (fn (lab,pat') => (lab,patType(pat',kind))) fields
	    in if flex
	       then let val ty = FLEXRECORDty(ref(OPEN labtys))
		     in typ := ty; ty
		    end
	       else recordTy(labtys)
	   end
       | APPpat(DATACON{typ,...},arg) =>  (* danger, exception case *)
	   let val argty = patType(arg,kind)
	    in applyType(applyPoly typ, argty)
	       handle Unify => 
		(complain "type error: constructor and argument don't agree in pattern";
		 PrintType.resetPrintType();
		 print "  constructor: "; printType typ;
		 print "\n  argument : "; printType argty;
		 print "\n  pattern:\n   "; printPat(pat,!printDepth);
		 print "\n\n";
		 ERRORty)
	   end

       | CONSTRAINTpat(pat,ty) => 
	   let val patTy = patType(pat,kind)
	    in (unifyTy(patTy, ty); ty)
	       handle Unify =>
	         (complain "type error: pattern and constraint don't agree";
		  PrintType.resetPrintType();
		  prstr "pattern: ";
		  printPat(pat,!printDepth); prstr " : "; printType patTy;
		  prstr "\nconstraint: "; printType ty;
		  print "\n\n";
	          ERRORty)
	   end
       | LAYEREDpat(varPat,pat) =>
	   let val VARpat(VALvar{vtype = refty,...}) = varPat
	    in refty := patType(pat,kind);
	       !refty
	   end
       | p => impossible "patType -- unexpected pattern"

fun expType(exp) : ty =
    case exp
      of VARexp(ref(VALvar{vtype,name,access})) => 
	   let val ty = applyPoly(!vtype)
	    in if Prim.special access  (* =, <>, :=, update special cases *)
	       then vtype := ty
	       else ();
	       ty
	   end
       | VARexp(refvar as ref(OVLDvar _)) =>
	   pushOverloaded(refvar)
       | VARexp _ => impossible "expType -- bad VARexp"
       | CONexp(DATACON{typ,...}) => applyPoly(typ)
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType exp')))
				  fields
	    in recordTy(sortNumbered tyfields)
	   end
       | SEQexp exps => 
	   let fun scan nil = unitTy
	         | scan [e] = expType e
		 | scan (e::rest) = (expType e; scan rest)
	    in scan exps
	   end
       | APPexp(rator, rand) =>
	   let val ratorTy = expType rator
	       and randTy = expType rand
	    in applyType(ratorTy, randTy)
	       handle Unify => 
		 (complain "type error: operator and operand don't agree";
		  PrintType.resetPrintType();
		  print "operator : "; printType ratorTy; newline();
		  print "operand : "; printType randTy; newline();
		  print "expression:\n  "; printExp(exp,2,!printDepth);
		  print "\n\n";
		  ERRORty)
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val ety = expType(e)
	    in (unifyTy(ety, ty); ty)
	       handle Unify =>
	         (complain "types of expression and constraint don't agree";
		  PrintType.resetPrintType();
		  print "  expression:\n   "; printExp(e,3,!printDepth);
		  prstr "\n  : "; printType ety;
		  print "\n  constraint: "; printType ty;
		  print "\n\n";
		  ERRORty)
	   end
       | HANDLEexp(e,HANDLER h) =>
	   let val ety = expType e
	       and hty = expType h
	    in (unifyTy(hty, exnTy --> ety); ety)
	       handle Unify =>
	         (complain "types of expression and handler don't agree";
		  PrintType.resetPrintType();
		  print "  expression ty: "; printType ety;
		  print "\n  handler ty: "; printType hty;
		  print "\n  expression:\n   "; printExp(exp,3,!printDepth);
		  print "\n\n";
		  ERRORty)
	   end
       | RAISEexp(e) =>
	   (unifyTy(expType(e),exnTy)
	   handle Unify =>
	     (complain "type error: argument of raise is not of type exn";
	      PrintType.resetPrintType();
	      print "expression:\n  "; printExp(e,2,!printDepth); print "\n"; ());
	      VARty(mkTyvar METAARG))
       | LETexp(d,e) => (decType(d); expType(e))
       | CASEexp(e,rules) =>
	   let val ety = expType e
	       and rty = matchType rules
	    in applyType(rty,ety)
	       handle Unify => 
	       (complain "type error: case object and rules don't agree";
	        PrintType.resetPrintType();
		print "  object : "; printType ety;
		print "\n  rules : "; printType rty;
		print "\n  expression:\n   "; printExp(exp,3,!printDepth);
		print "\n\n";
	        ERRORty)
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp rules => matchType rules

and ruleType(RULE(pat,exp)) =  
    patType(pat,METALAM(!lambdaDepth)) --> expType(exp)

and matchType l =
    let val d = !lambdaDepth
     in (inc lambdaDepth;
         (case l
	 of [] => impossible "empty rule list in typecheck.matchType"
          | [rule] => ruleType rule
          | rule::rest =>
	      let val rty = ruleType rule
		  fun checkrule rule' =
		      let val rty' = ruleType rule'
		       in unifyTy(rty, rty')
			  handle Unify =>
			    (complain "type error: rules don't agree";
			     PrintType.resetPrintType();
			     print " expected: "; printType rty;
			     print "\n found:\n  "; printRule(rule',2,!printDepth); 
			     print "\n  : "; printType rty'; newline();
			     ())
		      end
	       in app checkrule rest; rty
	      end)
	 before lambdaDepth := d)
     end

and decType decl =
    case decl
      of VALdec(vbs) =>
	   let fun vbType(vb as VB{pat, exp, tyvars}) =
	       let val pty = patType(pat,METAARG)
		   and ety = (markOverloaded(); expType exp)
		in unifyTy(pty,ety)
		     handle Unify =>
		       (complain "type error: pattern and expression\
				 \ in val dec";
		        PrintType.resetPrintType();
		        print "pattern: "; printType pty;
			print "\nexpression: "; printType ety;
			print "\ndeclaration:\n  "; printVB(vb,2,!printDepth);
			newline());
		   resolveOverloaded();
		   generalizePat(pat,tyvars)
               end
	    in app vbType vbs
	   end
       | VALRECdec(rvbs) =>
 	   let fun setType(RVB{var=VALvar{vtype,...}, resultty=NONE, ...}) =
			    vtype := VARty(mkTyvar(METALAM(1+ !lambdaDepth)))
		 | setType(RVB{var=VALvar{vtype,...}, resultty=SOME ty, ...}) =
			    vtype := ty
		 | setType _  = impossible "typecheck.783"
	       fun rvbType(rvb as RVB{var, exp, resultty, tyvars}) =
		   let val VALvar{vtype = refty,...} = var
		       val d = !lambdaDepth;
		    in inc lambdaDepth;
		       (markOverloaded();
			let val ety = expType exp
		         in unifyTy(!refty, ety)
		            handle Unify =>
		            (complain "type error: pattern and expression\
				      \ in val rec dec";
			     PrintType.resetPrintType();
		             print "pattern: "; printType (!refty);
			     print "\nexpression: "; printType ety;
			     print "\ndeclaration:\n  ";
			     printRVB(rvb,2,!printDepth);
			     newline())
			end;
		        resolveOverloaded();
		        lambdaDepth := d)
		   end
 	       fun genType(RVB{var=VALvar{name,vtype,...},tyvars,...}) =
		     generalizeTy(vtype,tyvars)
		 | genType _ = impossible "typecheck.845"
	    in app setType rvbs;
	       app rvbType rvbs;
	       app genType rvbs
	   end
       | EXCEPTIONdec(ebs) =>
	   let fun ebType(EB{exn = DATACON{typ,...},
			     ty = opTy, def = opExn}) =
		   case opTy
		     of NONE => ()
		      | SOME ty => 
			  (case opExn
			     of NONE => ()
			      | SOME(DATACON{typ = typ',...}) =>
			          unifyTy(typ, typ')) (* fix -- bogus *)
            in app ebType ebs
	   end
       | LOCALdec(decIn,decOut) =>
	   (decType decIn; decType decOut)
       | SEQdec(decls) => app decType decls
       | ABSTYPEdec{body,...} => decType body
       | _ => ()

end (* structure Typecheck *)
