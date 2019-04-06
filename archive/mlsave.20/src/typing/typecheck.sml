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
    let val resultType = VARty(mkTyvar defaultMETA)
     in unifyTy(operType, (randType --> resultType));
	resultType
    end;

exception NotThere

fun generalizeTy(refty as ref ty: ty ref, userbound: tyvar list,
		 abs: int, toplevel: bool) : unit =
    let val index = ref 0  (* counts no of type variables bound *)
	fun next() = !index before inc index
	val sign = ref([]: {weakness:int,eq:bool} list)
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
		     of META{depth,weakness,eq} =>
			  if depth > !lambdaDepth
			  then if weakness > abs
			       then lookup tv
			         handle NotThere =>
			         (sign := {weakness=weakness,eq=eq} :: !sign;
				  bind(tv,VARty(ref(IBOUND(next())))))
			       else if toplevel
			       then condemn "nongeneric weak type variable"
			       else ty
			  else ty (* raise SHARE *)
		      | UBOUND{weakness,eq,...} =>
			 (let val i = pos(tv,userbound)
			   in case (uenv sub i)
			        of UNDEFty =>
				     if weakness > abs
				     then let val new = VARty(ref(IBOUND(next())))
					   in update(uenv,i,new);
					      sign := {weakness=weakness,eq=eq}
						      :: !sign;
					      new
					  end
				     else condemn "nongeneric weak type variable"
			         | ty => ty  (* raise SHARE *)
			  end
			  handle NotThere =>
			    if toplevel
			    then condemn "(user) bound type variable propagated \
					 \out of scope" 
			    else ty)  (* raise SHARE *)
		      | INSTANTIATED ty => gen ty
		      | IBOUND n => impossible("generalizeTy--IBOUND "^makestring n))
	       | CONty(reftyc,args) => CONty(reftyc, map gen args) (* shareMap *)
	       | FLEXRECORDty(ref(CLOSED ty)) => gen ty
	       | FLEXRECORDty _ =>
		   (complain "unresolved flex record in let pattern";
		    PrintType.resetPrintType();
		    print "  type: "; printType ty; newline();
		    ERRORty)
	       | ERRORty => ERRORty
	       | _ => impossible "generalizeTy -- bad arg"
        val ty = gen ty
     in if !index > 0 
	then refty := POLYty{sign = rev(!sign),
		             tyfun = TYFUN{arity=(!index),body=ty}}
	else ()
    end

fun generalizePat(pat: pat, userbound: tyvar list, abs: int, toplevel: bool) : unit =
    let fun gen(pat) =
	    case pat
	      of VARpat(VALvar{name,vtype,...}) => 
		  (generalizeTy(vtype,userbound,abs,toplevel)
		   handle Syntax =>
		     (prstr "  "; printSym name; prstr " : ";
		      printType(!vtype); newline(); raise Syntax))
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
       | CONpat(DATACON{typ = ty,...}) => applyPoly(ty,0)
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
       | APPpat(DATACON{typ,rep,...},arg) =>  (* danger, exception case *)
	   let val argty = patType(arg,kind)
	    in applyType(applyPoly((case rep of REF  => refPatType | _ => typ),0),
			 argty)
	       handle Unify(mode) => 
		(complain("constructor and argument don't agree in pattern ("
			  ^ mode ^ ")");
		 PrintType.resetPrintType();
		 print "  constructor: "; printType typ; newline();
		 print "  argument:    "; printType argty; newline();
		 print "  in pattern:"; newline();
		 print "    "; printPat(pat,!printDepth); newline();
		 ERRORty)
	   end

       | CONSTRAINTpat(pat',ty) => 
	   let val patTy = patType(pat',kind)
	    in (unifyTy(patTy, ty); ty)
	       handle Unify(mode) =>
	         (complain("pattern and constraint don't agree ("
			   ^ mode ^ ")");
		  PrintType.resetPrintType();
		  print "  pattern:    ";  printType patTy; newline();
		  print "  constraint: "; printType ty; newline();
		  print "  in pattern:"; newline();
		  print "    "; printPat(pat,!printDepth); newline();
	          ERRORty)
	   end
       | LAYEREDpat(varPat,pat) =>
	   let val VARpat(VALvar{vtype = refty,...}) = varPat
	    in refty := patType(pat,kind);
	       !refty
	   end
       | p => impossible "patType -- unexpected pattern"

fun expType(exp,abs) : ty =
    case exp
      of VARexp(ref(VALvar{vtype,name,access})) => 
	   let val ty = applyPoly(!vtype,abs)
	    in if Prim.special access  (*  =, <>, :=, update  special cases *)
	       then vtype := ty
	       else ();
	       ty
	   end
       | VARexp(refvar as ref(OVLDvar _)) =>
	   pushOverloaded(refvar)
       | VARexp _ => impossible "expType -- bad VARexp"
       | CONexp(DATACON{typ,...}) => applyPoly(typ,abs)
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType(exp',abs))))
				  fields
	    in recordTy(sortNumbered tyfields)
	   end
       | SEQexp exps => 
	   let fun scan nil = unitTy
	         | scan [e] = expType(e,abs)
		 | scan (e::rest) = (expType(e,abs); scan rest)
	    in scan exps
	   end
       | APPexp(rator, rand) =>
	   let val ratorTy = expType(rator,abs-1)
	       and randTy = expType(rand,abs)
	    in applyType(ratorTy, randTy)
	       handle Unify(mode) => 
		 let val ratorTy = prune ratorTy
		     val reducedRatorTy = headReduceType ratorTy
		  in PrintType.resetPrintType();
		     if isArrowType(reducedRatorTy)
		     then (complain("operator and operand don't agree ("
				    ^ mode ^ ")");
			   print "  operator domain: ";
			   printType(domain reducedRatorTy); newline();
			   print "  operand:         ";
			   printType randTy; newline();
			   print "  in expression:"; newline();
			   print "    "; printExp(exp,4,!printDepth); newline();
			   ERRORty)
		     else (complain("operator is not a function");
			   print "  operator: "; printType(ratorTy); newline();
			   print "  in expression:"; newline();
			   print "    "; printExp(exp,4,!printDepth); newline();
			   ERRORty)
		 end
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val ety = expType(e,abs)
	    in (unifyTy(ety, ty); ty)
	       handle Unify(mode) =>
	         (complain("expression and constraint don't agree ("
			   ^ mode ^ ")");
		  PrintType.resetPrintType();
		  prstr "  expression: "; printType ety; newline();
		  print "  constraint: "; printType ty; newline();
		  print "  in expression:"; newline();
		  print "    "; printExp(e,4,!printDepth); newline();
		  ERRORty)
	   end
       | HANDLEexp(e,HANDLER h) =>
	   let val ety = expType(e,abs)
	       and hty = expType(h,abs)
	    in (unifyTy(hty, exnTy --> ety); ety)
	       handle Unify(mode) =>
	         let val hty = prune hty
		  in PrintType.resetPrintType();
		     if ((unifyTy(domain hty,exnTy); false) handle Unify _ => true)
		     then (complain("handler domain is not exn");
			   print "  handler domain: "; printType(domain hty);
			   newline();
		     	   print "  in expression:"; newline();
			   print "    "; printExp(exp,4,!printDepth); newline())
		     else (complain("expression and handler don't agree ("
			   ^ mode ^ ")");
			   print "  body:          "; printType ety; newline();
			   print "  handler range: "; printType(range hty);
			   newline();
		     	   print "  in expression:"; newline();
			   print "    "; printExp(exp,4,!printDepth); newline());
		     ERRORty
		 end
	   end
       | RAISEexp(e) =>
	   let val ety = expType(e,abs)
	    in unifyTy(ety,exnTy)
	       handle Unify(mode) =>
		(complain("argument of raise is not an exception");
		 PrintType.resetPrintType();
		 print "  raised: "; printType ety; newline();
		 print "  in expression:"; newline();
		 print "    "; printExp(exp,4,!printDepth); newline());
	       VARty(mkTyvar defaultMETA)
	   end
       | LETexp(d,e) => (decType0(d,false,abs); expType(e,abs))
       | CASEexp(e,rules) =>
	   let val ety = expType(e,abs)
	       and rty = matchType(rules,abs-1)
	    in applyType(rty,ety)
	       handle Unify(mode) => 
	       (complain("case object and rules don't agree ("
			   ^ mode ^ ")");
	        PrintType.resetPrintType();
		print "  rule domain: "; printType(domain rty); newline();
		print "  object:      "; printType ety; newline();
		print "  in expression:"; newline();
		print "    "; printExp(exp,4,!printDepth); newline();
	        ERRORty)
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp rules => matchType(rules,abs)

and ruleType(RULE(pat,exp),abs) =  
    patType(pat,META{depth=(!lambdaDepth),weakness=infinity,eq=false})
      --> expType(exp,abs+1)

and matchType(l,abs) =
    let val d = !lambdaDepth
     in (inc lambdaDepth;
         (case l
	    of [] => impossible "empty rule list in typecheck.matchType"
	     | [rule] => ruleType(rule,abs)
	     | rule::rest =>
		 let val rty = ruleType(rule,abs)
		     fun checkrule rule' =
			 let val rty' = ruleType(rule',abs)
			  in unifyTy(rty, rty')
			     handle Unify(mode) =>
			       (complain("rules don't agree (" ^ mode ^ ")");
				PrintType.resetPrintType();
				print "  expected: "; printType rty; newline();
				print "  found:    "; printType rty'; newline();
				print "  rule:"; newline();
				print "    "; printRule(rule',4,!printDepth);
				newline();
				())
			 end
		  in app checkrule rest; rty
		 end)
	 before lambdaDepth := d)
    end

and decType0(decl,toplevel,abs) =
    case decl
      of VALdec(vbs) =>
	   let fun vbType(vb as VB{pat, exp, tyvars}) =
	       let val pty = patType(pat,defaultMETA)
		   and ety = (markOverloaded(); expType(exp,abs))
		in unifyTy(pty,ety)
		     handle Unify(mode) =>
		       (complain("pattern and expression\
				 \ in val dec don't agree (" ^ mode ^ ")");
		        PrintType.resetPrintType();
		        print "  pattern:    "; printType pty; newline();
			print "  expression: "; printType ety; newline();
			print "  in declaration:"; newline();
			print "    "; printVB(vb,2,!printDepth); newline());
		   resolveOverloaded();
		   generalizePat(pat,tyvars,abs,toplevel)
               end
	    in app vbType vbs
	   end
       | VALRECdec(rvbs) =>
 	   let fun setType(RVB{var=VALvar{vtype,...}, resultty=NONE, ...}) =
		     vtype := VARty(mkTyvar(META{depth = 1+ !lambdaDepth,
						 weakness = infinity, eq = false}))
		 | setType(RVB{var=VALvar{vtype,...}, resultty=SOME ty, ...}) =
		     vtype := ty
		 | setType _  = impossible "typecheck.783"
	       fun rvbType(rvb as RVB{var, exp, resultty, tyvars}) =
		   let val VALvar{vtype = refty,...} = var
		       val d = !lambdaDepth;
		    in inc lambdaDepth;
		       (markOverloaded();
			let val ety = expType(exp,abs)
		         in unifyTy(!refty, ety)
		            handle Unify(mode) =>
		            (complain("pattern and expression\
				      \ in val rec dec don't agree ("
				      ^ mode ^ ")");
			     PrintType.resetPrintType();
		             print "  pattern:    "; printType (!refty); newline();
			     print "  expression: "; printType ety; newline();
			     print "  in declaration:"; newline();
			     print "    "; printRVB(rvb,4,!printDepth);
			     newline())
			end;
		        resolveOverloaded();
		        lambdaDepth := d)
		   end
 	       fun genType(RVB{var=VALvar{name,vtype,...},tyvars,...}) =
		    (generalizeTy(vtype,tyvars,abs,toplevel)
		     handle Syntax =>
		       (prstr "  "; printSym name; prstr " : ";
		        printType(!vtype); newline(); raise Syntax))
		 | genType _ = impossible "typecheck.845"
	    in app setType rvbs;
	       app rvbType rvbs;
	       app genType rvbs
	   end
       | EXCEPTIONdec(ebs) =>
	   let fun checkWeak(VARty(ref(UBOUND{weakness,...}))) = 
		     if weakness > abs
		     then condemn "type variable in exception type not weak enough"
		     else ()
		 | checkWeak(CONty(_,args)) =
		     app checkWeak args
		 | checkWeak _ = ()
	       fun ebType(EBgen{exn = DATACON{name,...},etype}) =
		   (case etype
		     of NONE => ()
		      | SOME ty => checkWeak(ty))
	         | ebType _ = ()
            in app ebType ebs
	   end
       | LOCALdec(decIn,decOut) =>
	   (decType0(decIn,false,abs); decType0(decOut,toplevel,abs))
       | SEQdec(decls) => app (fn decl => decType0(decl,toplevel,abs)) decls
       | ABSTYPEdec{body,...} => decType0(body,toplevel,abs)
       | _ => ()

fun decType(dec) = decType0(dec,true,0)

end (* structure Typecheck *)
