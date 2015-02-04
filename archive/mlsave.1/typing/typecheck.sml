structure Typecheck  : TYPECHECK = struct

structure BareAbsyn = BareAbsyn

local
  open Basics BasicTypes TypesUtil BareAbsyn Overload ErrorMsg
       PrintUtil PrintType PrintAbsyn
  infix before
  fun a before b = a
in

infix -->

(* sorting numbered fields, used in typing record expressions *)
local
  val maxFieldNum = 100
  val buffer = array(maxFieldNum,
	             (SymbolTable.StringToSymbol("bogus"), UNKNOWNty))
in
  fun sortNumbered(numberedFields) =
      (app (fn (n,idty) => update(buffer,n,idty)) numberedFields;
       let fun collect(i,l) = 
		 if i<0 
		   then l
		   else collect(i-1,(buffer sub i)::l)
	in collect(length(numberedFields)-1,nil)
       end)
end (* local *)

val lambdaDepth = ref 0;  (* error recovery *)

fun reset() = (lambdaDepth := 0)

fun applyType(operType: ty, randType: ty) : ty =
    let val resultType = VARty(newTyvar METAARG)
     in unifyTy(operType, (randType --> resultType));
	resultType
    end;

fun generalizeTy(ty: ty) : unit =
    case prune ty
      of VARty(TYVAR{status=refstatus,...}) =>
	   (case !refstatus
	     of METALAM n =>
		  if n > !lambdaDepth
		     then refstatus := BOUND
		     else ()
	      | METAARG => refstatus := BOUND
	      | _ => ())
       | CONty(_,args) => app generalizeTy args
       | FLEXRECORDty _ =>
	   (Complain "unresolved flex record in pattern:";
	    printType ty; ())
       | _ => ();

fun generalizePat(pat: pat) : unit =
    case pat
      of VARpat(VALvar{vtype = ref ty,...}) => generalizeTy(ty)
       | RECORDpat{fields,...} =>
	   app (fn (lab,pat) => generalizePat(pat)) fields
       | APPpat(_,arg) => generalizePat(arg)
       | CONSTRAINTpat(pat',_) => generalizePat(pat')
       | LAYEREDpat(varPat,pat') =>
	   (generalizePat(varPat); generalizePat(pat'))
       | _ => ()

fun patType(pat: pat, status: tvstatus) : ty =
    case pat
      of WILDpat => VARty(newTyvar(status))  (* was newTyvar(METAARG) *)
       | VARpat(VALvar{vtype = refty,...}) =>
	   (refty := VARty(newTyvar(status)); !refty)
       | INTpat _ => intTy
       | REALpat _ => realTy
       | STRINGpat _ => stringTy
       | CONpat(DATACON{vtype = ty,...}) => freshTy ty   (* danger, exn types *)
       | RECORDpat{fields,flex,typ,...} =>
	   (* fields assumed already sorted by label *)
	   let val labtys =
		   map (fn (lab,pat') => (lab,patType(pat',status))) fields
	    in if flex
	       then let val ty = FLEXRECORDty{fields = labtys,
				              completion = ref UNKNOWNty}
		     in typ := ty; ty
		    end
	       else RECORDty(labtys)
	   end
       | APPpat(DATACON{vtype,...},arg) =>  (* danger, exception case *)
	   let val argty = patType(arg,status)
	    in applyType(freshTy vtype, argty)
	       handlex unify => 
		(Complain "type error: constructor and argument don't agree in pattern";
		 print "constructor: "; printType vtype;
		 print "\nargument : "; printType argty;
		 print "\npattern:\n"; printPat pat;
		 UNKNOWNty)
	   end

       | CONSTRAINTpat(pat,ty) => 
	   let val patTy = patType(pat,status)
	    in (unifyTy(patTy, ty); ty)
	       handlex unify =>
	         (Complain "type error: pattern and constraint don't agree";
		  prstr "pattern: ";
		  printPat pat; prstr " : "; printType patTy;
		  prstr "\nconstraint: "; printType ty; newline();
	          UNKNOWNty)
	   end
       | LAYEREDpat(varPat,pat) =>
	   let val VARpat(VALvar{vtype = refty,...}) = varPat
	    in refty := patType(pat,status);
	       !refty
	   end
       | p => Impossible "patType -- unexpected pattern"

fun expType(exp) : ty =
    case exp
      of VARexp(ref(VALvar{vtype = refTy,...})) => freshTy(!refTy)
       | VARexp(refvar as ref(OVLDvar _)) =>
	   pushOverloaded(refvar)
       | VARexp _ => Impossible "expType -- bad VARexp"
       | CONexp (DATACON{vtype = ty,...}) => freshTy(ty)
(*     |         !EXNcon(EXNCON{vtype,...}) => freshTy(!vtype))  (* danger *) *)
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType exp')))
				  fields
	    in RECORDty(sortNumbered tyfields)
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
	       handlex unify => 
		 (Complain "type error: operator and operand types\
			   \ don't agree in expression";
		  print "rator : "; printType ratorTy; newline();
		  print "rand : "; printType randTy; newline();
		  print "expression:\n  "; printExp exp; newline();
		  UNKNOWNty)
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val ety = expType(e)
	    in (unifyTy(ety, ty); ty)
	       handlex unify =>
	         (Complain "type error: expression and constraint don't agree";
		  print "expression: "; printExp e; prstr " : "; printType ety;
		  print "\nconstraint: "; printType ty; newline();
		  UNKNOWNty)
	   end
       | HANDLEexp(e,HANDLERX h) =>
	   let val ety = expType e
	       and hty = expType h
	    in (unifyTy(hty, exnTy --> ety); ety)
	       handlex unify =>
	         (Complain "type error: expression and handler don't agree";
		  print "expression ty: "; printType ety;
		  print "\nhandler ty: "; printType hty; newline();
		  print "expression:\n  "; printExp exp;
		  UNKNOWNty)
	   end
       | RAISEXexp(e) => (unifyTy(expType(e),exnTy)
		       handlex unify =>
		         (Complain "type error: argument of raise is not of type exn";
			  print "expression:\n  "; printExp e; print "\n"; ());
		       VARty(newTyvar METAARG))
       | LETexp(d,e) => (decType(d); expType(e))
       | CASEexp(e,rules) =>
	   let val ety = expType e
	       and rty = matchType rules
	    in applyType(rty,ety)
	       handlex unify => 
	       (Complain "type error: case object and rules don't agree";
		print "  object : "; printType ety;
		print "\n  rules : "; printType rty;
		print "\nexpression:\n"; printExp exp; newline();
	        UNKNOWNty)
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp rules => matchType rules
       | _ => Impossible "Typecheck--expType"

and ruleType(RULE(pat,exp)) =  
    patType(pat,METALAM(!lambdaDepth)) --> expType(exp)

and matchType l =
    let val d = !lambdaDepth
     in (inc lambdaDepth;
         (case l
	 of [] => Impossible "empty rule list in typecheck.matchType"
          | [rule] => ruleType rule
          | rule::rest =>
	      let val rty = ruleType rule
		  fun checkrule rule' =
		      let val rty' = ruleType rule'
		       in unifyTy(rty, rty')
			  handlex unify =>
			    (Complain "type error: rules don't agree";
			     print " expected: "; printType rty;
			     print "\n found:\n  "; printRule rule'; 
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
		     handlex unify =>
		       (Complain "type error: pattern and expression\
				 \ in val dec";
		        print "pattern: "; printType pty;
			print "\nexpression: "; printType ety;
			print "\ndeclaration:\n"; printVB vb; newline();
			());
		   resolveOverloaded();
		   generalizePat(pat);
		   app (fn (TYVAR{status,...}) => status := BOUND) tyvars
		(* danger: scope of constraint type variables, i.e.
		   which FIXED tyvars should be made BOUND *)
               end
	    in app vbType vbs
	   end
       | VALRECdec(rvbs) =>
 	   let fun setType(RVB{var=VALvar{vtype,...}, resultty=NONE, ...}) =
			    vtype := VARty(newTyvar(METALAM(1+ !lambdaDepth)))
		 | setType(RVB{var=VALvar{vtype,...}, resultty=SOME ty, ...}) =
			    vtype := ty
		 | setType _  = Impossible "typecheck.783"
	       fun rvbType(rvb as RVB{var, exp, resultty, tyvars}) =
		   let val VALvar{vtype = refty,...} = var
		       val d = !lambdaDepth;
		    in inc lambdaDepth;
		       (markOverloaded();
			let val ety = expType exp
		         in unifyTy(!refty, ety)
		            handlex unify =>
		            (Complain "type error: pattern and expression\
				      \ in val rec dec";
		           print "pattern: "; printType (!refty);
			   print "\nexpression: "; printType ety;
			   print "\ndeclaration:\n"; printRVB rvb; newline();
			   ())
			end;
		        resolveOverloaded();
		        lambdaDepth := d);
		       app (fn (TYVAR{status,...}) => status := BOUND) tyvars
		         (* danger: which FIXED tyvars bound *)
		   end
 	       fun genType(RVB{var=VALvar{vtype,...},...}) =
				 generalizeTy(!vtype)
		 | genType _ = Impossible "typecheck.845"
	    in app setType rvbs;
	       app rvbType rvbs;
	       app genType rvbs
	   end
       | EXCEPTIONdec(ebs) =>
	   let fun ebType(EB{exn = DATACON{vtype,...},
			     ty = opTy, def = opExn}) =
		   case opTy
		     of NONE => ()
		      | SOME ty => 
			  (case opExn
			     of NONE => ()
			      | SOME(DATACON{vtype = vtype',...}) =>
			          unifyTy(vtype, vtype')) (* fix -- bogus *)
            in app ebType ebs
	   end
       | LOCALdec(decIn,decOut) =>
	   (decType decIn; decType decOut)
       | SEQdec(decls) => app decType decls
       | _ => ();


fun expTypeTop(exp) =
    (lambdaDepth := 0;
     resetOverloaded();
     expType(exp) before resolveOverloaded())


end (* local open Bindings etc. *)

end (* structure Typecheck *)
