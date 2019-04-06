(* Copyright 1989 by AT&T Bell Laboratories *)
structure Typecheck  : TYPECHECK = struct

structure BareAbsyn = BareAbsyn

open Basics BasicTypes TypesUtil Unify BareAbsyn Overload ErrorMsg
     PrintUtil PrintType PrintAbsyn

val printDepth = System.Control.Print.printDepth

(* sorting numbered fields, used in typing record expressions *)
local
  val maxFieldNum = 100
  val buffer = array(maxFieldNum,
	             (Symbol.symbol("bogus"), UNDEFty))
in
  fun sortNumbered(numberedFields) =
      (app (fn (n,idty) => update(buffer,n,idty)) numberedFields;
       let fun collect(i,l) = 
	       if i<0 then l else collect(i-1,(buffer sub i)::l)
	in collect(length(numberedFields)-1,nil)
       end)
end (* local *)

exception NotThere

(* dbm, 6/23/90: constructor T added to properly identify toplevel and
   fix bug 224. *)
local datatype absp = A of int*int*int*absp| C of int*int*int*absp | N | T
in
   type occ = int * int * int * absp
   fun Rand (_,_,_,A x) = Rand(x)
     | Rand (lamd,absd,wmax,x) = (lamd,absd,min(wmax,absd),x)
   fun Abs (lamd,_,wmax,A(_,absd,_,x)) = (lamd+1,absd,wmax,x) 
     | Abs(x as (lamd,absd,wmax,_)) = (lamd+1,absd+1,wmax,C x)
   fun Rator(x as (lamd,absd,wmax,_)) = (lamd,absd-1,wmax,A x)
   fun LetDef(lamd,absd,wmax,x) = (lamd,absd,infinity,N)
   val Root = (0,0,infinity,T)
   fun lamdepth (lamd,_,_,_) = lamd
   fun abscount (_,absd,_,_) = absd
   fun wmax     (_,_,wm,_) = wm
   fun toplevel (0,0,_,T) = true | toplevel _ = false
end

fun decType(dec,toplev,err,loc) = 

let
fun generalizeTy(VALvar{typ,name=[n],...}, userbound: tyvar list,
		 occ:occ, loc) : unit =
    let fun complain() = 
		(err loc COMPLAIN "nongeneric weak type variable";
	     	 print "  "; printSym n; print " : ";
		 printType(!typ); newline())
	val index = ref 0  (* counts no of type variables bound *)
	fun next() = !index before inc index
	val sign = ref([]: {weakness:int,eq:bool} list)
	val uenv = array(length userbound, UNDEFty)
	fun pos(tv,tv'::rest) = if eqTyvar(tv,tv') then 0 else pos(tv,rest)+1
	  | pos(_,[]) = raise NotThere
	val menv = ref([]: (tyvar*ty) list)
	fun lookup tv =
	    let fun find [] = raise NotThere
		  | find((tv',ty)::rest) = if eqTyvar(tv,tv') then ty 
							      else find rest
	     in find(!menv)
	    end
	fun bind(b as (_,ty)) = (menv := b::(!menv); ty)
	fun gen(ty) =     
	    case ty
	     of VARty(tv as ref(META{depth,weakness,eq})) =>
			if depth > lamdepth occ
			  then if weakness > abscount occ
			       then lookup tv
			         handle NotThere =>
			         (sign := {weakness=weakness,eq=eq} :: !sign;
				  bind(tv,VARty(ref(IBOUND(next())))))
			       else (if toplevel occ then complain() else ();
				     ty)
			  else ty (* raise SHARE *)
	      | VARty(tv as ref(UBOUND{weakness,eq,...})) =>
			 (let val i = pos(tv,userbound)
			   in case (uenv sub i)
			        of UNDEFty =>
				   let val weakness = 
				         if weakness > abscount occ
					 then weakness
					 else (complain(); abscount occ + 1)
				       val new = VARty(ref(IBOUND(next())))
			            in update(uenv,i,new);
				       sign := {weakness=weakness,eq=eq}
					        :: !sign;
				       new
				   end
			         | ty => ty  (* raise SHARE *)
			  end
			  handle NotThere => ty)
	      | VARty(ref(INSTANTIATED ty)) => gen ty
	      | CONty(tyc,args) => CONty(tyc, map gen args) (*shareMap*)
	      | FLEXRECORDty(ref(CLOSED ty)) => gen ty
	      | FLEXRECORDty _ =>
		   (err loc COMPLAIN "unresolved flex record in let pattern";
		    PrintType.resetPrintType();
		    print "  type: "; printType ty; newline();
		    ERRORty)
	      | ERRORty => ERRORty
	      | _ => (err loc CASCADE "generalizeTy -- bad arg"; ERRORty)
	val ty = gen(!typ)
     in if !index > 0 
	then typ := POLYty{sign = rev(!sign), abs = abscount occ,
			   tyfun = TYFUN{arity=(!index),body=ty}}
	else ()
    end

fun generalizePat(pat: pat, userbound: tyvar list, occ: occ, loc) =
    let val rec gen = fn VARpat v => generalizeTy(v,userbound,occ,loc)
	       	       | RECORDpat{fields,...} => app (gen o #2) fields
	               | APPpat(_,arg) => gen arg
	               | CONSTRAINTpat(pat,_) => gen pat
	               | LAYEREDpat(varPat,pat) => (gen varPat; gen pat)
	               | _ => ()
     in gen pat
    end

fun applyType(ratorTy: ty, randTy: ty) : ty =
    let val resultType = VARty(mkTyvar defaultMETA)
     in unifyTy(ratorTy, (randTy --> resultType));
	resultType
    end

fun patType(pat: pat, tvkind, loc) : ty =
    case pat
      of WILDpat => VARty(mkTyvar tvkind)
       | VARpat(VALvar{typ as ref UNDEFty,...}) => 
					(typ := VARty(ref tvkind); !typ)
       | INTpat _ => intTy
       | REALpat _ => realTy
       | STRINGpat _ => stringTy
       | CONpat(DATACON{typ,...}) => applyPoly(typ,0,infinity)
       | RECORDpat{fields,flex,typ,...} =>
	   (* fields assumed already sorted by label *)
	   let val labtys =
		   map (fn (lab,pat') => (lab,patType(pat',tvkind,loc))) fields
	    in if flex
	       then let val ty = FLEXRECORDty(ref(OPEN labtys))
		     in typ := ty; ty
		    end
	       else recordTy(labtys)
	   end
       | APPpat(DATACON{typ,rep,...},arg) =>
	   let val argty = patType(arg,tvkind,loc)
	    in applyType(applyPoly((case rep of REF  => refPatType | _ => typ),
				   0,infinity),
			 argty)
	       handle Unify(mode) => 
		(err loc COMPLAIN("constructor and argument don't agree in pattern ("
			  ^ mode ^ ")");
		 PrintType.resetPrintType();
		 print "  constructor: "; printType typ; newline();
		 print "  argument:    "; printType argty; newline();
		 print "  in pattern:"; newline();
		 print "    "; printPat(pat,!printDepth); newline();
		 ERRORty)
	   end

       | CONSTRAINTpat(pat',ty) => 
	   let val patTy = patType(pat',tvkind,loc)
	    in (unifyTy(patTy, ty); ty)
	       handle Unify(mode) =>
	         (err loc COMPLAIN("pattern and constraint don't agree ("
			   ^ mode ^ ")");
		  PrintType.resetPrintType();
		  print "  pattern:    ";  printType patTy; newline();
		  print "  constraint: "; printType ty; newline();
		  print "  in pattern:"; newline();
		  print "    "; printPat(pat,!printDepth); newline();
	          ERRORty)
	   end
       | LAYEREDpat(VARpat(VALvar{typ,...}),pat) =>
			   (typ := patType(pat,tvkind,loc); !typ)
       | p => impossible "patType -- unexpected pattern"

fun expType(exp: exp, occ: occ, loc) : ty =
    case exp
      of VARexp(r as ref(VALvar{typ,access,name})) => 
	   let val ty = applyPoly(!typ,abscount occ,wmax occ)
	    in if Prim.special access  (*  =, <>, :=, update  special cases *)
	       then r := VALvar{typ= ref ty,access=access,name=name}
	       else ();
	       ty
	   end
       | VARexp(refvar as ref(OVLDvar _)) =>
	   pushOverloaded(refvar, err loc)
       | CONexp(DATACON{typ,...}) => applyPoly(typ,abscount occ,wmax occ)
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType(exp',occ,loc))))
				  fields
	    in recordTy(sortNumbered tyfields)
	   end
       | SEQexp exps => 
	   let fun scan nil = unitTy
	         | scan [e] = expType(e,occ,loc)
		 | scan (e::rest) = (expType(e,occ,loc); scan rest)
	    in scan exps
	   end
       | APPexp(rator, rand) =>
	   let val ratorTy = expType(rator,Rator occ,loc)
	       val randTy = expType(rand,Rand occ,loc)
	    in applyType(ratorTy,randTy)
	       handle Unify(mode) => 
	       let val ratorTy = prune ratorTy
		   val reducedRatorTy = headReduceType ratorTy
		in PrintType.resetPrintType();
		   if isArrowType(reducedRatorTy)
		   then (err loc COMPLAIN("operator and operand don't agree ("
				  ^ mode ^ ")");
			 print "  operator domain: ";
			 printType(domain reducedRatorTy); newline();
			 print "  operand:         ";
			 printType randTy; newline();
			 print "  in expression:"; newline();
			 print "    "; printExp(exp,4,!printDepth); newline();
			 ERRORty)
		   else (err loc COMPLAIN("operator is not a function");
			 print "  operator: "; printType(ratorTy); newline();
			 print "  in expression:"; newline();
			 print "    "; printExp(exp,4,!printDepth); newline();
			 ERRORty)
	       end
	   end
       | CONSTRAINTexp(e,ty) =>
	   let val ety = expType(e,occ,loc)
	    in (unifyTy(ety, ty); ty)
	       handle Unify(mode) =>
	         (err loc COMPLAIN("expression and constraint don't agree ("
			   ^ mode ^ ")");
		  PrintType.resetPrintType();
		  print "  expression: "; printType ety; newline();
		  print "  constraint: "; printType ty; newline();
		  print "  in expression:"; newline();
		  print "    "; printExp(e,4,!printDepth); newline();
		  ERRORty)
	   end
       | HANDLEexp(e,HANDLER h) =>
	   let val ety = expType(e,occ,loc)
	       and hty = expType(h,occ,loc)
	    in (unifyTy(hty, exnTy --> ety); ety)
	       handle Unify(mode) =>
	         let val hty = prune hty
		  in PrintType.resetPrintType();
		     if ((unifyTy(domain hty,exnTy); false) handle Unify _ => true)
		     then (err loc COMPLAIN("handler domain is not exn");
			   print "  handler domain: "; printType(domain hty);
			   newline();
		     	   print "  in expression:"; newline();
			   print "    "; printExp(exp,4,!printDepth); newline())
		     else (err loc COMPLAIN("expression and handler don't agree ("
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
	   let val ety = expType(e,occ,loc)
	    in unifyTy(ety,exnTy)
	       handle Unify(mode) =>
		(err loc COMPLAIN("argument of raise is not an exception");
		 PrintType.resetPrintType();
		 print "  raised: "; printType ety; newline();
		 print "  in expression:"; newline();
		 print "    "; printExp(exp,4,!printDepth); newline());
	       VARty(mkTyvar defaultMETA)
	   end
       | LETexp(d,e) => (decType0(d,LetDef(occ),loc); 
			 expType(e,occ,loc))
       | CASEexp(e,rules) =>
	   let val ety = expType(e,occ,loc)
	       and rty = matchType(rules,Rator occ,loc)
	    in applyType(rty,ety)
	       handle Unify(mode) => 
	       (err loc COMPLAIN("case object and rules don't agree ("
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
       | FNexp rules => matchType(rules,occ,loc)
       | MARKexp(e,locL,locR) => expType(e,occ,(locL,locR))

and ruleType(RULE(pat,exp),occ,loc) =  
 let val occ = Abs occ
  in patType(pat, META{depth=lamdepth occ, weakness=infinity,eq=false}, loc)
      --> expType(exp,occ,loc)
 end

and matchType(l,occ,loc) =
      case l
       of [] => impossible "empty rule list in typecheck.matchType"
        | [rule] => ruleType(rule,occ,loc)
        | rule::rest =>
   	 let val rty = ruleType(rule,occ,loc)
   	     fun checkrule rule' =
   		 let val rty' = ruleType(rule',occ,loc)
   		  in unifyTy(rty, rty')
   		     handle Unify(mode) =>
   		       (err loc COMPLAIN("rules don't agree (" ^ mode ^ ")");
   			PrintType.resetPrintType();
   			print "  expected: "; printType rty; newline();
   			print "  found:    "; printType rty'; newline();
   			print "  rule:"; newline();
   			print "    "; printRule(rule',4,!printDepth);
   			newline();
   			())
   		 end
   	  in app checkrule rest; rty
   	 end

and decType0(decl,occ,loc) =
    case decl
      of VALdec vbs =>
	   let fun vbType(vb as VB{pat, exp, tyvars}) =
	       let val pty = patType(pat,defaultMETA,loc)
		   and ety = expType(exp,occ,loc)
		in unifyTy(pty,ety)
		     handle Unify(mode) =>
		       (err loc COMPLAIN("pattern and expression\
				 \ in val dec don't agree (" ^ mode ^ ")");
		        PrintType.resetPrintType();
		        print "  pattern:    "; printType pty; newline();
			print "  expression: "; printType ety; newline();
			print "  in declaration:"; newline();
			print "    "; printVB(vb,2,!printDepth); newline());
		   generalizePat(pat,tyvars,occ,loc)
               end
	    in app vbType vbs
	   end
       | VALRECdec(rvbs) =>
 	   let fun setType(RVB{var=VALvar{typ,...}, resultty=NONE, ...}) =
		     typ := VARty(mkTyvar(META{depth = 1+ lamdepth occ,
					    weakness = infinity, eq = false}))
		 | setType(RVB{var=VALvar{typ,...}, resultty=SOME ty, ...}) =
		     typ := ty
		 | setType _  = impossible "typecheck.783"
	       fun rvbType(rvb as RVB{var=VALvar{typ,...},
				      exp,resultty,tyvars}) =
		   let val ety = expType(exp,Abs(Rator occ),loc)
		    in unifyTy(!typ, ety)
		        handle Unify(mode) =>
		        (err loc COMPLAIN("pattern and expression\
				      \ in val rec dec don't agree ("
				      ^ mode ^ ")");
			 PrintType.resetPrintType();
		         print "  pattern:    "; printType (!typ); newline();
			 print "  expression: "; printType ety; newline();
			 print "  in declaration:"; newline();
			 print "    "; printRVB(rvb,4,!printDepth);
			 newline())
		   end
 	       fun genType(RVB{var,tyvars,...}) =
			  generalizeTy(var,tyvars,occ,loc)
	    in app setType rvbs;
	       app rvbType rvbs;
	       app genType rvbs
	   end
       | EXCEPTIONdec(ebs) =>
	   let fun checkWeak(VARty(ref(UBOUND{weakness,...}))) = 
		     if weakness > abscount occ
		     then err loc COMPLAIN "type variable in exception type not weak enough"
		     else ()
		 | checkWeak(CONty(_,args)) =
		     app checkWeak args
		 | checkWeak _ = ()
	       fun ebType(EBgen{etype=SOME ty,...}) = checkWeak(ty)
	         | ebType _ = ()
            in app ebType ebs
	   end
       | LOCALdec(decIn,decOut) =>
	   (decType0(decIn,Abs(Rator occ),loc); 
	    decType0(decOut,occ,loc))
       | SEQdec(decls) => app (fn decl => decType0(decl,occ,loc)) decls
       | ABSTYPEdec{abstycs,withtycs,body} => 
	     let fun makeAbstract(GENtyc{stamp,arity,eq,path,kind}) =
		     (kind := ABStyc(GENtyc{stamp=stamp,arity=arity,path=path,
					    eq=eq,kind=ref(!kind)});
		      eq := NO)
	      in decType0(body,occ,loc);
		 app makeAbstract abstycs
	     end
       | MARKdec(dec, a,b) => decType0(dec,occ,(a,b))
       | _ => ()

in resetOverloaded(); 
   decType0(dec, if toplev then Root else Abs(Rator Root), loc);
   resolveOverloaded ()
end

end (* structure Typecheck *)
