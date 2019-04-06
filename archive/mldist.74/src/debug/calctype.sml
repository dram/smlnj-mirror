(* originally coded d r tarditi aug 89 *)

signature CALCTYPE = sig
  type encfunct
  datatype enclosure = 
      ENCLOSED of BareAbsyn.pat * (unit -> enclosure) * (unit -> args) 
    | NOTENCLOSED
  and args = 
      ARGS of BareAbsyn.exp * (unit -> enclosure) * (unit -> encfunct)
    | NOTAVAIL
  val getType : BareAbsyn.exp * (unit -> enclosure) * (unit -> encfunct)
                      -> Types.ty 
  val deabstyc : (unit -> encfunct) -> Types.tycon -> Types.tycon
  val deabsstr : (unit -> encfunct) -> Modules.Structure -> Modules.Structure
end  (* signature CALCTYPE *)

structure CalcType : CALCTYPE = struct

open Array List Types Variables BasicTypes Unify TypesUtil BareAbsyn Overload
     ErrorMsg PrintUtil PrintType PrintAbsyn  DebugUtil
infix 9 sub

(* for debugging *)
fun unifyTy (where:string) (ty1,ty2) =
  (if (!debugdebug) then
	(print where; print " unifying "; printType std_out (!debugEnv) ty1; 
	 print " with "; printType std_out (!debugEnv) ty2; print "\n")
   else ();
   Unify.unifyTy (ty1, ty2) handle Unify mode => 
       (print "Unify raised: "; print where; print " "; 
	print mode; print "\n";
        raise Unify mode))

type encfunct = (BareAbsyn.strexp * int) option
datatype enclosure = 
    ENCLOSED of pat * (unit -> enclosure) * (unit -> args)
  | NOTENCLOSED
and args = 
    ARGS of exp * (unit -> enclosure) * (unit -> encfunct)
  | NOTAVAIL

(* sorting numbered fields, used in typing record expressions *)
local
  val maxFieldNum = 10038
  val buffer = array(maxFieldNum,
	             (Symbol.labSymbol("bogus"), UNDEFty))
in
  fun sortNumbered(numberedFields) =
      (app (fn (n,idty) => update(buffer,n,idty)) numberedFields;
       let fun collect(i,l) = 
	       if i<0 then l else collect(i-1,(buffer sub i)::l)
	in collect(length(numberedFields)-1,nil)
       end)
end (* local *)

fun applyType(ratorTy: ty, randTy: ty) : ty =
    let val resultType = VARty(mkTyvar defaultMETA)
     in unifyTy "applyTy" (ratorTy, (randTy --> resultType));
	resultType
    end

val patType = fn mapTy =>
let
   (* loop invariant: mapTy will be applied once to each saved type.  A saved
      type is a type that was left after unification. *)
   fun patType(pat: pat, kind: tvkind) : ty =
    case pat
      of WILDpat => VARty(mkTyvar(kind))
       | VARpat(VALvar{typ,...}) => mapTy(!typ)
       | INTpat _ => intTy
       | REALpat _ => realTy
       | STRINGpat _ => stringTy
       | CONpat(DATACON{typ,...}) => mapTy (*??*)(applyPoly(typ,Root))
       | RECORDpat{fields,flex,typ,...} =>
	   (* fields assumed already sorted by label *)
	   let val labtys =
		   map (fn (lab,pat') => (lab,patType(pat',kind))) fields
	    in if flex
	       then mapTy(!typ)
	       (* recordTy consists of types calculated using patType.  This 
		  implies that mapType has already been applied *)
	       else recordTy(labtys)
	   end
       | APPpat(DATACON{typ,rep,...},arg) =>  (* danger, exception case *)
	   (* invariant is true here also *)
	   let val argty = patType(arg,kind)
	       val opty = applyPoly((case rep of Access.REF => refPatType | _ => typ),Root)
           in applyType(mapTy (*??*) opty,argty)
	   end
       | CONSTRAINTpat(pat',ty) => mapTy ty
       | LAYEREDpat(VARpat(VALvar{typ,...}),pat) => mapTy(!typ)
       | p => impossible "patType -- unexpected pattern"
in patType
end

val expType = fn mapTy =>
let val patType = patType mapTy
 fun expType(exp: exp, occ: occ) : ty =
    case exp
      of VARexp(ref(VALvar{typ,...})) => 
	           mapTy(applyPoly(!typ,occ))
       | VARexp _ => impossible "expType -- bad VARexp"
       | CONexp(DATACON{typ,...}) => 
	           mapTy (*??*)(applyPoly(typ,occ))
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType(exp',occ))))
				  fields
	    in recordTy(sortNumbered tyfields)
	   end
       | SEQexp exps => 
	   let fun scan nil = unitTy
	         | scan [e] = expType(e,occ)
		 | scan (e::rest) = scan rest
	    in scan exps
	   end
       | APPexp(rator, rand) => 
	   applyType(expType(rator,Rator occ),expType(rand,Rand occ))
       | CONSTRAINTexp(e,ty) => mapTy ty
       | HANDLEexp(e,HANDLER h) =>
	   let val ety = expType(e,occ)
	       and hty = expType(h,occ)
	    in (unifyTy "HANDLEexp" (hty, exnTy --> ety); ety)
	   end
       | RAISEexp(e) =>
	   let val ety = expType(e,occ)
	   in unifyTy "RAISEexp" (ety,exnTy);
	      VARty(mkTyvar defaultMETA)
	   end
       | LETexp(d,e) => expType(e,occ)
       | CASEexp(e,rules) =>
	   let val ety = expType(e,occ)
	       and rty = matchType(rules,Rator occ)
	    in applyType(rty,ety)
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp rules => matchType(rules,occ)
       | MARKexp(e,_,_) => expType(e,occ)

and ruleType(RULE(pat,exp),occ) =
 let val occ = Abstr occ
 in patType(pat,META{depth=lamdepth occ,weakness=infinity,eq=false})
      --> expType(exp, occ)
 end

and matchType(l,occ) = 
           case l
	    of [] => impossible "empty rule list in typecheck.matchType"
	     | [rule] => ruleType(rule,occ)
	     | rule::rest =>
		 let val rty = ruleType(rule,occ)
		     fun checkrule rule' =
			 let val rty' = ruleType(rule',occ)
			  in unifyTy "matchType" (rty, rty')
			 end
		  in app checkrule rest; rty
		 end

in expType
end

fun makeMap deabs =
let val l = ref (nil : (tyvar * tyvar) list)
    fun find a =
      let fun f nil = NONE
	    | f ((key,data)::r) = if key=a then SOME data else f r
      in case a 
	 of (ref (b as META _)) =>
	     (case f (!l)
	      of NONE => let val newRef = ref b
		         in l := ((a,newRef) :: (!l));
			    newRef
		         end
	       | SOME data => data)
 	  | (ref (UBOUND{depth,weakness,eq,...})) =>
	     (case f (!l) 
	      of NONE => let val newRef = ref (META{depth=depth,
						    weakness=weakness,eq=eq})
			 in l := ((a,newRef) :: (!l));
			    newRef
			 end
	       | SOME data => data)
	  | _ => a
      end
    fun mapTy ty =
	case ty
	of VARty (ref (INSTANTIATED ty)) => mapTy ty
	 | VARty tyvar => VARty(find tyvar)
         | CONty (tyc,args) => CONty(deabs tyc,map mapTy args)
         | FLEXRECORDty(ref(CLOSED ty)) => mapTy ty
	 | UNDEFty => ty
	 | ERRORty => ty
	 | _ => impossible "makeMap/mapTy--- impossible type in mapTy"
in mapTy
end 

val getMetaVars = fn ty =>
   let fun f (ty,l) =
     case ty
	of VARty (ref (INSTANTIATED ty)) => f(ty,l)
	 | VARty (r as ref (META _)) =>
	     if List.exists (fn a=>a=r) l then l else r :: l
	 | VARty _ => l
         | CONty (tyc,args) => fold f args l
         | FLEXRECORDty(ref(CLOSED ty)) => f (ty,l)
	 | UNDEFty => l
	 | ERRORty => l
	 | _ => impossible "getMetaVars/mapTy--- impossible type in mapTy"
   in f (ty,nil)
   end

fun disjoint(nil,_) = true
  | disjoint(_,nil) = true
  | disjoint(h::t,l) =
	if List.exists (fn a=>h=a) l then false else disjoint(t,l)

fun isConcrete (ref (META _)) = false
  | isConcrete (ref (INSTANTIATED ty)) = (length (getMetaVars ty) = 0)
  | isConcrete _ = true

val notConcrete = fn l =>
	fold (fn (x,r) => if isConcrete x then r else x::r) l nil


(* Analyse tycon, instantiating any parameter-dependent aspects if we're
   within a functor. Replace:
     FORMtycs with the actual parameter tyc,
     ABStycs with the "hidden" tyc,
     DATAtycs with their instantiated version,
     DEFtycs by recursing on their RHSs. *)
fun deabstyc _ tyc = tyc  (* kludge - new modules system *)
(**********
local 
  val {strStamps=adhocStrStamps,tycStamps=adhocTycStamps} = Stampset.newStampsets()
  val adhocStrs = ref nil:(int * Stampset.stamp * Modules.Structure) list ref
  val adhocTycs = ref nil:(int * Stampset.stamp * Types.tycon) list ref
in
fun deabstyc encfunct tyc = 
  let fun instance ef stamp = 
	(case ef of
	   APPstr{oper=FCTvar{binding=FUNCTOR{param,...},...},
		  str=STRstr{env=resultenv,...},...} =>
	     let val STRstr{kind=SIGkind{stamps={strStamps,...},...},
			     env,...} = param
		 fun find (REL{s,t},path) =
		      let fun tscan i =
			       case t sub i of
				 GENtyc{stamp=istamp,...} =>
				   if stamp = istamp then rev(i::path)
				   else tscan (i+1)
			       | _ => 
				 debugPanic "calctype.deabs bad tscan"
			  fun sscan i =
			       case s sub i of
				 STRstr{env,stamp,...} =>
				   if Stampset.member(stamp,strStamps) then
				     (find (env,i::path)
				        handle Subscript => sscan(i+1))
				   else sscan(i+1)
			       | NULLstr => sscan(i+1)
			       | _ => 
				 debugPanic "calctype.deabs bad sscan"
		      in tscan 0 
			 handle Subscript => sscan 1 
		      end
		| find _ = debugPanic "calctype.deabs bad find args"
	     in SOME(tyconInContext resultenv (RELtyc (find(env,[1]))))
		   handle Subscript => NONE
	     end
		| _ => (printStrexp (!debugEnv) (ef,0,1000);
		 debugPanic "calctype.deabs bad encfunct strexp"))
      fun detyfun (TYFUN{arity,body}) = 
    	      TYFUN{arity=arity,body=dety body}
      and dety (CONty(tycon,tyl)) = CONty(detyc tycon,map dety tyl)
        | dety (POLYty{sign,tyfun,abs}) = 
    	      POLYty{sign=sign,tyfun=detyfun tyfun,abs=abs}
        | dety ty = ty
      and detyc (tyc as GENtyc{kind=ref(FORMtyc),stamp,...}) = 
	   (case encfunct() of
	      SOME (ef,_) => 
		  (case instance ef stamp of
		     SOME tyc => tyc
		   | NONE => debugPanic "calctype.deabs FORMtyc bad stamp")
	    | NONE => debugPanic "calctype.deabs FORMtyc not in functor")
	| detyc (GENtyc{kind=ref(ABStyc hiddentyc),...}) = detyc hiddentyc
        | detyc (tyc as GENtyc{kind=ref(DATAtyc dcl),stamp,arity,eq,path}) =
	   (case encfunct() of
	      SOME (ef,tag) => 
	        (case instance ef stamp of
	           SOME tyc => tyc
		 | NONE => instantTyc(tag,tyc))
	    | NONE => tyc)
 	| detyc (DEFtyc{path,strict,tyfun}) =
	    DEFtyc{path=path,strict=strict,tyfun=detyfun tyfun}
	| detyc tyc = tyc
      and instantTyc (tag,GENtyc{kind=ref (DATAtyc dcl),stamp,arity,eq,path}) =
	  let fun find ((tag',stamp',tyc)::r) =
		     if tag = tag' andalso stamp = stamp' then tyc else find r
		| find nil = 
		     let val kind = ref(DATAtyc dcl)
			 val tyc = GENtyc{kind=kind,
					   stamp=Stampset.newStamp adhocStrStamps,
					   arity=arity, 
					   eq=eq,  (* this may be wrong ! *)
					   path=path}
		         fun dedcon (DATACON{name,const,typ,rep,sign}) =
			                DATACON{name=name,const=const,
						rep=rep,sign=sign,
						typ=dety typ}
		     in adhocTycs := (tag,stamp,tyc)::(!adhocTycs);
			kind := DATAtyc (map dedcon dcl);
		        tyc
		     end
	  in find (!adhocTycs)
          end
  in if (!debugdebug) then
	(print "deabs from "; printTycon std_out (!debugEnv) tyc; 
	 print "\n")
     else ();
     let val tyc' = detyc tyc
     in if (!debugdebug) then 
          (print "to "; printTycon std_out (!debugEnv) tyc'; print "\n")
        else ();
	tyc'
     end
  end
***********)

fun deabsstr _ str = str  (* kludge: new modules system *)
(**********
fun deabsstr encfunct (str as STRstr{stamp,sign,table,env,kind}) =
    let fun instance ef stamp = 
	(case ef of
	   APPstr{oper=FCTvar{binding=FUNCTOR{body=STRstr{env,...},...},...},
		  str,...} =>
	      let fun find (REL{s,t},path) =
		      let fun sscan i =
			       case s sub i of
				 STRstr{env,stamp=istamp,...} =>
				   if stamp = istamp then rev(i::path)
				   else (find (env,i::path)
				            handle Subscript => sscan(i+1))
			       | NULLstr => sscan(i+1)
			       | _ => 
				 debugPanic "calctype.deabsstr bad sscan"
		      in sscan 1 
		      end
		    | find _ = debugPanic "calctype.deabsstr bad find args"
	     in SOME(getEpath(find(env,[]),str))
		   handle Subscript => NONE
	     end
	 | _ => (printStrexp (!debugEnv) (ef,0,1000);
		 debugPanic "calctype.deabs bad encfunct strexp"))
    in (case encfunct() of
	   SOME (ef,tag) => 
	     (case instance ef stamp of
	        SOME str' => str'  (* already instantiated *)
      	      | NONE => (* not instantiated; must have been thinned out *)
		        str (* punt for now *))
         | NONE => str  (* not in functor *))
    end
  | deabsstr encfunct _ = debugPanic "calctype.deabsstr"
end
************)	

fun makeConcrete mapTy =
   let val expType' = expType mapTy
       val patType' = patType mapTy
   in fn (exp,encfn) =>
	let val _ = if !debugdebug then (print "exp = ";
					 PrintAbsyn.printExp (!debugEnv) (exp,0,1000);
					 print "\n")
		    else ()
	    val resultTy = expType'(exp,Root)
	    val _ = if !debugdebug then (print "stored type = ";
			                 printType std_out (!debugEnv) resultTy;
				         print "\n")
		    else ()
	    val metaVarList = getMetaVars resultTy
	    fun loop (_,nil) = resultTy
              | loop (encfn,l) =
		 (case (encfn()) of
		    NOTENCLOSED => resultTy
	          | ENCLOSED(pat,encfn,argFunc) =>
		     let val _ = if !debugdebug then (print "pat = ";
						      PrintAbsyn.printPat (!debugEnv) (pat,1000);
						      print "\n")
				 else ()
			 val patTy = patType' (pat, META{depth=0,
						   weakness=infinity,eq=false})
			 val _ = if !debugdebug then (print "pattern type = ";
						      printType std_out (!debugEnv) patTy;
						      print "\n")
				 else ()
		         val patMetaVarList = getMetaVars patTy
		     in if disjoint(l,patMetaVarList)
		 	then loop(encfn,l)
			else case (argFunc()) of
			       ARGS args =>
			         let val argType = getType args
			         in unifyTy "ARGS" (patTy,argType);
			            loop(encfn,notConcrete l)
			         end
			     | NOTAVAIL => resultTy  (* best can do -- a.t. *)
		     end)
	     val result = loop(encfn,metaVarList)
	     val _ = if !debugdebug then (print "end type = ";
				          printType std_out (!debugEnv) result;
				          print"\n")
		     else ()
	  in result
	  end
    end
				 
and getType (exp,encfn,encfunct) =
   let val newMap = makeMap (deabstyc encfunct)
       val _ = if !debugdebug then print "entering getType\n"
	       else ()
       val result = makeConcrete newMap (exp,encfn)
   in if !debugdebug then (print "exiting getType ";
		           printType std_out (!debugEnv) result;
		           print "\n")
      else ();
      result
   end

end
