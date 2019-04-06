
structure EqTypes : EQTYPES =
struct
  (* functions to determine and check equality types *)

open Basics Stampset ErrorMsg

fun for l f = app f l
fun all (f: 'a -> bool) [] = true
  | all f (x::r) = f x andalso all f r

exception INCONSISTENT

fun join(MAYBE,YES) = YES
  | join(YES,MAYBE) = YES
  | join(MAYBE,NO) = NO
  | join(NO,MAYBE) = NO
  | join(IND,YES) = YES
  | join(YES,IND) = YES
  | join(IND,NO) = NO
  | join(NO,IND) = NO
  | join(MAYBE,IND) = MAYBE
  | join(IND,MAYBE) = MAYBE
  | join(IND,IND) = IND
  | join(MAYBE,MAYBE) = MAYBE
  | join(NO,NO) = NO
  | join(YES,YES) = YES
  | join(OBJ,OBJ) = OBJ
  | join(OBJ,_) = impossible "join"
  | join(_,OBJ) = impossible "join"
  | join _ = raise INCONSISTENT

fun objectTyc(TYCON{eq,...}) = case !eq of OBJ => true | _ => false

(* calculating eqtypes in toplevel signatures *)

exception NOT_EQ

fun checkdcons(datatycStamp: int,
	       findtyc: tycon -> tycon,
	       dcons: datacon list) : (eqprop * stamp list) =
    (* external stamps may occur in the depend set, in which case they will necessarily
       have IND eqprop *)
    let val depend = ref([]: stamp list)
	fun member(stamp,[]) = false
	  | member(st,st'::rest) = st=st' orelse member(st,rest)
        fun eqtyc(tyc as TYCON{stamp,kind,path,eq,...}) =
	    (case !eq
	       of YES => ()
		| OBJ => ()
	        | NO => raise NOT_EQ
		| IND =>
		    if member(stamp,!depend) orelse stamp = datatycStamp then ()
		    else depend := stamp :: !depend
		| MAYBE => 
		    if member(stamp,!depend) orelse stamp = datatycStamp then ()
		    else depend := stamp :: !depend)
	  | eqtyc _ = impossible "EqTypes.checkdcons"
        and eqdcon(DATACON{typ=ref(CONty(_,[dom,_])),const=false,...}) = eqty dom
	  | eqdcon(DATACON{typ=ref(POLYty{tyfun=TYFUN{body=CONty(_,[dom,_]),...},...}),
			   const=false,...}) = eqty dom
	  | eqdcon _ = ()
        and eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(ref tyc, args)) =
	    let val tyc = findtyc tyc  (* assuming that tyc cannot be a DEFtyc *)
	     in if objectTyc tyc then ()
		else (eqtyc(tyc); app eqty args)
	    end
	  | eqty _ = ()
     in app eqdcon dcons;
        case !depend
	  of [] => (YES,[])
	   | d => (MAYBE,d)
    end
    handle NOT_EQ => (NO,[])


fun eqAnalyze(sign as STRstr{env,
			     kind = SIGkind{stamps={strStamps,tycStamps},...},
			     ...},
	      err : complainer) : unit =
let val tycons: tycon list stampmap = newMap(tycStamps,[])
    val depend: stamp list list stampmap = newMap(tycStamps,[])
    val dependr: stamp list stampmap = newMap(tycStamps,[])
    val eqprop: eqprop stampmap = newMap(tycStamps,IND)
    val depext: stamp list ref = ref []
    val err = err COMPLAIN
    fun escan(env as REL{s,t}) =
	let fun tscan i =
		let val tyc as TYCON{stamp, eq, kind, ...} = t sub i
		 in (if member(stamp,tycStamps)
		     then (updateMap tycons (stamp,tyc :: applyMap(tycons,stamp));
			   case kind
			     of DATAtyc(dcons) =>
				  let val (eqp,deps) =
					 checkdcons(stamp,(TypesUtil.tyconInContext env),dcons)
				       val eq' = join(join(!eq,applyMap(eqprop,stamp)),eqp)
				   in  eq := eq';
				       updateMap eqprop (stamp,eq');
				       for deps (fn s =>
					 if member(s,tycStamps)
					 then updateMap dependr
						 (s, stamp :: applyMap(dependr,s))
					 else depext := stamp :: !depext);
				       if length deps = 1
					   andalso member(hd deps, tycStamps)
				       then updateMap dependr
					     (stamp, hd deps :: applyMap(dependr,stamp))
				       else ();
				       updateMap depend
					 (stamp, deps :: applyMap(depend,stamp))
				   end
			      | ABStyc _ =>
				   let val eqp = join(applyMap(eqprop,stamp),!eq)
				    in eq := eqp;
				       updateMap eqprop (stamp,eqp)
				   end)
		    else ()) (* external -- assume already defined *)
		    handle INCONSISTENT => err "inconsistent equality properties";
		    tscan(i+1) 
		end
	    fun sscan i =
		(case s sub i
		  of STRstr{env,stamp,sign,kind,...} =>
		       if member(stamp,strStamps)
		       then escan(env)
		       else ()
		   | _ => impossible "escan.sscan";
		 sscan(i+1))
	 in tscan 0
	    handle Subscript =>
	      sscan 2    (* not scanning parameter signature! *)
	      handle Subscript => ()
	end
      | escan(DIR) = impossible "EqTypes.eqAnalyze.escan"
    fun propNO(stamp,stamp') =
	(* propagate the NO eqprop *)
	if applyMap(eqprop,stamp) = NO
	then let val deps' = applyMap(dependr,stamp)
	     in  for deps' (fn s =>
		    if applyMap(eqprop,s) <> NO
		    then (updateMap eqprop
			   (s, join(NO,applyMap(eqprop,s)));
			  if s < stamp'
			  then propNO(s,stamp')
			  else ())
		    else ())
	     end
	else ()
    fun propIND(stamp,stamp') =
	(* propagate the IND eqprop *)
	if applyMap(eqprop,stamp) = IND
	then let val deps' = applyMap(dependr,stamp)
	     in  for deps' (fn s =>
		    if applyMap(eqprop,s) <> IND
		    then (updateMap eqprop
			   (s, join(IND,applyMap(eqprop,s)));
			  if s < stamp'
			  then propIND(s,stamp')
			  else ())
		    else ())
	     end
	else ()
 in escan env;
    (* pass 1: propagate NOs *)
    appStampset (fn s => propNO(s,s)) tycStamps;
    (* pass 2: propagate INDs *)
    for (!depext) (fn s =>  (* first propagate external INDs *)
      case applyMap(eqprop,s)
	of NO => ()
	 | YES => err "inconsistent equality properties (2)"
	 | IND => ()
	 | MAYBE => updateMap eqprop (s,IND));
    appStampset (fn s => propIND(s,s)) tycStamps;
    (* pass 3: convert MAYBE to YES and update tycon eq fields *)
    appStampset
      (fn s =>
         let val eqp = case applyMap(eqprop,s)
			 of MAYBE => YES
			  | e => e
	     val tycs = applyMap(tycons,s)
	  in updateMap eqprop (s,eqp);
	     for tycs (fn tyc as TYCON{eq,...} => eq := eqp)
	 end)
      tycStamps;
    (* pass 4: check consistency of NO tycons with their depend sets.
     This pass could be ommitted; any such inconsistency would prevent the
       specs from being matched. *)
    appStampset
      (fn s =>
         case applyMap(eqprop,s)
	   of NO =>
	       if all (fn [] => true
		        | l => exists (fn s' => (applyMap(eqprop,s') = NO)) l)
		      (applyMap(depend,s))
	       then ()
	       else err "inconsistent NO tycon"
	    | _ => ())
      tycStamps
end

exception CHECKEQ

fun defineEqTycon findtyc (tyc as TYCON{kind=DATAtyc _,...}) =
    let val visited = ref([]: tycon list)
	fun member(tyc,[]) = false
	  | member(tyc,tyc'::rest) =
	      TypesUtil.eqTycon(tyc,tyc') orelse member(tyc,rest)
        fun eqtyc(tyc as TYCON{kind,eq,...}) =
	      (case !eq
		 of MAYBE => 
		      if member(tyc,!visited) then MAYBE
		      else (visited := tyc :: !visited;
			    case kind
			      of DATAtyc(dcons) =>
				   let val eqp = checkdcons dcons
				    in eq := eqp;
				       eqp
				   end
			       | _ => impossible "defineEqTycon/eqtyc nonDATA")
		  | eqp => eqp)
	  | eqtyc _ = impossible "defineEqTycon/eqtyc -- bad tycon"
        and checkdcons dcons =
	    let fun loop([],eqp) = eqp
		  | loop(d::rest,eqp) =
		      case eqdcon d
			of NO => NO  (* return NO immediately, no further checking *)
			 | YES => loop(rest,eqp)
			 | IND => loop(rest,IND)
			 | MAYBE => 
			     (case eqp
				of IND => loop(rest,IND)
				 | _ => loop(rest,MAYBE))
		         | OBJ => impossible "defineEqTycon/checkdcons OBJ"
	     in loop(dcons,YES)
	    end
        and eqdcon(DATACON{typ=ref(CONty(_,[dom,_])),const=false,...}) = eqty dom
	  | eqdcon(DATACON{typ=ref(POLYty{tyfun=TYFUN{body=CONty(_,[dom,_]),...},...}),
			   const=false,...}) = eqty dom
	  | eqdcon _ = YES
        and eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(ref(tyc),args)) =
	      (case findtyc tyc
		 of TYCON{kind=DEFtyc(tyfun),...} =>
		     (* expand definitions unconditionally *)
		     eqty(TypesUtil.applyTyfun(tyfun,args))
		  | tyc => 
		     (case eqtyc tyc
			of NO => NO
			 | OBJ => YES
			 | YES => eqtys(args)
			 | MAYBE => (case eqtys(args) of YES => MAYBE | e => e)
			 | IND => IND))
	  | eqty _ = YES
	and eqtys(tys) =
	    let fun loop([],eqp) = eqp
		  | loop(ty::rest,eqp) =
		      case eqty ty
			of NO => NO  (* return NO immediately;
				      no further checking *)
			 | YES => loop(rest,eqp)
			 | IND => loop(rest,IND)
			 | MAYBE => 
			     (case eqp
				of IND => loop(rest,IND)
				 | _ => loop(rest,MAYBE))
		         | OBJ => impossible "defineEqTycon/eqtycs OBJ"
	     in loop(tys,YES)
	    end
     in case eqtyc(tyc)
	  of YES => for (!visited) (fn TYCON{eq,...} =>
		      (case !eq of MAYBE => eq := YES | _ => ()))
	   | MAYBE => for (!visited) (fn TYCON{eq,...} =>
		        (case !eq of MAYBE => eq := YES | _ => ()))
	   | NO => for (!visited) (fn TYCON{eq,...} =>
		      (case !eq of IND => eq := MAYBE | _ => ()))
		(* have to be reanalyzed, throwing away information *)
	   | IND => ()
	   | OBJ => impossible "defineEqTycon OBJ"
    end
  | defineEqTycon findtyc (tyc as TYCON{eq,kind=DEFtyc(TYFUN{body,...}),...}) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(ref(tyc),args)) =
	      (case findtyc tyc
		 of TYCON{kind=DEFtyc(tyfun),...} =>
		     (* expand definitions unconditionally *)
		     eqty(TypesUtil.applyTyfun(tyfun,args))
		  | TYCON{eq=ref eqp,...} => 
		     (case eqp
			of NO => NO
			 | OBJ => YES
			 | YES => eqtys(args)
			 | IND => IND
			 | MAYBE => impossible "defineEqTycon/DEFtyc 1"))
	  | eqty _ = YES
	and eqtys(tys) =
	    let fun loop([],eqp) = eqp
		  | loop(ty::rest,eqp) =
		      case eqty ty
			of NO => NO  (* return NO immediately;
				      no further checking *)
			 | YES => loop(rest,eqp)
			 | IND => loop(rest,IND)
			 | _ => impossible "defineEqTycon/DEFtyc 2"
	     in loop(tys,YES)
	    end
     in case !eq
	 of YES => ()
	  | NO => ()
	  | _ => eq := eqty(body)
    end
  | defineEqTycon _ _ = ()

fun isEqType ty =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(OPEN fields))) = raise CHECKEQ
	     (* bug? - regarded as nonequality because of missing fields *)
	      (* app (fn (_,ty) => eqty ty) fields  -- old version *)
	  | eqty(CONty(ref(TYCON{eq,kind=DEFtyc(tyfun as TYFUN{arity,...}),...}),
		       args)) =
	       if arity > 0
	       then eqty(TypesUtil.applyTyfun(tyfun,args))
	       else (case !eq
		       of YES => ()
			| NO => raise CHECKEQ
			| IND => raise CHECKEQ
			| _ => impossible "isEqType 1")
	  | eqty(CONty(ref(TYCON{eq,...}), args)) =
	      (case !eq
		 of OBJ => ()
		  | YES => app eqty args
		  | NO => raise CHECKEQ
		  | IND => raise CHECKEQ
		  | MAYBE => impossible "isEqType 2")
	  | eqty(VARty(ref(UBOUND{eq,...}))) =
	      if eq then () else (raise CHECKEQ)
	  | eqty _ = ()
     in eqty ty; true
    end
    handle CHECKEQ => false

fun checkEqTySig(ty, sign: polysign) =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(ref(TYCON{kind=DEFtyc tyfun,...}), args)) =
	      eqty(TypesUtil.applyTyfun(tyfun,args))
	  | eqty(CONty(ref(TYCON{eq,...}), args)) =
	     (case !eq  (* no DEFtyc case in signatures (yet!) *)
		of OBJ => ()
		 | YES => app eqty args
		 | NO => raise CHECKEQ
		 | IND => raise CHECKEQ
		 | MAYBE => impossible "checkEqTySig")
	  | eqty(VARty(ref(IBOUND n))) = 
	      let val {eq,...} = nth(sign,n)
	       in if eq then () else raise CHECKEQ
	      end
	  | eqty _ = ()
     in eqty ty;
	true
    end
    handle CHECKEQ => false

fun isEqTycon(TYCON{stamp,kind,path,eq,...}) =
    (case !eq
       of YES => true
	| NO => false
	| IND => false
	| _ => impossible "isEqTycon 1")
  | isEqTycon _ = impossible "isEqTycon 2"

end (* structure EqTypes *)
