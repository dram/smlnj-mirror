(* Copyright 1989 by AT&T Bell Laboratories *)
(* eqtypes.sml *)

structure EqTypes : EQTYPES =
struct
  (* functions to determine and check equality types *)

open Basics Stampset ErrorMsg TypesUtil

fun for l f = app f l
fun all (f: 'a -> bool) [] = true
  | all f (x::r) = f x andalso all f r

exception INCONSISTENT

fun join(MAYBE,YES) = YES
  | join(YES,MAYBE) = YES
  | join(MAYBE,NO) = NO
  | join(NO,MAYBE) = NO
  | join(MAYBE,IND) = IND
  | join(IND,MAYBE) = IND
  | join(MAYBE,MAYBE) = MAYBE
  | join(IND,YES) = YES
  | join(YES,IND) = YES
  | join(IND,NO) = NO
  | join(NO,IND) = NO
  | join(IND,IND) = IND
  | join(YES,YES) = YES
  | join(NO,NO) = NO
  | join(OBJ,OBJ) = OBJ
  | join(OBJ,_) = impossible "join"
  | join(_,OBJ) = impossible "join"
  | join _ = raise INCONSISTENT

fun objectTyc(GENtyc{eq=ref OBJ,...}) = true
  | objectTyc _ = false

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
        fun eqtyc(tyc as GENtyc{stamp,kind,path,eq,...}) =
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
	  | eqtyc(RECORDtyc _) = ()
	  | eqtyc _ = impossible "EqTypes.checkdcons"
        and eqdcon(DATACON{typ=CONty(_,[dom,_]),const=false,...}) = eqty dom
	  | eqdcon(DATACON{typ=POLYty{tyfun=TYFUN{body=CONty(_,[dom,_]),...},...},
			   const=false,...}) = eqty dom
	  | eqdcon _ = ()
        and eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(tyc,args)) =
	     (case findtyc tyc
	       of GENtyc{eq=ref OBJ,...} => ()
		| tyc' as GENtyc _ => (eqtyc tyc'; app eqty args)
                | tyc' as DEFtyc{tyfun,...} => eqty(applyTyfun(tyfun,args))
		| _ => app eqty args)
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
	       (case t sub i
		 of tyc as GENtyc{stamp, eq, kind, ...} =>
		     if member(stamp,tycStamps)
		     then (updateMap tycons (stamp,
					     tyc :: applyMap(tycons,stamp));
			   case kind
			     of DATAtyc(ref dcons) =>
				  let val oldeq = case !eq of IND => MAYBE | e => e
				      val (eqp,deps) =
					 checkdcons(stamp,(tyconInContext env),dcons)
				      val eq' = join(join(oldeq,applyMap(eqprop,stamp)),
						     eqp)
				   in eq := eq';
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
				handle INCONSISTENT => 
					err "inconsistent equality properties"
		     else () (* external -- assume already defined *)
		  | DEFtyc _ => ();
	         tscan(i+1))
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
			   (s, join(NO,applyMap(eqprop,s)))
				handle INCONSISTENT => 
					err "inconsistent equality properties";
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
	     for tycs (fn tyc as GENtyc{eq,...} => eq := eqp)
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

fun defineEqTycon findtyc (tyc as GENtyc{kind=DATAtyc _,path=n::_,...}) =
    let val visited = ref([]: eqprop ref list)
	fun member(eq,[]) = false
	  | member(eq: eqprop ref, eq'::rest) = eq=eq' orelse member(eq,rest)
        fun eqtyc(GENtyc{eq as ref MAYBE,kind=DATAtyc(ref dcons),...}) =
		      if member(eq,!visited) then MAYBE
		      else (visited := eq :: !visited;
			    eq := checkdcons dcons;
			    !eq)
	  | eqtyc(GENtyc{eq=ref eqp,...}) = eqp
	  | eqtyc(RECORDtyc _) = YES
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
        and eqdcon(DATACON{typ=CONty(_,[dom,_]),const=false,...}) = eqty dom
	  | eqdcon(DATACON{typ=POLYty{tyfun=TYFUN{body=CONty(_,[dom,_]),...},...},
			   const=false,...}) = eqty dom
	  | eqdcon _ = YES
        and eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(CONty(tyc,args)) =
	      (case findtyc tyc
		 of DEFtyc{tyfun,...} =>
		     (* expand definitions unconditionally *)
		     eqty(applyTyfun(tyfun,args))
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
     in case eqtyc tyc
	  of YES => for (!visited) (fn eq as ref MAYBE => eq := YES | _ => ())
	   | MAYBE => for (!visited) (fn eq as ref MAYBE => eq:=YES | _ => ())
	   | NO => for (!visited) (fn eq as ref IND => eq := MAYBE | _ => ())
		(* have to be reanalyzed, throwing away information *)
	   | IND => ()
	   | OBJ => impossible "defineEqTycon OBJ"
    end
  | defineEqTycon _ _ = ()

fun isEqType ty =
    let fun eqty(VARty(ref(INSTANTIATED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(CLOSED ty))) = eqty ty
	  | eqty(FLEXRECORDty(ref(OPEN fields))) = raise CHECKEQ
	     (* bug? - regarded as nonequality because of missing fields *)
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) = eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc{eq,...}, args)) =
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
	  | eqty(CONty(DEFtyc{tyfun,...}, args)) =
	      eqty(applyTyfun(tyfun,args))
	  | eqty(CONty(GENtyc{eq,...}, args)) =
	     (case !eq
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

fun replicate(0,x) = nil | replicate(i,x) = x::replicate(i-1,x)

fun isEqTycon(GENtyc{stamp,kind,path,eq,...}) =
    (case !eq
       of YES => true
	| OBJ => true
	| NO => false
	| IND => false
	| _ => impossible "isEqTycon 1")
  | isEqTycon(DEFtyc{tyfun as TYFUN{arity,...},...}) =
	isEqType(applyTyfun(tyfun,replicate(arity,BasicTypes.intTy)))
  | isEqTycon _ = impossible "isEqTycon 2"

end (* structure EqTypes *)
