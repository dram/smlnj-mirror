(* Copyright 1989 by AT&T Bell Laboratories *)
(* eqtypes.sml *)

structure EqTypes : EQTYPES =
struct
  (* functions to determine and check equality types *)

open Array List Basics Stampset ErrorMsg TypesUtil
infix 9 sub

fun for l f = app f l
fun all (f: 'a -> bool) [] = true
  | all f (x::r) = f x andalso all f r

(* DEBUGGING *)
fun printEqProp YES = print "YES"
  | printEqProp NO = print "NO"
  | printEqProp IND = print "IND"
  | printEqProp OBJ = print "OBJ"
  | printEqProp DATA = print "DATA"
  | printEqProp UNDEF = print "UNDEF"

exception INCONSISTENT

fun join(UNDEF,YES) = YES
  | join(YES,UNDEF) = YES
  | join(UNDEF,NO) = NO
  | join(NO,UNDEF) = NO
  | join(UNDEF,IND) = IND
  | join(IND,UNDEF) = IND
  | join(UNDEF,DATA) = DATA
  | join(DATA,UNDEF) = DATA
  | join(UNDEF,UNDEF) = UNDEF
  | join(DATA,YES) = YES
  | join(YES,DATA) = YES
  | join(DATA,NO) = NO
  | join(NO,DATA) = NO
  | join(DATA,IND) = IND
  | join(IND,DATA) = IND
  | join(DATA,DATA) = DATA
  | join(IND,NO) = NO
  | join(NO,IND) = NO
  | join(IND,IND) = IND
  | join(YES,YES) = YES
  | join(NO,NO) = NO
  | join(OBJ,OBJ) = OBJ
  | join _ = raise INCONSISTENT

fun objectTyc(GENtyc{eq=ref OBJ,...}) = true
  | objectTyc _ = false

(* calculating eqtypes in toplevel signatures *)

exception NOT_EQ

fun eqAnalyze(sign as STRstr{env,
			     kind = SIGkind{stamps={strStamps,tycStamps},...},
			     ...},
	      err : complainer) : unit =
let val tycons: tycon list stampmap = newMap(tycStamps,[])
    val depend: stamp list stampmap = newMap(tycStamps,[])
    val dependr: stamp list stampmap = newMap(tycStamps,[])
    val eqprop: eqprop stampmap = newMap(tycStamps,UNDEF)
    val depIND: stamp list ref = ref []
    val err = err COMPLAIN
    fun checkdcons(datatycStamp: int,
		   findtyc: tycon -> tycon,
		   dcons: datacon list) : (eqprop * stamp list) =
	let val depend = ref([]: stamp list)
	    fun member(stamp,[]) = false
	      | member(st,st'::rest) = st=st' orelse member(st,rest)
	    fun eqtyc(tyc as GENtyc{stamp,kind,path,eq,...}) =
		(case !eq
		   of YES => ()
		    | OBJ => ()
		    | NO => raise NOT_EQ
		    | IND =>
			if member(datatycStamp, !depIND) then () 
			else depIND := datatycStamp :: !depIND
		    | DATA =>
			if member(stamp,!depend) orelse stamp = datatycStamp then ()
			else depend := stamp :: !depend
		    | UNDEF => 
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
	       | d => (DATA,d)
	end
	handle NOT_EQ => (NO,[])
    fun escan(env as REL{s,t}) =
	let fun tscan i =
	       (case t sub i
		 of tyc as GENtyc{stamp, eq, kind, path=name::_, ...} =>
		     if member(stamp,tycStamps)  (* local spec *)
		     then (updateMap tycons (stamp,
					     tyc :: applyMap(tycons,stamp));
			   case !kind
			     of DATAtyc dcons =>
				  let val eqpStored = !eq
				      val (eqpCalc,deps) =
					  case eqpStored
					   of DATA => 
					      checkdcons(stamp,(tyconInContext env),
							 dcons)
					    | e => (e,[])  (* e = YES or NO *)
				      val eq' = join(join(eqpStored,
							  applyMap(eqprop,stamp)),
						     eqpCalc)
				   in eq := eq';
				      updateMap eqprop (stamp,eq');
				      for deps (fn s =>
					  updateMap dependr
					    (s, stamp :: applyMap(dependr,s)));
				      updateMap depend
					 (stamp, deps @ applyMap(depend,stamp))
				   end
			      | FORMtyc =>
				   let val eqp = join(applyMap(eqprop,stamp),!eq)
				    in eq := eqp;
				       updateMap eqprop (stamp,eqp)
				   end
			      | _ => impossible "eqAnalyze.escan.tscan")
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
    fun propagate (eqp,depset,earlier) =
	let fun prop stamp' =
		for (depset(stamp')) (fn s =>
		    let val eqpold = applyMap(eqprop,s)
                        val eqpnew = join(eqp,eqpold)
		     in if eqpold <> eqpnew
			then (updateMap eqprop (s,eqp);
			      if earlier s
			      then prop s
			      else ())
			else ()
		    end
		    handle INCONSISTENT =>
	              err "inconsistent equality properties B")
	in  prop
	end
    fun propagate_YES_NO(stamp) =
	(* propagate the NO eqprop forward and the YES eqprop backward *)
	let fun earlier s = s < stamp
	 in case applyMap(eqprop,stamp)
	     of YES => propagate (YES,(fn s => applyMap(depend,s)),earlier) stamp
	      | NO => propagate (NO,(fn s => applyMap(dependr,s)),earlier) stamp
              | _ => ()
	end
    fun propagate_IND(stamp) =
	(* propagate the IND eqprop *)
	let fun depset s = applyMap(dependr,s)
	    fun earlier s = s < stamp
	 in case applyMap(eqprop,stamp)
	     of UNDEF => 
		 (updateMap eqprop (stamp,IND);
		  propagate (IND,depset,earlier) stamp)
	      | IND => propagate (IND,depset,earlier) stamp
	      | _ => ()
	end
 in (* phase 0: scan signature strenv, joining eqprops of shared tycons *)
    escan env;
    (* phase 1: propagate YES backwards and NO forward *)
    appStampset propagate_YES_NO tycStamps;
    (* phase 2: convert UNDEF to IND and propagate INDs *)
    for (!depIND) (fn s =>  (* first propagate external INDs *)
      case applyMap(eqprop,s)
	of NO => ()
	 | YES => err "inconsistent equality properties (2)"
	 | IND => ()
	 | DATA => updateMap eqprop (s,IND)
	 | UNDEF => updateMap eqprop (s,IND)
         | _ => impossible "eqAnalyze: phase 2");
    appStampset propagate_IND tycStamps;  (* convert UNDEFs to INDs and propagate *)
    (* phase 3: convert DATA to YES; reset stored eqprops from eqprop map *)
    appStampset
      (fn s =>
          let val eqp = case applyMap(eqprop,s)
			  of DATA => YES
			   | e => e
	  in  for (applyMap(tycons,s)) (fn tyc as GENtyc{eq,...} => eq := eqp)
	  end)
      tycStamps
end

exception CHECKEQ

fun defineEqTycon findtyc (tyc as GENtyc{kind=ref(DATAtyc _),path=n::_,...}) =
    let val visited = ref([]: eqprop ref list)
	fun member(eq,[]) = false
	  | member(eq: eqprop ref, eq'::rest) = eq=eq' orelse member(eq,rest)
        fun eqtyc(GENtyc{eq as ref DATA,kind=ref(DATAtyc dcons),...}) =
	      if member(eq,!visited) then DATA
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
			 | DATA => 
			     (case eqp
				of IND => loop(rest,IND)
				 | _ => loop(rest,DATA))
		         | _ => impossible "defineEqTycon/checkdcons"
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
			 | DATA => (case eqtys(args) of YES => DATA | e => e)
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
			 | DATA => 
			     (case eqp
				of IND => loop(rest,IND)
				 | _ => loop(rest,DATA))
		         | _ => impossible "defineEqTycon/eqtycs"
	     in loop(tys,YES)
	    end
     in case eqtyc tyc
	  of YES => for (!visited) (fn eq as ref DATA => eq := YES | _ => ())
	   | DATA => for (!visited) (fn eq as ref DATA => eq := YES | _ => ())
	   | NO => for (!visited) (fn eq as ref IND => eq := DATA | _ => ())
		(* have to be reanalyzed, throwing away information *)
	   | IND => ()
	   | _ => impossible "defineEqTycon"
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
		  | _ => impossible "isEqType")
	  | eqty(CONty(RECORDtyc _, args)) = app eqty args
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
		 | _ => impossible "checkEqTySig")
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

fun isEqTycon(GENtyc{eq,...}) =
    (case !eq
       of YES => true
	| OBJ => true
	| _ => false)
  | isEqTycon(DEFtyc{tyfun as TYFUN{arity,...},...}) =
	isEqType(applyTyfun(tyfun,replicate(arity,BasicTypes.intTy)))
  | isEqTycon _ = impossible "isEqTycon 2"

end (* structure EqTypes *)
