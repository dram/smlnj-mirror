(* sigmatch.sml *)

structure SigMatch = struct

local 
  open List2 PrintUtil ErrorMsg Access Basics BareAbsyn BasicTypes
       EnvAccess EnvAccess.Env TypesUtil
  open PrintType
  val symName = Symbol.Name

 in
  fun arraymap (f,a) =
    let val b = array(length a, a sub 0) handlex subscript => a
        fun loop i = (update(b,i,f(a sub i)); loop(i+1))
     in loop 0 handlex subscript => b
    end

fun boundStamp stamp = stamp < 10000

fun (* newsig {t=0,s=0} = (fn x=>x)
  | *) newsig {t=tycNameBase, s=strNameBase} =
    let fun newEnv{s,t} = {s=arraymap(newStr,s), t=arraymap(newTyc,t)}
	and newStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if boundStamp(stamp)
	    then STRstr{stamp=stamp + strNameBase,
		        env=newEnv(env),
		        sign=sign,table=table,kind=kind}
	    else str
	  | newStr _ = Impossible "8123 in sigmatch"
	and newTyc(tyc) =
	    (case tyc
	      of VARtyc{stamp,name,arity} => (* VARtyc always bound *)
		   VARtyc{stamp=stamp+tycNameBase,name=name,arity=arity}
	       | DATAtyc{stamp,name,params,dcons} =>
		   if boundStamp(stamp)
		   then DATAtyc{stamp=stamp+tycNameBase,name=name,
				    params=params,dcons=dcons}
		   else tyc
	       | _ => tyc)
     in newStr
    end

exceptionx compareTypes

fun compareTypes(spec: ty, senv, actual: ty, name) : unit =
    let val env = ref []
	exceptionx lookup : unit
	fun lookup(tyv,env) =
	    case env
	      of [] => raisex lookup
	       | (tyv',ty)::rest => 
		   if eqTyvar(tyv,tyv') then ty else lookup(tyv,rest)
        fun bind(tyv,ty) = env := (tyv,ty)::(!env)
	fun comp1(ty1,ty2) =
	    case (ty1,ty2)
	      of (VARty tyv1,VARty tyv2) => 
		    if eqTyvar(tyv1,tyv2) then () else raisex compareTypes
	       | (CONty(ref tycon1, args1), CONty(ref tycon2, args2)) =>
		   if eqTyconRel(tycon1,senv,tycon2,senv)
		     then app2 comp1 (args1,args2)
		     else let val tycon1 = tyconInContext(tycon1,senv)
			      and tycon2 = tyconInContext(tycon2,senv)
			      val ty1 = CONty(ref tycon1,args1)
			      val ty2 = CONty(ref tycon2,args2)
			   in (case (tycon1,tycon2)
			         of (TYPEtyc _, _) => comp1(expandTy(ty1),ty2)
				  | (_, TYPEtyc _) => comp1(ty1,expandTy(ty2))
				  | _ => raisex compareTypes)
			  end
	       | _ => raisex compareTypes
	fun comp(spec,actual) =
	    case (spec,actual)
	      of (_, VARty(TYVAR{status=ref(INSTANTIATED(ty)),...})) =>
		   comp(spec,ty)
	       | (_, VARty(tyv)) =>
		   comp1(spec, lookup(tyv,!env))
		   handlex lookup => bind(tyv, spec)
	       | (CONty(ref tycon, args), CONty(ref tycon', args')) =>
		   let val tycon = tyconInContext(tycon,senv)
		    in if eqTycon(tycon,tycon')
		       then app2 comp (args,args')
		       else (case (tycon, tycon')
			       of (TYPEtyc _, _) => 
				    comp(expandTy(CONty(ref tycon,args)), actual)
			        | (_,TYPEtyc _) => comp(spec, expandTy actual)
				| _ => raisex compareTypes)
		   end
	        | (_, UNKNOWNty) => ()
		| _ => raisex compareTypes
     in comp(spec,actual)
	handlex compareTypes =>
		    (Complain "Type in structure doesn't match signature";
		     print ("name = " ^ symName name ^ "\nspec = ");
		     PrintType.printType(spec); print "\nactual = ";
		     PrintType.printType(actual); newline())
    end


(* signature matching *)

val defaultMapfns = 
    let fun ignore _ = ()
     in {mapstr=ignore,mapstr1=ignore,maptyc=ignore}
    end

fun sameSign(SOME(stamp1:int), SOME(stamp2:int)) = (stamp1=stamp2)
  | sameSign _ = false

fun sigMatch(mapfns as {mapstr,mapstr1,maptyc},
    	     sgn as STRstr{sign,...},
	     str as STRstr{stamp,sign=sign',table,env,...})
	   : Structure * thinning =
    if sameSign(sign,sign')
    then (mapstr(sgn,str); (str,NONE))
    else let val v = mkLvar()
	     val mark1 = mark() before (openStr(); openOld(([v],env),table))
	     val (str',transl) = sigFill(mapfns,stamp,sgn)
          in close mark1; (str',SOME(v,transl))
         end
  | sigMatch _ = Impossible "843 in sigmatch"

and sigFill(mapfns as {mapstr1,maptyc,mapstr},stamp,
	    STRstr{stamp=stamp',sign,table,env={s=senv,t=tenv},
	    	   kind=SIGkind{bindings,...},...})
	   : Structure * trans list =
    let val sNew = array(length senv, INDstr(~1))
	val tNew = array(length tenv, INDtyc [])
		   (* dummy initial values, never referenced *)
	val newenv = {s=sNew,t=tNew}
        fun checkSpec spec =
	    case spec
	      of STRbind(STRvar{name,binding=INDstr i,...}) =>
	       (* sharing checking ? *)
		  (let val STRvar{access,binding=str',...} = lookSTR name
		       val (newStr,thin) = sigMatch(mapfns, senv sub i, str')
		    in update(sNew,i,newStr);
		       [case thin
		          of NONE => VALtrans access
			   | SOME(v,transl) => THINtrans(access,v,transl)]
		   end
		   handlex Table.notfound =>
		     Condemn("sigMatch: missing structure " ^ symName name))
	       | TYCbind(ref(INDtyc [i])) =>
	       (* sharing checking ? *)
		 (let val tycon = tenv sub i
		      val name = tycName tycon
		   in (case (tycon, !(lookTYC name))
			 of (DATAtyc{params,dcons,...},
			     tycon' as DATAtyc{params=params',dcons=dcons',...}) =>
				 if length params = length params'
				     andalso length(!dcons) = length(!dcons')
				       then (update(tNew,i,tycon');
					     maptyc(tycon,tycon'); nil)
				       else Condemn "sigMatch: datatype mismatch"
			  | (DATAtyc _, _) => 
				Condemn("sigMatch: datatype expected - "
					^symName(name))
			  | (tycon,tycon') =>      
				 if tyconArity(tycon) = tyconArity(tycon')
				   then (update(tNew,i,tycon');
					 maptyc(tycon,tycon'); nil)
				   else Condemn "sigMatch: tycon arity")
		      handlex Table.notfound =>
		        Condemn ("sigMatch: missing type "^symName(name))
		   end)
	       | CONbind(DATACON{name,vtype,rep=ref(VARIABLE _),const,...}) =>
		  (let val DATACON{vtype=vtype',rep=ref(VARIABLE(access)),...} =
			     lookEXN name
		    in compareTypes(vtype,newenv,vtype',name);
		       [VALtrans access]
		   end
		   handlex Table.notfound =>
		     Condemn ("sigMatch: missing exception "^symName(name)))
	       | CONbind(DATACON{name,vtype,...}) =>
		  (let val DATACON{vtype=vtype',...} = lookCON name
		    in compareTypes(vtype,newenv,vtype',name); nil
		   end
		   handlex Table.notfound =>
		     Condemn ("sigMatch: missing data constructor "^symName(name)))
	       | VARbind(VALvar{name,vtype,...}) =>
		  ((case (lookVARCON name)
		     of VARbind(VALvar{access,vtype=vtype',...}) =>
			  (* no propagation of INLINE access!! *)
			 (compareTypes(!vtype,newenv,!vtype',name);
 			  [case access of INLINE _ => VALtrans access
					| PATH _ => VALtrans access
					| _ => Impossible "sigmatch.1"])
		      | CONbind(dcon as DATACON{vtype=vtype',...}) =>
			  (compareTypes(!vtype,newenv,vtype',name);
			   [CONtrans dcon])
		      | _ => Impossible "sigmatch.476")
		   handlex Table.notfound =>
		     Condemn ("sigMatch: missing val "^symName(name)))
	       | _ => Impossible "sigMatch--unexpected spec. "
        fun checkList (a::rest) =
	       (checkSpec a handlex Syntax => nil) @ checkList rest
          | checkList nil = nil
        val str = STRstr{stamp=stamp,sign=sign,table=table,env=newenv,kind=STRkind}
     in mapstr1(stamp',str);
	(str, checkList bindings)
    end
  | sigFill _ = Impossible "783 in sigmatch"

fun fctApply(FUNCTOR{param,tycCount,body},arg) : Structure * thinning =
    let val STRstr{kind=SIGkind{stampcounts={s=sCount,t=tCount},...},...} = param
	val tycmap = array(tycCount,INDtyc [])
	val strmap = array(sCount,INDstr(~1))
	fun maptyc(tycon,tycon') = update(tycmap,tycStamp tycon,tycon')
	fun mapstr1(stamp,str) = update(strmap,stamp,str)
	fun mapstr(sgn as STRstr{stamp,env={s,t},kind=SIGkind{bindings,...},...},
		   str as STRstr{table,env={s=s',t=t'},...}) =
	    let fun dobindings(STRbind(STRvar{name,binding=INDstr i,...})::rest) =
		      let val sgn' = s sub i
			  val str' = let val STRvar{binding,...} = 
					       lookSTRinTable(table,name)
				      in case binding
					   of INDstr j => s' sub j
					    | st => st
				     end
		       in mapstr(sgn',str');
			  dobindings(rest)
		      end
		  | dobindings(TYCbind(ref(INDtyc [i]))::rest) =
		      let val tycon = t sub i
			  val name = tycName tycon
			  val tycon' = case !(lookTYCinTable(table,name))
					 of INDtyc [j] => t' sub j
					  | tyc => tyc
		       in maptyc(tycon,tycon');
			  dobindings(rest)
		      end
		  | dobindings _ = ()
	     in update(strmap,stamp,str);
		dobindings(bindings)
	    end
          | mapstr _ = Impossible "887 in sigmatch"
	fun insttyc(tyc) =
	    case tyc
	      of VARtyc{stamp,...} => tycmap sub stamp
	       | DATAtyc{stamp,name,params,dcons} =>
		   if stamp < tCount  (* param *)
		   then tycmap sub stamp
		   else if stamp < stampBase  (* generative *)
		   then (case tycmap sub stamp
			  of INDtyc _ =>
			       let val newdcons = ref []
				   val newtyc = mkDATAtyc(name,params,newdcons)
				   val _ = update(tycmap,stamp,newtyc)
				in newtyc
			       end
			       (* BUG -- not properly updating dcons ? *)
			   | tyc' => tyc')
		   else tyc
	       | TYPEtyc{stamp,name,params,def} =>
		   if stamp < stampBase  (* generative *)
		   then (case tycmap sub stamp
			  of INDtyc _ =>
			       let val newtyc = mkTYPEtyc(name,params,insttype def)
				in update(tycmap,stamp,newtyc);
				   newtyc
			       end
			   | tyc' => tyc')
		   else tyc
	       | _ => tyc
	and insttype(ty) =
	    case ty
	      of CONty(ref tycon,args) =>
		   CONty(ref(insttyc tycon),map insttype args)
	       | _ => ty
	fun inststr(str as STRstr{stamp,sign,table,env,kind}) =
	    if stamp < sCount  (* parameter formal *)
	    then let val str' as STRstr{stamp=stamp',sign=sign',...}
			 = strmap sub stamp
		  in if sameSign(sign,sign')
		     then str'
		     else STRstr{stamp=stamp',sign=sign,table=table,kind=kind,
				 env=instenv(env)}
		 end
	    else if stamp < stampBase  (* generative formal *)
	    then mkSTR(sign,table,instenv(env))
	    else str  (* absolute *)
	  | inststr (INDstr i) = Impossible ("8484 in sigmatch: "^makestring i)
	and instenv(env as {s,t}:strenv) =
	    let val sNew = array(length s, INDstr(~1))
		val tNew = array(length t, INDtyc [])
			   (* dummy initial values, never referenced *)
		fun do_s(i) = (update(sNew,i,inststr(s sub i)); do_s(i+1))
		fun do_t(i) = (update(tNew,i,insttyc(t sub i)); do_t(i+1))
	     in do_s(0) handlex subscript => do_t(0) handle subscript => ();
		{s=sNew,t=tNew}
	    end
	val (_,thin) = sigMatch({mapstr=mapstr,mapstr1=mapstr1,maptyc=maptyc},
				param,arg)
	    (* don't need the instantiated parameter structure *)
     in (inststr body, thin)
    end

end (* local open Basics *)

end (* structure SigMatch *)

