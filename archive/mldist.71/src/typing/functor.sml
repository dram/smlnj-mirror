(* Copyright 1989 by AT&T Bell Laboratories *)
(* functor.sml *)

(* functor Functor (structure PrintType:PRINTTYPE): FUNCTOR =
struct

structure Env = PrintType.Env *)

structure Functor : FUNCTOR =

struct

open Array List ErrorMsg Stampset Basics TypesUtil EnvAccess 
     BareAbsyn ModUtil EqTypes PrintUtil
infix 9 sub

fun for l f = app f l

type epath = int list

fun backout([],m) = m
  | backout(_::r,m) = backout(r,0::m)

(* relative(curloc,rootpath) calculates the relative path from a given location,
   curloc, to the location indicated by a path, rootpath, which will be
   relative to a given root structure, i.e. the functor body structure *)

fun relative(l,y as [_]) = backout(l,y)
  | relative(i::l,y as j::m) = if i=j then relative(l,m) else backout(l,0::y)
  | relative([],y) = y
  | relative _ = impossible "Functor.relative"

(* abstraction of functor body; replacing all bound type constructors
   appearing in body table with relative versions *)

fun abstractBody
      (str : Structure,
       param as STRstr{env=paramenv,
		       stamp=paramstamp,
		       sign=paramsign,
		       kind=SIGkind{stamps={strStamps=paramStrStamps,
					    tycStamps=paramTycStamps},...},...},
       bodyStamps as {strStamps=bodyStrStamps,
		      tycStamps=bodyTycStamps}: stampsets,
       baseSigStamp : int,
       err)
      : Structure  =

let val paramStrStampMap = newMap(paramStrStamps,[]: (int * int list) list)
    val paramTycStampMap = newMap(paramTycStamps,[]: int list)
    val bodyStrStampMap  = newMap(bodyStrStamps,[]: (int * int list) list)
    val bodyTycStampMap  = newMap(bodyTycStamps,[]: int list)

    val bodyDEFtycs : (tycon * int list) list ref = ref nil
    exception Lookdeftyc
    fun lookdeftyc(t,(t',p)::r) = if t=t' then p else lookdeftyc(t,r)
      | lookdeftyc(_,nil) = raise Lookdeftyc

    (* initMaps, used to initialize param and body stampMaps *)
    fun initMaps (strStamps,tycStamps,strStampMap,tycStampMap) =
	let fun scanenv (REL{s,t},path) =
		let fun tscan i =
			(case t sub i
			  of GENtyc{stamp,...} =>
			      if member(stamp,tycStamps)
				andalso null(applyMap(tycStampMap,stamp))
				 then updateMap tycStampMap (stamp,rev(i::path))
				 else ()
			   | _ => ();
			 tscan(i+1))
		    fun sscan i =
			(case s sub i
			  of STRstr{env,stamp,sign,kind,...} =>
			       if member(stamp,strStamps)
			       then (updateMap strStampMap	
				      (stamp,(sign,rev(i::path))
					     ::applyMap(strStampMap,stamp));
				     scanenv(env, i::path))
			       else ()
			   | NULLstr => ()  (* for empty parameter slots *)
			   | _ => impossible "abstractBody.initMap.scanenv.sscan";
			 sscan(i+1))
		 in tscan 0
		    handle Subscript =>
		      sscan 1
		      handle Subscript => ()
		end
	      | scanenv _ = impossible "abstractBody.initMap.scanenv"
	 in scanenv
	end

    val initParamMaps = initMaps(paramStrStamps,paramTycStamps,
				 paramStrStampMap,paramTycStampMap)
    val initBodyMaps  = initMaps(bodyStrStamps,bodyTycStamps,
				 bodyStrStampMap,bodyTycStampMap)

    fun abstractType(path: int list, ty0: ty) : ty =
	let fun absty ty =
		case ty
		  of VARty(ref(INSTANTIATED ty')) => absty ty'
		   | FLEXRECORDty(ref(CLOSED ty')) => absty ty'
		   | POLYty{tyfun=TYFUN{body,arity},sign,abs} =>
		      POLYty{tyfun=TYFUN{body=absty body,arity=arity},sign=sign,abs=abs}
		   | VARty _ => ty
		   | CONty(tyc,args) =>
		     CONty(
		      (case tyc
			 of DEFtyc _ => 
			      (RELtyc(relative(path,
					       lookdeftyc(tyc,!bodyDEFtycs)))
			       handle Lookdeftyc => tyc)
			  | RECORDtyc _ => tyc
			  | GENtyc{stamp,path=spath,...} =>
			      if member(stamp,paramTycStamps)
			      then 
				RELtyc(relative(path,
					    applyMap(paramTycStampMap,stamp)))
			      else if member(stamp,bodyTycStamps) then
				 let val p = applyMap(bodyTycStampMap,stamp)
				  in if null p
				     then (err COMPLAIN("hidden tycon in functor body: ");
					   prSymPath(rev spath); newline(); 
					   tyc)
				     else RELtyc(relative(path,p))
				 end
			      else tyc   (* free tycon in body *)
			  | RELtyc _ => tyc (* ok when called from absLocSig *)
			  | _ => raise Cascade "abstractBody.abstractType 1"),
		       map absty args)
		   | ERRORty => ty
		   | _ => raise Cascade "abstractBody.abstractType 2"
	 in absty ty0 
	end

    (* absLocSig is used for substructures that are instantiations of local
      (open) signature expressions  *)

    fun absLocSig(str as STRstr{table,stamp,sign,env as REL{s,t},kind},
		  parent,epath) =
	let fun absbnd(VARbind(VALvar{name,typ,access})) =
		  VARbind(VALvar{name=name,access=access,
				 typ= ref(abstractType(epath,!typ))})
	      | absbnd(CONbind(DATACON{name,typ,const,rep,sign})) =
		  CONbind(DATACON{name=name, const=const, sign=sign, rep=rep,
				  typ=(abstractType(epath,typ))})
	      | absbnd(b as STRbind(STRvar{binding= INDstr i,name,access})) =
		 let val str' as STRstr{sign,...} = s sub i
		  in if sign > baseSigStamp
		     then (* substructure also locally constrained *)
			  STRbind(STRvar{binding=absLocSig(str',str,epath@[i]),
					 name=name,access=access})
		     else b
		 end
	      | absbnd binding = binding
	    val newtable = Env.map absbnd table
	 in update(s,0,parent);
	    STRstr{table=newtable,stamp=stamp,sign=sign,env=env,kind=kind}
	end
      | absLocSig _ = impossible "absLogSig - bad arg"

    (* this should be redefinced in Basics *)
    type binder = Symbol.symbol * Basics.binding

    fun bindGt((_,STRbind(STRvar{binding=STRstr{stamp=s1,...},...})),
	       (_,STRbind(STRvar{binding=STRstr{stamp=s2,...},...}))) =
		s1 > s2
      | bindGt((_,TYCbind _),_) = false
      | bindGt(_,(_,TYCbind _)) = true
      | bindGt((_,STRbind _),_) = true
      | bindGt _ = false    

    fun abstractStr(STRstr{stamp,sign,env=DIR,table,kind},parent,param,epath) =
	  let val bindref = ref(nil : binder list)
              fun collect binder = bindref := binder :: !bindref
              val binders = (Env.app collect table; !bindref)
              val binders = Sort.sort bindGt binders
	      val env = ref (Env.empty : Basics.env)
	      fun add (s,b) = env := Env.bind(s,b,!env)
	      val tycCount = ref 0
	      val strCount = ref 2
	      val tenvtemp = ref([]: tycon list)
	      val senvtemp = ref([]: Structure list)
	       (* better not update typ in place in abstractBind because of possible
	          sharing (?) *)
	      fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
		  DATACON{name=name,const=const,rep=rep,sign=sign,
			  typ=(abstractType(epath,typ))}
	      fun abstractBind(s,VARbind(VALvar{name,access,typ})) =
		    add(s,VARbind(VALvar{name=name,access=access,
					   typ=ref(abstractType(epath,!typ))}))
		| abstractBind(s,CONbind(dcon)) =
		    add(s,CONbind(abstractDcon dcon))
		| abstractBind(s,b as FIXbind _) = add(s,b)
		| abstractBind _ = ()
     (* abstractTycon should not be needed.  This was so that dcons could be
        instantiated when the functor is applied.  This is now done by insttype
	in applyFunctor. -- may be needed after all, for import
	      fun abstractTycon(GENtyc{kind=ref(DATAtyc dcons),stamp,arity,eq,path}) =
		  GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
			kind=ref(DATAtyc(map abstractDcon dcons))}
	        | abstractTycon(tycon) = tycon
      *)
	   in for binders
	       (fn (s,tb as TYCbind(tyc as DEFtyc _)) => 
		   (bodyDEFtycs := (tyc,rev(!tycCount::epath)) :: !bodyDEFtycs;
		    add(s,TYCbind(INDtyc(!tycCount)));
		    tenvtemp := tyc :: !tenvtemp;
		    inc tycCount)
	         | (s,tb as TYCbind(tyc as GENtyc{stamp,...})) => 
		   if member(stamp,bodyTycStamps)
		   then case applyMap(bodyTycStampMap,stamp)
			 of [] => (updateMap bodyTycStampMap
				    (stamp,rev(!tycCount::epath));
				   add(s,TYCbind(INDtyc(!tycCount)));
				   tenvtemp := (* abstractTycon *) tyc :: !tenvtemp;
				   inc tycCount)
			  | path => 
			       add(s,TYCbind(SHRtyc(relative(epath,path))))
		   else if member(stamp,paramTycStamps)
		      (* parameter bound tycon -- e.g. from opened param *)
		   then let val path = applyMap(paramTycStampMap,stamp)
			 in add(s,TYCbind(SHRtyc(relative(epath,path))))
			end
		   else (* fixed tycon - e.g. from opened external structure *)
			add(s,tb)
	        | (s,sb as STRbind(STRvar{name,access,
					     binding as STRstr{stamp,sign,...}})) =>

		   let fun addStr(stampMap,paths) =
			(updateMap stampMap
			   (stamp,(sign,rev(!strCount :: epath))::paths);
			 add(s,STRbind(STRvar{name=name,access=access,
			 			binding=INDstr(!strCount)}));
			 senvtemp := binding :: !senvtemp;
			 inc strCount)
		       fun look((sign',path)::rest) =
			     if sign = sign'
			     then path
			     else look rest
			 | look [] = []
		    in if member(stamp,bodyStrStamps) then
			  let val paths = applyMap(bodyStrStampMap, stamp)
			      val path = look paths
			   in if null path
			      then addStr(bodyStrStampMap,paths)
			      else add(s, STRbind(
					STRvar{name=name,access=access,
				    	       binding=SHRstr(relative(epath,path))}))
			  end
		       else if member(stamp,paramStrStamps) (* parameter structure *)
		       then let val paths = applyMap(paramStrStampMap, stamp)
				val path = look paths
			     in if null path
				then addStr(paramStrStampMap,paths)
				else add(s,STRbind(
					STRvar{name=name,access=access,
				    	       binding=SHRstr(relative(epath,path))}))
			    end
		       else add(s,sb)  (* fixed structure *)
		   end
	         | _ => ());
	      app abstractBind binders;
	      let val substrs = rev(!senvtemp)
		  val senv  = array(length substrs + 2, NULLstr)
		  val tenv  = arrayoflist(rev(!tenvtemp))
		  val newenv = REL{s=senv,t=tenv}
		  val newstr = STRstr{stamp=stamp,sign=sign,kind=kind,
				      table= !env,
				      env=newenv}
	       in update(senv,0,parent);
		  update(senv,1,param);
		  revfold 
		    (fn (str,i) =>
			(update(senv,i,abstractStr(str,newstr,NULLstr,i::epath));
			 i+1))
		    substrs 2;
		  newstr
	      end
	  end
      | abstractStr(str as STRstr{stamp,sign,env as REL _,...},parent,_,epath) =
	  (if sign > baseSigStamp (* constrained by local, open signature *)
	   then absLocSig(str,parent,epath)
	   else str) (* either constrained by global, closed signature, 
		      or instance of unconstrained functor body --
		      in either case no further abstraction needed *)
	  before (if member(stamp,bodyStrStamps)
	  	  then initBodyMaps(env,epath)
	  	  else ())
      | abstractStr _ = impossible "abstractStr - bad arg"

     in updateMap paramStrStampMap (paramstamp,[(paramsign,[1])]);
	initParamMaps(paramenv,[1]);	(* initialize paramTycStampMap *)
	abstractStr(str,NULLstr,param,[])
    end
  | abstractBody _ = impossible "abstractBody - bad args"

fun applyFunctor(parseEnv,FUNCTOR{paramName,param,body,paramVis,
    			 stamps={strStamps=bodyStrStamps,
    			 	 tycStamps=bodyTycStamps}},
                 arg,path,globalStamps,err)
     : Structure * thinning =
    let val STRstr{kind=SIGkind{stamps={strStamps=paramStrStamps,
	    				tycStamps=paramTycStamps},...},...}
	      = param
	val {strStamps=globalStrStamps,tycStamps=globalTycStamps} = globalStamps
	val paramTycMap = newMap(paramTycStamps,NULLtyc) : tycon stampmap
	val paramStrMap = newMap(paramStrStamps,NULLstr) : Structure stampmap
	val bodyTycMap = newMap(bodyTycStamps,NULLtyc) : tycon stampmap
	val bodyStrMap = newMap(bodyStrStamps,NULLstr) : Structure stampmap

	val ptycRealize = updateMap paramTycMap
	val pstrRealize = updateMap paramStrMap
	val btycRealize = updateMap bodyTycMap
	val bstrRealize = updateMap bodyStrMap

	fun maptyc(stamp,tycon) =
	    if member(stamp,paramTycStamps) then ptycRealize(stamp,tycon) else ()
	fun mapstr1(stamp,str) =
	    if member(stamp,paramStrStamps) then pstrRealize(stamp,str) else ()
	fun mapstr(sgn as STRstr{stamp,env=REL{s,t},kind=SIGkind{bindings,...},...},
		   str as STRstr{table,env as env',...}) =
	    let fun mapbinding(STRbind(STRvar{name=[id],binding=INDstr i,...})) =
		      let val sgn' = s sub i
			  val str' = let val STRvar{binding,...} = 
					       lookSTR table id
				      in case binding
					   of INDstr j =>
					       (case env'
					         of REL{s=s',...} => s' sub j
						  | DIR => impossible "mapstr 1")
					    | SHRstr(j::r) =>
					       (case env'
					         of REL{s=s',...} =>
						      getEpath(r,s' sub j)
						  | DIR => impossible "mapstr 2")
					    | st => st
				     end
		       in mapstr(sgn',str')
		      end
		  | mapbinding(TYCbind(INDtyc i)) =
		      (case t sub i
			of GENtyc{stamp,path=name::_,...} =>
			   maptyc(stamp,
				  case lookTYC table name
					 of INDtyc j =>
					     (case env'
					       of REL{t=t',...} => t' sub j
					        | DIR => impossible "mapstr 3")
					  | SHRtyc p => getEpathTyc(p,env)
					  | tyc => tyc)
		        | _ => ())
		  | mapbinding _ = ()
	     in if member(stamp,paramStrStamps)
		then (pstrRealize(stamp,str);
		      app mapbinding bindings)
		else ()
	    end
          | mapstr _ = impossible "applyFunctor.mapstr"

	fun insttyc(tyc) =
	    case tyc
	      of DEFtyc{path=path',tyfun=TYFUN{arity,body}} =>
		      DEFtyc{path=path'@path,
			     tyfun=TYFUN{arity=arity,body=insttype body}}
	       | RECORDtyc _ => tyc
	       | GENtyc{stamp,arity,eq,path=path',kind} =>
		   if member(stamp,paramTycStamps)
		   then applyMap(paramTycMap, stamp)
		   else if member(stamp,bodyTycStamps) (* generative *)
		   then (case applyMap(bodyTycMap, stamp)
			  of NULLtyc =>  (* not yet instantiated *)
			       let val kind = case !kind
					      of DATAtyc d=>ref(DATAtyc d)
					       | _ => kind
				   val newtyc =
				      GENtyc{stamp = newStamp(globalTycStamps),
					     eq = eq,
(* OLD				          eq = case !eq
					            of IND => ref DATA
						     | _ => eq,
*)
					     arity = arity,
					     path = path'@path,
					     kind = kind}
				in btycRealize(stamp,newtyc);
				   newtyc
			       end
			   | tyc' => tyc')
		   else tyc  (* free type constructor *)
	       | _ => (print "insttyc: "; PrintType.printTycon std_out parseEnv tyc; print "\n";
		       impossible "Functor.applyFunctor.insttyc")

	and insttype ty =
	    case ty
	      of CONty(tycon,args) => CONty(insttyc tycon, map insttype args)
	       | POLYty{sign,tyfun=TYFUN{arity,body},abs} =>
	           POLYty{sign=sign,abs=abs,
		  	 tyfun=TYFUN{arity=arity,body=insttype body}}
	       | _ => ty

	fun inststr(str as STRstr{stamp,sign,table,env,kind}) =
	    if member(stamp,paramStrStamps)  (* parameter stamp *)
	    then case applyMap(paramStrMap, stamp)
		  of str' as STRstr{stamp=stamp',sign=sign',...} =>
		       if sign = sign'
		       then str'
		       else STRstr{stamp=stamp',sign=sign,table=table,
			    	   env=instenv(env),
				   kind=case kind
					 of STRkind _ => kind
					  | _ => STRkind{path=[]}}
		   | NULLstr => raise Cascade "inststr NULLstr"
		   | _ => impossible "inststr ???"
	    else if member(stamp,bodyStrStamps)  (* generative stamp *)
	    then case applyMap(bodyStrMap, stamp)
		  of NULLstr  =>
		     let val STRkind{path=path'} = kind
		         val newstr =
			     STRstr{stamp = newStamp(globalStrStamps),
				    sign = sign,
				    table = table,
				    env = instenv(env),
				    kind = STRkind{path=path'@path}}
		      in bstrRealize(stamp,newstr);
			 newstr
		     end
		   | str' as STRstr{stamp=stamp',sign=sign',...} =>
		     if sign = sign'
		     then str'
		     else let val STRkind{path=path'} = kind
			      val newstr =
				  STRstr{stamp = stamp',
					 sign = sign,
					 table = table,
					 env = instenv(env),
					 kind = STRkind{path=path'@path}}
			   in newstr
			  end
		   | _ => impossible "Functor.inststr"
	    else str  (* absolute *)
	  | inststr (NULLstr) = NULLstr  (* undefined parameter case *)
	  | inststr (INDstr i) = impossible ("Functor.applyFunctor.instsr: INDstr "
					       ^makestring i)
	  | inststr (SHRstr _) = impossible ("Functor.applyFunctor.instsr: SHRstr")

	and instenv(REL{s,t}: strenv) : strenv =
	    let fun mapi (f,a,start,default) =
		  let val b = array(Array.length a, default)
		      fun loop i = (update(b,i,f(a sub i)); loop(i+1))
		   in loop start handle Subscript => b
		  end
	     in REL{s=mapi(inststr,s,1,NULLstr),t=mapi(insttyc,t,0,NULLtyc)}
	    end
	  | instenv(DIR) = impossible "instenv"

        fun findtyc env =
	    fn (tyc as GENtyc{stamp,eq,arity,path,kind}) =>
		  if member(stamp,paramTycStamps)
		      then applyMap(paramTycMap,stamp)
		   else if member(stamp,bodyTycStamps) (* generative *)
		   then (case applyMap(bodyTycMap, stamp)
			  of NULLtyc =>  (* not yet instantiated *)
			       let val newtyc =
				      GENtyc{stamp = stamp,
					     eq = eq,
(* OLD					     eq = case !eq
					            of IND => ref DATA
						     | DATA =>
							 impossible
							 "Functor.applyFunctor.findtyc"
						     | _ => eq,
*)
					     arity = arity,
					     path = path,
					     kind = kind}
				in btycRealize(stamp,newtyc);
				   newtyc
			       end
			   | tyc' => tyc')
		   else tyc  (* free type constructor *)
	      | tyc => tyconInContext env tyc

	(* we only have to do this in order to print constructed values
           at top level.  What a waste. *)
	fun redefineCon env (GENtyc{stamp,kind=r as ref(DATAtyc dcons),...}) =
	    (* needs to be streamlined:  here it is copying each datacon type
	       twice, once using typeInContext to get rid of RELtycs, and a
	       second time using insttype to get rid of bound tycons.
	       Another streamlining would be to do this only on freshly 
	       generated tycons (using the stamp to determine that they were
	       newly generated). *)
 	       r := DATAtyc(map (fn DATACON{name,const,typ,rep,sign} =>
	  	 	            DATACON{name=name,const=const,rep=rep,sign=sign,
				            typ=insttype(typeInContext(typ,env))})
		            dcons)
	  | redefineCon env (DEFtyc{tyfun=TYFUN{body,...},...}) =
	    let fun redefty(CONty(tyc,args)) =
		    (redefineCon env tyc; app redefty args)
		  | redefty _ = ()
	     in redefty body
	    end
	  | redefineCon _ _ = ()

        val topLevelApplication = isFixed globalStamps

        fun redoTycs(STRstr{env as REL{s,t},...}) =
	    (ArrayExt.app(redefineCon env,t,0);
	     ArrayExt.app(redoTycs,s,2))
          | redoTycs(STRstr{env as DIR,...}) =
	      raise Cascade "Functor.applyFunctor.redoTycs 1"
          | redoTycs(NULLstr) = impossible "Functor.applyFunctor.redoTycs 2"
          | redoTycs _ = impossible "Functor.applyFunctor.redoTycs 3"

	val (argstr,thin) = SigMatch.match0 {mapstr=mapstr,mapstr1=mapstr1,maptyc=maptyc}
			      (parseEnv,false,[paramName],globalStamps,param,arg,NULLstr,err)
	    (* argstr is the instantiated parameter structure *)
	val str as STRstr{env as REL{s,...},...} = inststr(body)
     in (* fill parameter slot of instantiated body with instantiated parameter *)
	if paramVis
	then update(s,1,argstr)
	else ();
	linkParents str;
	redoTycs str;
	(str, thin)
    end

end (* structure Functor *)
