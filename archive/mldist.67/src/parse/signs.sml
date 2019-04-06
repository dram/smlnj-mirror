(* Copyright 1989 by AT&T Bell Laboratories *)
signature SIGNS = 
sig type spectype
    type signtype
    type symbol 
    type spath
    type 'a epathed
    type tyvar 
    type ty 
    type 'a enved
    type 'a uvars
    type 'a withenv
    type fixity 
    type tycon 
    type datacon
    val makeSIGid: symbol * ErrorMsg.complainer
	           -> bool * bool * Basics.Structure -> signtype
    val makeSIG: spectype * ErrorMsg.complainer
	         -> bool * bool * Basics.Structure -> signtype
    val make_includespec: symbol * ErrorMsg.complainer -> spectype
    val make_openspec: spath list * ErrorMsg.complainer -> spectype
    val make_strspec: symbol * signtype -> spectype
    val make_dtyspec: (symbol * int * datacon list withenv epathed) list
	               -> spectype
    val make_tyspec: Basics.eqprop * tyvar list * symbol * ErrorMsg.complainer
	             -> spectype
    val make_valspec:	symbol * ty enved uvars -> spectype
    val make_exnspec:	symbol -> spectype
    val make_exnspecOF:	symbol * ty enved uvars -> spectype
    val make_fixityspec: fixity * symbol list -> spectype
    val make_type_sharespec: spath list -> spectype
    val make_str_sharespec: spath list -> spectype
end

structure Signs : SIGNS = struct

  open ErrorMsg Symbol PrintUtil EqTypes
  open Access Basics BasicTypes TypesUtil Absyn Stampset
  open EnvAccess ModUtil Misc TyvarSet

type signContext = 
    {stamps: Stampset.stampsets,
     nextSlot: unit->int,
     typeSharing: spath list list ref,
     strSharing: spath list list ref,
     parseEnv: Basics.env ref}

type stampsets = Stampset.stampsets
type 'a uvars = 'a * tyvarset
type 'a epathed = Basics.env * spath -> 'a
type 'a enved = Basics.env -> 'a
type 'a withenv = 'a * Basics.env
type spectype = signContext -> binding list 
type signtype = Basics.env * stampsets -> Structure
		    
fun smash f l = fold (fn (a,c) => f a @ c) l []

fun bindingName(VARbind(VALvar{name=[name],...})) = name
  | bindingName(CONbind(DATACON{name,...})) = name
  | bindingName(TYCbind tyc) = tycName tyc
  | bindingName(SIGbind (SIGvar{name,...})) = name
  | bindingName(STRbind (STRvar{name=[name],...})) = name
  | bindingName(FCTbind (FCTvar{name,...})) = name
  | bindingName(FIXbind (FIXvar{name,...})) = name
  | bindingName _ = impossible "Signs.bindingName"

val emptyStrStamps = Stampset.newStampset()
val emptyStrStampMap = newMap(emptyStrStamps,[]:(int * int list) list)
val emptyTycStamps = Stampset.newStampset()
val emptyTycStampMap = newMap(emptyTycStamps,[]: int list)

fun abstractSig
  (env,
   str as STRstr{env=DIR,stamp,sign,
 	         kind=SIGkind{share,bindings,
	                      stamps=bodyStamps as {strStamps=bodyStrStamps,
						    tycStamps=bodyTycStamps}},
		 ...},
					     
   param) =
  let

    fun backout([],m) = m
      | backout(_::r,m) = backout(r,0::m)
    
    (* relative(curloc,rootpath) calculates the relative 
       path from a given location,
       curloc, to the location indicated by a path, rootpath, which will be
       relative to a given root structure, i.e. the functor body structure *)
    
    fun relative(l,y as [_]) = backout(l,y)
      | relative(i::l,y as j::m) = 
		if i=j then relative(l,m) else backout(l,0::y)
      | relative([],y) = y
      | relative _ = impossible "Signs.abstractSig.relative"

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
			  of STRstr{env=strenv,stamp,sign,kind,...} =>
			       if member(stamp,strStamps)
			       then (updateMap strStampMap	
				      (stamp,(sign,rev(i::path))
					     ::applyMap(strStampMap,stamp));
				     scanenv(strenv, i::path))
			       else ()
			   | NULLstr => ()  (* for empty parameter slots *)
			   | _ => impossible "Signs.abstractSig.initMap.scanenv.sscan";
			 sscan(i+1))
		 in tscan 0
		    handle Subscript =>
		      sscan 1
		      handle Subscript => ()
		end
	      | scanenv _ = impossible "Signs.abstractSig.initMap.scanenv"
	 in scanenv
	end

    val (paramStrStamps,paramTycStamps,paramStrStampMap,paramTycStampMap) =
      case param of
	STRstr{env=paramenv,
	       stamp=paramstamp,
	       sign=paramsign,
	       kind=SIGkind{stamps={strStamps=paramStrStamps,
			            tycStamps=paramTycStamps},...},...} =>
	  let val paramStrStampMap = 
			newMap(paramStrStamps,[]: (int * int list) list)
	      val paramTycStampMap = newMap(paramTycStamps,[]: int list)
          in updateMap paramStrStampMap (paramstamp,[(paramsign,[1])]);
	     initMaps (paramStrStamps,paramTycStamps,
		       paramStrStampMap,paramTycStampMap) (paramenv,[1]);
	     (paramStrStamps,paramTycStamps,paramStrStampMap,paramTycStampMap)
	  end
      | NULLstr => 
	     (emptyStrStamps,emptyTycStamps,emptyStrStampMap,emptyTycStampMap)
      | _ => impossible  "Signs.abstractSig - bad param arg"

    val bodyStrStampMap  = newMap(bodyStrStamps,[]: (int * int list) list)
    val bodyTycStampMap  = newMap(bodyTycStamps,[]: int list)

    val initBodyMaps  = initMaps(bodyStrStamps,bodyTycStamps,
				 bodyStrStampMap,bodyTycStampMap)

(*    fun dumpStampset ({base,limit}:stampset) = 
	  (print "["; print base; print ","; print (!limit); print "]")

    val _ = (print "bodyStrStamps = "; dumpStampset (bodyStrStamps); print "\n")
    val _ = (print "bodyTycStamps = "; dumpStampset (bodyTycStamps); print "\n")
    val _ = (print "paramStrStamps = "; dumpStampset (paramStrStamps); print "\n")
    val _ = (print "paramTycStamps = "; dumpStampset (paramTycStamps); print "\n")
*)
    val bodyDEFtycs : (tycon * int list) list ref = ref nil
    exception Lookdeftyc
    fun lookdeftyc(t,(t',p)::r) = if t=t' then p else lookdeftyc(t,r)
      | lookdeftyc(_,nil) = raise Lookdeftyc

    fun abstractType(path: int list, ty: ty) : ty =
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
					then (prSymPath(rev spath); print "\n";
					      raise Cascade "Signs.abstractSig.abstractType 1")
				    else RELtyc(relative(path,p))
				 end
			      else tyc   (* free tycon in body *)
			  | _ => raise Cascade "Signs.abstractSig.abstractType 2"),
		       map absty args)
		   | ERRORty => ty
		   | _ => raise Cascade "Signs.abstractSig.abstractType 3"
	 in absty ty 
	end

    fun abstractStr(STRstr{stamp,sign,env=DIR,
			   kind=SIGkind{share,stamps,bindings,...},...},
		    param,epath) =
	  let val env = ref (Env.empty : Basics.env)
              fun add(name,binding) =
		  (env := Env.bind(name,binding,!env);
		   binding)
	      val tycCount = ref 0
	      val strCount = ref 2
	      val tenvtemp = ref([]: tycon list)
	      val senvtemp = ref([]: Structure list)
	       (* better not update typ in place in abstractBind 
			because of possible sharing (?) *)
	      fun abstractDcon(DATACON{name,const,typ,rep,sign}) =
		  DATACON{name=name,const=const,rep=rep,sign=sign,
			  typ=abstractType(epath,typ)}
	      fun abstractBind(VARbind(VALvar{name as [n],access,typ})) =
		    add(n,
			VARbind(VALvar{name=name,access=access,
			  	       typ=ref(abstractType(epath,!typ))}))
		| abstractBind(CONbind(dcon as DATACON{name,...})) =
		    add(name,CONbind(abstractDcon dcon))
		| abstractBind(b as FIXbind(FIXvar{name,...})) = 
		    add(name,b)
		| abstractBind b = b
     (* abstractTycon should not be needed.  This was so that dcons could be
        instantiated when the functor is applied.  
	*** A version is needed after all, for eqtype processing;
	      see below.
	      fun abstractTycon(GENtyc{kind=ref(DATAtyc dcons),stamp,arity,eq,path}) =
		  GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
			kind=ref(DATAtyc(map abstractDcon dcons))}
	        | abstractTycon(tycon) = tycon
     *)
	      fun abstractTycon(GENtyc{kind=kind as ref(DATAtyc dcons),...}) =
		  kind := DATAtyc(map abstractDcon dcons)
		| abstractTycon(tycon) = ()
	      val build =
	       (fn (tb as TYCbind(tyc as DEFtyc{path=name::_,...})) => 
		   (* N.B. This case shouldn't occur at present *)
		   (bodyDEFtycs := (tyc,rev(!tycCount::epath)) :: !bodyDEFtycs;
		    add(name,TYCbind(INDtyc(!tycCount)))
		    before (tenvtemp := tyc :: !tenvtemp;
		    	    inc tycCount))
	         | (tb as TYCbind(tyc as GENtyc{stamp,path=name::_,...})) => 
		   if member(stamp,bodyTycStamps)
		   then case applyMap(bodyTycStampMap,stamp)
			 of [] => 
			    (updateMap bodyTycStampMap
				(stamp,rev(!tycCount::epath));
			     add(name,TYCbind(INDtyc(!tycCount)))
			     before 
			     (tenvtemp := (*abstractTycon*) tyc :: !tenvtemp;
			      inc tycCount))
			  | path => 
			     add(name,TYCbind(SHRtyc(relative(epath,path))))
		   else if member(stamp,paramTycStamps)
		      (* parameter bound tycon -- e.g. from opened param *)
		   then let val path = applyMap(paramTycStampMap,stamp)
			 in add(name,TYCbind(SHRtyc(relative(epath,path))))
			end
		   else (* fixed tycon - e.g. from opened external structure *)
			add(name,tb)
	        | (sb as STRbind(STRvar{name as [n],access,
			  	        binding as STRstr{stamp,sign,...}})) =>
		   let fun addStr(stampMap,paths) =
			(updateMap stampMap
			   (stamp,(sign,rev(!strCount :: epath))::paths);
			 add(n,
			     STRbind(STRvar{name=name,access=access,
			 		    binding=INDstr(!strCount)}))
			 before 
			 (senvtemp := binding :: !senvtemp;
			  inc strCount))
		       fun look((sign',path)::rest) =
			     if sign = sign'
			     then path
			     else look rest
			 | look [] = []
		    in if member(stamp,bodyStrStamps) then
			  let val paths = applyMap(bodyStrStampMap, stamp)
			      val path = look paths
			   in if null path  (* irrelevant?? *)
			      then addStr(bodyStrStampMap,paths)
			      else add(n,STRbind(
					STRvar{name=name,access=access,
				    	       binding=SHRstr(relative
							      (epath, path))}))
			  end
		       else if member(stamp,paramStrStamps) (* parameter structure *)
		       then let val paths = applyMap(paramStrStampMap, stamp)
				val path = look paths
			     in if null path (* irrelevant?? *)
				then addStr(paramStrStampMap,paths)
				else add(n,STRbind(
					STRvar{name=name,access=access,
				    	       binding=SHRstr(relative(epath,path))}))
			    end
		       else add(n,sb)  (* fixed structure *)
		   end
	         | b => b)
	      val bindings' = map build bindings
	      val substrs = rev(!senvtemp)
	      val senv  = array(length substrs + 2, NULLstr)
	      val tenv  = arrayoflist(rev(!tenvtemp))
	      val newenv = REL{s=senv,t=tenv}
	  in update(senv,1,param);
	     revfold 
	       (fn (str,i)=>(update(senv,i,abstractStr(str,NULLstr,i::epath));
			     i+1))
	       substrs 2;
  	     let val newbindings = map abstractBind bindings'
		 val newstr =
		    	 STRstr{stamp=stamp,sign=sign,
			        kind=SIGkind{share=share,bindings=newbindings,
				  	     stamps=stamps},
			        table= !env,env=newenv}
	     in ArrayExt.app(ModUtil.resetParent newstr,senv,2);
	        ArrayExt.app(abstractTycon,tenv,0);
		newstr
	     end
	  end
      | abstractStr(str as STRstr{stamp,sign,env=strenv as REL _,...},_,epath) =
	 (if member(stamp,bodyStrStamps) then initBodyMaps(strenv,epath) else ();
	  str)
      | abstractStr _ = impossible "Signs.abstractSig.abstractStr - bad arg"
  in abstractStr(str,param,[])
  end
  | abstractSig _ = impossible "Signs.abstractSig - bad args"

fun includeSig(
	   {stamps,nextSlot,typeSharing,strSharing,parseEnv,...}: signContext,
	    STRstr{kind=SIGkind{share={s=strPairs,t=tycPairs},bindings,
			        stamps={strStamps,tycStamps}},
 		   env=strenv as REL{s=senv,t=tenv},...}) =
    let val {strStamps=strStamps0, tycStamps=tycStamps0} = stamps
	val transStrStamp = Stampset.join(strStamps0,strStamps)
	val transTycStamp = Stampset.join(tycStamps0,tycStamps)

	fun addSharing((p1,p2),shar) = [p1,p2]::shar

        fun adjustType strenv =
	    let fun adjust(CONty(tyc as RELtyc p, args)) =
		      CONty(tyconInContext strenv tyc, map adjust args)
		  | adjust(CONty(tyc,args)) =
		      CONty(tyc, map adjust args)
		  | adjust(POLYty{sign,tyfun=TYFUN{arity,body},abs}) =
		      POLYty{sign=sign, abs=abs,
			     tyfun=TYFUN{arity=arity,body=adjust body}}
		  | adjust ty = ty
	     in adjust 
	    end

	fun transDATACON strenv (DATACON{name,typ,const,rep,sign}) =
	    DATACON{name=name,const=const,sign=sign,rep=rep,
		    typ=adjustType strenv typ}

	fun transTBinding strenv binding =
	    case binding
	     of VARbind(VALvar{name,typ,access}) =>
		  VARbind(VALvar{name=name,access=access,
				 typ= ref(adjustType strenv (!typ))})
	      | CONbind(dcon) => CONbind(transDATACON strenv dcon)
	      | _ => binding

	fun transLBinding f binding =
	    case binding
	     of VARbind(VALvar{name=[n],...}) => f n
	      | CONbind(DATACON{name,typ,const,rep,sign}) => f name
	      | _ => binding

	fun newTyc (tyc as GENtyc{stamp,arity,eq = ref eq',path,kind}) =
	    if Stampset.tycFixed(stamp)
	    then tyc
	    else GENtyc{stamp=transTycStamp(stamp),
		       	arity=arity,
			eq=ref(case eq'
				of IND =>
				    (case !kind
				      of DATAtyc _ => DATA
				       | FORMtyc => UNDEF
				       | _ => impossible "Signs.includeSig.newTyc")
				 | _ => eq'),
			path=path,
			kind=kind}
	  | newTyc tyc = tyc

        fun adjustTycon strenv (GENtyc{kind=kind as ref(DATAtyc dcons),...}) =
		  kind := DATAtyc(map (transDATACON strenv) dcons)
	  | adjustTycon _  tycon = ()

	fun newEnv(REL{s,t}) =
            let val s' = mapSubstrs(newStr,s)
		val t' = ArrayExt.map(newTyc,t,0)
	        val strenv' = REL{s=s',t=t'}
	    in ArrayExt.app(adjustTycon strenv',t',0);
	       strenv'
	    end
	  | newEnv _ = impossible "Signs.includeSig.newEnv"

	and newStr (str as STRstr{stamp,sign,table,env,
				  kind=SIGkind{stamps,share,bindings}}) =
	    if Stampset.strFixed(stamp)
	    then str
	    else let val newenv as REL{s,t} = newEnv env
		     val newtable = Env.consolidate(
					 Env.open'(table,transTBinding newenv,
						   Env.empty))
		     val new =
			 STRstr{stamp=transStrStamp(stamp),
				table=newtable,
				kind=SIGkind{stamps=stamps,share=share,
				        bindings=map
				            (transLBinding (Env.look newtable))
				      	      bindings},
				        env=newenv, sign=sign}
		  in ArrayExt.app(ModUtil.resetParent new, s, 2);
		     new
		 end
	  | newStr (INDstr i) = impossible("Signs.newStr INDstr "^
					   makestring i)
	  | newStr (SHRstr _) = impossible "Signs.newStr SHRstr"
	  | newStr (NULLstr) = impossible "Signs.newStr NULLstr"
	  | newStr _ = impossible "Signs.newStr STRkind"

        val strenv' as REL{s=senv',t=tenv'} = newEnv strenv

	val adjustBinding =
             fn VARbind(VALvar{name=[n],typ,...}) =>
		  let val binding = 
		    VARbind(VALvar{name=[n],
		            typ= ref(adjustType strenv' (!typ)),
			    access=SLOT(nextSlot())})
		  in parseEnv := Env.bind(n,binding,!parseEnv);
		     binding
		  end
	      | CONbind(DATACON{name,typ,const,rep as VARIABLE(SLOT _),sign}) =>
		  let val binding = 
		    CONbind(DATACON{name=name,
	   		 	    const=const,
				    sign=sign,
				    typ=adjustType strenv' typ,
				    rep=VARIABLE(SLOT(nextSlot()))})
		  in parseEnv := Env.bind(name,binding,!parseEnv);
		     binding
		  end
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  let val binding = 
		    CONbind(DATACON{name=name,
				    const=const,
				    sign=sign,
				    typ=adjustType strenv' typ,
				    rep=rep})
		  in parseEnv := Env.bind(name,binding,!parseEnv);
		     binding
		  end
	      | TYCbind(INDtyc i) =>
		  let val tyc = tenv' sub i
		      val name = tycName tyc
		      val binding = TYCbind tyc
		  in parseEnv := Env.bind(name,binding,!parseEnv);
		     binding
		  end
	      | STRbind(STRvar{name as [n],binding=INDstr i,...}) =>
		  let val binding = 
	 	    STRbind(STRvar{name=name,
			 	   binding=senv' sub i,
				   access=SLOT(nextSlot())})
		  in parseEnv := Env.bind(n,binding,!parseEnv);
		     binding
		  end
	      | binding as (FIXbind(fixvar as FIXvar{name,...})) =>
		  (parseEnv := Env.bind(name,FIXbind fixvar,!parseEnv);
		   binding)
              | _ => impossible "Signs.includeSig.adjustBinding"
     in strSharing := fold addSharing strPairs (!strSharing);
	typeSharing := fold addSharing tycPairs (!typeSharing);
	map adjustBinding bindings
    end (* includeSig *)
  | includeSig _ = impossible "Signs.includeSig - bad arg"

fun getSIGbinding env (ID,err) =
    let val SIGvar{binding,...} = lookSIG env ID in binding end
    handle Unbound => (err COMPLAIN ("unbound signature: "^Symbol.name ID);
		       bogusSIGbody)

fun makeSIGid (ID,err) (toplev,funcparam,_) (env,stamps) =
    let val binding = getSIGbinding env (ID,err)
(* 65's original code:
>      in if depth>1  
> 	then ModUtil.shiftSigStamps(toplev,stamps,(fn s => s),binding)
> 	else binding
 depth always >= 1
 condition > 1 equivalent to: in a strspec in a signature 
                                          (depth > enclosing depth), or
                              functor formal parameter signature 
				          (depth = 2 or 3)
 toplev equivalent to: NOT a strspec in a signature 
 funcparam equivalent to: is functor formal parameter signature
*)
    in if ((not toplev) orelse funcparam) then
	  ModUtil.shiftSigStamps(toplev,stamps,(fn s => s),binding) 
       else binding
    end
 
fun makeSIG(specs,err) (toplev,_,param) (env,stamps) = 
 let fun pairs (nil : spath list list) : (spath*spath) list = nil
       | pairs ((a::b::r) :: s) = (a,b) :: pairs((b::r) :: s)
       | pairs ( _ :: s ) = pairs s
     val strSharing : spath list list ref = ref nil
     val typeSharing : spath list list ref = ref nil

     val slot = ref 0
     fun nextSlot() = (!slot before inc slot)
     val parseEnv = ref(env)

     val signContext : signContext =
	   {stamps=stamps, nextSlot=nextSlot,
	    typeSharing=typeSharing, strSharing=strSharing,
	    parseEnv=parseEnv}
     val stamp = Stampset.newStamp(#strStamps stamps)
     val bindings = specs signContext
     val newEnv = fold (fn (binding,env) =>
			 Env.bind(bindingName(binding),binding,env))
		       bindings
		       Env.empty
     val sShare = pairs(!strSharing)
     val tShare = pairs(!typeSharing)
     val result0 =
           STRstr{stamp=stamp,
     	     sign=Stampset.newStamp(Stampset.sigStamps),
     	     table=newEnv,
     	     env=DIR,
     	     kind=SIGkind{share={s=sShare,t=tShare},
     			  bindings=bindings,
     			  stamps=stamps}}
     val result1 = if toplev
		   then abstractSig(!parseEnv,result0,param)
		   else result0
     val result = if null sShare andalso null tShare
		  then result1
		  else Sharing.doSharing (!parseEnv (*???*),result1,err)
  in if toplev then eqAnalyze(result,err) else ();
     result
 end

fun make_includespec (ID,err) (($ as {parseEnv,...}): signContext) =
  let val binding = getSIGbinding (!parseEnv) (ID,err) 
  in includeSig($,binding) 
  end
           
fun make_openspec (strpaths: symbol list list, err: complainer)
 ({parseEnv,...}: signContext) =
    let fun openStrInSig(p:spath) =
          let val strvar = case p of
	                     [s] => lookSTR (!parseEnv) s
			   | _ => lookPathSTR (!parseEnv) (p,err COMPLAIN)
	  in parseEnv := openStructureVar (!parseEnv) strvar
          end
     in app openStrInSig strpaths;
	nil (* no bindings returned *)
    end

fun make_strspec(name,sign) ({stamps,nextSlot,parseEnv,...}:signContext) =
    let val sgn = sign(!parseEnv,stamps)
	val binding = STRbind (STRvar{name=[name],access=SLOT(nextSlot()),
				  binding=sgn})
     in parseEnv := Env.bind(name,binding,!parseEnv);
        [binding]
    end

fun make_dtyspec db ({stamps,parseEnv,...}:signContext) =
    let fun predefine(id,arity,f) = 
		let val r = ref(DATAtyc nil) 
		    val binding = TYCbind (GENtyc{path=[id],arity=arity,
				    stamp=Stampset.newStamp(#tycStamps stamps),
				    eq=ref DATA,kind=r})
		in parseEnv := Env.bind(id,binding,!parseEnv);
		   (binding,r,f)
		end
	fun redefine(b,r,f) = 
		let val (r',env') = f(!parseEnv,[])
		in r := DATAtyc(r');
		   parseEnv := Env.atop(env',!parseEnv);
		   b
		end
	val pre = map predefine db
	val tycbinds = map redefine pre
        val tycons = map (fn (TYCbind t) => t) tycbinds
	fun collectdcons((_,ref(DATAtyc dcons),_)::rest,dcbinds) =
	     let fun binddcons(DATACON{name,...}::rest',dcbs) =
		      binddcons(rest',
			(let val b = EnvAccess.lookVARCON (!parseEnv) name
			  in b::dcbs
			 end
			 handle Unbound => dcbs))
		   | binddcons([],dcbs) = dcbs
	      in collectdcons(rest,binddcons(dcons,dcbinds))
	     end
	  | collectdcons([],dcbinds) = dcbinds
	  | collectdcons _ = impossible "Signs.collectdcons"
     in tycbinds @ collectdcons(pre,[]) 
    end

fun make_tyspec(eq,tyvars,name,err) 
                   ({stamps={tycStamps,...},parseEnv,...}:signContext) =
     let val _ = checkbound(no_tyvars,tyvars,err)
         val binding = TYCbind(GENtyc{stamp = Stampset.newStamp(tycStamps),
				      path = [name], arity = length tyvars,
				      eq = ref eq, kind = ref(FORMtyc)})
     in parseEnv := Env.bind(name,binding,!parseEnv);
	[binding]
     end

fun make_valspec(name,(ty,tv)) ({nextSlot,parseEnv,...}:signContext) =
    let val body = ty (!parseEnv)
	val typ = case get_tyvars tv
		   of [] => body
		    | tvs => let val sign = TypesUtil.bindTyvars1 tvs
			      in POLYty{sign = sign, abs=0,
					tyfun = TYFUN{arity = length tvs, 
						      body = body}}
			     end
	val _ = TypesUtil.compressTy typ
	val binding = 
	    VARbind(VALvar{name=[name],typ= ref typ,access=SLOT(nextSlot())})
     in parseEnv := Env.bind(name,binding,!parseEnv);
	[binding]
    end

fun make_exnspec name ({nextSlot,parseEnv,...}:signContext) =
  let val binding = CONbind(DATACON{name=name,const=true,typ=exnTy,sign=[],
				    rep=VARIABLE(SLOT(nextSlot()))})
  in parseEnv := Env.bind(name,binding,!parseEnv);
     [binding]
  end

fun make_exnspecOF(name,(ty,tv)) ({nextSlot,parseEnv,...}:signContext) =
  let val body = ty (!parseEnv)
      val tvs = get_tyvars tv
      val typ = case length tvs
		 of 0 => body --> exnTy
		  | n => (TypesUtil.bindTyvars tvs;
			  POLYty{sign = mkPolySign n, abs=0,
			         tyfun = TYFUN{arity = n,
				               body = body --> exnTy}})
      val _ = TypesUtil.compressTy typ
      val binding = CONbind(DATACON{name=name, const=false, typ= typ,sign=[],
				      rep=VARIABLE(SLOT(nextSlot()))})
  in parseEnv := Env.bind(name,binding,!parseEnv);
     [binding]
  end

fun make_fixityspec(fixity,ops) ({parseEnv,...}:signContext) = 
  let fun f i = 
        let val binding = FIXbind(FIXvar{name=i,binding=fixity})
        in 
	    parseEnv := Env.bind(i,binding,!parseEnv);
	    binding
	end
  in map f ops (*???? try returning list *)
  end  

fun make_type_sharespec patheqn ({typeSharing,...}:signContext) =
		(typeSharing := patheqn :: !typeSharing; nil)

fun make_str_sharespec patheqn ({strSharing,...}:signContext) =
		(strSharing := patheqn :: !strSharing; nil)
end


