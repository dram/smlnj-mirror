(* modutil.sml *)

structure ModUtil : MODUTIL =
struct

open ErrorMsg Basics TypesUtil Stampset

fun mapSubstrs(f,senv) =
  (* Creates a new copy of a structure environment array by applying f to
     substructures.  Leaves parent and param slots of new env undefined *)  
    let val new = array(Array.length senv, NULLstr)
	fun loop i = (update(new,i,f(senv sub i)); loop(i+1))
     in loop 2 handle Subscript => new
    end

(* setParent only sets parents that are initially NULLstr *)
fun setParent (parent: Structure) =
    fn (STRstr{env=REL{s,...},...}) =>
	((case s sub 0
	   of NULLstr => update(s,0,parent)
	    | _ => ())
	 handle Subscript => ())
     | (STRstr{env=DIR,...}) => ()
     | NULLstr => ()
     | _ => impossible "ModUtil.setParent"

(* resetParent redefines parents unconditionally *)
fun resetParent (parent: Structure) =
    fn (STRstr{env=REL{s,...},...}) =>
         (update(s,0,parent) handle Subscript => impossible "ModUtil.resetParent 1")
     | (STRstr{env=DIR,...}) => ()
     | NULLstr => ()
     | _ => impossible "ModUtil.resetParent 2"

fun linkParents(str as STRstr{env=REL{s,...},...}) =
    ArrayExt.app((fn str' => (setParent str str'; linkParents str')), s, 2)
  | linkParents(STRstr{env=DIR,...}) = ()
  | linkParents(NULLstr) = ()
  | linkParents _ = ErrorMsg.impossible "ModUtil.linkParents"

fun shiftStamps {is_sign: bool,  (* indicates signature vs functor body *)
		 interior: bool, (* indicates interior signature vs top level *)
                 str: Structure,
		 newStamps: stampsets,
		 transSigStamp: stamp -> stamp,
		 transStrStamp: stamp -> stamp,
		 transTycStamp: stamp -> stamp,
		 paramstamp: stamp,   (* last 3 arguments relevant to functors *)
		 paramsign: stamp,
		 paramstr: Structure} =

  (* is_sign and not(interior) => shifting stamps of imported signature *)
    let (* translations is an association list that maps kind field (of ref type)
	   to translated GENtyc tycons.  maptyc and lookup add associations
           and lookup tycons with kind as key.
	   These are used when translating functor bodies to avoid circularities
	   when translating types of data constructors in the kind field of
	   datatypes. *)

	val translations : (tyckind ref * tycon) list ref = ref nil
	fun maptyc b = translations := b :: !translations
        fun lookup kind =
	    let fun look [] = NONE
		  | look ((kind',tyc)::rest) =
		    if kind = kind' then SOME tyc else look rest
	    in  look(!translations)
	    end

	(* lookup and maptyc are not used when translating signatures, so they
	   are rebound here to trivial functions *)
	val lookup = if is_sign then (fn _ => NONE) else lookup
        val maptyc = if is_sign then (fn _ => ()) else maptyc

	fun transTyc(tyc as DEFtyc{path,tyfun=TYFUN{arity,body}}) =
		DEFtyc{path=path,tyfun=TYFUN{arity=arity,body=transType body}}
	  | transTyc(tyc as GENtyc{stamp,arity,eq as ref(eqprop),path,kind}) =
	    let val newstamp = transTycStamp(stamp)
	    in  if newstamp = stamp  (* stamp not translated *)
		then tyc	     (* therefore not a bound stamp *)
		else case lookup kind
		      of NONE =>
			  let val neweq =
				  if interior (* always false for functor *)
				  then ref(case eqprop
					of IND =>
					    (case !kind
					      of DATAtyc _ => DATA
					       | FORMtyc => UNDEF
					       | _ => impossible "transTyc kind")
					 | _ => eqprop)
				  else eq
			      val newkind =
				  if is_sign then ref(!kind) else kind
			      val newtyc =
				  GENtyc{stamp=newstamp,
					 arity=arity,
					 eq=neweq,
					 path=path,
					 kind=newkind}
			  in  maptyc(kind,newtyc);
			      newtyc
			  end
		      | SOME tyc' => tyc'
	    end
	  | transTyc tyc = tyc
	and transType(ty) =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => transType ty'
	       | FLEXRECORDty(ref(CLOSED ty')) => transType ty'
	       | POLYty{tyfun=TYFUN{body,arity},sign,abs} =>
		   POLYty{tyfun=TYFUN{body=transType body,arity=arity},
			  sign=sign,abs=abs}
	       | VARty _ => ty
	       | CONty(tyc,args) => CONty(transTyc tyc, map transType args)
	       | ERRORty => ty
	       | _ => impossible "shiftStamps.transType"
        fun transEnv(REL{s,t}) =
	    let val s' = array(Array.length s, NULLstr)
		fun loop i = (update(s',i,transStr(s sub i)); loop(i+1))
	     in loop 1 handle Subscript => 
		  REL{s=s',t=ArrayExt.map(transTyc,t,0)}
	    end
	  | transEnv _ = raise Cascade "ModUtil.shiftStamps.transEnv - bad arg"
	and transStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if Stampset.strFixed(stamp)
	    then str
	    else if stamp=paramstamp andalso sign=paramsign
	    then paramstr
	    else let val newenv as REL{s,...} = transEnv env
		     val new = STRstr{stamp=transStrStamp(stamp),
			       	      kind= case kind
					     of SIGkind{stamps,share,bindings} =>
						 SIGkind{stamps=newStamps,
						 	 share=share,
							 bindings=bindings}
					      | _ => kind,
				      env=newenv,
				      sign=transSigStamp sign,
				      table=table}
		     val setpar = setParent new
		     fun loop i = (setpar(s sub i); loop(i+1))
		  in loop 1 handle Subscript => new
		 end
	  | transStr(NULLstr) = NULLstr
          | transStr(INDstr i) = impossible("ModUtil.shiftStamps.transStr INDstr "^
					   makestring i)
	  | transStr(SHRstr _) = impossible "ModUtil.shiftStamps.transStr SHRstr"

	(* functions transDcon and transKind are for translating types of
	   data constructors of data types *)
	fun transDcon(DATACON{name,const,typ,rep,sign}) =
	    DATACON{name=name,const=const,rep=rep,sign=sign,
		    typ=transType typ}
	fun transKind(GENtyc{kind as ref(DATAtyc dcons),...}) =
	    kind := DATAtyc(map transDcon dcons)
	  | transKind(GENtyc{kind as ref(ABStyc tyc),...}) =
	    kind := ABStyc(transTyc tyc)
	  | transKind _ = ()

	val newstr = transStr str

    in  if is_sign then ()
	else app (fn (_,tyc) => transKind tyc) (!translations);
	newstr
    end

fun shiftSigStamps(toplev : bool, base : stampsets, transSigStamp: stamp -> stamp,
		   sgn as STRstr{kind=SIGkind{stamps,...},...}) =
    let val {strStamps=sbase,tycStamps=tbase} = base
	and {strStamps=soffset,tycStamps=toffset} = stamps
     in shiftStamps{is_sign=true,
		    interior=(not toplev),
	            str=sgn,
		    newStamps=base,
		    transSigStamp=transSigStamp,
		    transStrStamp=join(sbase,soffset),
		    transTycStamp=join(tbase,toffset),
		    paramstamp= ~1, paramsign= ~1, paramstr=NULLstr}
    end
  | shiftSigStamps _ = impossible "ModUtil.shiftSigStamps -- bad arg"

(* shiftFctStamps is only applied to functor representations that have been
   imported *)
fun shiftFctStamps(transSigStamp,
		   FUNCTOR{paramName,paramVis,param,body,
    			   stamps as {strStamps=bodyStrStamps,
				      tycStamps=bodyTycStamps}}) =
    let val newParamStamps = newStampsets()
	val newBodyStamps = newStampsets()
	val STRstr{stamp=pstamp,sign=psign,
	           kind=SIGkind{stamps=pstamps as {strStamps=paramStrStamps,
					           tycStamps=paramTycStamps},
				...},
		   ...} =
	      param
	val bodyStrTrans = join(#strStamps newBodyStamps, bodyStrStamps)
	val bodyTycTrans = join(#tycStamps newBodyStamps, bodyTycStamps)
	val paramStrTrans = join(#strStamps newParamStamps, paramStrStamps)
	val paramTycTrans = join(#tycStamps newParamStamps, paramTycStamps)
	fun transStrStamp x =
	    let val x' = bodyStrTrans x
	     in if x' = x then paramStrTrans x else x'
	    end
	fun transTycStamp x =
	    let val x' = bodyTycTrans x
	     in if x' = x then paramTycTrans x else x'
	    end
	val newparam = shiftStamps{is_sign=true,
				   interior=false,
	                           str=param,
				   newStamps=newParamStamps,
				   transSigStamp=transSigStamp,
				   transStrStamp=paramStrTrans,
				   transTycStamp=paramTycTrans,
				   paramstamp= ~1, paramsign = ~1, paramstr=NULLstr}
	val newbody =  shiftStamps{is_sign=false,
				   interior=false,
	    			   str=body,
				   newStamps=newBodyStamps,
				   transSigStamp=transSigStamp,
				   transStrStamp=transStrStamp,
				   transTycStamp=transTycStamp,
	    			   paramstamp=pstamp, paramsign=psign,
				   paramstr=newparam}
     in FUNCTOR{paramName=paramName,
		paramVis=paramVis,
		param=newparam,
		body=newbody,
		stamps=newBodyStamps}
    end

end (* structure ModUtil *)
