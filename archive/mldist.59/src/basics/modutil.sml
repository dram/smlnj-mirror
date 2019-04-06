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

fun shiftStamps {str: Structure,
		 newStamps: stampsets,
		 transStrStamp: stamp -> stamp,
		 transTycStamp: stamp -> stamp,
		 paramstamp: stamp,
		 paramsign: stamp,
		 paramstr: Structure} =
    let fun newTyc(tyc as DEFtyc{path,tyfun=TYFUN{arity,body}}) =
		DEFtyc{path=path,tyfun=TYFUN{arity=arity,body=newType body}}
	  | newTyc(tyc as GENtyc{stamp,arity,eq=ref(eqprop),path,kind}) =
	    let val newstamp = transTycStamp(stamp)
	     in if newstamp = stamp  
		then tyc	(* not a bound stamp *)
		else GENtyc{stamp=newstamp,
			   arity=arity,
			   eq=ref(eqprop),
			   path=path,
			   kind=case kind
				  of ABStyc t => ABStyc(newTyc t)
				   | _ => kind}
		    (* Ignoring the dcons of DATAtycs on the assumption that
		       the type stamps in their typ components are not going
		       to be used in typechecking, and the stamps are not
		       relevant to using the dcon types for printing values
		       of the datatype.  Processing the dcons leads to
		       circularities. *)
	    end
	  | newTyc tyc = tyc
	and newType(ty) =
	    case ty
	      of VARty(ref(INSTANTIATED ty')) => newType ty'
	       | FLEXRECORDty(ref(CLOSED ty')) => newType ty'
	       | POLYty{tyfun=TYFUN{body,arity},sign,abs} =>
		   POLYty{tyfun=TYFUN{body=newType body,arity=arity},
			  sign=sign,abs=abs}
	       | VARty _ => ty
	       | CONty(tyc,args) => CONty(newTyc tyc, map newType args)
	       | ERRORty => ty
	       | _ => impossible "shiftStamps.newType"
        fun newEnv(REL{s,t}) =
	    let val s' = array(Array.length s, NULLstr)
		fun loop i = (update(s',i,newStr(s sub i)); loop(i+1))
	     in loop 1 handle Subscript => 
		  REL{s=s',t=ArrayExt.map(newTyc,t,0)}
	    end
	  | newEnv _ = raise Cascade "ModUtil.shiftStamps.newEnv - bad arg"
	and newStr(str as STRstr{stamp,sign,table,env,kind}) =
	    if Stampset.strFixed(stamp)
	    then str
	    else if stamp=paramstamp andalso sign=paramsign
	    then paramstr
	    else let val newenv as REL{s,...} = newEnv env
		     val new = STRstr{stamp=transStrStamp(stamp),
			       	      kind= case kind
					     of SIGkind{stamps,share,bindings} =>
						 SIGkind{stamps=newStamps,
						 	 share=share,
							 bindings=bindings}
					      | _ => kind,
				      env=newenv,
				      sign=sign,
				      table=table}
		     val setpar = setParent new
		     fun loop i = (setpar(s sub i); loop(i+1))
		  in loop 1 handle Subscript => new
		 end
	  | newStr(NULLstr) = NULLstr
          | newStr(INDstr i) = impossible("ModUtil.shiftStamps.newStr INDstr "^
					   makestring i)
	  | newStr(SHRstr _) = impossible "ModUtil.shiftStamps.newStr SHRstr"
     in newStr str
    end

fun shiftSigStamps(base : stampsets, sgn as STRstr{kind=SIGkind{stamps,...},...}) =
    let val {strStamps=sbase,tycStamps=tbase} = base
	and {strStamps=soffset,tycStamps=toffset} = stamps
     in shiftStamps{str=sgn,
		    newStamps=base,
		    transStrStamp=join(sbase,soffset),
		    transTycStamp=join(tbase,toffset),
		    paramstamp= ~1, paramsign= ~1, paramstr=NULLstr}
    end
  | shiftSigStamps _ = impossible "ModUtil.shiftSigStamps -- bad arg"

(* shiftFctStamps is only applied to functor representations that have been imported *)
fun shiftFctStamps(FUNCTOR{paramName,paramVis,param,body,
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
	val newparam = shiftStamps{str=param,
				   newStamps=newParamStamps,
				   transStrStamp=paramStrTrans,
				   transTycStamp=paramTycTrans,
				   paramstamp= ~1, paramsign = ~1, paramstr=NULLstr}
	val newbody = shiftStamps {str=body,
				   newStamps=newBodyStamps,
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
