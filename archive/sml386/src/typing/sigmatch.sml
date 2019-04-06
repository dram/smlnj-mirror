(* Copyright 1989 by AT&T Bell Laboratories *)
(* sigmatch.sml *)

structure SigMatch : SIGMATCH = struct

structure Basics = Basics

open List2 PrtUtil ErrorMsg Access Stampset Basics BareAbsn BasicTyp
     EnvAcc EnvAcc.Env TypesUtl PrtType ModUtil EqTypes

val symName = Symbol.name
val anonName = Symbol.symbol "Anon"
fun for a b = app b a

exception CompareTypes
exception REFtyc
val GENtyc{stamp=refstamp,...} =  refTycon
and GENtyc{stamp=arraystamp,...} = arrayTycon

fun compType(specty, specsign:polysign, actty, actsign:polysign, actarity): unit =
    let val env = array(actarity,UNDEFty)
	fun comp(ty1, VARty(ref(INSTANTIATED(ty2)))) =
	      comp(ty1,ty2)
	  | comp(ty1, FLEXRECORDty(ref(CLOSED ty2))) = comp(ty1,ty2)
	  | comp(ty1, VARty(ref(IBOUND i))) =
	     (case env sub i
		of UNDEFty =>
		    (let val {weakness=aw,eq=ae} = nth(actsign,i)
		     in if aw < infinity
			then let fun checkweak(VARty(ref(IBOUND n))) =
					let val {weakness=sw,...} = nth(specsign,n)
					 in if sw > aw then raise CompareTypes
					    else ()
					end
				    | checkweak(CONty(_,args)) = app checkweak args
				    | checkweak _ = impossible "compType/checkweak"
			      in checkweak ty1
			     end
			else ();
			if ae andalso not(checkEqTySig(ty1,specsign))
			then raise CompareTypes
			else ();
			update(env,i,ty1)
		    end handle Nth => ())
		 | ty => if equalType(ty1,ty)
			 then ()
			 else raise CompareTypes)
	  | comp(ty1 as CONty(tycon, args), ty2 as CONty(tycon', args')) =
	      if eqTycon(tycon,tycon')
	      then app2 comp (args,args')
	      else (comp(reduceType ty1, ty2)
		    handle ReduceType =>
		      comp(ty1, reduceType ty2)
		      handle ReduceType => raise CompareTypes)
	  | comp(_, ERRORty) = ()
	  | comp _ = raise CompareTypes
     in comp(specty,actty)
    end

fun compareTypes(spec: ty, actual: ty, name, err) : unit =
    let fun error() =
	    (err COMPLAIN "value type in structure doesn't match signature spec";
	     PrtType.resetPrtType();
	     print ("  name: " ^ symName name ^ "\n  spec:   ");
	     PrtType.printType(spec);
	     print "\n  actual: ";
	     PrtType.printType(actual); newline())
	val actual = prune actual
     in case spec
	  of POLYty{sign,tyfun=TYFUN{body,...},...} =>
	      (case actual
		 of POLYty{sign=sign',tyfun=TYFUN{arity,body=body'},...} =>
		      (compType(body,sign,body',sign',arity)
		       handle CompareTypes => error())
		  | ERRORty => ()
		  | _ => error())
	   | ERRORty => ()
	   | _ =>
	      (case actual
		 of POLYty{sign,tyfun=TYFUN{arity,body},...} =>
		      (compType(spec,[],body,sign,arity)
		       handle CompareTypes => error())
		  | ERRORty => ()
		  | _ => if equalType(spec,actual)
			 then ()
			 else error())
    end

(* making abstraction structures *)

fun abstract(sgn as STRstr{kind=SIGkind{stamps={strStamps=sigStrStamps,
		    			        tycStamps=sigTycStamps},
					...},
			   ...},
	     str, {strStamps, tycStamps}) =
    let val transStrStamp = join(strStamps,sigStrStamps)
	val transTycStamp = join(tycStamps,sigTycStamps)
	fun abstractTyc(sigtyc,strtyc) =
	    case sigtyc
	      of GENtyc{kind=ref(DATAtyc _),...} => strtyc
	       | GENtyc{kind=ref(FORMtyc),stamp,arity,eq,path} => 
			if tycFixed stamp then strtyc
			else GENtyc{kind=ref(ABStyc strtyc),
				    stamp=transTycStamp stamp,
				    arity=arity,eq=ref NO,
				    path=tycPath strtyc}
	       | _ => impossible "Sigmatch.abstractTyc"
	fun abstractStr(STRstr{stamp,sign,table,env,...},
			str as STRstr{env=env',...}) =
	      if strFixed stamp
	      then str
	      else let val newenv as REL{s,t} = abstractEnv(env,env')
		       val newstr = STRstr{stamp=transStrStamp(stamp),
				    	   env=newenv,
					   sign=sign,table=table,
					   kind=STRkind{path=[]}}
		                            (* ??? def of kind *)
		    in ArrayExt.app((setParent newstr), s, 2);
		       newstr
		   end
	  | abstractStr (INDstr i,_) =
	      impossible ("3437 in sigmatch: " ^makestring i)
	  | abstractStr _ = impossible "9833 in sigmatch (abstractStr)"
	and abstractEnv(REL{s=sSig,t=tSig}:strenv, REL{s=sStr,t=tStr}:strenv) =
	    let val sNew = array(Array.length sSig, NULLstr)
		val tNew = array(Array.length tSig, NULLtyc)
		fun foreachStr i =
		    (update(sNew,i,abstractStr(sSig sub i, sStr sub i));
		     foreachStr(i+1))
		fun foreachTyc i =
		    (update(tNew,i,abstractTyc(tSig sub i, tStr sub i));
		     foreachTyc(i+1))
	     in foreachStr 2  (* ignoring parent and parameter slots *)
		handle Subscript =>
		foreachTyc 0
		handle Subscript =>
		REL{s=sNew,t=tNew}
	    end
     in	abstractStr(sgn,str)
    end
  | abstract _ = impossible "8375 in sigmatch (abstract)"

(* signature matching *)

fun matchx (parent: Structure)
    	   (mapfns as {mapstr,mapstr1,maptyc})
    	   (abs, path, stamps,
	    sgn as STRstr{stamp,sign,...},
	    str as STRstr{stamp=stamp',sign=sign',table,env,...},
	    param: Structure,
	    err) 
	   : Structure * thinning =
      (if strFixed(stamp) andalso stamp <> stamp'
        then (err COMPLAIN "fixed signature doesn't agree with structure";
	      print "fixed signature stamp: "; print stamp;
	      print "\nstructure stamp: "; print stamp';
	      print "\npath: "; prSymPath(rev path); print "\n")
        else ();
      if sign = sign'
      then (mapstr(sgn,str);
	    (if abs then abstract(sgn,str,stamps) else str, NONE))
      else let val v = mkLvar()
	       val _ = (openStr(); openOld({path=[v],strenv=env},table))
	       val (str',transl) =
		     realizex parent mapfns (abs,path,stamps,stamp',sgn,param,err)
	    in closeStr();
	       (str',SOME(v,transl))
	   end)
  | matchx _ _ _ = impossible "843 in sigmatch"

and realizex (parent: Structure)
    	     (mapfns as {mapstr1,maptyc,mapstr})
	     (abs, path, stamps, strStamp,
	      sgn as STRstr{stamp = boundStamp, sign, table,
		     	    env = sigenv as REL{s=sSig,t=tSig},
			    kind = SIGkind{bindings,share,...},...},
	      param: Structure,
	      err)
	     : Structure * trans list =
    let val sNew = array(Array.length sSig, NULLstr)
	val tNew = array(Array.length tSig, NULLtyc)
	val newenv = REL{s=sNew,t=tNew}
        val newstr = STRstr{stamp=strStamp,sign=sign,table=table,env=newenv,
		  	    kind=STRkind{path=path}}
	fun checkDataconSign(name,DATACON{sign=s1,...}::_,
				  DATACON{sign=s2,...}::_)=
		if s1=s2 then ()
		else err COMPLAIN ("The datatype "^Symbol.name name^
				   " has different representations in the\n\
				   \signature and the structure.  \
			\Change the definition of the datatype in the\n\
			\signature to be more explicit about the \
			\types of the constructors.")	   

        fun checkSpec spec =
	    case spec
	      of STRbind(STRvar{name=[id],binding=INDstr i,...}) =>
		  (let val STRvar{access,binding=str',...} = lookSTRlocal id
		       val (str,thin) = matchx newstr mapfns
					    (false, id::path, stamps,
					     sSig sub i, str',NULLstr,err)
		    in update(sNew,i,str);
		       [case thin
		          of NONE => VALtrans access
			   | SOME(v,transl) => THINtrans(access,v,transl)]
		   end handle Unbound =>
		       (err COMPLAIN("unmatched structure spec: "^ symName id);
			update(sNew,i,
				let val STRvar{binding,...}=EnvAcc.bogusSTR
				 in binding
				end);
			nil))
	       | TYCbind(INDtyc i) =>
		 (let val sigTycon = tSig sub i
		      val name = tycName sigTycon
		      val strTycon = lookTYClocal name
		      fun complain s = err COMPLAIN(s^": "^symName name)
		      fun doFixed() = if equalTycon(sigTycon,strTycon)
				       then ()
				       else complain 
					   "bad match for fixed type spec"
		   in update(tNew,i,strTycon);
		      case sigTycon
		       of DEFtyc _ => doFixed()
		        | GENtyc{stamp=s,arity,kind,eq=ref eq,...} =>
		           if tycFixed s
				   then doFixed()
			   else if arity <> tyconArity strTycon
				   then complain "mismatching tycon arities"
		      	   else (maptyc(s,strTycon);
			     case (!kind, strTycon)
			      of (DATAtyc dcons,
				  GENtyc{arity=a',kind=ref(DATAtyc dc'),...})=>
				   if length dcons = length dc'
				   then checkDataconSign(name,dcons,dc')
				   else complain "mismatching datatype spec"
			       | (DATAtyc _, _) => 
				   complain "unmatched datatype spec"
			       | (FORMtyc, _) =>
				   if eq=YES andalso not(isEqTycon strTycon)
				   then complain "mismatched eqtype spec"
				   else ()
			       | _ => impossible "realizex.checkSpec")
		   end  handle Unbound =>
			  err COMPLAIN("unmatched type spec: "^ 
						symName(tycName(tSig sub i)));
		   nil)
	       | CONbind(DATACON{name,typ,rep=VARIABLE _,const,...}) =>
		 ((case lookCONlocal name
		    of DATACON{typ=typ',rep=VARIABLE(access),...} =>
		      (compareTypes(typeInContext(typ,newenv),typ',name,err);
		       [VALtrans access])
		     | _ => raise Unbound)
		   handle Unbound => 
		      (err COMPLAIN("unmatched exception spec: "^symName(name));
			nil))
	       | CONbind(DATACON{name,typ,...}) =>
		  (let val DATACON{typ=typ',...} = lookCONlocal name
		    in compareTypes(typeInContext(typ,newenv),typ',name,err)
		   end handle Unbound =>
		      err COMPLAIN ("unmatched data constructor spec: "
				       ^symName(name));
		    nil)
	       | VARbind(VALvar{name=[id],typ,...}) =>
		  ((case lookVARCONlocal id
		     of VARbind(VALvar{access,typ=typ',...}) =>
			  (* no propagation of INLINE access!! *)
			 (compareTypes(typeInContext(!typ,newenv),
				       !typ',id,err);
 			  [VALtrans access])
		      | CONbind(dcon as DATACON{typ,...}) =>
			  (compareTypes(typeInContext(typ,newenv),typ,id,err);
			   [CONtrans dcon])
		      | _ => impossible "sigmatch.476")
		     handle Unbound =>
			(err COMPLAIN("unmatched val spec: "^symName(id)); []))
	       | _ => nil (* nonchecked binding (FIXbind) *)
        fun checkList (a::rest) =
	      (checkSpec a handle Syntax => nil) @ checkList rest
          | checkList nil = nil

	val _ = update(sNew,0,parent)  (* define parent before checking specs *)
	val _ = update(sNew,1,param)   (* ditto for param *)
	val trans = checkList bindings
	val _ = Sharing.checkSharing(table,newenv,share,err)
	val str = if abs then abstract(sgn,newstr,stamps) else newstr
     in mapstr1(boundStamp,str);
	linkParents str;  (* should be redundant *)
	(str, trans)
    end
  | realizex _ _ _ = raise ErrorMsg.Cascade "783 in sigmatch"

val defaultMapfns = 
    let fun ignore _ = ()
     in {mapstr=ignore,mapstr1=ignore,maptyc=ignore}
    end

val match0 = matchx NULLstr
val match = matchx NULLstr defaultMapfns
val realize = realizex NULLstr defaultMapfns

end (* structure SigMatch *)
