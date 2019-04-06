(* Copyright 1989 by AT&T Bell Laboratories *)
signature SIGNS = 
sig type spectype
    type signtype
    type symbol type 'a tpathed
    type tyvar type ty type 'a tsusp type 'a uvars
    type fixity type tycon type datacon
    val makeSIGid:	symbol * ErrorMsg.complainer -> bool * Basics.Structure -> signtype
    val makeSIG:	spectype * ErrorMsg.complainer -> bool * Basics.Structure -> signtype
    val make_includespec: symbol * ErrorMsg.complainer -> spectype
    val make_openspec:  symbol list list * ErrorMsg.complainer -> spectype
    val make_strspec:	symbol * signtype -> spectype
    val make_dtyspec:	 (symbol * int * datacon list tpathed) list -> spectype
    val make_tyspec:	Basics.eqprop * tyvar list * symbol
					 * ErrorMsg.complainer -> spectype
    val make_valspec:	symbol * ty tsusp uvars -> spectype
    val make_exnspec:	symbol -> spectype
    val make_exnspecOF:	symbol * ty tsusp uvars -> spectype
    val make_fixityspec: fixity * symbol list -> spectype
    val make_type_sharespec: symbol list list -> spectype
    val make_str_sharespec: symbol list list -> spectype
end

structure Signs : SIGNS = struct

  open ErrorMsg Symbol PrintUtil EqTypes
  open Access Basics BasicTypes TypesUtil Absyn
  open Env EnvAccess ModUtil Misc TyvarSet

type tyclooker = symbol list * int * (string->unit) -> tycon

type signContext = 
    {stamps: Stampset.stampsets,
     tables: symtable list ref,
     nextSlot: unit->int,
     sNext: Structure->Structure,
     tNext: tycon->tycon,
     sCount: int ref,
     tCount: int ref,
     tempenv: strenv,
     depth: int, 
     tyclooker : tyclooker,
     typeSharing: spath list list ref,
     strSharing: spath list list ref}

type stampsets = Stampset.stampsets
type 'a tstamped = tyvarset * stampsets -> 'a
type 'a uvars = 'a * tyvarset
type 'a tpathed = tyclooker * symbol list -> 'a
type 'a tsusp = tyclooker -> 'a
type spectype = signContext -> binding list 
type signtype = int * stampsets -> Structure
		    
fun includeSig({stamps,nextSlot,sNext,tNext,sCount,tCount,
		typeSharing,strSharing,...}: signContext,
	       STRstr{kind=SIGkind{share={s=strPairs,t=tycPairs},bindings,
				   stamps={strStamps,tycStamps}},
 		      env=REL{s=senv,t=tenv},...}) =
    let val {strStamps=strStamps0, tycStamps=tycStamps0} = stamps
	val transStrStamp = Stampset.join(strStamps0,strStamps)
	val transTycStamp = Stampset.join(tycStamps0,tycStamps)
	val sOffset = !sCount - 2 (* offset for structure indices *)
	val tOffset = !tCount     (* offset for tycon indices *)

	fun addSharing((p1,p2),shar) = [p1,p2]::shar

	(* adjustPath(depth: int, path: int list): int list *)
	fun adjustPath(0,[i]) = [i+tOffset]
	  | adjustPath(0,i::r) = (i+sOffset) :: r
	  | adjustPath(0,[]) = impossible "sigBody.includeSig.adjustPath"
	  | adjustPath(d,0::(r as _::_)) = 0 :: adjustPath(d-1,r)
	  | adjustPath(d,p) = p

	fun adjustType(depth,ty) =
	    let fun adjust(CONty(RELtyc p, args)) =
		      CONty(RELtyc(adjustPath(depth,p)), map adjust args)
		  | adjust(CONty(tyc,args)) =
		      CONty(tyc, map adjust args)
		  | adjust(POLYty{sign,tyfun=TYFUN{arity,body},abs}) =
		      POLYty{sign=sign, abs=abs,
			     tyfun=TYFUN{arity=arity,body=adjust body}}
		  | adjust ty = ty
	     in adjust ty
	    end

	fun transDATACON depth (DATACON{name,typ,const,rep,sign}) =
	    DATACON{name=name,const=const,sign=sign,rep=rep,
		    typ=adjustType(depth,typ)}

	fun transTBinding depth binding =
	    case binding
	     of VARbind(VALvar{name,typ,access}) =>
		  VARbind(VALvar{name=name,access=access,
				 typ= ref(adjustType(depth,!typ))})
	      | CONbind(dcon) => CONbind(transDATACON depth dcon)
	      | _ => binding

	fun transLBinding table binding =
	    case binding
	     of VARbind(VALvar{name=[n],...}) =>
		  IntStrMap.map table (NameSpace.varKey n)
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  IntStrMap.map table (NameSpace.conKey name)
	      | _ => binding

	fun newTyc depth (tyc as GENtyc{stamp,arity,eq = ref eq',path,kind}) =
	    if Stampset.tycFixed(stamp)
	    then tyc
	    else GENtyc{stamp=transTycStamp(stamp),
		       	arity=arity,
			eq=ref(case eq'
				of IND =>
				    (case !kind
				      of DATAtyc _ => DATA
				       | FORMtyc => UNDEF
				       | _ => impossible "includeSig.newTyc")
				 | _ => eq'),
			path=path,
			kind= case !kind
			       of DATAtyc dcons =>
				    ref(DATAtyc(map (transDATACON depth) 
						        dcons))
				| _ => kind}
	  | newTyc _ tyc = tyc

	fun newEnv(depth,REL{s,t}) =
	     REL{s=mapSubstrs(newStr depth,s), t=ArrayExt.map(newTyc depth,t,0)}
	  | newEnv _ = impossible "Parse.includeSig.newEnv"

	and newStr depth (str as STRstr{stamp,sign,table,env,
					kind=SIGkind{stamps,share,bindings}}) =
	    if Stampset.strFixed(stamp)
	    then str
	    else let val newenv as REL{s,t} = newEnv(depth+1,env)
		     val newtable =
			 IntStrMap.transform (transTBinding depth) table
		     val new =
			 STRstr{stamp=transStrStamp(stamp),
				table=newtable,
				kind=SIGkind{stamps=stamps,share=share,
					     bindings=map
						      (transLBinding newtable)
						      bindings},
				      env=newenv, sign=sign}
		  in ArrayExt.app(ModUtil.resetParent new, s, 2);
		     new
		 end
	  | newStr _ (INDstr i) = impossible("sigbody.newStr INDstr "^
					   makestring i)
	  | newStr _ (SHRstr _) = impossible "sigbody.newStr SHRstr"
	  | newStr _ (NULLstr) = impossible "sigbody.newStr NULLstr"
	  | newStr _ _ = impossible "sigbody.newStr STRkind"

	fun adjustBinding binding =
	    case binding
	     of VARbind(VALvar{name=[n],typ,...}) =>
		  bindVAR(n,VALvar{name=[n],typ= ref(adjustType(0,!typ)),
				   access=SLOT(nextSlot())})
	      | CONbind(DATACON{name,typ,const,rep as VARIABLE(SLOT _),sign}) =>
		  bindCON(name,DATACON{name=name,
				       const=const,
				       sign=sign,
				       typ=adjustType(0,typ),
				       rep=VARIABLE(SLOT(nextSlot()))})
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  bindCON(name,DATACON{name=name,
				       const=const,
				       sign=sign,
				       typ=adjustType(0,typ),
				       rep=rep})
	      | TYCbind(INDtyc i) =>
		  let val tyc = tenv sub i
		      val name = tycName tyc
		   in bindTYC(name,tNext(newTyc 0 tyc))
		  end
	      | STRbind(STRvar{name as [n],binding=INDstr i,...}) =>
		  bindSTR(n,STRvar{name=name,
				   binding=sNext(newStr 1 (senv sub i)),
				   access=SLOT(nextSlot())})
	      | FIXbind(fixvar as FIXvar{name,...}) =>
		  bindFIX(name,fixvar)
	      | _ => impossible "sigBody.adjustBinding"

     in strSharing := fold addSharing strPairs (!strSharing);
	typeSharing := fold addSharing tycPairs (!typeSharing);
	map adjustBinding bindings
    end (* includeSig *)
  | includeSig _ = impossible "Parse.includeSig - bad arg"


fun getSIGbinding (ID,err) =
    let val SIGvar{binding,...} = lookSIG ID in binding end
    handle Unbound => (err COMPLAIN ("unbound signature: "^Symbol.name ID);
		       bogusSIGbody)

fun makeSIGid (ID,err) (toplev,_) (depth,stamps) =
    let val binding = getSIGbinding (ID,err)
     in if depth>1
	then ModUtil.shiftSigStamps(toplev,stamps,(fn s => s),binding)
	else binding
    end
 
val maxTypSpecs = 100  (*maximum number of type specs in a signature *)
val maxStrSpecs = 100  (*maximum number of structure specs in a signature *)

fun makeSIG(specs,err) (toplev,param) (depth,stamps) = 
 let val tComps = array(maxTypSpecs,NULLtyc)
     and tCount = ref 0
     fun tNext x = (update(tComps,!tCount,x);
     	            INDtyc(!tCount before inc tCount))
     val sComps = array(maxStrSpecs,NULLstr)
     and sCount = ref 2 (* slots 0,1 reserved for parent, fct param (if any) *)
     fun sNext x = (update(sComps,!sCount,x);
     	            INDstr(!sCount before inc sCount))
     val _ = update(sComps,1,param)
     val tempenv = REL{t=tComps,s=sComps}
     fun pairs (nil : spath list list) : (spath*spath) list = nil
       | pairs ((a::b::r) :: s) = (a,b) :: pairs((b::r) :: s)
       | pairs ( _ :: s ) = pairs s
     val strSharing : spath list list ref = ref nil
     val typeSharing : spath list list ref = ref nil

     val slot = ref 0
     fun nextSlot() = (!slot before inc slot)

     val tyArg = lookPathArTYCinSig depth
     val table = newTable()
     val tables = ref [table]
     val signContext : signContext =
		{stamps=stamps, tables=tables, nextSlot=nextSlot, tempenv=tempenv,
		 sNext=sNext, tNext=tNext, depth=depth,
		 sCount=sCount, tCount=tCount, tyclooker=tyArg,
		 typeSharing=typeSharing, strSharing=strSharing}

     val stamp = Stampset.newStamp(#strStamps stamps)
     val _ = openStr()   (* this is out of date, check parse.sml *)
     val _ = openNew({path=[~depth],strenv=tempenv},table)
     val bindings = specs signContext
     val _ = closeStr()
     val table =
	 let val bottom::rest = rev(!tables)
	  in revfold
	      (fn (table,acc) => (IntStrMap.app (IntStrMap.add acc) table; acc))
	      rest bottom
	 end
     val senv = ArrayExt.copy(sComps,!sCount)
     val env = REL{s=senv, t=ArrayExt.copy(tComps,!tCount)}
     val sShare = pairs(!strSharing)
     val tShare = pairs(!typeSharing)
     val shareSpec =
           if null sShare andalso null tShare
           then {s=[],t=[]}
           else Sharing.doSharing(table,env,stamps,{s=sShare,t=tShare},err)
     val result =
           STRstr{stamp=stamp,
     	     sign=Stampset.newStamp(Stampset.sigStamps),
     	     table=table,
     	     env=env,
     	     kind=SIGkind{share=shareSpec,
     			  bindings=bindings,
     			  stamps=stamps}}
  in ArrayExt.app((ModUtil.setParent result),senv,2);
     if toplev then eqAnalyze(result,err) else ();
     result
 end

fun make_includespec (ID,err) ($ : signContext) =
  let val binding = getSIGbinding (ID,err) in includeSig($,binding) end

fun make_openspec (strpaths: symbol list list, err: complainer)
                  ({tables,depth,tempenv,...}: signContext) =
    let val newtable = newTable()
	fun openStrInSig(p:spath) =
	    case lookPathSTRinSig(p,err COMPLAIN)
	     of (STRstr{table,env,...},p) => openOld({path=p,strenv=env},table)
	      | _ => impossible "openStrInSig -- bad arg"
     in app openStrInSig strpaths;
	openNew({path=[~depth],strenv=tempenv},newtable);
	tables := newtable :: !tables;
	[] (* no bindings returned *)
    end

fun make_strspec(name,sign) ({depth,stamps,nextSlot,sNext,...}:signContext) =
  let val sgn = sign(depth+1,stamps)
   in [bindSTR(name,STRvar{name=[name],access=SLOT(nextSlot()),
				    binding=sNext(sgn)})]
  end

fun make_dtyspec db ({stamps,tempenv,tNext,tyclooker,...}:signContext) =
    let fun predefine(id,arity,f) = 
		let val r = ref(DATAtyc nil)
      		 in (bindTYC(id,tNext(GENtyc{path=[id],arity=arity,
				stamp=Stampset.newStamp(#tycStamps stamps),
				eq=ref DATA,kind=r})),
		     r,f)
		end
	fun redefine(b,r,f) = (r := DATAtyc(f(tyclooker,[])); b)
	val pre = map predefine db
	val tycbinds = map redefine pre
        val tycons = map (fn (TYCbind t) => t) tycbinds
	fun collectdcons((_,ref(DATAtyc dcons),_)::rest,dcbinds) =
	     let fun binddcons(DATACON{name,...}::rest',dcbs) =
		      binddcons(rest',
		           (let val (b,_) = Env.look(NameSpace.conKey(name))
			     in b::dcbs
			    end
			    handle Unbound => dcbs))
		   | binddcons([],dcbs) = dcbs
	      in collectdcons(rest,binddcons(dcons,dcbinds))
	     end
	  | collectdcons([],dcbinds) = dcbinds
	val tycFinder = tyconInContext tempenv
     in tycbinds @ collectdcons(pre,[])
    end

fun make_tyspec (eq,tyvars,name,err)
                ({stamps={tycStamps,...},tNext,...}:signContext) =
    (checkbound(no_tyvars,tyvars,err);
     [bindTYC(name, tNext(GENtyc{stamp = Stampset.newStamp(tycStamps),
				 path = [name], arity = length tyvars,
				 eq = ref eq, kind = ref(FORMtyc)}))])

fun make_valspec(name,(ty,tv)) ({nextSlot,tyclooker,...}:signContext) =
   let val body = ty tyclooker
       val typ = case get_tyvars tv
		  of [] => body
		   | tvs => let val sign = TypesUtil.bindTyvars1 tvs
			     in POLYty{sign = sign, abs=0,
				       tyfun = TYFUN{arity = length tvs, 
						     body = body}}
	  		    end
   in TypesUtil.compressTy typ;
      [bindVAR(name,VALvar{name=[name],typ= ref typ,access=SLOT(nextSlot())})]
  end

fun make_exnspec name ({nextSlot,...}:signContext) =
  [bindCON(name,DATACON{name=name,const=true,typ=exnTy,sign=[],
		rep=VARIABLE(SLOT(nextSlot()))})]

fun make_exnspecOF(name,(ty,tv)) ({nextSlot,tyclooker,...}:signContext) =
  let val body = ty tyclooker
      val tvs = get_tyvars tv
      val typ = case length tvs
		 of 0 => body --> exnTy
		  | n => (TypesUtil.bindTyvars tvs;
			  POLYty{sign = mkPolySign n, abs=0,
			         tyfun = TYFUN{arity = n,
				               body = body --> exnTy}})
   in TypesUtil.compressTy typ;
      [bindCON(name, DATACON{name=name, const=false, typ= typ,sign=[],
				      rep=VARIABLE(SLOT(nextSlot()))})]
  end

fun make_fixityspec(fixity,ops) _ = 
  (app(fn i => bindFIX(i,FIXvar{name=i,binding=fixity})) ops;  nil)

fun make_type_sharespec patheqn ({typeSharing,...}:signContext) =
		(typeSharing := patheqn :: !typeSharing; nil)

fun make_str_sharespec patheqn ({strSharing,...}:signContext) =
		(strSharing := patheqn :: !strSharing; nil)
end
