(* Copyright 1989 by AT&T Bell Laboratories *)
signature SIGNS = 
sig type spectype
    type signtype
    type symbol type 'a tpathstamped
    type tyvar type ty type 'a tsusp type 'a uvars
    type fixity type tycon
    val makeSIGid:	symbol * ErrorMsg.complainer -> bool * Basics.Structure -> signtype
    val makeSIG:	spectype * ErrorMsg.complainer -> bool * Basics.Structure -> signtype
    val make_includespec: symbol * ErrorMsg.complainer -> spectype
    val make_strspec:	symbol * signtype -> spectype
    val make_dtyspec:	 (symbol * int * tycon tpathstamped) list -> spectype
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

type tyclooker = symbol list * int * (string->unit) -> tycon ref

type signContext = {stamps: Stampset.stampsets, nextSlot: unit->int,
		    sNext: Structure->Structure, tNext: tycon->tycon,
		    tNext': tycon*tycon->unit,
		    sCount: int ref, tCount: int ref,
		    tempenv: strenv, depth: int, 
		    tyclooker : tyclooker,
		    typeSharing: spath list list ref,
		    strSharing: spath list list ref}

type stampsets = Stampset.stampsets
type 'a tstamped = tyvarset * stampsets -> 'a
type 'a uvars = 'a * tyvarset
type 'a tpathstamped = tyclooker * symbol list * stampsets -> 'a
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
	    let fun adjust(CONty(ref(RELtyc(p)),args)) =
		      CONty(ref(RELtyc(adjustPath(depth,p))), map adjust args)
		  | adjust(CONty(reftyc,args)) =
		      CONty(reftyc, map adjust args)
		  | adjust(POLYty{sign,tyfun=TYFUN{arity,body},abs}) =
		      POLYty{sign=sign, abs=abs,
			     tyfun=TYFUN{arity=arity,body=adjust body}}
		  | adjust ty = ty
	     in adjust ty
	    end

	fun transTBinding depth binding =
	    case binding
	     of VARbind(VALvar{name,typ,access}) =>
		  VARbind(VALvar{name=name,access=access,
				 typ=ref(adjustType(depth,!typ))})
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  CONbind(DATACON{name=name, const=const, sign=sign, rep=rep,
				  typ=ref(adjustType(depth,!typ))})
	      | _ => binding

	fun transLBinding table binding =
	    case binding
	     of VARbind(VALvar{name=[n],typ,access}) =>
		  IntStrMap.map table (NameSpace.varKey n)
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  IntStrMap.map table (NameSpace.conKey name)
	      | _ => binding

	fun newTyc(tyc as TYCON{stamp,arity,eq = ref eq',path,kind}) =
	    if Stampset.tycFixed(stamp)
	    then tyc
	    else (case kind
		   of ABStyc _ => 
		       TYCON{stamp=transTycStamp(stamp),
		       	     arity=arity,
			     eq=ref(eq'),
			     path=path,
			     kind=kind}
		    | DATAtyc _ =>
		       TYCON{stamp=transTycStamp(stamp),
		       	     arity=arity,
			     eq=ref(eq'),
			     path=path,
			     kind=kind}
		    | _ => tyc)
	  | newTyc _ = impossible "Parse.includeSig.newTyc"

	fun newEnv(depth,REL{s,t}) =
	     REL{s=mapSubstrs(newStr depth,s), t=ArrayExt.map(newTyc,t,0)}
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
		  bindVAR(n,VALvar{name=[n],typ=ref(adjustType(0,!typ)),
				   access=SLOT(nextSlot())})
	      | CONbind(DATACON{name,typ,const,rep as VARIABLE(SLOT _),sign}) =>
		  bindCON(name,DATACON{name=name,
				       const=const,
				       sign=sign,
				       typ=ref(adjustType(0,!typ)),
				       rep=VARIABLE(SLOT(nextSlot()))})
	      | CONbind(DATACON{name,typ,const,rep,sign}) =>
		  bindCON(name,DATACON{name=name,
				       const=const,
				       sign=sign,
				       typ=ref(adjustType(0,!typ)),
				       rep=rep})
	      | TYCbind(ref(INDtyc i)) =>
		  let val tyc = tenv sub i
		      val name = tycName tyc
		   in bindTYC(name,ref(tNext(newTyc(tyc))))
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

fun makeSIGid (ID,err) _ (depth,stamps) =
    let val binding = getSIGbinding (ID,err)
     in if depth>0 then ModUtil.shiftSigStamps(stamps,binding) else binding
    end
 
val maxTypSpecs = 100  (*maximum number of type specs in a signature *)
val maxStrSpecs = 100  (*maximum number of structure specs in a signature *)

fun makeSIG(specs,err) (toplev,param) (depth,stamps) = 
 let val tComps = array(maxTypSpecs,NULLtyc)
     and tCount = ref 0
     fun tNext x = (update(tComps,!tCount,x);
     	            INDtyc(!tCount before inc tCount))
     fun tNext'(INDtyc i, x) = update(tComps,i,x)
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

     val signContext : signContext =
		{stamps=stamps,nextSlot=nextSlot, tempenv=tempenv,
		 sNext=sNext,tNext=tNext, tNext'=tNext', depth=depth,
		 sCount=sCount,tCount=tCount,tyclooker=tyArg,
		 typeSharing=typeSharing,strSharing=strSharing}

     val stamp = Stampset.newStamp(#strStamps stamps)
     val _ = openStr()   (* this is out of date, check parse.sml *)
     val table = newTable()
     val _ = openNew({path=[~depth],strenv=tempenv},table)
     val bindings = specs signContext
     val _ = closeStr()
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

fun make_strspec(name,sign) ({depth,stamps,nextSlot,sNext,...}:signContext) =
  let val sgn = sign(depth+1,stamps)
   in [bindSTR(name,STRvar{name=[name],access=SLOT(nextSlot()),
				    binding=sNext(sgn)})]
  end

fun make_dtyspec db ({stamps,tempenv,tNext,tNext',tyclooker,...}:signContext) =
    let fun predefine(id,arity,f) = 
		(bindTYC(id,ref(tNext(mkUNDEFtyc(id,arity)))),f)
	fun redefine(b as TYCbind(ref tyc), f) = 
		let val newtyc = f(tyclooker,[],stamps)
		 in tNext'(tyc,newtyc);
		    (b, newtyc)
		end
	val dtycs = map redefine (map predefine db)
	val tycbinds = map #1 dtycs and tycons = map #2 dtycs
	fun collectdcons(tyc::rest,dcbinds) =
	     let val TYCON{kind=DATAtyc(dcons),...} = tyc
		 fun binddcons(DATACON{name,...}::rest',dcbs) =
		      binddcons(rest',
		           (let val (b,_) = Env.look(NameSpace.conKey(name))
			     in b::dcbs
			    end
			    handle Unbound => dcbs))
		   | binddcons([],dcbs) = dcbs
	      in collectdcons(rest,binddcons(dcons,dcbinds))
	     end
	  | collectdcons([],dcbinds) = dcbinds
     in app (defineEqTycon (tyconInContext tempenv)) tycons;
	tycbinds @ collectdcons(tycons,[])
    end

fun make_tyspec(eq,tyvars,name,err) ({stamps,tNext,...}:signContext) =
    (checkbound(no_tyvars,tyvars,err);
     [bindTYC(name, ref(tNext(mkABStyc([name],length tyvars,eq,stamps))))])

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
      [bindVAR(name,VALvar{name=[name],typ=ref typ,access=SLOT(nextSlot())})]
  end

fun make_exnspec name ({nextSlot,...}:signContext) =
  [bindCON(name,DATACON{name=name,const=true,typ=ref exnTy,sign=[],
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
      [bindCON(name, DATACON{name=name, const=false, typ= ref typ,sign=[],
				      rep=VARIABLE(SLOT(nextSlot()))})]
  end

fun make_fixityspec(fixity,ops) _ = 
  (app(fn i => bindFIX(i,FIXvar{name=i,binding=fixity})) ops;  nil)

fun make_type_sharespec patheqn ({typeSharing,...}:signContext) =
		(typeSharing := patheqn :: !typeSharing; nil)

fun make_str_sharespec patheqn ({strSharing,...}:signContext) =
		(strSharing := patheqn :: !strSharing; nil)
end
