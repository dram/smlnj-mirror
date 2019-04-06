(* Copyright 1989 by AT&T Bell Laboratories *)
signature STRS = 
sig type strArg
    type strtype type spectype
    type functorFormal
    type 'a pathstamped
    type 'a pathnstamped
    type 'a susp
    type functorActual
    type symbol type structureVar type strb type signatureVar
    type dec type Functor type functorVar type strexp type Structure
    type signtype type fctb
    val makeSTRBs:	(symbol*structureVar*strb) list pathstamped
			 -> dec pathnstamped
    val makeSTRB:	symbol * (Structure -> Structure option) * strtype
				* ErrorMsg.complainer -> bool -> 
			 (symbol * structureVar * strb) list pathstamped
    val make_str_qid:	symbol list * ErrorMsg.complainer -> strtype
    val make_str_struct: dec list pathnstamped * ErrorMsg.complainer -> strtype
    val spread_args:	dec list pathnstamped -> functorActual
    val single_arg:	strtype -> functorActual
    val make_str_app:	symbol * ErrorMsg.complainer * functorActual -> strtype
    val make_str_let:	dec list pathnstamped * strtype -> strtype
    val make_sigb:	symbol * signtype -> signatureVar list susp
    val makeSIGdec:	signatureVar list susp * ErrorMsg.complainer -> dec pathnstamped
    val single_formal:	symbol * signtype -> functorFormal
    val spread_formal:	spectype * ErrorMsg.complainer -> functorFormal
    val makeFCTB:	symbol * functorFormal * (Structure -> Structure option) * strtype * ErrorMsg.complainer ->
			    (symbol * functorVar * fctb) list susp
    val makeFCTdec: (symbol * functorVar * fctb) list susp * ErrorMsg.complainer ->
			dec pathnstamped
    val makeLOCALsdecs:	dec list pathnstamped * dec list pathnstamped
			   -> dec list pathnstamped
end

structure Strs: STRS = 
struct

  open ErrorMsg Symbol PrintUtil
  open Access Basics BasicTypes TypesUtil Absyn
  open Env
  open EnvAccess
  open ModUtil
  open Misc

type stampsets = Stampset.stampsets
type 'a susp = unit -> 'a
type 'a pathstamped = symbol list * stampsets -> 'a
type 'a pathnstamped = symbol list * bool * stampsets -> 'a
type signtype = Signs.signtype
type spectype = Signs.spectype

type strArg = {abstract:bool,constraint: Structure option,
		path:symbol list, param: Structure,
		stamps:Stampset.stampsets}
type strtype = strArg -> strexp * Structure * thinning

type functorFormal = (symbol * Structure * bool) susp
type functorActual = Functor * strArg -> strexp * Structure
fun makeSTRBs strb' (path,_,stamps) =
  STRdec(map (fn (name,strVar,strSyn) => (bindSTR(name,strVar); strSyn)) (strb' (path,stamps)))

fun makeSTRB(name,constraint,str,err) abstract ($ as (path,stamps)) =
 case (constraint(NULLstr),abstract)
  of (NONE,true) =>
	(err COMPLAIN "abstraction requires a signature constraint";
	 makeSTRB(name,fn _ =>NONE,str,err) false $)
   | (signopt,_) =>
  let val (strexp,str,thin)=
		str{abstract=abstract,constraint=signopt,path=name::path,
	            param=NULLstr,stamps=stamps}
      val strVar = STRvar{access=LVAR(namedLvar(name)),	name=[name],
				binding=str}
   in [(name, strVar,
	     STRB{strvar=strVar, def=strexp, constraint=signopt, thin=thin})]
  end

fun make_str_qid (qid,err) ({abstract,path,stamps,constraint,param,...}:strArg) =
   let val strVar as STRvar{binding,...} = getSTRpath(qid,err)
    in case constraint
	 of NONE => (VARstr strVar, binding, NONE)
	  | SOME sgn =>
	      let val (str,thin) =
		    SigMatch.match(abstract,path,stamps,sgn,binding,param,err)
	       in (VARstr strVar, str, thin)
	      end
   end

fun make_str_struct (sdecs,err) ({abstract,path,param,stamps,constraint,...}:strArg) =
    let val _ = openStr()
	val body = sdecs(path,true,stamps)
     in case constraint
	 of NONE => let val (thin,table) = BuildMod.buildStrTable ()
			val str = mkSTR(path,table,DIR,stamps)
		     in (STRUCTstr{body=body,locations=thin,str=str},
		    	 str,
		   	 NONE)
	      	    end
	  | SOME sgn => let val (str,thin) =
				 SigMatch.realize(abstract,path,stamps,
				    Stampset.newStamp(#strStamps stamps),
				    sgn,param,err)
			 in closeStr();
			    (STRUCTstr{body=body,locations=thin,str=str},
			     str, NONE)
		        end
    end

fun spread_args sdecs (_,({stamps,...}:strArg)) =
  let val _ = openStr()
      val body = sdecs([anonParamName],true,stamps)
      val (thin,table) = BuildMod.buildStrTable ()
      val str = mkSTR([anonParamName],table,DIR,stamps)
   in (STRUCTstr{body=body,locations=thin,str=str}, str)
  end

fun single_arg str (fct,({stamps,...}:strArg)) =
    let val FUNCTOR{paramName,...} = fct
        val (strexp,str,_) = str{abstract=false,constraint=NONE,
			 path=[paramName],param=NULLstr,stamps=stamps}
     in (strexp,str)
    end

val bogusID = Symbol.symbol "bogus"
val bogusFCT = FCTvar{name=bogusID, access=PATH[0],
		     binding=FUNCTOR{paramName=bogusID,
				     param=bogusSIGbody,
				     body=bogusSIGbody,
				     paramVis=false,
				     stamps=Stampset.newStampsets()}}

fun make_str_app (id,err,arg) (info as ({abstract,path,stamps,constraint,param,...}:strArg)) =
    let val fctVar as FCTvar{binding=fct,...} = lookFCT id
		handle Unbound => (err COMPLAIN ("unbound functor identifier: " ^ Symbol.name id);
			           bogusFCT)
	val (argexp,argstr) = arg(fct,info)
	val (result,thin1) = Functor.applyFunctor(fct,argstr,path,stamps,err)
	val strexp = APPstr{oper=fctVar, argexp=argexp, argthin=thin1,str=result}
     in case constraint
	  of NONE => (strexp,result,NONE)
	   | SOME sgn =>
	       let val (thinned,thin2) =
		       SigMatch.match(abstract,path,stamps,sgn,result,param,err)
		in (strexp,thinned,thin2)
	       end
    end

fun make_str_let (sdecs,str) (info as ({path,stamps,...}:strArg)) =
   protect(protectScope,fn()=>
      let val locals = sdecs(path,false,stamps)
	  val (bodyexp,bodystr,thin) = str info
       in (LETstr(SEQdec locals, bodyexp),bodystr,thin)
      end)

fun make_sigb(name,sign) () =
   let val sigvar = SIGvar{name=name,binding=sign(1,Stampset.newStampsets())}
    in bindSIG(name, sigvar); [sigvar]
   end

fun makeSIGdec (sigb,_) ([],_,stamps) = SIGdec(sigb())
  | makeSIGdec (sigb,err) (path,x,stamps) =
			 (err WARN "signature found inside structure or functor"; 
			  makeSIGdec (sigb,err) ([],x,stamps))

fun single_formal(name,sign) () =
    (name,sign(1,Stampset.newStampsets()),false)

fun spread_formal (spec_s,err) () =
    (anonParamName,
     Signs.makeSIG (spec_s,err) (true,NULLstr) (2,Stampset.newStampsets()),
     true)

fun makeFCTB(name,fparam,constraint_op,str,err) () =
   let val mEntry = openScope()
       val (pname,param,spreadParams) = fparam()
       val paccess = LVAR(namedLvar name) 
       val resSign =
	   if spreadParams
	   then let val STRstr{env,table,...} = param
		    and LVAR plvar = paccess
		in openOld({path=[~1,1],strenv=env},table);
		    let val resSign = constraint_op param
		     in resetEnv(mEntry);
			openOld({path=[plvar],strenv=env},table);
			resSign
		    end
		end
	   else let val senv = array(2, NULLstr) and tenv = array(0, NULLtyc)
		 in update(senv,1,param);
		    openNew({path=[~1], strenv=REL{t=tenv,s=senv}}, newTable());
		    bindSTR(pname,STRvar{name=[pname], access=paccess,
					 binding=INDstr(1)});
		    let val resSign = constraint_op param
		     in	resetEnv(mEntry);
			openNew({path=[],strenv=DIR},newTable());
			bindSTR(pname,STRvar{name=[pname], access=paccess,
					     binding=param});
			resSign
		    end
		end
       val bodystamps = Stampset.newStampsets()
       val (bodyexp,bodystr,thin) = 
		str{abstract=false,constraint=resSign,path=[],
			stamps=bodystamps,param=param}
       val openBody = case bodystr
		       of STRstr{stamp=bodystamp,env=DIR,...} =>
			   Stampset.member(bodystamp,(#strStamps bodystamps))
		        | _ => false
       val paramVis = case resSign of SOME _ => true | NONE => openBody
       val body = if openBody
		    then Functor.abstractBody(bodystr,param,bodystamps,
			   Stampset.newStamp(Stampset.sigStamps),err)
		    else bodystr
       val paramvar = STRvar{name=[pname], access=paccess, binding=param}
       val fctv = FCTvar{name=name, 
	  		 access=LVAR(namedLvar(name)),
       		  	 binding=FUNCTOR{paramName=pname, param=param,
       				  	 body=body, paramVis=paramVis,
       				  	 stamps=bodystamps}}
       val fb = FCTB{fctvar=fctv, param=paramvar, def=bodyexp, thin=thin,
	       	      constraint=resSign}
    in resetEnv(mEntry);
       [(name,fctv,fb)]
   end

fun makeFCTdec (fctb,_) ([],_,stamps) =
  FCTdec(map (fn(name,fctVar,fctSyn)=>(bindFCT(name,fctVar); fctSyn)) (fctb()))
  | makeFCTdec (fctb,err) (path,x,stamps) =
   (err WARN "functor found inside structure or functor"; 
    makeFCTdec (fctb,err) ([],x,stamps))

fun makeLOCALsdecs(sdecs1,sdecs2) ($ as (path,top,stamps)) =
  let val envLocal = openScope()
      val ld1 = sdecs1 (path,false,stamps)
      val envIn = (openScope(); current())
      val ld2 = sdecs2 $
   in splice(envLocal,envIn);
      [LOCALdec(SEQdec ld1, SEQdec ld2)]
  end

end
