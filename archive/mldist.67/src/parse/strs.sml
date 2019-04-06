(* Copyright 1989 by AT&T Bell Laboratories *)
signature STRS = 
sig type strArg
    type strtype 
    type spectype
    type functorFormal
    type 'a enved
    type 'a epathstamped
    type 'a epathnstamped
    type 'a susp
    type 'a withenv
    type functorActual
    type symbol type spath type structureVar type strb type signatureVar
    type dec type Functor type functorVar type strexp type Structure
    type signtype type fctb
    val makeSTRBs: (symbol*structureVar*strb) list epathstamped
		    -> dec withenv epathnstamped
    val makeSTRB:  symbol * ((Basics.env * Structure) -> Structure option)
	           * strtype * ErrorMsg.complainer -> bool -> 
			 (symbol * structureVar * strb) list epathstamped
    val getSTRpath: Basics.env -> spath * ErrorMsg.complainer
	            -> Basics.structureVar
    val make_str_qid:	spath * ErrorMsg.complainer -> strtype
    val make_str_struct: dec list withenv epathnstamped * ErrorMsg.complainer -> strtype
    val spread_args:	dec list withenv epathnstamped -> functorActual
    val single_arg:	strtype -> functorActual
    val make_str_app:	symbol * ErrorMsg.complainer * functorActual -> strtype
    val make_str_let:	dec list withenv epathnstamped * strtype -> strtype
    val make_sigb:	symbol * signtype -> signatureVar list withenv enved
    val makeSIGdec:	signatureVar list withenv enved * ErrorMsg.complainer -> dec withenv epathnstamped
    val single_formal:	symbol * signtype -> functorFormal
    val spread_formal:	spectype * ErrorMsg.complainer -> functorFormal
    val makeFCTB:	symbol * functorFormal 
	                   * (Basics.env * Structure -> Structure option) 
	                   * strtype * ErrorMsg.complainer ->
	                          (symbol * functorVar * fctb) list enved
    val makeFCTdec: (symbol * functorVar * fctb) list enved * ErrorMsg.complainer ->
			dec withenv epathnstamped
    val makeLOCALsdecs:	dec list withenv epathnstamped * dec list withenv epathnstamped
			   -> dec list withenv epathnstamped
end

structure Strs: STRS = 
struct

  open ErrorMsg Symbol PrintUtil
  open Access Basics BasicTypes TypesUtil Absyn
  open EnvAccess
  open ModUtil
  open Misc

type stampsets = Stampset.stampsets
type 'a susp = unit -> 'a
type 'a enved = Basics.env -> 'a
type 'a epathstamped = Basics.env * spath * stampsets -> 'a
type 'a epathnstamped = Basics.env * spath * bool * stampsets -> 'a
type 'a withenv = 'a * Basics.env
type signtype = Signs.signtype
type spectype = Signs.spectype

type strArg = {abstract:bool,constraint: Structure option,
		path:spath, param: Structure,
		stamps:Stampset.stampsets, env:Basics.env}
type strtype = strArg -> strexp * Structure * thinning

type functorFormal = (symbol * Structure * bool) enved
type functorActual = Functor * strArg -> strexp * Structure
fun makeSTRBs strb' (env,path,_,stamps) =
   let val env' = ref(Env.empty: Basics.env)
       fun f (name,strVar,strSyn) =  
		(env' := Env.bind(name,STRbind strVar,!env');
		 strSyn)
   in (STRdec (map f (strb' (env,path,stamps))), !env')
   end

fun makeSTRB(name,constraint_op,str,err) abstract ($ as (env,path,stamps)) =
 case (constraint_op(env,NULLstr),abstract)
  of (NONE,true) =>
	(err COMPLAIN "abstraction requires a signature constraint";
	 makeSTRB(name,fn _ =>NONE,str,err) false $)
   | (signopt,_) =>
  let val (strexp,str,thin)=
		str{abstract=abstract,constraint=signopt,path=name::path,
	            param=NULLstr,stamps=stamps,env=env}
      val strVar = STRvar{access=PATH[namedLvar(name)],	name=[name],
				binding=str}
   in [(name, strVar,
	     STRB{strvar=strVar, def=strexp, constraint=signopt, thin=thin})]
  end

fun getSTRpath env ([id],err) = (lookSTR env id
	   handle Unbound => 
		      (err COMPLAIN ("unbound structure name: " ^ name id);
		       bogusSTR))
  | getSTRpath env (q,err) = lookPathSTR env (q,err COMPLAIN)

fun make_str_qid (qid,err) ({abstract,path,stamps,constraint,param,env,...}:strArg) =
   let val strVar as STRvar{binding,...} = getSTRpath env (qid,err)
    in case constraint
	 of NONE => (VARstr strVar, binding, NONE)
	  | SOME sgn =>
	      let val (str,thin) =
		    SigMatch.match(env,abstract,path,stamps,sgn,binding,param,err)
	       in (VARstr strVar, str, thin)
	      end
   end

fun make_str_struct (sdecs,err) ({abstract,path,param,stamps,constraint,env,...}:strArg) =
    let val (body,env') = sdecs(env,path,true,stamps)
     in case constraint
	 of NONE => let val (thin,table) = BuildMod.buildStrTable env'
			val str = mkSTR(path,table,DIR,stamps)
		     in (STRUCTstr{body=body,locations=thin,str=str},
		    	 str,
		   	 NONE)
	      	    end
	  | SOME sgn => let val (str,thin) =
				 SigMatch.realize(env,env',abstract,path,stamps,
				    Stampset.newStamp(#strStamps stamps),
				    sgn,param,err)
			 in (STRUCTstr{body=body,locations=thin,str=str},
			     str, NONE)
		        end
    end

fun spread_args sdecs (_,({stamps,env,...}:strArg)) =
  let val (body,env') = sdecs(env,[anonParamName],true,stamps)
      val (thin,table) = BuildMod.buildStrTable env'
      val str = mkSTR([anonParamName],table,DIR,stamps)
   in (STRUCTstr{body=body,locations=thin,str=str}, str)
  end

fun single_arg str (fct,({stamps,env,...}:strArg)) =
    let val FUNCTOR{paramName,...} = fct
        val (strexp,str,_) = str{abstract=false,constraint=NONE,
			 path=[paramName],param=NULLstr,stamps=stamps,env=env}
     in (strexp,str)
    end

val bogusID = Symbol.strSymbol "bogus"
val bogusFCT = FCTvar{name=bogusID, access=PATH[0],
		     binding=FUNCTOR{paramName=bogusID,
				     param=bogusSIGbody,
				     body=bogusSIGbody,
				     paramVis=false,
				     stamps=Stampset.newStampsets()}}

fun make_str_app (id,err,arg) (info as ({abstract,path,stamps,constraint,param,env,...}:strArg)) =
    let val fctVar as FCTvar{binding=fct,...} = lookFCT env id
		handle Unbound => (err COMPLAIN ("unbound functor identifier: " ^ Symbol.name id);
			           bogusFCT)
	val (argexp,argstr) = arg(fct,info)
	val (result,thin1) = Functor.applyFunctor(env,fct,argstr,path,stamps,err)
	val strexp = APPstr{oper=fctVar, argexp=argexp, argthin=thin1,str=result}
     in case constraint
	  of NONE => (strexp,result,NONE)
	   | SOME sgn =>
	       let val (thinned,thin2) =
	              SigMatch.match(env,abstract,path,stamps,sgn,result,param,err)
		in (strexp,thinned,thin2)
	       end
    end

fun make_str_let (sdecs,str) ({abstract,constraint,path,param,stamps,env}:strArg) =
      let val (locals,localenv) = sdecs(env,path,false,stamps)
	  val (bodyexp,bodystr,thin) = str 
		{abstract=abstract,constraint=constraint,path=path,param=param,
		 stamps=stamps,env=Env.atop(localenv,env)}
      in (LETstr(SEQdec locals,bodyexp),bodystr,thin)
      end

fun make_sigb(name,sign) env =
   let val sigvar = SIGvar{name=name,binding=sign(env,Stampset.newStampsets())}
    in ([sigvar],Env.bind(name,SIGbind sigvar,Env.empty))
   end

fun makeSIGdec (sigb,_) (env,[],_,stamps) = 
		let val (sigvarl,env') = sigb env
		in (SIGdec sigvarl,env')
		end
  | makeSIGdec (sigb,err) (env,path,x,stamps) =
		(err WARN "signature found inside structure or functor"; 
		 makeSIGdec (sigb,err) (env,[],x,stamps))

fun single_formal(name,sign) env =
    (name,sign(env,Stampset.newStampsets()),false)

fun spread_formal (spec_s,err) env =
    (anonParamName,
     Signs.makeSIG (spec_s,err) (true,true,NULLstr) 
                                         (env,Stampset.newStampsets()),
     true)

fun makeFCTB(name,fparam,constraint_op,str,err) env =
   let val (pname,param,spreadParams) = fparam env
       val plvar = namedLvar pname
       val paccess = PATH[plvar]
       val env' =
	   if spreadParams
	   then let val STRstr{env=strenv,table,...} = param
		in Env.open'(table,
			     transBinding({path=[plvar],strenv=strenv},[pname]),
			     env)
	        end
	    else let val strvar = 
		            STRvar{name=[pname],access=paccess,binding=param}
		 in Env.bind(pname,STRbind strvar,env)
	    end
       val resSign = constraint_op (env',param)
       val bodystamps = Stampset.newStampsets()
       val baseSigStamp = Stampset.newStamp(Stampset.sigStamps)
       val (bodyexp,bodystr,thin) = 
		str{abstract=false,constraint=resSign,path=[],
			stamps=bodystamps,param=param,env=env'}
       val openBody = case bodystr
		       of STRstr{stamp=bodystamp,env=DIR,...} =>
			   Stampset.member(bodystamp,(#strStamps bodystamps))
		        | _ => false
       val paramVis = case resSign of SOME _ => true | NONE => openBody
       val body = if openBody
		    then Functor.abstractBody(bodystr,param,bodystamps,
			   baseSigStamp,err)
		    else bodystr
       val paramvar = STRvar{name=[pname], access=paccess, binding=param}
       val fctv = FCTvar{name=name, 
	  		 access=PATH[namedLvar(name)],
       		  	 binding=FUNCTOR{paramName=pname, param=param,
       				  	 body=body, paramVis=paramVis,
       				  	 stamps=bodystamps}}
       val fb = FCTB{fctvar=fctv, param=paramvar, def=bodyexp, thin=thin,
	       	      constraint=resSign}
    in [(name,fctv,fb)]
   end

fun makeFCTdec (fctb,_) (env,[],_,stamps) =
	let val env' = ref(Env.empty : Basics.env)
	    fun f (name,fctVar,fctSyn) =
		(env' := Env.bind(name,FCTbind fctVar,!env');
		 fctSyn)
	in (FCTdec (map f (fctb env)), !env')
	end		
  | makeFCTdec (fctb,err) (env,path,x,stamps) =
	   (err WARN "functor found inside structure or functor"; 
	    makeFCTdec (fctb,err) (env,[],x,stamps))

fun makeLOCALsdecs(sdecs1,sdecs2) ($ as (env,path,top,stamps)) =
  let val (ld1,env1) = sdecs1 $
      val (ld2,env2) = sdecs2 (Env.atop(env1,env),path,top,stamps)
  in ([LOCALdec(SEQdec ld1,SEQdec ld2)], env2)
  end

end
