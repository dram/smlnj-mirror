(* Copyright 1989 by AT&T Bell Laboratories *)
signature STRS = 
sig type strArg
    type strtype 
    type spectype
    type signtype
    type functorFormal
    type 'a enved
    type 'a estamped
    type 'a epathstamped
    type 'a epathnstamped
    type 'a susp
    type 'a withenv
    type functorActual
    type constraint
    type symbol type spath type structureVar type strb type signatureVar
    type dec type Functor type functorVar type strexp type Structure
    type fctb
    val makeSTRBs: (symbol*structureVar*strb) list epathstamped
		    -> dec withenv epathnstamped
    val makeSTRB:  symbol * constraint * strtype * ErrorMsg.complainer ->
	                 bool -> 
			 (symbol * structureVar * strb) list epathstamped
    val make_str_qid:	spath * ErrorMsg.complainer -> strtype
    val make_str_struct: dec list withenv epathnstamped * ErrorMsg.complainer
			 -> strtype
    val spread_args:	dec list withenv epathnstamped -> functorActual
    val single_arg:	strtype -> functorActual
    val make_str_app:	symbol * ErrorMsg.complainer * functorActual -> strtype
    val make_str_let:	dec list withenv epathnstamped * strtype -> strtype
    val make_sigb:	symbol * signtype -> signatureVar list withenv estamped
    val makeSIGdec:	signatureVar list withenv estamped * ErrorMsg.complainer
			-> dec withenv epathnstamped
    val single_formal:	symbol * signtype -> functorFormal
    val spread_formal : spectype * ErrorMsg.complainer -> functorFormal
    val makeFCTB:	symbol * functorFormal * constraint * strtype *
			ErrorMsg.complainer ->
	                        (symbol * functorVar * fctb) list estamped
    val makeFCTdec: (symbol * functorVar * fctb) list estamped *
	                    ErrorMsg.complainer
		     -> dec withenv epathnstamped
    val makeLOCALsdecs:	dec list withenv epathnstamped
			* dec list withenv epathnstamped
			-> dec list withenv epathnstamped
end

structure Strs: STRS = 
struct

  open Types Access Modules Variables
  open ErrorMsg Symbol PrintUtil System.Control.Print
  open BasicTypes TypesUtil Absyn
  open ModuleUtil
  open Misc
  val DEBUG = true

type stampsets = Stamps.scope
type 'a susp = unit -> 'a
type 'a enved = Modules.env -> 'a
type 'a estamped = Modules.env * stampsets -> 'a
type 'a epathstamped = Modules.env * spath * stampsets -> 'a
type 'a epathnstamped = Modules.env * spath * bool * stampsets -> 'a
type 'a withenv = 'a * Modules.env
type spectype = Signs.spectype
type signtype = Signs.signtype
type constraint =  Modules.Signature option estamped
    
type strArg = {abstract:bool, constraint: Signature option,
	       path:spath, env:Modules.env, scope:stampsets}
type strtype = strArg -> strexp * Structure * thinning

type functorFormal = (symbol * Signature * bool) estamped

type functorActual = strArg -> strexp * Structure

fun makeSTRBs strb'  (env,path,_,stamps) =
   let val env' = ref(Env.empty: Modules.env)
       fun f (name,strVar,strSyn) =
		(env' := Env.bind(name,STRbind strVar,!env');
		 strSyn)
   in (STRdec (map f (strb' (env,path,stamps))), !env')
   end

fun makeSTRB(name,constraint_op,str,err) abstract ($ as (env,path,scope)) =
 case (constraint_op (env,scope),abstract)
  of (NONE,true) =>
	(err COMPLAIN "abstraction requires a signature constraint";
	 makeSTRB(name,fn _ =>NONE,str,err) false $)
   | (signopt,_) =>
  let val (strexp,str,thin)=
		str{abstract=abstract,constraint=signopt,path=name::path,
	            scope=scope,env=env}
      val strVar = STRvar{access=PATH[namedLvar(name)],	name=name,
				binding=str}
   in [(name, strVar,
	     STRB{strvar=strVar, def=strexp, constraint=signopt, thin=thin})]
  end

fun make_str_qid (qid,err) ({abstract,path,constraint,env,scope,...}:strArg) =
   let val strVar as STRvar{binding=str,...} = lookSTR (env,qid,err)
   in case constraint
	 of NONE => (VARstr strVar, str, NONE)
	  | SOME sgn =>
	      let val (str',thin) = SigMatch.match {sign=sgn,
						    str=str,
						    spath=path,
						    scope=scope,
						    err=err,
						    printEnv=env,
						    abstract=abstract,
						    self=false}
	       in (VARstr strVar, str', thin)
	      end
   end

(* struct ... end expressions *)
fun make_str_struct (sdecs,err)
                    ({abstract,path,constraint,env,scope}:strArg) =
    let val (body,env') = sdecs(env,path,true,scope)
     in case constraint
	 of NONE =>
	      let val (str,thin) = ModuleUtil.newStr (Stamps.newStamp scope (),
						      path,env')
	       in (STRUCTstr{body=body,locations=thin,str=str},
		   str, NONE)
	      end
	  | SOME sgn =>
	      let val (str,thin) = ModuleUtil.newStr(Stamps.newStamp scope (),
						     path,env')
	          val (str',thin') = SigMatch.match 
		                          {sign=sgn,
					   str=str,
					   spath=path,
					   scope=scope,
					   err=err,
					   printEnv=env,
					   abstract=abstract,
					   self=true}
		             (* self-origin *)
		  (* newStr followed by match becomes realize *)
	       in (STRUCTstr{body=body,locations=ModuleUtil.compose(thin',thin),str=str'},
		   str', NONE)
	      end
    end

fun spread_args sdecs ({scope,env,path,...}:strArg) =
  let val (body,env') = sdecs(env,[anonParamName],true,scope)
      val (str,thin) = ModuleUtil.newStr (Stamps.newStamp scope (),
					  [anonParamName], env')
   in (STRUCTstr{body=body,locations=thin,str=str}, str)
  end

fun single_arg str ({scope,env,path,...}:strArg) =
    let val (strexp,str',_) = str{abstract=false,constraint=NONE,env=env,
			          path=[anonParamName],
                                  scope=scope}
     in (strexp,str')
    end

fun make_str_app (id,err,arg)
                 (info as ({abstract,path,scope,constraint,env,...}:strArg))=
    let val fctVar as FCTvar{binding=fct,...} = lookFCT (env,id,err)
	val (argexp,argstr) = arg info
	val (result,thin1) = ApplyFunctor.applyFunctor(fct,argstr,scope,path,err,env)
	val strexp = APPstr{oper=fctVar, argexp=argexp, argthin=thin1,str=result}
     in case constraint
	  of NONE => (strexp,result,NONE)
	   | SOME sgn =>
	       let val (thinned,thin2) =
	             SigMatch.match
		             {sign=sgn,
			      str=result,
			      spath=path,
			      scope=scope,
			      err=err,
			      printEnv=env,
			      abstract=abstract,
			      self=false}
		in (strexp,thinned,thin2)
	       end
    end

fun make_str_let (sdecs,str) ({abstract,constraint,path,scope,env}:strArg) =
      let val (locals,localenv) = sdecs(env,path,false,scope)
	  val (bodyexp,bodystr,thin) =
                  str {abstract=abstract,constraint=constraint,path=path,
		       scope=scope,env=Env.atop(localenv,env)}
      in (LETstr(SEQdec locals,bodyexp),bodystr,thin)
      end

fun make_sigb(name,sign) (arg as (env,scope)) =
   let val binding = sign (Signs.MakeTOP arg)
       val binding' =
	   case binding
	   of SIG{symbols,env,kind,path,stamp} =>
	         SIG{symbols=symbols,env=env,kind=kind,path=SOME name,
		     stamp=stamp}
	    | _ => binding
       val sigvar = SIGvar{name=name,binding=binding'}
    in ([sigvar],Env.bind(name,SIGbind sigvar,Env.empty))
   end

fun makeSIGdec (sigb,_) (env,[],_,stamps) = 
		let val (sigvarl,env') = sigb(env,stamps)
		in (SIGdec sigvarl,env')
		end
  | makeSIGdec (sigb,err) (env,path,x,stamps) =
		(err WARN "signature found inside structure or functor"; 
		 makeSIGdec (sigb,err) (env,[],x,stamps))

fun single_formal(name,sign) (arg as (env,scope)) =
    (name,sign(Signs.MakeTOP arg),false)

fun spread_formal (spec_s,err) (arg as (env,scope)) =
    (anonParamName,
     Signs.makeSIG (spec_s,err) (Signs.MakeTOP arg),true)

 
fun makeFCTB(name,fparam,constraint_op,str,err) ($ as (env,scope)) =
   let val (pname,param_sign,spreadParams) = fparam $
       val plvar = namedLvar pname
       val paccess = PATH[plvar]
       val bodyScope = Stamps.newBoundScope()
       val paramScope = Stamps.newBoundScope()
       val isBodyStamp = Stamps.isBound bodyScope
       val isParamStamp = Stamps.isBound paramScope
       val _ = if !System.Control.internals andalso DEBUG then
	          (print "starting instantiate\n") else ()
       val param_str = Instantiate.instantiate
	                   (param_sign,
			    if spreadParams
                              then []
                              else [pname],
			    paramScope,err)
       val _ = (if !System.Control.internals andalso DEBUG then
		     (print "\nparameter structure:\n";
		      PrintBasics.printStructure(env,param_str,0,!signatures))
		else ())
       val paramvar = STRvar{name=pname,access=paccess,binding=param_str}
       val env' = if spreadParams
	          then ModuleUtil.openStructureVar (env,paramvar)
                  else Env.bind(pname,STRbind paramvar,env)
       val resSign = constraint_op(env',bodyScope)
       val (bodyexp,bodystr,thin) = 
		str{abstract=false,constraint=resSign,path=[],
			scope=bodyScope,env=env'}
       val _ = (if !System.Control.internals andalso DEBUG then
		    (print "\nbody structure:\n";
		     PrintBasics.printStructure(env,bodystr,0,!signatures))
		else ())
       val absbody = AbstractFct.abstractBody(bodystr,param_str,isBodyStamp,isParamStamp)
       val fct = FCT {paramName=pname,argument=param_sign,body=absbody}
       val fctv = FCTvar{name=name, 
	  		 access=PATH[namedLvar(name)],
       		  	 binding=fct}
       val fb = FCTB{fctvar=fctv, param=paramvar, def=bodyexp, thin=thin,
	       	      constraint=resSign}
    in [(name,fctv,fb)]
   end

fun makeFCTdec (fctb,_) (env,[],_,stamps) =
	let val env' = ref(Env.empty : Modules.env)
	    fun f (name,fctVar,fctSyn) =
		(env' := Env.bind(name,FCTbind fctVar,!env');
		 fctSyn)
	in (FCTdec (map f (fctb(env,stamps))), !env')
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
