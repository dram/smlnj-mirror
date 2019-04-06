(* Copyright 1989 by AT&T Bell Laboratories *)

structure EnvAccess : ENVACCESS = struct

(* lookup  functions *)

open Array List ErrorMsg PrintUtil Access Basics Symbol BasicTypes TypesUtil
infix 9 sub

fun impossible s = ErrorMsg.impossible ("EnvAccess." ^ s)

type symtable = binding IntStrMap.intstrmap
fun newTable () = IntStrMap.new(32, Env.Unbound) : symtable

val debugBind = System.Control.debugBind

val bogusVarID = Symbol.varSymbol "bogus"
val bogusStrID = Symbol.strSymbol "Bogus"
val bogusTycID = Symbol.tycSymbol "Bogus"
val bogusSigID = Symbol.sigSymbol "Bogus"

val bogusStrStamp = Stampset.newStamp(Stampset.fixedStrStamps)

local val b = STRstr{stamp=bogusStrStamp, sign=0, table=Env.empty, env=DIR,
		     kind=STRkind{path=[bogusStrID]}}
 in val bogusSTR = STRvar{name=[bogusStrID], access=PATH[0], binding=b}
    val bogusSTR' = STRvar{name=[bogusStrID], access=SLOT 0, binding=b}
end

(* type constructors *)

val bogusTyc = DEFtyc{path=[bogusTycID],tyfun=TYFUN{arity=0,body=ERRORty}}

fun lookTYC env (id:symbol) =
    let val TYCbind tyc = Env.look env id in tyc end

fun lookTYCinStr(STRstr{table,env,stamp,...}: Structure, id: symbol,_,
		  err : string->unit) : tycon =
    ((case lookTYC table id
	of INDtyc i =>
	     (case env
	       of REL{s,t} => t sub i
	        | DIR => impossible "lookTYCinStr 1")
	 | SHRtyc p => getEpathTyc(p,env)
	 | tyc => tyc)
     handle Unbound => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound type in structure: " ^ Symbol.name id);
	 bogusTyc))
  | lookTYCinStr _ = impossible "lookTYCinStr 2"

(* addzeros also defined in Parse *)
fun addzeros(0,l) = l
  | addzeros(n,l) = addzeros(n-1,0::l)

(* tycon lookup with arity checking *)

fun checkArity(tycon, arity,err) =
    if tyconArity(tycon) <> arity
    then err("type constructor "^(Symbol.name(tycName(tycon)))^
	          " has wrong number of arguments: "^makestring arity)
    else ()

fun lookArTYC env (id,arity,err) =
    let val tyc = lookTYC env id
     in checkArity(tyc,arity,err);
        tyc
    end
    handle Unbound => 
      (err("unbound type constructor: " ^ Symbol.name id);
       bogusTyc)


(* constructors *)

fun dconApplied(DATACON{name,const,typ,rep,sign},{path,strenv}) : datacon =
    DATACON{name = name, const = const, sign=sign,
            rep = (case rep
		     of VARIABLE(SLOT n) => VARIABLE(PATH(n::path))
		      | VARIABLEc(SLOT n) => VARIABLEc(PATH(n::path))
		      | _ => rep),  (* nonexception datacon *)
            typ = typeInContext(typ,strenv)}

val bogusCON = DATACON{name=bogusVarID,const=true,typ=ERRORty,
		       rep=UNDECIDED,sign=[]}

fun lookCONinStr(STRstr{table,env,stamp,...},id,ap,err): datacon =
    ((case Env.look table id
        of CONbind c => dconApplied(c,{path=ap,strenv=env})
         | _ => raise Env.Unbound)
      handle Env.Unbound => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound constructor in structure: " ^ Symbol.name id);
	 bogusCON))
  | lookCONinStr _ = impossible "lookCONinStr"


(* variables *)

fun varApplied(VALvar{access,name,typ}: var, {path, strenv}) : var =
      VALvar{access = case access
			of SLOT(n) => PATH(n::path)
			 | _ => access,
	     typ = case path
		     of [] => typ
		      | _ => ref(typeInContext(!typ,strenv)),
	     name = name}
  | varApplied(v, _) = v

val lookVARCON = Env.look

fun lookVARCONinStr(STRstr{table,env,stamp,...},id,ap,err): binding =
    ((case Env.look table id
       of VARbind v => VARbind(varApplied(v,{path=ap,strenv=env}))
        | CONbind c => CONbind(dconApplied(c, {path=ap,strenv=env}))
	| _ => impossible "lookVARCONinStr 1")
     handle Unbound =>
	(if stamp=bogusStrStamp then ()
	 else err("unbound variable or constructor in structure: "
		       ^ Symbol.name id);
	 CONbind bogusCON))
  | lookVARCONinStr(NULLstr,id,_,_) =
      (printSym id; print "\n"; impossible "lookVARCONinStr 2")
  | lookVARCONinStr(_,id,_,_) =
      (printSym id; print "\n"; impossible "lookVARCONinStr 3")

(* exceptions *)

fun notInitialLowerCase string =
    (* string does NOT start with lower-case alpha *)
    let val firstchar = ordof(string,0)
     in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
    end

(* signatures *)

val bogusSIGStampsets = Stampset.newStampsets()
val bogusSIGbody = 
    STRstr{stamp=Stampset.newStamp(#strStamps bogusSIGStampsets),
           sign=Stampset.newStamp(Stampset.sigStamps),
           table=Env.empty,
	   env=DIR,
	   kind=SIGkind{share={s=nil,t=nil},
		        bindings=nil,stamps=bogusSIGStampsets}}
val bogusSIG=SIGvar{name=bogusSigID,binding=bogusSIGbody}

fun lookSIG env id = 
  let val SIGbind sign = Env.look env id
  in sign 
  end

(* structures *)

fun strApplied(STRvar{name,access,binding},{path=ap,strenv}) =
    STRvar{name=name,
	   binding=(case (binding,strenv)
		     of (INDstr i,REL{s,...}) => s sub i
		      | (SHRstr(i::r),REL{s,...}) => getEpath(r,s sub i)
		      | (STRstr _, _) => binding
		      | _ => impossible "strApplied: bad binding/env"),
	   access=(case access
		     of SLOT(n) => PATH(n::ap)
		      | _ => impossible "strApplied: bad access")}

fun transBinding (info as {path,strenv}) =
   (fn (CONbind c) => CONbind(dconApplied(c,info))
     | (VARbind v) => VARbind(varApplied(v,info))
     | (STRbind v) => STRbind(strApplied(v,info))
     | (TYCbind(INDtyc i)) =>
	   let val REL{s,t} = strenv
	    in TYCbind(t sub i
		       handle Subscript => impossible "transBinding 1")
	   end
     | (TYCbind(SHRtyc p)) =>
	   TYCbind(getEpathTyc(p,strenv)
		   handle Subscript => impossible "transBinding 2")
     | (tb as TYCbind tyc) => tb
     | b => b)

fun openStructureVar env (STRvar{access,binding,...}) : env =
    (case binding
      of STRstr{table,env=strenv,kind,...} =>
	   let val p = case access of PATH p => p | _ => [0]
	    in Env.open' (table, transBinding({path=p,strenv=strenv}), env)
	   end
       | _ => impossible "openStructureVar -- bad binding")
  | openStructureVar _ _ = impossible "openStructureVar -- bad access value"

fun lookSTR env id =
    let val STRbind(strvar) = Env.look env id in strvar end

fun lookSTRinStr(STRstr{table,env,stamp,...},id,ap,err) =
    (strApplied(lookSTR table id,{path=ap,strenv=env})
     handle Unbound => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound structure in path: " ^ Symbol.name id);
	 bogusSTR))
  | lookSTRinStr _ = impossible "lookSTRinStr"


(* functors *)

fun lookFCT env id =
    let val FCTbind(fctvar) = Env.look env id in fctvar end

(* fixity bindings *)

fun lookFIX env id = 
    let val FIXbind(FIXvar{binding,...}) = Env.look env id
    in binding
    end handle Unbound =>  NONfix


fun allButLast [a] = nil
  | allButLast (a::rest) = a:: allButLast(rest)
  | allButLast _ = impossible "38838 in Envaccess"

(* lookup using symbolic path *)
fun lookPathinStr
      (str: Structure, ap: Access.path, spath as _::rest : symbol list,
       err: string -> unit,
       lookLast: Structure * symbol * Access.path * 
			(string->unit) -> 'a) : 'a =
    let fun getStr([id],str,ap) = lookLast(str,id,ap,err)
	  | getStr(id::rest,STRstr{table,stamp,env,...},ap) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTR table id
		      handle Unbound => 
			(if stamp=bogusStrStamp then ()
		         else (err("unbound intermediate structure: "
				        ^ name id);
		               print "  in path: ";
			       printSequence "." printSym spath;
		               newline());
		         bogusSTR')
	       in getStr(rest,
		  	 (case binding
			   of INDstr i => 
			      (case env
			        of REL{s,...} => s sub i
			         | DIR => impossible "lookPathinStr.getStr 1")
			    | SHRstr(i::r) => 
			      (case env
			        of REL{s,...} => getEpath(r,s sub i)
			         | DIR => impossible "lookPathinStr.getStr 2")
			    | _ => binding),
			 n::ap)
	      end
	  | getStr _ = impossible "lookPathinStr.getStr"
     in getStr(rest,str,ap)
    end
  | lookPathinStr _ = impossible "lookPathinStr"

fun lookPath (lookLast: Structure * symbol * Access.path * (string->unit)->'a) 
	     env
	     (spath as first::rest, err: string->unit) : 'a =
    let val STRvar{access,binding,...} =
	      lookSTR env first
	      handle Unbound => 
	        (err("unbound head structure: " ^ name first);
		 print " in path: "; printSequence "." printSym spath;
		 newline();
		 bogusSTR)
     in lookPathinStr(binding,
		      case access
			of PATH ap => ap
		         | _ => [],    (* in sig *)
		      spath,err,lookLast)
    end
  | lookPath _ _ _ = impossible "lookPath"

val lookPathSTR  = lookPath lookSTRinStr
val lookPathVARCON  = lookPath lookVARCONinStr
val lookPathCON  = lookPath lookCONinStr
val lookPathTYC  = lookPath lookTYCinStr

fun lookPathArTYC env ([id],a,err) = lookArTYC env (id,a,err)
  | lookPathArTYC env (path: symbol list, arity: int,err) =
    let val tyc = lookPathTYC env (path, err)
     in checkArity(tyc,arity,err);
	tyc
    end


(* functions to collect stale lvars for unbinding *)
exception NotStale

fun last [x] = x | last (a::r) = last r | last nil =impossible "Envaccess.last"

fun checkopen newenv v =
    let fun test (s:symbol) =
            case Env.look newenv s
              of VARbind(VALvar{access=PATH p,...}) =>
                  if last p = v then raise NotStale else ()
               | CONbind(DATACON{rep=VARIABLE(PATH p),...}) =>
                  if last p = v then raise NotStale else ()
               | CONbind(DATACON{rep=VARIABLEc(PATH p),...}) =>
                  if last p = v then raise NotStale else ()
               | STRbind(STRvar{access=PATH p,...}) =>
                  if last p = v then raise NotStale else ()
               | _ => ()

	fun trytable(STRstr{table,...}) =
	    (Env.app tryname table
	     handle Unbound => ())
	  | trytable _ = impossible "EnvAccess.trytable"

	and tryname (s,STRbind(STRvar{binding as STRstr _,...})) =
	    (test s; trytable binding)
	  | tryname (s,_) = test s

     in tryname
    end

fun staleLvars(newEnv,baseEnv) : int list =
    let val lvarset = ref([] : int list)
	val check = checkopen (Env.atop(newEnv, baseEnv))
        fun collect (s,_) = 
	  let val v = case Env.look baseEnv s
		       of VARbind(VALvar{access=PATH[v],...}) => v
		        | (b as STRbind(STRvar{access=PATH[v],...})) =>
			      (check v (s,b); v)
		        | FCTbind(FCTvar{access=PATH[v],...}) => v
			| CONbind(DATACON{rep=VARIABLE(PATH[v]),...}) => v
			| CONbind(DATACON{rep=VARIABLEc(PATH[v]),...}) => v
		        | _ => raise NotStale
	   in lvarset := v :: !lvarset
 	   end handle NotStale => ()
		    | Unbound => ()
     in Env.app collect newEnv;
        !lvarset
    end

end (* structure EnvAccess *)
