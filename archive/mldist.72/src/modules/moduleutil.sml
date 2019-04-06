structure ModuleUtil : MODULE_UTIL =
struct
  structure Modules = Modules
  open Modules ErrorMsg Variables Access Types TypesUtil PrintUtil Symbol

  exception UnboundComponent of spath
  exception ErrorStructure

  fun last [x] = x
    | last nil = impossible "ModuleUtil.last"
    | last (h :: t) = last t

  fun getStrStamp (SIMPLE{stamp,...}) = stamp
    | getStrStamp (INSTANCE{origin=SIMPLE{stamp,...},...}) = stamp
    | getStrStamp (INSTANCE{origin=SELF stamp,...}) = stamp
    | getStrStamp (INSTANCE{origin=INSTANCE{origin=SELF stamp,...},...})= stamp
    | getStrStamp ERROR_STR = Stamps.error
    | getStrStamp _ = impossible "ModuleUtil.getStrStamp"

  fun getSignStamp (SIG{stamp,...}) = stamp
    | getSignStamp ERROR_SIG = Stamps.error

  fun eqSign (a,b) = getSignStamp a = getSignStamp b

  fun getOrigin (str as INSTANCE{origin = SELF _, ...}) = str
    | getOrigin (INSTANCE{origin, ...}) = origin
    | getOrigin str = str

  fun eqOrigin (x,y) =
      case (getOrigin x,getOrigin y)
      of (ERROR_STR,_) => true
       | (_,ERROR_STR) => true
       | (ox,oy) => getStrStamp ox = getStrStamp oy
 
  fun mkStructure (env,path)= SIMPLE {stamp=Stamps.newFree(),env=env,path=path}
  
  fun newStr (stamp,path, env : Modules.env) : Modules.Structure * trans list =
    let val newenv = ref(Env.empty : Modules.env)
	fun look s = Env.look (!newenv,s)
	fun add(s,b) = newenv := Env.bind(s,b,!newenv)
	val bindings = 
	    (* no sorting done, except chronological by Env.app *)
	    let val r = ref (nil : (symbol * binding) list)
		fun add (s,b) = r := (s,b) :: !r
	     in Env.app add env; (* r now sorted youngest first *)
		!r
	    end
        fun last(x as [_]) = x
          | last(a::b) = last b
          | last [] = impossible "ModuleUtil.last"
	fun fill (nil,count) = nil
	  | fill ((bdg as (s,binding))::rest,count) =
	       (look s; fill(rest,count)) handle Env.Unbound =>	
	     case binding
	      of VARbind(var as VALvar{access,name,typ}) =>
		   (add(s,
		        VARbind(
			  case access
			   of INLINE(_) => var
			    | _ =>
				VALvar{access = SLOT count,
				       typ = typ,
				       name = last name}));
		    VALtrans(access)::fill(rest,count+1))
	       | CONbind(DATACON{name,const,sign,typ,rep}) =>
		   let val dcon = DATACON{name=name,const=const,sign=sign,
					  typ=typ,
					  rep=case rep
					        of VARIABLE _ => VARIABLE(SLOT count)
						 | VARIABLEc _ => VARIABLEc(SLOT count)
						 | _ => rep}
		    in add(s,CONbind(dcon));
		       case rep
			 of VARIABLE access => (* exception constructor *)
			     VALtrans(access)::fill(rest,count+1)
			  | VARIABLEc access => (* exception constructor *) 
			     VALtrans(access)::fill(rest,count+1)
			  | _ => fill(rest,count) (* datatype constructor *)
		   end
	       | STRbind(STRvar{name,access,binding}) =>
		      (add(s, STRbind(STRvar{name=name,
				               binding=binding,
				               access=SLOT(count)}));
		       VALtrans(access)::fill(rest,count+1))
	       | TYCbind tycon =>
		   let fun newDcon(DATACON{name,const,typ,rep,sign}) =
		           DATACON{name=name,const=const,rep=rep,sign=sign,
				   typ=typ}
		       val tycon' =
		           case tycon
			     of GENtyc{stamp,arity,eq,path,kind=ref(DATAtyc dcons)} =>
				 GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
				        kind=ref(DATAtyc(map newDcon dcons))}
			      | tyc => tyc
		    in add(s, TYCbind tycon');
		       fill(rest,count)
		   end
	       | _ => (add bdg; fill(rest,count))
        val trans = fill(bindings,0)
     in (SIMPLE{stamp=stamp,path=path,env=Env.consolidate(!newenv)},trans)
    end

  fun compose (NONE,r) = r
    | compose (SOME(v,result),trans) =
        let val trans' : Access.access array =
	       Array.arrayoflist(map (fn VALtrans access => access
			         | _ => impossible "ModuleUtil.compose") trans)
	    fun replace (PATH [slot,lv]) = Array.sub(trans',slot)
              | replace (access as INLINE _) = access
              | replace _ = impossible "ModuleUtil.compose"
        in map (fn VALtrans access => VALtrans(replace access)
		 | THINtrans(access,t,l) => THINtrans(replace access,t,l)
		 | v => v) result
        end

  fun getSubStrs str =
      let val r = ref nil
	  val (env, f) =
	    case str
	      of (SIMPLE{env,...}) =>
		  (env,(fn (sym,STRbind(STRvar{binding=str,...})) =>
			   r := (sym,str) :: !r
			 | _ => ()))
	       | (INSTANCE{sign=SIG{env,...},subStrs,...}) =>
		  (env,(fn (sym,STRbind(STRvar{binding=FORMAL{pos,...},...})) =>
			    r := (sym,Array.sub(subStrs,pos)) :: !r
			 | _ => ()))
               | (INSTANCE{sign=ERROR_SIG,...}) => (Env.empty,fn _ => ())
               | ERROR_STR => (Env.empty,fn _ => ())
               | _ => impossible "getSubStrs"
      in Env.app f env; !r
      end

  fun appendAccess (SLOT s, l) = PATH (s :: l)
    | appendAccess (acc as INLINE _, l) = acc
    | appendAccess (acc,_) =
         (* hack so that we can print structures w/ bogus access paths *)
              if !System.Control.internals then acc
              else impossible "ModuleUtil.appendAccess"
 
  fun transPosTycon str path =
      let fun f ([pos],INSTANCE{types,...}) =
	                    Array.sub(types,pos)
            | f (h::t,INSTANCE{subStrs,...}) =
	            f(t,Array.sub(subStrs,h))
	    | f (_,ERROR_STR) = ERRORtyc
            | f _ = impossible "ModuleUtil.transPosTycon 1"
     in f (path,str) handle Array.Subscript =>
	    (print "path: "; PrintUtil.prIntPath path;
	     impossible "ModuleUtil.transPosTycon 2")
     end

  fun transPosStr str path =
      let fun f ([],str) = str
            | f (h::t,INSTANCE{subStrs,...}) =
	            f(t,Array.sub(subStrs,h))
            | f (_,ERROR_STR) = ERROR_STR
            | f _ = impossible "ModuleUtil.transPosStr 1"
     in f (path,str) handle Array.Subscript =>
	    (print "path: "; PrintUtil.prIntPath path;
	     impossible "ModuleUtil.transPosStr 2")
     end

  fun transType (subStrs, types) =
      let fun findFormStr(str,[]) = str
	    | findFormStr(INSTANCE{subStrs, ...},h::t) =
		findFormStr(Array.sub(subStrs,h),t)
            | findFormStr(ERROR_STR,_) = ERROR_STR 
	    | findFormStr _ = impossible "ModuleUtil.findFormStr"

	  fun findFormTyc([], tycIndex) = Array.sub(types,tycIndex)
	    | findFormTyc(h::t, tycIndex) =
	      (case findFormStr(Array.sub(subStrs,h), t)
		of INSTANCE{types, ...} => Array.sub(types,tycIndex)
		 | ERROR_STR => ERRORtyc
		 | _ => impossible "ModuleUtil.findFormTyc")

	  fun transTycon (RELtyc {pos=tycAddress,...}) = findFormTyc tycAddress
	    | transTycon tyc = tyc

	  fun transType0 ty =
	      case ty
	      of VARty _ => ty
	       | CONty (tc, tl) =>
		   CONty (transTycon tc, map transType0 tl)
	       | FLEXRECORDty (ref rt) =>
		   impossible "Modules.transType0"
	       | POLYty {sign, tyfun=TYFUN {arity, body}, abs} =>
		   POLYty{sign=sign,
			  tyfun=TYFUN{arity=arity,body=transType0 body},
			  abs=abs}
	       | UNDEFty => ty
	       | ERRORty => ty
       in transType0
      end

  (* transBindingINSTANCE: Structure array * tycon array * int list
                             -> Modules.binding -> Modules.binding

     The binding argument is assumed to be from the environment of
     signature of the structure (which must be an INSTANCE), so its access,
     if any, will be a SLOT or INLINE.
     transBindingINSTANCE interprets types of value and constructor bindings,
     and adjusts access info to make it absolute (i.e. relative to the
     top-level dynamic environment).  The int list argument is the access
     path of the structure argument. *)

  fun transBindingINSTANCE (subStrs,types,apath) binding =
      let val transType = transType (subStrs, types)
	  (* invariant: Any structure binding in the sign of an
	     INSTANCE structure is a FORMAL *)
       in case binding
	  of VARbind (VALvar {access, name, typ}) =>
		  VARbind (VALvar {access=appendAccess (access, apath),
				   name=name,typ=ref(transType(!typ))})
	   | VARbind (OVLDvar _) => impossible "Modules.transBindingINSTANCE"
	   | VARbind ERRORvar => binding
	   | CONbind (DATACON {name, const, typ, rep, sign}) =>
		 CONbind (DATACON {name=name, const=const,
				   rep=
				   case rep
				   of VARIABLE access =>
				       VARIABLE (appendAccess(access,apath))
				    | VARIABLEc access =>
				       VARIABLEc (appendAccess (access,apath))
				    | _ => rep,
				   sign=sign,
				   typ=transType typ})
	   | TYCbind (FORMtyc {pos, ...}) => TYCbind (Array.sub(types,pos))
	   | SIGbind _ => binding
	   | STRbind (STRvar {name, access, binding=FORMAL{pos, ...}}) =>
		 STRbind (STRvar {access=appendAccess (access, apath),
				  name=name,binding=Array.sub(subStrs,pos)})
	   | FCTbind (FCTvar {name, access, binding}) =>
		 FCTbind (FCTvar {access=appendAccess (access, apath),
				  name=name,binding=binding})
	   | _ => binding
     end

  (* transBindingSIMPLE: int list -> Modules.binding -> Modules.binding
     just adjusts access fields of bindings.  bindings assumed to come
     from a SIMPLE structure, and the int list is its top-level access path *)
  fun transBindingSIMPLE apath binding =
      case binding
       of VARbind (VALvar {access, name, typ}) =>
	    VARbind (VALvar {access=appendAccess (access, apath),
			     name=name,typ=typ})
	| CONbind (DATACON {name, const, typ, rep, sign}) =>
	    CONbind (DATACON {name=name, const=const, sign=sign,
			      rep=
			      case rep
			      of VARIABLE access =>
			           VARIABLE(appendAccess (access, apath))
			       | VARIABLEc access =>
				   VARIABLEc(appendAccess(access,apath))
			       | rep => rep,
			      typ=typ})
	| STRbind (STRvar {name, access, binding}) =>
	    STRbind (STRvar {access=appendAccess (access, apath),
			     name=name,binding=binding})
	| FCTbind (FCTvar {name, access, binding}) =>
	    FCTbind (FCTvar {access=appendAccess (access, apath),
			     name=name,binding=binding})
	| binding => binding

      (* err: raise an exception when an unbound component in 
	 the symbolic path is found.  It is passed the remainder
	 of the symbolic path, including the unbound component.
	 From this it computes the symbolic path to the unbound
	 component. *)

        fun err spath (r as (h::t)) =
	  let fun g (0,_) = [h]
	       | g (i,h::t) = h :: g(i-1,t)
	       | g _ = impossible "ModuleUtil.err"
 	  in raise UnboundComponent(g(length spath-length r,spath))
 	  end
          | err spath _ = impossible "ModuleUtil.spath"

    (* find a binding, adjust its access paths and interpret its types.*)

  fun lookBinding (topStr,spath,apath) : binding =
     let val err' = err spath
	 fun get (str, [sym], apath) =
	  ((case str
	      of SIMPLE {env,...} =>
		 transBindingSIMPLE apath (Env.look(env,sym))
	       | INSTANCE {subStrs,types,sign=SIG{env,...},...} =>
		 transBindingINSTANCE(subStrs,types,apath) (Env.look(env,sym))
	       | ERROR_STR => raise ErrorStructure
	       | _ => impossible "lookBinding")
            handle Env.Unbound => raise UnboundComponent spath)
	   | get (str, spath as (h::t), apath) =
	       let val (str',slot) =
		      (case str
			of SIMPLE {env,...} =>
			    let val STRbind(STRvar{access=SLOT slot,
						    binding=str,
						    ...}) =
				      Env.look(env,h)
			     in  (str,slot)
			     end
			 | INSTANCE{sign=SIG{env,...},subStrs,...} =>
			     let val STRbind(STRvar{access=SLOT slot,
						    binding=FORMAL{pos,...},
						    ...}) =
				      Env.look(env,h)
			      in (Array.sub(subStrs,pos),slot)
			     end
			 | ERROR_STR => raise ErrorStructure
			 | _ => impossible "ModuleUtil.lookUnadjusted 2")
		       handle Env.Unbound => err' spath
		in get(str', t, slot :: apath)
	       end
	    | get (ERROR_STR, _,_) = raise ErrorStructure
	    | get _ = impossible "Modules.lookBinding DD95"
      in get (topStr,spath,apath)
      end


  (* lookBindingSTR: look up a structure binding, but don't adjust
     access paths.*)

  fun lookBindingSTR (str,spath) =
      case lookBinding (str,spath,[])
      of STRbind str => str
       | _ => impossible "lookBindingSTR"

 (* lookBindTYC: look up a type binding *)

  fun lookBindingTYC (str,spath) =
      case lookBinding (str,spath,[])
      of TYCbind tyc => tyc
       | _ => impossible "lookBindingTYC"

  fun makeEnv (str as INSTANCE{sign=SIG{env,...},subStrs,types, ...}, apath) =
	Env.open' (env, transBindingINSTANCE(subStrs,types,apath), Env.empty)
    | makeEnv (str as SIMPLE{env, ...}, apath) =
	Env.open' (env, transBindingSIMPLE(apath), Env.empty)
    | makeEnv (ERROR_STR, _) = Env.empty
    | makeEnv _ = impossible "ModuleUtil.makeEnv"

  val bogusSTRvar = STRvar{name=Symbol.strSymbol "Bogus",
			   access=SLOT 0, binding=ERROR_STR}
  val bogusFCTvar = FCTvar{name=Symbol.fctSymbol "Bogus",
			   access=SLOT 0, binding=ERROR_FCT}

  (* signatures *)

  val symbolToName = fn s => Symbol.nameSpaceToString(Symbol.nameSpace s)

  fun lookSIG (env,id,err) = 
      let val SIGbind(SIGvar{binding,...}) = Env.look(env,id)
       in binding
      end handle Env.Unbound =>
	  (err COMPLAIN ("unbound signature: "^Symbol.name id); ERROR_SIG)
               | Bind => impossible ("ModuleUtil.lookSIG: bind exception looking up "^Symbol.name id^" in name space "^symbolToName id)

  (* functors *)

  fun lookFCT (env,id,err) =
      let val FCTbind fv = Env.look(env,id) in fv end 
      handle Env.Unbound =>
	   (err COMPLAIN ("unbound functor: "^Symbol.name id);
	    bogusFCTvar)
           | Bind => impossible ("ModuleUtil.lookFCT: bind exception looking up "^Symbol.name id^" in name space "^symbolToName id)

  (* fixity bindings *)

  fun lookFIX (env,id) =
      let val FIXbind(FIXvar{binding,...}) = Env.look(env,id)
      in binding
      end handle Env.Unbound =>  Fixity.NONfix
	       | Bind => impossible ("ModuleUtil.lookFix: bind exception looking up "^Symbol.name id^" in name space "^symbolToName id)


  (* lookFormalBinding: given a symbolic path, find a formal binding.
     Also return a relative path to it.*)

  fun lookFormalBinding(env,spath) : binding * int list =
      let val err' = err spath
          fun get (env,[id],p) =
	        ((Env.look (env,id),rev p)
	         handle Env.Unbound => raise (UnboundComponent spath))
            | get (env,spath as (first::rest),p) =
	       ((case Env.look (env,first)
		of STRbind(STRvar{binding=FORMAL{pos,
						 spec=SIG{env,kind,...},...},
				  ...}) =>
	             get(env,rest,
			 case kind of EMBEDDED => p | TOP _ => pos::p)
                 | STRbind(STRvar{binding=ERROR_STR,...}) =>
		     raise ErrorStructure
                 | _ => impossible "ModuleUtil.lookFormalBinding 1")
	        handle Env.Unbound => err' spath)
            | get _ = impossible "ModuleUtil.lookFormalBinding 2"
      in get (env,spath,[])
       end

  (* lookGen: generic lookup function for identifiers which may occur 
     in:
	 1. environments
	 2. actual structure environments
	 3. signature parsing environments *)
    
  fun lookGen (extract,errorVal) (env,path,err) =
      (case path
       of [id] => extract ((Env.look(env,id),nil)
			  handle Env.Unbound => raise UnboundComponent path)
        | first::rest =>
	    let val STRbind(STRvar strvar) =
		  Env.look(env,first)
		  handle Env.Unbound => raise UnboundComponent [first]
	    in case strvar
	       of {binding=str,access=PATH p,...} =>
		       extract (lookBinding (str,rest, p),nil)
	        | {binding=FORMAL{pos,spec=SIG{env,kind,...}},...} =>
		  let val (binding,relpath) = lookFormalBinding(env,rest)
		  in extract (binding,case kind 
			              of EMBEDDED => relpath
				       | TOP _ => pos :: relpath)
		  end
		| {binding=ERROR_STR,...} => raise ErrorStructure
	        | {binding=FORMAL{spec=ERROR_SIG,...},...} =>
		                 raise ErrorStructure
		| _ => impossible "ModuleUtil.lookGen 1"
             end
         | _ => impossible "ModuleUtil.lookGen 2")
      handle UnboundComponent spath => 
	    let val badsym = last spath
	    in err COMPLAIN ("unbound "^symbolToName badsym^" "^
	                     Symbol.name badsym^
			     (case path
			      of _ :: _ :: _ => " in path "^formatQid path
                               | _ => ""));
	       errorVal
            end
           | ErrorStructure => errorVal
           | Bind => ErrorMsg.impossible ("bind exception: ModuleUtil.lookGen: looking up "^formatQid path^" as a "^symbolToName (last path))
           | exn => raise exn

  val bogusCONbind = CONbind bogusCON

  fun lookShortVARCON (arg as (env,name,err)) =
            Env.look(env,name)
            handle Env.Unbound => 
		 (err COMPLAIN ("unbound "^symbolToName name^" "^
				Symbol.name name);
		  bogusCONbind)
				  
  val lookVARCON = lookGen (fn (x,_) => x,bogusCONbind)

  val lookSTR = lookGen (fn (STRbind sv,_) => sv
			  | _ => impossible "ModuleUtil.lookSTR",
			 bogusSTRvar)

  fun lookTYC (arg as (_,qid,_)) =
      lookGen (fn (TYCbind (FORMtyc{pos,spec,...}),relpath) =>
		        RELtyc{name=qid,pos=(relpath,pos)}
	         | (TYCbind tyc,_)=> tyc
	         | _ => impossible "ModuleUtil.lookTYC",
	       ERRORtyc) arg

  (* tycon lookup with arity checking *)

  fun checkArity(tycon, arity,err) =
      case tycon
      of ERRORtyc => ()
       | _ =>
         if tyconArity(tycon) <> arity
         then err COMPLAIN ("type constructor "^(Symbol.name(tycName(tycon)))^
		       " has the wrong number of arguments: "^makestring arity)
         else ()

  fun lookArTYC (env,qid: symbol list, arity: int, err) =
        lookGen (fn (TYCbind (FORMtyc {pos,spec,...}),relpos) =>
		            (checkArity(spec,arity,err);
			     RELtyc{name=qid,pos=(relpos,pos)})
		  | (TYCbind tyc,_) => (checkArity(tyc,arity,err); tyc)
                  | _ => impossible "ModuleUtil.lookArTyc",
		ERRORtyc)  (env,qid,err)

  fun lookEXN (env,path,err) =
      let val binding =
	    case path
	    of [id] => (Env.look(env,id)
			handle Env.Unbound => raise UnboundComponent path)
	     | first::rest =>
		   (let val STRbind(STRvar binding) = Env.look(env,first)
	            in case binding
	                of {binding=str,access=PATH p,...} =>
		                 lookBinding (str,rest, p)
 		         | {binding=ERROR_STR,...} => raise ErrorStructure
 		         | _ => impossible "ModuleUtil.lookExn 1"
                    end
		    handle Env.Unbound => raise UnboundComponent [first])
             | _ => impossible "ModuleUtil.lookExn 2"
     in case binding
	of CONbind c =>
	     (case c
	      of DATACON {rep=VARIABLE _,...} => c
	       | DATACON {rep=VARIABLEc _,...} => c
	       | _ => (err COMPLAIN ("found data constructor \
				    \instead of exception");
		       bogusEXN))
         | VARbind _ =>
	        (err COMPLAIN ("found variable instead of exception");
		 bogusEXN)
         | _ => ErrorMsg.impossible ("ModuleUtil.lookEXN: looking up "
				      ^formatQid path^" as a "
				      ^symbolToName (last path))
     end
      handle UnboundComponent spath => 
	    (err COMPLAIN ("unbound " ^
			  (if length path=length spath then "exception "
			   else "structure ") ^
			  Symbol.name (last spath)^
			  (case path
			   of _ :: _ :: _ => " in path "^formatQid path
			    | _ => ""));
	       bogusEXN)
           | ErrorStructure => bogusEXN
           | exn => raise exn

  fun openSigStructure (env,path,newEnv,err) =
          impossible "ModuleUtil.openSigStructure: unimplemented"

  (* functions to collect stale lvars for unbinding *)
  exception NotStale

  fun openStructureVar (env,STRvar{access=PATH p,binding=str,...}) : env =
	Env.atop (makeEnv (str, p), env)
       (* will generate spurious error messages unless we give up completely *)
    | openStructureVar (env,STRvar{binding=ERROR_STR,...}) = env 
    | openStructureVar _ = impossible "ModuleUtil.openStructureVar"


  (* checkopen: takes a new environment, an lvar for a structure,
     and a structure binding for the lvar.  Tests if any value
     bound in the structure is still accessible in the new
     environment.*)

  fun checkopen newenv v =
      let fun test (s:symbol) =
	      let fun check(PATH path) =
		         if last path=v then raise NotStale else ()
                    | check _ = ()
	      in case Env.look(newenv,s)
		    of VARbind(VALvar{access=a,...}) => check a
		     | CONbind(DATACON{rep,...}) =>
			 (case rep
			  of VARIABLE a => check a
			   | VARIABLEc a => check a
			   | _ => ())
		     | STRbind(STRvar{access=a,...}) => check a
		     | _ => ()
	      end handle Env.Unbound => ()

	  and tryname (s,b) =
	        (test s;
		 case b
		 of STRbind(STRvar{binding,...}) => teststr binding
		  | _ => ())
	  and teststr (SIMPLE {env,...}) = Env.app tryname env
	    | teststr (INSTANCE{sign=SIG{env,...},subStrs,...}) =

		    (* interpret formal substructures *)

		     Env.app (fn (s,STRbind(STRvar{binding=FORMAL{pos,...},
						   ...})) =>
			         (test s; teststr (Array.sub(subStrs,pos)))
			       | other => tryname other) env
	      | teststr _ = impossible "ModuleUtil.teststr 2"
       in tryname
      end

  (* staleLvars: takes a new environment and a base environment to which
     it is to be added and returns a list of lvars that are unreachable 
     when the new environment is added to the base environment*)

  fun staleLvars(deltaEnv,baseEnv) : int list =
      let val lvarset = ref([] : int list)
	  val check = checkopen (Env.atop(deltaEnv, baseEnv))

	  (* collect: for each symbol bound in the new environment,
	     check if there was a binding in the base environment.
	     If there was a binding and the binding is not a structure,
             and it has an lvar associated with it, add it to the list
	     of stale lvars.  If it is a structure, call "check" to
	     see if values from the structure are still accessible
	     via "open" *)

	  fun collect (s,_) = 
	    let val v = case Env.look(baseEnv,s)
			 of VARbind(VALvar{access=PATH[v],...}) => v
			  | (b as STRbind(STRvar{access=PATH[v],...})) =>
				(check v (s,b); v)
			  | FCTbind(FCTvar{access=PATH[v],...}) => v
			  | CONbind(DATACON{rep=VARIABLE(PATH[v]),...}) => v
			  | CONbind(DATACON{rep=VARIABLEc(PATH[v]),...}) => v
			  | _ => raise NotStale
	     in lvarset := v :: !lvarset
	     end handle NotStale => ()
		      | Env.Unbound => ()
       in Env.app collect deltaEnv;
	  !lvarset
      end

 (* findPath:  convert symbolic path names to a printable string in the
    context of an environment.

    Its arguments are the path name in reverse order, a static semantic value,
    an equality function on static semantic values, and a lookup function
    mapping paths to their bindings (if any) in an environment.   The second
    argument of the lookup function is a function which is called if there
    is no binding for a path name in the environment.

    It looks up each suffix of the path name, going from shortest to longest
    suffix,in the current environment until it finds one whose lookup value
    equals the static semantic value argument.  It then converts that suffix
    to a string.  If it doesn't find any suffix, it returns "?" concatenated
    with the full path name.

    Example:
           Given A.B.t as a path, and a lookup function for an
	   environment, this function tries:
	             t
		     B.t
		     A.B.t
           If none of these work, it returns ?.A.B.t
    
    Note: the symbolic path is passed in reverse order because that is
    the way all symbolic path names are stored within static semantic objects.
   *)
    
 fun findPath (p,elem0,eq,look) =
    let fun try(name::untried,tried) =
	      (let val elem = look (name :: tried,fn _ => raise Env.Unbound)
               in if eq(elem0,elem)
		  then formatQid(name::tried)
		  else try(untried,name::tried)
                end handle Env.Unbound => try(untried,name::tried))
	  | try([],tried) = "?." ^ formatQid tried
     in try(p,[])
    end

 (* sortEnvBindings: sort the bindings in an environment for printing
    purposes.  The bindings are sorted in the following order:
               signatures
	       functors
	       structures
	       types
	       constructors
	       values
	       fixity declarations
   It is only correct to sort environments which have no duplicate bindings.
   All routines which build structure environments maintain this
   invariant, so it is ok to sort any structure environment using
   this function.
  *)

local
  open Symbol
   fun binderGt(bind1: symbol * Modules.binding,
		bind2: symbol * Modules.binding) =
    case (bind1,bind2)
      of ((n1,FIXbind _),(n2,FIXbind _)) => symbolGt(n1,n2)
       | ((_,FIXbind _),_) => true
       | (_,(_,FIXbind _)) => false
       | ((n1,VARbind _),(n2,VARbind _)) => symbolGt(n1,n2)
       | ((_,VARbind _),_) => true
       | (_,(_,VARbind _)) => false
       | ((n1,CONbind _),(n2,CONbind _)) => symbolGt(n1,n2)
       | ((_,CONbind _),_) => true
       | (_,(_,CONbind _)) => false
       | ((n1,TYCbind _),(n2,TYCbind _)) => symbolGt(n1,n2)
       | ((_,TYCbind _),_) => true
       | (_,(_,TYCbind _)) => false
       | ((n1,STRbind _),(n2,STRbind _)) => symbolGt(n1,n2)
       | ((_,STRbind _),_) => true
       | (_,(_,STRbind _)) => false
       | ((n1,FCTbind _),(n2,FCTbind _)) => symbolGt(n1,n2)
       | ((_,FCTbind _),_) => true
       | (_,(_,FCTbind _)) => false
       | ((n1,SIGbind _),(n2,SIGbind _)) => symbolGt(n1,n2)
in
  fun sortEnvBindings env =
       let val bl : (Symbol.symbol * Modules.binding) list ref = ref nil
       in Env.app(fn b => bl := b :: !bl) env;
	  Sort.sort binderGt (!bl)
       end
end

  (* notInitialLowerCase:  this function not currently used.  It could be
     used to detect anomalous noncapitalization of constructors. *)

  fun notInitialLowerCase string =
      (* string does NOT start with lower-case alpha *)
      let val firstchar = ordof(string,0)
       in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
       end

fun getStrPath (SIMPLE {path, ...}) = path
  | getStrPath (INSTANCE {path,...}) = path
  | getStrPath _ = []

end  (* structure ModuleUtil *)
