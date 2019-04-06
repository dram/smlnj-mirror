(* Copyright 1989 by AT&T Bell Laboratories *)
signature SIGNS = 
sig type spectype
    type signtype
    type symbol 
    type spath
    type 'a epathed
    type tyvar 
    type ty 
    type 'a enved
    type 'a uvars
    type 'a withenv
    type fixity 
    type tycon 
    type datacon
    type makeSigOption
    val MakeTOP : Modules.env * Stamps.scope -> makeSigOption
    val makeSIGid: symbol * ErrorMsg.complainer -> signtype
    val makeSIG: spectype * ErrorMsg.complainer -> signtype
    val make_includespec: symbol * ErrorMsg.complainer -> spectype
    val make_openspec: spath list * ErrorMsg.complainer -> spectype
    val make_strspec: symbol * (makeSigOption -> Modules.Signature) * ErrorMsg.complainer -> spectype
    val make_dtyspec: (symbol * int * datacon list withenv epathed) list * ErrorMsg.complainer
	               -> spectype
    val make_tyspec: Types.eqprop * tyvar list * symbol * ErrorMsg.complainer
	             -> spectype
    val make_valspec:	symbol * ty enved uvars * ErrorMsg.complainer -> spectype
    val make_exnspec:	symbol * ErrorMsg.complainer -> spectype
    val make_exnspecOF:	symbol * ty enved uvars * ErrorMsg.complainer -> spectype
    val make_fixityspec: fixity * symbol list * ErrorMsg.complainer -> spectype
    val make_type_sharespec: spath list * ErrorMsg.complainer -> spectype
    val make_str_sharespec: spath list * ErrorMsg.complainer -> spectype
end

structure Signs : SIGNS = struct

  open Types Access Modules Symbol Variables Fixity
  open ErrorMsg PrintUtil EqTypes
  open BasicTypes TypesUtil Absyn
  open ModuleUtil Misc TyvarSet

(* type context:
       strs: the number of structures declared in the signature
       tycons: the number of types declared in the signature,
       slots: the number of slots for values in the structure record
       inner: the current local environment
       total: the complete environment
       enclosing: a stack of enclosing (outer environments) for the
                 current inner environment
       names: a stack of the names of the enclosing structures.
              There is no name for the outermost environment
	      enclosing the declaration of a top-level signature.
       s: a list of structure sharing constraints.  All sharing
          contraints for embedded signatures are moved to the
	  top-level signatures enclosing them.   When this is done,
	  symbolic constraints are normalized relative to the
	  enclosing top-level signature.
       t: a list of type sharing constraints, 

invariants: |names| = |enclosing|-1
*)
  type context = {strs:int ref,tycons:int ref,slots:int ref,
		  inner: env ref, total: env ref, enclosing: env list,
		  names: symbol list,makeStamp : unit -> Stamps.stamp,
		  s:{internal:spath list,external:Structure option} list ref,
		  t:{internal:spath list,external:Types.tycon option} list ref}

datatype makeSigOption = MakeTOP of Modules.env  * Stamps.scope
                       | MakeEMBED of (context * Symbol.symbol)

type signtype = makeSigOption -> Signature
type 'a uvars = 'a * tyvarset
type 'a epathed = Modules.env * spath -> 'a
type 'a enved = Modules.env -> 'a
type 'a withenv = 'a * Modules.env
type spectype = context -> symbol list 
		    
fun next r = (!r) before (inc r)

fun bind (sym,binding,{inner,total,...} : context, err) =
       (Env.look (!inner,sym);
        err COMPLAIN ("duplicate specifications for "
		      ^Symbol.nameSpaceToString(Symbol.nameSpace sym)
		      ^" "^Symbol.name sym^" in signature"))
          handle Env.Unbound =>
	     (inner := Env.bind(sym,binding,!inner);
	      total := Env.bind(sym,binding,!total))

val addExistingConstraints = fn ({t,s,names,...}: context,
                                 {tConstraints=t',sConstraints=s'}) =>
    let val names' = rev names
        val prefix = fn {internal: spath list,external: 'a} =>
	      {internal = map (fn qid => names' @ qid) internal,
	       external=external}
    in t := ((map prefix t') @ !t);
       s := ((map prefix s') @ !s)
    end

fun makeSIGid (ID,err) sigoption =
   let val env = case sigoption
		 of MakeTOP(env,_) => env
		 | MakeEMBED ({total,...} : context,_) => !total
   in lookSIG (env,ID,err)
   end

 fun makeSIG (specs,err) (MakeEMBED (context as {strs,tycons,slots,makeStamp,
				     inner,total,enclosing,names,t,s},name)) =
      let val context' = ({strs=strs,
			  tycons=tycons,
			  slots=ref 0,
			  makeStamp = makeStamp,
			  inner = ref Env.empty,
			  total = ref (!total),
			  enclosing = !inner :: enclosing,
			  names = name :: names,
			  t=t,s=s} : context)
          val symbols = specs context'
      in SIG {symbols=symbols,env= !(#inner context'),kind=EMBEDDED,
	      stamp=makeStamp(),path=NONE}  
      end
  | makeSIG (specs, err) (MakeTOP(env,scope)) =
    let val makeStamp = Stamps.newGenStamp scope
	val context = ({strs=ref 0,
                        tycons=ref 0,
                        slots=ref 0,
			makeStamp = makeStamp,
                        inner = ref Env.empty,
		        total = ref env,
		        enclosing = [env],
		        names = [],
		        t = ref [],
		        s = ref []} : context)
        val symbols = specs context
	val r = SIG {symbols = symbols,
		    env = !(#inner context),
		    path=NONE,
		    stamp=makeStamp(),
		    kind = TOP{strcount = !(#strs context),
			       typecount = !(#tycons context),
			       slotcount = !(#slots context),
			       sConstraints = !(#s context),
			       tConstraints = !(#t context)}}
    in if !System.Control.instSigs 
       then (Instantiate.instantiate(r,[],Stamps.newBoundScope(),err); ())
       else ();
       r
    end

local 

   (* adjustBinding: adjust all the slots and relative positions
      of bindings in an included SIGNATURE.*)

   fun adjustBinding (basetype,basestr,baseslot,makeStamp) =
     let fun adjust baseslot binding =
	  case binding
	  of VARbind v =>
	         VARbind (case v
			 of VALvar {access=SLOT i,typ,name} =>
			      VALvar {access=SLOT (i+baseslot),
				      typ=ref(adjustType (!typ)),name=name}
		          | ERRORvar => ERRORvar
			  | _ => impossible "Sign.adjustbinding/VARbind")
          | CONbind (DATACON{name,const,typ,rep,sign}) =>
	      CONbind (DATACON{name=name,const=const,typ=adjustType typ,
			       rep=case rep
			           of VARIABLE(SLOT i) =>
				             VARIABLE(SLOT(i+baseslot))
				    | VARIABLEc(SLOT i) =>
					     VARIABLEc(SLOT(i+baseslot))
				    | _ => rep,sign=sign})
          | TYCbind tyc => TYCbind (adjustTycon tyc)
	  | STRbind(STRvar {name,access=SLOT k,binding=FORMAL{pos,spec}}) =>
	       let val binding =
		     FORMAL{pos=pos+basestr,spec=
			    case spec
			    of SIG {symbols,env,kind=EMBEDDED,...} =>
				   SIG{symbols=symbols,
				       path=NONE,
				       stamp=makeStamp(),
				       env=Env.map (adjust 0) env,
				       kind=EMBEDDED}
			     | _ => spec}
                in STRbind(STRvar{name=name,access=SLOT (k+baseslot),
				  binding=binding})
		end
          | STRbind(STRvar {binding=ERROR_STR,...}) => binding
          | FIXbind _ => binding
          | _ => impossible "adustbinding"
        and adjustType ty =
	    case ty
	    of (CONty (tycon,tylist)) =>
		    CONty(adjustTycon tycon,map adjustType tylist)
             | (POLYty{sign,tyfun=TYFUN{arity,body},abs}) =>
	           POLYty{sign=sign,
			  tyfun=TYFUN{arity=arity,body=adjustType body},
		          abs=abs}
             | ty => ty
       and adjustTycon tycon =
	   case tycon
	   of FORMtyc{pos,spec} =>
              FORMtyc{pos=pos+basetype,
		      spec=
		 case spec
		 of GENtyc{stamp,arity,eq,path,kind} =>
		      GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
			   kind=
			   case !kind
			   of FORMtyck => kind
			    | DATAtyc dl =>
			       ref (DATAtyc
			         (map (fn DATACON {name,const,typ,rep,sign} =>
				       (DATACON{name=name,const=const,
					      typ=adjustType typ,
					      rep=rep,sign=sign})) dl))}}
	    | RELtyc {name,pos=([],offset)} =>
		        RELtyc {name=name,pos=([],offset+basetype)}
            | RELtyc {name,pos=(h::t,offset)} =>
		       RELtyc {name=name,pos=((h+basetype)::t,offset)}
	    | tyc => tyc
   in adjust baseslot
   end
in
fun make_includespec (ID,err) (context as {strs,tycons,slots,total,makeStamp,...}) =
  case lookSIG (!total,ID,err)
  of SIG{symbols,kind=TOP {sConstraints=s,tConstraints=t,
			    strcount,typecount,slotcount},env,...} =>
    let val adjust = adjustBinding (!tycons,!strs,!slots,makeStamp)
    in app (fn name => bind(name,adjust(Env.look(env,name)),context,err))
	    symbols;
       addExistingConstraints(context,{sConstraints=s,tConstraints=t});
       strs := strcount + !strs;
       tycons := typecount + !tycons;
       slots := slotcount + !slots;
       symbols
    end
  | ERROR_SIG => nil
  | _ => impossible "make_includespec"
end
         
fun make_openspec (strpaths: spath list, err) 
                  (context as {inner,total=total as ref env,...}: context) =
     (err COMPLAIN "open in signatures is not supported";
(*    app (fn p => total := openSigStructure (env,p,!total,err)) strpaths; *)
      nil) (* no bindings returned *)

fun make_strspec (name,sign,err) (context as {strs,slots,inner,...}) =
    let val sgn = sign (MakeEMBED (context, name))
	val binding = STRbind (STRvar{name=name,access=SLOT (next slots),
				      binding=FORMAL{pos=next strs,spec=sgn}})
     in bind(name,binding,context,err);
        [name]
    end

fun make_dtyspec (db,err) (context as {tycons,total,...}:context) =
    let fun predefine(id,arity,parse_rhs) = 
	      let val r = ref(DATAtyc nil) 
		  val spec = GENtyc{path=[id],arity=arity,
				    stamp=Stamps.null,
				    eq=ref DATA,kind=r}
		  val pos = next tycons
		  val binding = TYCbind (FORMtyc{pos=pos,spec=spec})
	      in bind(id,binding,context,err);
		 (id,r,parse_rhs)
	      end

        (* add each constructor to the environment, checking whether env'
	   contains constructors whose names were used before in a val
	   spec or datatype spec *)
 
	fun redefine(name,r,parse_rhs) = 
	      let val (r',env') = parse_rhs(!total,[])
	      in r := DATAtyc(r');
                 Env.app (fn (name,binding) =>
			      bind(name,binding,context,err)) env';
		 name
	      end
 
	val pre = map predefine db
	val tycbinds = map redefine pre

        (* get the name for a constructor *)

        val findcon =  fn DATACON{name,...} => name
 
        (* find all the names for the constructors of a datatype *)

	val findvals = fn (_,ref(DATAtyc conlist),_) => map findcon conlist

        (* find the names for all the constructors in all the datatypes *)

        val alldconvals = fold (op @) (map findvals pre) nil
     in tycbinds @ alldconvals
    end

fun make_tyspec(eq,tyvars,name,err) 
                   (context as {tycons,...}:context) =
    let val _ = checkbound(no_tyvars,tyvars,err)
	val pos = next tycons
	val spec = GENtyc{stamp = Stamps.null,
			  path = [name], arity = length tyvars,
			  eq = ref eq, kind = ref FORMtyck}
	val binding = TYCbind(FORMtyc{pos=pos,spec=spec})
    in bind(name,binding,context,err);
       [name]
    end

fun make_valspec(name,(ty,tv),err) (context as {slots,total,...}:context) =
    let val body = ty (!total)
	val typ = case get_tyvars tv
		   of [] => body
		    | tvs => let val sign = TypesUtil.bindTyvars1 tvs
			      in POLYty{sign = sign, abs=0,
					tyfun = TYFUN{arity = length tvs, 
						      body = body}}
			     end
	val _ = TypesUtil.compressTy typ
	val binding = 
	    VARbind(VALvar{name=[name],typ= ref typ,access=SLOT(next slots)})
     in bind(name,binding,context,err);
	[name]
    end

fun make_exnspec (name,err) (context as {slots,...}:context) =
  let val binding = CONbind(DATACON{name=name,const=true,typ=exnTy,sign=[],
				    rep=VARIABLEc(SLOT(next slots))})
  in bind(name,binding,context,err);
     [name]
  end

fun make_exnspecOF(name,(ty,tv),err) (context as {slots,total,...}:context) =
  let val body = ty (!total)
      val tvs = get_tyvars tv
      val typ = case length tvs
		 of 0 => body --> exnTy
		  | n => (TypesUtil.bindTyvars tvs;
			  POLYty{sign = mkPolySign n, abs=0,
			         tyfun = TYFUN{arity = n,
				               body = body --> exnTy}})
      val _ = TypesUtil.compressTy typ
      val binding = CONbind(DATACON{name=name, const=false, typ= typ,sign=[],
				      rep=VARIABLE(SLOT(next slots))})
  in bind(name,binding,context,err);
     [name]
  end

fun make_fixityspec(fixity,ops,err) (context : context) =
    let fun f i = 
	    let val binding = FIXbind(FIXvar{name=i,binding=fixity})
	     in bind(i,binding,context,err);
		i
	    end
     in map f ops 
    end  

local

 datatype 'a Constraint = GLOBAL of 'a | LOCAL of spath
 fun normalizeConstraint 

      (look : Modules.env * Modules.spath * ErrorMsg.complainer -> 'a,
       eq : 'a * 'a -> bool,
       deferr : ErrorMsg.complainer -> Modules.spath * Modules.spath -> unit)

      (qids : Modules.spath list,
       names : Symbol.symbol list,
       envs : Modules.env list,
       totalenv : Modules.env,
       err : ErrorMsg.complainer) =

    let 

    (* find: find the qualified identifier in an enclosing environment
          Invariant for find: |name list| = |env|-2; the global
	  environment and the outermost environment for the signature
	  don't have names associated with them.*)

    fun find (qid,nil,[localenv,topenv]) =
	     ((look (localenv,qid,fn _ => raise Env.Unbound);
	       LOCAL qid)
	      handle Env.Unbound=> GLOBAL(look(topenv,qid,err)))
      | find (qid,names as (name::outernames),env::outerenvs) =
	      ((look (env,qid,fn _ => raise Env.Unbound);
		LOCAL (rev names @ qid))
		handle Env.Unbound => find(qid,outernames,outerenvs))
      | find _ = impossible "normalizeConstraint: find"

    (* scan: scan a list of qualified identifiers, dividing them into local
       and definitional constraints.  Keep only one copy of the definitional
       constraints, since they've all got to be the same anyway.*)

    fun scan(nil,internal,definitional,_) =
	          {internal=internal,external=definitional}
      | scan (qid::r,internal,definitional,defqid) =

             (* look up the qualified identifer in the total environment
	        to make sure it exists and hasn't been hidden.*)

	  let val errflag = ref false
	      val _ = look (totalenv,qid,fn a => (errflag := true;err a))
	  in if !errflag then scan(r,internal,definitional,qid)
	     else case find(qid,names,envs)
	          of GLOBAL a => 
		    (case definitional
		     of NONE => scan(r,internal,SOME a,qid)
		      | SOME existing =>
			  (if eq(a,existing) then ()
			   else deferr err (defqid,qid);
			   scan(r,internal,definitional,defqid)))
		   | LOCAL l => scan(r,l::internal,definitional,defqid)
	  end
   in scan(qids,[],NONE,[])
   end

   val normalizeStrConstraint =
      let val lookSTR' = fn arg =>
	   (fn STRvar{binding=b1,...} => b1) (lookSTR arg)
      in normalizeConstraint (lookSTR',eqOrigin,
	                 fn err => fn (qid1,qid2) =>
			     (err COMPLAIN 
			       ("definitional sharing constraint " ^
				 formatQid qid1 ^ " = " ^ formatQid qid2 ^
				 " can never be satisfied")))
       end

   val normalizeTypeConstraint = 
       normalizeConstraint (lookTYC,equalTycon,
			fn err => fn (qid1,qid2) =>
			 (err COMPLAIN 
			      ("definitional type sharing constraint " ^
			       formatQid qid1 ^ " = " ^ formatQid qid2 ^
			       " can never be satisfied")))
    val addStrConstraint =
	fn ({names,enclosing,inner=ref e,s,total,...} : context,qids,err) =>
          let val constraints=
		 normalizeStrConstraint (qids,names,e::enclosing,!total,err)
	  in s := constraints :: !s
	  end


   val addTypeConstraint =
       fn ({names,enclosing,inner=ref e,t,total,...} : context,qids,err) =>
          let val constraints =
		 normalizeTypeConstraint (qids,names,e::enclosing,!total,err)
	  in t := (constraints :: !t)
	  end
in
   fun make_type_sharespec (patheqn,err) context =
      (addTypeConstraint(context,patheqn,err); nil)

   fun make_str_sharespec (patheqn,err) context =
      (addStrConstraint(context,patheqn,err); nil)
end (* local ... datatype 'a Constraint *)

end (* structure Signs *)
