(* Copyright 1991 by AT&T Bell Laboratories *)
(* instantiate.sml *)

(* This function constructs a dummy structure which satisfies all sharing
 * constraints (explicit or induced) of a given signature.  The resulting
 * structure is used as the dummy parameter of a functor while parsing,
 * type-checking, and abstracting the functor body.
 *
 * The process of constructing the structure is essentially a unification
 * problem.  The algorithm used here is based on the Linear Unification
 * algorithm first presented in [1] which was subsequently corrected
 * and cleaned up in [2].
 *
 * This code was originally designed by Damien Doligez, Georgez Gothier, 
 * and Dave MacQueen.  Greg Morrisett, Dave MacQueen, and David Tarditi
 * modified the code to work with the current implementation of the
 * SML/NJ module system.
 *
 * The basic algorithm makes 2 passes.  The first pass builds a DAG in
 * a quasi-top down fashion which corresponds to the minimal structure 
 * needed to match the signature.  The second pass takes the DAG and
 * constructs the actualy dummy structure in a bottom-up fashion.
 * Pass 1 has a fairly complicated control structure.  The major 
 * invariant is that no node in the graph is expanded unless all 
 * of its ancestors have been expanded.  This insures that all sharing
 * constraints (explicit or implicit) have reached the node at the
 * time of its expansion.  The second major invariant is that no
 * node is finalized until all members in its equivalence class have
 * been found.
 *
 * [1] Paterson, M.S., and Wegman, M.N., "Linear Unification", 
 *     J. Comp. Sys. Sci. 16,2 (April 1978), pp. 158-167.
 *
 * [2] de Champeaux, D., "About the Paterson-Wegman Linear Unification
 *     Algorithm", J. of Comp. Sys. Sci. 32, 1986, pp. 79-88.
 *)

structure Instantiate :
    sig
	val instantiate : 
	    (Modules.Signature * Modules.spath * Stamps.scope *
	     (ErrorMsg.severity -> string -> unit)) -> Modules.Structure
    end (* sig *) 

= struct
open Symbol Modules Types ModuleUtil ErrorMsg TypesUtil PrintBasics
     System.Control PrintType Access Stamps PrintUtil

open Array List
infix 9 sub

val DEBUG = false
fun tprint (s:string) = if DEBUG andalso !internals then (print s) else ()
fun tprintSym (s:Symbol.symbol) = 
    if DEBUG andalso !internals then (printSym s) else ()
fun print_tycon tyc = printTycon std_out Env.empty tyc
val error_found = ref false

(* This datatype represents the continually changing DAG that is being 
 * constructed by instantiate.  We start off with just an Initial node.  
 * It is expanded into a Partial node whose children (subs and typs) are 
 * initialized to Initial nodes.  When all of the members of the nodes
 * equivalence class have been found, and converted to Partial nodes, 
 * the node is converted to either Final_top or Final_embed depending 
 * on its signature kind.  Finally, we recurse on the children of the
 * node.  
 *
 * Invariants:
 *
 *    The parent node is in a singleton equivalence class.
 *
 *    All nodes that are about to be explored are either Initial or Partial.
 *    (Exploring a Final node implies circularity.)
 *
 *    If a Final node's expanded field is true, then all of its children
 *    are Final with expanded field true.
 *
 *    The node (subs sub i) of a structure corresponds to the sub-structure
 *    (actual_subStrs sub i) to be built.  Similarly for (typs sub i) and 
 *    (actual_types sub i)
 *)
datatype instance = Final_top of { sign : Signature,
				   actual_subStrs : Structure array,
				   actual_types : Types.tycon array,
				   origin_info : instance_origin ref,
                                   subs : instance array,
                                   typs : type_ins array,
				   final_struct: Structure option ref,
				   expanded : bool ref}
                  | Final_embed of { sign : Signature,
                                     path : Symbol.symbol list,
                                     origin_info : instance_origin ref,
                                     subs : instance array,
                                     typs : type_ins array,
				     expanded : bool ref}
                  | Partial of { signat : Signature,
                                 path : Symbol.symbol list,
                                 subs : instance array,
                                 typs : type_ins array,
                                 depth : int,
				 final_rep : instance option ref}
                  | Initial of { signat : Signature,
                                 path : Symbol.symbol list,
                                 parent_typs : type_ins array,
                                 inherited : constraint list ref }
                  | NULLinstance
                  | ERRORinstance

(* These are the similar nodes for types. *)
and type_ins = tFinal of tycon
             | tPartial of { tycon : tycon,
                             path : Symbol.symbol list }
             | tInitial of { tycon : tycon,
                             path : Symbol.symbol list,
                             inherited : constraint list ref }
             | tNULLinstance
             | tERRORinstance

(* Some of the nodes of the structure are not actually constructed
 * since they were previously defined structures or types.  These
 * are labelled with EXTstr and EXTtyc respectively.  The pointers
 * to the nodes of the DAG are destructively updated, 
 * so they are represented using "slots" of an array.
 *)
and instrep = EXTstr of Structure
            | EXTtyc of tycon
            | SLOTstr of instance slot
            | SLOTtyc of type_ins slot

(* In the final versions of instances, we need to be able to build an
 * origin structure which has the "union" of all components of structures
 * which it is equivalent too.  If a Final_top or Final_embed instance
 * has origin_info set to BUILT, then the origin field of the structure
 * field points to the origin structure.  If the origin_info is set to
 * NEED_SIMPLE (env), then a SIMPLE origin structure must be constructed 
 * from the instrep env to be shared among all members of an equivalence
 * class of structures as their origin.  If the origin_info is set to 
 * NEED_SELF then the structure is in a singleton equivalence
 * class and we may use the structure as its own origin.  
 *)
and instance_origin = 
    BUILT of Structure | 
    NEED_SELF |
    NEED_SIMPLE of (instrep Env.env)

(* A constraint is essentially a directed arc indicating that two
 * nodes are to be identified.  The constraint is always interpreted
 * relative to an instance node.  The my_path field is a symbolic
 * path indicating which subcomponent of the instance is participating
 * in the sharing.  The other component is accessed by first finding
 * the instance node in the its_ancestor slot, and then following
 * the symbolic path its_path to the node.  By going through the
 * ancestor, we are able to insure that the ancestor is explored
 * before the actual component is.
 *)
withtype constraint = { my_path : Symbol.symbol list,
		        its_ancestor : instrep,
			its_path : Symbol.symbol list}

and 'a slot = { base : 'a array, offset : int }
fun get {base, offset} = base sub offset
fun set ({base, offset}, inst) = update (base, offset, inst)
fun push r x = (r := x::(!r))

(*********************)
(* Utility Functions *)
(*********************)	    
(* Retrieves all [formal] substructure components from a signature *)
fun getSubSigs (sign: Modules.Signature) : 
    (Symbol.symbol * Modules.strpos * Modules.Signature) list =
    case sign of
	SIG {symbols,env,...} =>
	let fun check_binding (s, prevSubSigs) =
	    case (nameSpace s) of
		STRspace => (case Env.look (env,s) of
				 STRbind 
				(STRvar {binding=FORMAL {pos,spec,...},...}) =>
				     (s,pos,spec) :: prevSubSigs
			       | _ => impossible 
				     "Modules.instantiate:getSubSigs")
	      | _ => prevSubSigs
	in
	    fold check_binding symbols []
	end
      | _ => []

(* Retrieves all [formal] type components from a signature *)
fun getTypes (sign: Modules.Signature) : 
    (Symbol.symbol * Modules.strpos * Types.tycon) list =
    case sign of
	SIG {symbols,env,...} =>
	let fun check_binding (s, prevTypes) =
	    case (nameSpace s) of
		TYCspace => (case Env.look(env,s) of
				 TYCbind (tyc as (FORMtyc {pos,spec})) =>
				      (s,pos,tyc) :: prevTypes
			       | _ => impossible 
				     "Modules.instantiate:getTypes")
	      | _ => prevTypes
	in
	    fold check_binding symbols []
	end
      | _ => []
 
fun pathName [sym] = Symbol.name sym
  | pathName (sym::rest) = (Symbol.name sym)^"."^(pathName rest)
  | pathName [] = "?"

fun strName (SIMPLE {path,...}) = pathName path
  | strName (INSTANCE {path,...}) = pathName path
  | strName _ = "?"

(* Creates instrep nodes for all of the substructures of a
 * presumably pre-defined (i.e. external) structure.
 *)
fun getStrSlots (str : Structure) : (symbol * instrep) list =
    let fun f (sym, str) = (sym, EXTstr (getOrigin str))
    in map f (getSubStrs str)
    end

fun getSubTyps str =
    let val r = ref nil
	val (env, f) = 
	    case str
		of (SIMPLE{env,...}) =>
		    (env,(fn (sym,TYCbind tyc) => (r := (sym,tyc) :: !r)
		           | _ => ()))
		  | (INSTANCE{sign=SIG{env,...},types,...}) =>
			(env,(fn (sym,TYCbind (FORMtyc{pos,...})) => 
			      r := (sym,Array.sub(types,pos)) :: !r
		               | _ => ()))
		  | (INSTANCE{sign=ERROR_SIG,...}) => 
			(error_found := true; (Env.empty,fn _ =>()))
		  | ERROR_STR => (error_found := true; (Env.empty,fn _ => ()))
		  | _ => impossible "Modules.Instantiate:getSubTyps"
    in Env.app f env; !r
    end
		           
fun getStrTSlots (str : Structure) : (symbol * instrep) list =
    let fun f (sym, tyc) = (sym, EXTtyc tyc)
    in map f (getSubTyps str)
    end

(* Finds all sub-signatures in a signature, and updates the corresponding
 * slots in the subs array so that they are Initial instances.
 *)
fun getSigSlots (sign, path, subs, typs) =
    let fun newInstance (signat, path) =
              Initial {signat=signat, path=path, parent_typs=typs,
                       inherited=ref []}
        fun f (name, pos, signat) =
             let val sl = {base=subs, offset=pos}
             in set (sl, newInstance (signat, name::path));
                (name, SLOTstr sl)
             end
    in map f (getSubSigs sign)
    end

(* Finds all types in a signature, and updates the corresponding slots
 * in the typs array so that they are tInitial instances.
 *)
fun getSigTSlots (sign, path, typs) =
    let fun newInstance (tycon, path) =
	(tprint "<tNULLinstance converted to tInitial>\n";
	 tInitial {tycon=tycon, path=path, inherited=ref []})
        fun f (name, pos, tycon) =
             let val sl = {base=typs, offset=pos}
             in set (sl, newInstance (tycon, name::path));
                (name, SLOTtyc sl)
             end
    in map f (getTypes sign)
    end

fun getStrPos str (sym: Symbol.symbol) = 
    case lookBindingSTR(str,[sym]) of
	(STRvar {binding=(FORMAL {pos,...}),...}) => pos
      | _ => impossible "Modules.Instantiate:getStrPos"

fun getStrTPos str sym =
    case lookBindingTYC (str,[sym]) of
	(FORMtyc {pos, ...}) => pos
      | _ => impossible "Modules.Instantiate:getStrTPos"

fun getSigPosGen (SIG {env, ...}) sym = Env.look(env,sym)
  | getSigPosGen _ _ = impossible "Modules.Instatiate:getSigPos"

fun getSigTPos (SIG {env, ...}) sym =
    (case Env.look(env,sym) of
	 TYCbind (FORMtyc {pos, ...}) => pos
       | _ => impossible "Modules.Instantiate:getSigTPos.1")
  | getSigTPos _ _ = impossible "Modules.Instantiate:getSigPos.2"

fun getSigPos (SIG {env, ...}) sym =
    (case Env.look(env,sym) of
	 (STRbind (STRvar {binding=FORMAL {pos, ...},...})) => pos
       | _ => impossible "Modules.Instantiate:getSigPos.1")
  | getSigPos _ _ = impossible "Modules.Instantiate:getSigPos.2"

fun eqSignature (SIG {stamp=s1,...}, SIG {stamp=s2,...}) = s1=s2
  | eqSignature _ = false


(***********************************************************************
 * This function merges a node into a class.  It does so by looking    *
 * up the elements the node requires in the "union" environment for    *
 * the class.  If a binding does not exist, then one is added to the   *
 * union.  If a binding does exist to some other node for the symbol,  *
 * then constraints are added to both nodes indicating that they are   *
 * equivalent.                                                         *
 ***********************************************************************)
fun merge (union, defined, err, path) (sym, rep) =
    (case (Env.look (!union,sym), rep)
     of (extrep as EXTstr _, SLOTstr sl)
        => (case get sl
            of Initial {inherited, ...}
                 => (push inherited 
		       {my_path=[], its_ancestor=extrep, its_path=[]};
		     union := Env.bind (sym,rep,!union))
	     | ERRORinstance => ()
             | _ => impossible "Modules.Instantiate:merge.1")
      | (SLOTstr sl, extrep as EXTstr _)
        => (case get sl
            of Initial {inherited, ...}
                 => push inherited 
		       {my_path=[], its_ancestor=extrep, its_path=[]}
             | ERRORinstance => (error_found := true)
             | _ => impossible "Modules.Instantiate:merge.2")
      | (SLOTstr sl1, SLOTstr sl2)
        => (case (get sl1, get sl2)
            of (Initial {inherited=inherited1, ...},
                Initial {inherited=inherited2, ...})
                 => (push inherited1 
                       {my_path=[], its_ancestor=rep, its_path=[]};
                     push inherited2 
		       {my_path=[], its_ancestor=SLOTstr sl1, its_path=[]})
	     | (ERRORinstance,_) => ()
	     | (_,ERRORinstance) => ()
             | _ => impossible "Modules.Instantiate:merge.3")
      | (extrep as EXTtyc _, SLOTtyc sl)
        => (case get sl
            of tInitial {inherited, ...}
                 => (push inherited 
		       {my_path=[], its_ancestor=extrep, its_path=[]};
                     union := Env.bind (sym,rep,!union))
	     | tERRORinstance => ()
             | _ => impossible "Modules.Instantiate:merge.4")
      | (SLOTtyc sl, extrep as EXTtyc _)
        => (case get sl
            of tInitial {inherited, ...}
                 => push inherited 
		       {my_path=[], its_ancestor=extrep, its_path=[]}
             | tERRORinstance => ()
             | _ => impossible "Modules.Instantiate:merge.5")
      | (SLOTtyc sl1, SLOTtyc sl2)
        => (case (get sl1, get sl2)
            of (tInitial {inherited=inherited1, ...},
                tInitial {inherited=inherited2, ...})
                 => (push inherited1 
		       {my_path=[], its_ancestor=rep, its_path=[]};
                     push inherited2 
		       {my_path=[], its_ancestor=SLOTtyc sl1, its_path=[]})
             | (tERRORinstance,_) => ()
             | (_,tERRORinstance) => ()
             | _ => impossible "Modules.Instantiate:merge.6")
      | _ => impossible "Modules.Instantiate:merge.7")
     handle Env.Unbound
       => case defined
          of SOME str
               => err WARN 
		   ("This signature cannot be matched : sharing "
                            ^(pathName path)^" = "^(strName str)
                            ^" : "^(strName str)^" has no "
                            ^(Symbol.name sym)^" component.")
           | NONE => union := Env.bind (sym,rep,!union)

(*************************************************************************
 * This function just checks to make sure that each of the bindings      *
 * that a class requires actually exits in the origin of some [external] *
 * structure.                                                            *
 *************************************************************************)
fun check_def (union,str,err,path) =
      let val env = case (getOrigin str) 
		      of (SIMPLE{env,...}) => env
		       | (INSTANCE{sign=SIG{env,...},...}) => env
		       | ERROR_STR => (error_found := true; Env.empty)
		       | _ => impossible "Modules.instantiate:check_def"
	  val path' = getStrPath str
	  fun check (sym, binding) =
	    (Env.look (env,sym); ())
	      handle Env.Unbound =>
		  err WARN 
                    ("This signature cannot be matched : sharing "
		     ^(pathName path')^" = "^(pathName path)^" : "
		     ^(pathName path')^" is also sharing with a structure with a "
		     ^(Symbol.name sym)^" component, and "^(pathName path)
		    ^" has no "^(Symbol.name sym)^" component.")
  in
      Env.app check (!union)
  end

(**************************************************************************
 * This function distributes the structure sharing constraints that a     *
 * signature has to the children of a corresponding node.  Note that this *
 * only deals with the explicit constraints.  Implied and inherited       *
 * constraints are propogated by merge and the constrain functions of     *
 * explore_class and explore_tclass.                                      *
 **************************************************************************)
fun distributeS (sign as SIG {kind=TOP{sConstraints,...},...}, subs, err) =
    let exception DistributeS
	fun f (sym::path) =
            let val pos = (getSigPos sign sym)
            in case subs sub pos
               of Initial {inherited, ...}
                    => (path, inherited, SLOTstr {base=subs, offset=pos})
		  | ERRORinstance => raise DistributeS
                  | _ => impossible "Modules.distributeS:f DD134"
            end
          | f [] = impossible "Modules.distributeS:f DD148"
	fun dist {internal=(p::rest),external} = 
	    let val (p1, h1, ir1) = f p
		fun g (p2, h2, ir2) =
		    (push h1 {my_path=p1, its_path=p2, its_ancestor=ir2};
		     push h2 {my_path=p2, its_path=p1, its_ancestor=ir1})
	    in
		app (fn p' => g (f p')) rest;
		case external of
		    NONE => ()
		  | SOME str => push h1 
			{my_path=p1,its_ancestor=EXTstr (getOrigin str),
			 its_path=[]}
	    end
	  | dist {internal=[],...} = ()
    in (app dist sConstraints) handle DistributeS => ()
    end
  (* don't have any sharing constraints to *)
  (* distribute if we don't have a TOP sig *)
  | distributeS _ = () 

(***************************************************************************
 * This function distributes the type sharing constraints that a signature *
 * has to the children of the corresponding node.                          *
 ***************************************************************************)
fun distributeT (sign as SIG {kind=TOP{tConstraints, ...},...},subs,typs,err) =
    let exception DistributeT
	fun f [sym] =
              let val pos = getSigTPos sign sym
              in case typs sub pos
                 of tInitial {inherited, ...}
                      => ([], inherited, SLOTtyc {base=typs, offset=pos})
		 | tERRORinstance => raise DistributeT
                 | _ => impossible "Modules.distributeT:f DD144"
              end
          | f (sym::path) =
              let val pos = (getSigPos sign sym)
            in case subs sub pos
               of Initial {inherited, ...}
                    => (path, inherited, SLOTstr {base=subs, offset=pos})
                  | _ => impossible "Modules.distributeT:f DD146"
            end
          | f [] = impossible "Modules.distributeT:f DD147"
	fun dist {internal=p::rest,external} = 
	    let val (p1,h1,ir1) = f p
		fun g (p2, h2, ir2) =
		    (push h1 {my_path=p1, its_path=p2, its_ancestor=ir2};
		     push h2 {my_path=p2, its_path=p1, its_ancestor=ir1})
	    in
		app (fn p' => g (f p')) rest;
		case external of
		    NONE => ()
		  | SOME tyc => push h1 {my_path=p1,its_ancestor=EXTtyc tyc,its_path=[]}
	    end
	  | dist {internal=[],...} = ()
    in (app dist tConstraints) handle DistributeT => ()
    end
  (* don't have any sharing constraints to *)
  (* distribute if we don't have a TOP sig *)
  | distributeT _ = () 

exception ExploreInst of Symbol.symbol list

(***************************************************************************
 * This is the main function of the algorithm.  Given an Initial node, 
 * it creates subs and typs arrays and converts the node to a Partial
 * node.  This step is skipped if the node has been explored previously
 * and is already Partial.  Then, we find all of the child nodes of
 * the node, create Initial instances of them, apply merge to them
 * resulting in a new union_components enviornment.  We then distribute
 * all of the sharing constraints to the new children and apply
 * the constrain function to our list of inherited constraints.  Constrain
 * in turn uses the constraints to track down other nodes that should
 * be placed in the same equivalence class.  If the ancestors of such
 * a node have not been explored yet, then they are explored first.
 * Once constrain is complete, class holds a list of instance nodes
 * (all Partial) that are equivalent and union_components holds an 
 * enviornment that maps the "union" of all components in the class
 * to instreps.  A class_origin is [lazily] built.  (The origin will
 * be available iff there is an external sharing constraint.)  Finally,
 * we apply the function finalize to each member of the class which
 * converts each of the Partial nodes to either Final_top or Final_embed
 * nodes.  
 *
 * This has been slightly modified so that if two slots in the class
 * have nodes that share the same signature, then the slots are made
 * to point to only one of the nodes.  Of course, the sharing constraints
 * for both must be propogated to the descendants.  
 ***************************************************************************)
fun explore_class (first_slot : instance slot, 
		   class_depth: int, 
		   err: severity -> string -> unit) : unit =
    let 
	val union_components = ref (Env.empty : instrep Env.env)
        val class = ref ([] : instance slot list)
        val class_def = ref (NONE : Structure option)
	exception Error_Sig
	(* Convert the node from Initial to Partial.  Merge its components
	 * into the union.  Push down any sharing constraints it has in 
         * its signature.  Then apply constrain to each of the inherited
         * constraints.
         *)
        fun explore_inst (sl: instance slot) : unit =
            (case (get sl) of
		 ERRORinstance => ()
	       | (Partial {depth, path, ...}) =>
		     if (depth = class_depth) then ()
		     else raise ExploreInst path
	       | (Initial {signat, path, parent_typs, inherited}) =>
		     let fun find_same_sig [] = false
			   | find_same_sig (sl'::rest) =
			     (case (get sl')
				of (p as (Partial{signat=signat',
						  subs,typs,
						  path=path',...})) =>
				    (if eqSignature(signat,signat') then
					 (set (sl,p);
					  push class sl;
					  app (constrain (signat, subs,
							  typs, path))
					    (!inherited);
					  true)
				     else
					 find_same_sig rest)
				 | ERRORinstance => false
				 | _ => impossible 
				          "Modules.Instantiate:find_same_sig")
		     in
			 if not(find_same_sig(!class)) then 
			     (let val subs = 
				 case signat
				   of SIG {kind=TOP {strcount, ...}, ...} =>
				       array (strcount, NULLinstance)
				    | SIG {kind=EMBEDDED, ...} => #base sl
				    | ERROR_SIG => raise Error_Sig
				 val typs = 
				     case signat
				       of SIG {kind=TOP {typecount,...},...} =>
					   array (typecount, tNULLinstance)
					| SIG {kind=EMBEDDED, ...} => 
					      parent_typs
					| _ => impossible
					      "Modules.Instantiate::explore_class.2"
			     in 
				 set (sl, Partial {signat=signat,
						   path=path,subs=subs,
						   typs=typs,
						   final_rep = ref NONE,
						   depth=class_depth});
				 push class sl;
				 app (merge (union_components, !class_def, 
					     err, path))
				   (getSigSlots (signat, path, subs, typs));
				 app (merge (union_components, !class_def, 
					     err, path))
				   (getSigTSlots (signat, path, typs));
				 distributeS (signat, subs, err);
				 distributeT (signat, subs, typs, err);
				 app (constrain (signat, subs, typs, path)) 
				   (!inherited)
			     end) handle Error_Sig => (error_found := true;
						       set (sl,ERRORinstance))
			 else () 
		     end
	       | _ => if (!error_found) then (set (sl,ERRORinstance))
		      else (impossible "Modules.Instantiate:explore_class.3"))

	(* Class shares with some external structure *)

        and constrain (signat, subs, _, path)
	              {my_path=[], its_ancestor=EXTstr str,...} =
	    (case !class_def
	       of SOME str' => (tprint "<checking eqOrigin>\n";
				if eqOrigin(str',str) then ()
				else 
				   err COMPLAIN
				   ("Inconsistent defining constraints : "
				   ^(strName str')^" = "^(strName str)))
		| NONE => (class_def := SOME str;
			   app (merge (union_components, NONE, err, []))
			       (getStrSlots str);
			   app (merge (union_components, NONE, err, []))
			       (getStrTSlots str);
			   check_def (union_components, str, err, path)))

	  (* Class shares with the structure in slot -- explore it *)

          | constrain (signat, subs, _, path)
	              {my_path=[],its_ancestor=SLOTstr slot,its_path=[]} =
	    (tprint "<calling explore_inst to add member to equiv class>\n";
	     explore_inst slot handle (ExploreInst path') =>
		(err COMPLAIN
		 "Sharing constraint is also sharing with a substructure";
	         set (slot, ERRORinstance)))

	  (* Class shares with another structure.  Make sure its ancestor
	   * has been explored.  Then push the constraint down a level.
	   *)

          | constrain (signat, subs', typs, path')
	              {my_path=[],its_ancestor=SLOTstr slot,
		       its_path=sym::rest} =
	    (case (get slot)
	       of Initial _ => 
	           (tprint "<Having to call explore class on an ancestor ";
		    tprint "of a node I'm equivalent to.>\n";
		    explore_class (slot, (class_depth+1), err)
	              handle (ExploreInst _) => 
			  impossible "Modules.Instantiate:explore_class.4")
		| ERRORinstance => ()
		| _ => ();
	     tprint "<finished exploring his ancestor>\n";
	     case (get slot)
	       of Final_top {sign, subs, ...} =>
		   (tprint "<calling constrain recursively>\n";
		    constrain (signat, subs', typs, path')
		      {my_path=[], its_path=rest,
		       its_ancestor=SLOTstr {base=subs,
					     offset=getSigPos sign sym}})
		| Final_embed {sign, subs, ...} =>
		      (tprint "<found Final_embed>\n";
		       constrain (signat, subs', typs, path')
		        {my_path=[], its_path=rest,
			 its_ancestor=SLOTstr {base=subs,
					       offset=getSigPos sign sym}})
	       | Partial _ =>
		     (err COMPLAIN
		      "Sharing constraint is also sharing with a substructure";
		      set (slot,ERRORinstance))
	       | ERRORinstance => ()
	       | _ => impossible "Modules.Instantiate:explore_class.5")


	  (* One of the nodes children shares with someone.  Push the
	   * constraint down to the child now that we are explored.
	   *)

	  | constrain (signat, subs, typs, _) 
	              {my_path=sym::rest, its_ancestor, its_path} =
	    (case getSigPosGen signat sym
	       of TYCbind (FORMtyc {pos, ...}) =>
		   (case typs sub pos
		      of tInitial {inherited, ...} =>
			  push inherited {my_path=[], 
					  its_ancestor=its_ancestor, 
					  its_path=its_path}
		       | _ => impossible "Modules.Instantiate:explore_class.6")
		| STRbind (STRvar {binding=FORMAL {pos, ...}, ...}) =>
		      (case subs sub pos
			 of Initial {inherited, ...} =>
			     push inherited {my_path=rest, 
					     its_ancestor=its_ancestor, 
					     its_path=its_path}
			  | _ => impossible 
				    "Modules.Instantiate:explore_class.7")
		 | _ => impossible "Modules.Instantiate:explore_class.8")
	  | constrain _ _ = impossible "Modules.Instantiate:explore_class.9"


	(* Should find everyone in the equiv. class and convert them to 
	 * Partial nodes.  
	 *)
        val _ = explore_inst first_slot;

	val class_origin = 
	      ref (case !class_def
		     of SOME str => BUILT (getOrigin str)
		      | NONE => 
			  (case !class
			     of [x] => NEED_SELF
			      | (_::_) => NEED_SIMPLE (!union_components)
			      | _ => if (!error_found) then NEED_SELF
				     else impossible
				       "Modules.Instantiate.explore_class.10"))

	(* Converts all of the nodes in the class (which should be Partial)
	 * to Final nodes.  Note that nodes which share the same signature
	 * should share the same Final nodes.  So, they are memoized using
	 * the final_rep field of the Partial node.
	 *)
	fun finalize sl =
	    (case (get sl) 
	       of ERRORinstance => ()
	        | Partial {signat, path, subs, typs, final_rep, ...} =>
		   (case (!final_rep) of
			SOME f => (set (sl, f))
		      | NONE =>
			    case signat of
				SIG {env,
				     kind=TOP{strcount,typecount,...},...} =>
				let val strArray = array (strcount, ERROR_STR)
				    val tycArray = array (typecount, ERRORtyc)
				    val f = 
					Final_top {sign = signat,
						   origin_info=class_origin,
						   actual_subStrs = strArray,
						   actual_types = tycArray,
						   subs = subs,
						   typs = typs,
						   final_struct = ref NONE,
						   expanded = ref false}
				in 
				    final_rep := SOME f;
				    set (sl, f)
				end
			  | SIG {env, kind=EMBEDDED, ...} =>
				(set (sl, 
				      Final_embed {sign=signat, path=path,
						   origin_info = class_origin,
						   subs=subs, 
						   typs=typs,
						   expanded=ref false}))
			  | _ => impossible 
				   "Modules.Instantiate:explore_class.12")
		| _ => impossible "Modules.Instantiate:explore_class.11")
    in 
	app finalize (!class)
    end (* explore_class *)

(*************************************************************************
 * This function deals with exploration of type nodes in the instance
 * graph and is similar to the explore_class function above.  It is
 * a bit simpler since it doesn't have to worry about "children" of
 * type nodes.  However, we must check that the arities of equivalenced
 * types are the same.  Also, if they have constructors, we must check
 * to see that they have the same constructor names.  We don't know how
 * to check that the types of the constructors are satisfiable -- this
 * involves a limited form of second-order unification.
 *************************************************************************)
fun explore_tclass (first_slot, make_stamp, err) =
    let val class = ref ([] : type_ins slot list)
        val class_def = ref (NONE : tycon option)
        fun explore_inst sl =
              (case get sl
               of tPartial _ => ()
                | tInitial {tycon, path, inherited} =>
                    (tprint "<setting tInitial to tPartial>\n";
		     set (sl, tPartial {tycon=tycon, path=path});
                     push class sl;
                     app constrain (!inherited))
		| tERRORinstance => ()
                | _ => impossible "Modules.Instantiate:explore_tclass.1")
        and constrain {my_path=[], its_ancestor=EXTtyc tyc, ...} =
              (case !class_def
               of SOME tyc' =>
                    if equalTycon (tyc',tyc)
                    then ()
                    else
			let val s1 ="Inconsistent defining constraints : type "
			    val s2 = (Symbol.name (tycName tyc')) ^ " = "
			    val s3 = (Symbol.name (tycName tyc))
			    val s = s1 ^ (s2 ^ s3)
			in
			    err COMPLAIN s
			end
                | NONE => class_def := SOME tyc)
          | constrain {my_path=[], its_ancestor=SLOTtyc slot, its_path=[]} =
              explore_inst slot
          | constrain {my_path=[],its_ancestor=SLOTstr slot,its_path=sym::rest}
	    =
              (case get slot
		 of Initial _ =>
		     (explore_class (slot, 0, err)
		      handle ExploreInst _ => 
			  impossible "Modules.Instantiate:explore_tclass.2")
		   | _ => ();
               case (get slot, rest)
		 of (Final_top {sign, typs, ...}, []) =>
  		        constrain {my_path=[], its_path=[],
				   its_ancestor=
				   SLOTtyc {base=typs,
					    offset=getSigTPos sign sym}}
		  | (Final_top {sign, subs, ...}, _) =>
			constrain {my_path=[], its_path=rest,
				   its_ancestor=
				   SLOTstr {base=subs,
					    offset=getSigPos sign sym}}
		  | (Final_embed {sign, typs, ...}, []) =>
			constrain {my_path=[], its_path=[],
				   its_ancestor=
				   SLOTtyc {base=typs,
					    offset=getSigTPos sign sym}}
		  | (Final_embed {sign, subs, ...}, _) =>
			constrain {my_path=[], its_path=rest,
				   its_ancestor=
				   SLOTstr {base=subs,
					    offset=getSigPos sign sym}}
		  | (ERRORinstance, _) => ()
		  | _ => impossible "Modules.Instantiate:explore_tclass.3")
          | constrain _ = 
	      impossible "Modules.Instantiate:explore_tclass:constrain.4"

        val _ = explore_inst first_slot

	exception GetProps

        fun getProps sl =            (* DEFtycs are not yet allowed in sigs. *)
            case get sl
	      of (tPartial {tycon=
			    FORMtyc {spec=GENtyc {arity,eq,kind,...}, ...}
			    ,path,...})
		  => (arity, eq, path, kind)
		| tERRORinstance => (raise GetProps)
		| _ => impossible "Modules.Instantiate:explore_tclass:getProps"

	exception GetPos

        fun getPos sl' =
            case get sl'
	      of (tPartial{tycon=FORMtyc {pos, ...},...}) => pos
	       | tERRORinstance => (raise GetPos)
	       | _ => impossible "Modules.Instantiate:explore_tclass:getPos"

        fun check_arity (ar1, ar2, path1, path2) =
            if ar1 = ar2 then ()
            else err COMPLAIN 
                     ("Inconsistent arities in sharing type "
                      ^(pathName path1)^" = "^(pathName path2)^" : "
                      ^(pathName path1)^" has arity "^(makestring ar1)^" and "
                      ^(pathName path2)^" has arity "^(makestring ar2)^".")

	(* Due to lg's comment, I have opted not to use the sort  *)
	(* routine found in the ../util/sort.sml file.  When this *)
	(* is fixed, however, we should use it and save ourselves *)
	(* some code.                                             *)

	fun mergeSort (op > : ('x * 'x -> bool)) =
	    let fun merge (L1 as h1 :: l1, L2 as h2 :: l2) =
		    if h1 > h2 then h2 :: (merge (L1, l2))
		    else h1 :: (merge (l1, L2))
		  | merge ([], l2) = l2
		  | merge (l1, []) = l1
		    
		fun merge_step (l1 :: l2 :: T) =
		    merge (l1, l2) :: merge_step T
		  | merge_step x = x
		    
		fun merge_sort [] = []
		  | merge_sort [l] = l
		  | merge_sort L = merge_sort (merge_step L)
	    in fn l => merge_sort (map (fn x => [x]) l)
	    end

	val sortD = mergeSort 
	    (fn (DATACON{name=name1,...},DATACON{name=name2,...}) =>
	     Symbol.symbolGt(name1,name2))

	fun eqDataCons (DATACON{name=name1,...},DATACON{name=name2,...}) =
	    Symbol.eq(name1,name2)

	fun compareD ([], []) = true
	  | compareD (d1::r1, d2::r2) = 
	    eqDataCons(d1,d2) andalso compareD (r1,r2)
	  | compareD _ = false
		    
	val class_tycons = ref 
	    (case (!class_def) of
		NONE => (NONE : datacon list option)
	      | SOME (GENtyc {kind=ref (DATAtyc datacons),...}) =>
		    (SOME (sortD datacons))
	      | SOME (DEFtyc {tyfun=TYFUN{body=ty,...},...}) =>
		    (case (headReduceType ty) of
			 CONty (GENtyc {kind = ref (DATAtyc datacons),...},_) 
			 => (SOME (sortD datacons))
		       | _ => NONE)
	      | SOME (_) => (NONE : datacon list option))

	fun check_kind (ref (DATAtyc datacons), path) = 
	    (case (!class_tycons) of
		 NONE => class_tycons := (SOME (sortD datacons))
	       | SOME (dcs) => 
		     if (compareD (dcs,datacons)) then ()
		     else err COMPLAIN
			 ("Inconsistent constructors in sharing datatype "
			  ^(pathName path)^"."))
	  | check_kind _ = ()

        val finalize =
            case !class_def
            of SOME (GENtyc {stamp, arity,eq=ref eq, kind, path})
               => (fn sl
                      => let val (arity', _, path', kind') = getProps sl
                             val result = GENtyc {stamp=stamp, arity=arity,
                                                  eq=ref eq,
						  kind=kind, path=path'}
                         in check_arity (arity, arity', path, path');
			    check_kind (kind',path');
                            set (sl, tFinal result)
                         end)
             | SOME (DEFtyc {path, tyfun as TYFUN {arity, ...}})
               => (fn sl
                      => (let val (arity', _, path', kind') = getProps sl
                             val result = DEFtyc {path=path', tyfun=tyfun}
                          in check_arity (arity, arity', path, path');
			     check_kind (kind',path');
                             set (sl, tFinal result)
			  end) handle GetProps => (error_found := true;
						   set (sl,tERRORinstance)))
             | SOME _ => impossible "Modules.Instantiate:explore_tclass.5"
             | NONE
               => case !class
                  of (sl::rest)
                     => ((let val (arity, _, path, kind) = getProps sl
			      val stamp = make_stamp ()
 			  in fn sl'
                           => ((let val (arity',ref eq',
					path',kind') = getProps sl'
				   val pos = getPos sl'
				   val tyc = GENtyc {stamp=stamp, arity=arity,
						     eq=ref eq', path=path',
						     kind=kind'}
			       in check_arity (arity, arity', path, path');
				  check_kind (kind',path');
				  set (sl', tFinal tyc)
			       end) handle GetProps => 
			                       (error_found := true;
						set (sl',tERRORinstance))
			                 | GetPos => 
					       (error_found := true;
						set (sl',tERRORinstance)))
			  end) handle GetProps => 
			    (error_found := true;
			     set (sl,tERRORinstance); (fn _ => ())))
                   | [] => impossible "Modules.Instantiate:explore_tclass.6"
    in 
	app finalize (!class)
    end (* explore_tclass *)

fun sig_to_instance (sign, path, makeStamp, err) : instance =

        let val dummy = array (1, Initial {signat=sign, path=path, 
					   inherited=ref [],
					   parent_typs=arrayoflist []})

	fun expand ERRORinstance = ()
	  | expand (Final_top {expanded=ref true,...}) = ()
	  | expand (Final_top {actual_subStrs,actual_types,sign,subs,
			       typs, expanded,...}) = 
	    (* We must expand the Final_top instance in a top-down 
	     * fashion.  So, we iterate through the bindings, updating
	     * the subs array appropriately.  As we encounter a
	     * sub signature or type, we recursively expand it.
	     *)
	    let fun expand_substr (subs, typs, actual_types) (sym,i,_) =
		   (tprint "<Expanding substr ";
		    tprintSym sym; tprint ">\n";
		    (case (subs sub i)
		      of Initial _ =>
			  (tprint "<substr was Initial, exploring class>\n";
			   explore_class ({base=subs, offset=i},0,err)
			   handle ExploreInst _
			    => impossible "Modules.instantiate.2")
		       | Partial _ => (tprint "<substr already partial>\n")
		       | _ => (tprint "<substr already final>");
		     case (subs sub i)
		      of (inst as (Final_top _)) =>
			  (tprint "<Going into substrs of top>\n";
			   expand inst)
		       | Final_embed {sign,...} =>
			   (tprint "<substr was embedded, going recursive>\n";
			    app (expand_substr (subs, typs, actual_types))
			                        (getSubSigs sign);
			    tprint "<Expanding types of substructure>\n";
			    app (expand_type (typs, actual_types))
			                        (getTypes sign))
		       | ERRORinstance => ()
		       | _ => impossible "Modules.instantiate.3"))

                and expand_type (typs,actual_types) (sym,i,_) =
                    (tprint "<Expanding type ";
		     tprintSym sym; tprint ">\n";
		     case (typs sub i)
		      of tInitial _ =>
			   explore_tclass ({base=typs, offset=i}, makeStamp, 
					   err)
		       | _ => ();
		     tprint "<Plugging typ into actual_types>\n";
                     case (typs sub i)
		      of tFinal tycon => update (actual_types, i, tycon)
		       | tERRORinstance => update (actual_types, i, ERRORtyc)
		       | _ => impossible"Modules.instantiate.4")
             in 
		expanded := true;
		tprint "<Expanding...\n>";
		case (getSubSigs sign)
		    of [] => (tprint "<No sub sigs>\n")
		     | l => (tprint "<app'ing expand_substr>\n";
			     app (expand_substr (subs,typs,actual_types)) l);
		case (getTypes sign)
		    of [] => (tprint "<No sub types>\n")
		     | l => (tprint "<app'ing expand_type>\n";
			     app (expand_type (typs,actual_types)) l)
            end
	  | expand _ = impossible "Modules.instantiate:expand"

    in 
	((explore_class ({base=dummy,offset=0},0,err))
	 handle (ExploreInst _) =>
	     impossible "Modules.instantiate.1");
	expand (dummy sub 0);
	(dummy sub 0)
    end

(*************************************************************************
 * This function takes an instance graph (assuming that the root node is
 * a Final_top node) and creates a structure corresponding to the graph.
 * It does so by building the graph bottom up.  When an origin structure
 * is needed, we look at the node and determine if one is already BUILT
 * or not.  If we need to build a SIMPLE origin, then we are given an
 * instrep env which we iterate over.  For each of the elements, we
 * recursively generate origin structures.
 * 
 * Structures that are constructed for Final_top nodes are memoized
 * (using the final_struct field) so that if two slots in the graph
 * point to the same node, they can use the same instance structure.
 ************************************************************************)
fun instance_to_structure (path,make_stamp)
    (instance as (Final_top {sign,actual_subStrs,actual_types,
			     subs,final_struct,...})) =
    (case (!final_struct) of
	 SOME str => str
       | NONE =>
	    let exception Get_Origin

		fun instrep_to_binding (sym, instrep) =
		    (case instrep of
			 EXTstr str => 
			     STRbind (STRvar {name = sym,
					      access = PATH [0],
					      binding=str})
		       | SLOTstr sl => 
			     STRbind 
			     (STRvar {name = sym,
				      access = PATH [0],
				      binding=get_origin(get sl)
				      })
		       | EXTtyc tyc => TYCbind tyc
		       | SLOTtyc sl =>
			     TYCbind 
			     (case (get sl)
				of tFinal tyc => tyc
				 | tERRORinstance => ERRORtyc
				 | _ => impossible
				       "Modules.Instantiate:make_simple"))

		and make_simple (env: instrep Env.env) =
		    let val str_env = ref Env.empty
			fun inst_to_binding (sym, instrep) =
			    let val binding = instrep_to_binding (sym, instrep)
			    in 
				str_env := Env.bind(sym,binding,!str_env)
			    end
		    in 
			Env.app inst_to_binding env;
			SIMPLE {stamp = make_stamp (),
				env = (!str_env),
				path = []}
		    end

		(* Gets the origin of an instance -- builds one if one is not
		 * already built. 
		 *)
		and get_origin instance =
		    (let val origin_info = case instance of
			 (Final_top {origin_info,...}) => origin_info
		       | (Final_embed {origin_info,...}) => origin_info
		       | ERRORinstance => raise Get_Origin
		       | _ => impossible "Modules.Instantiate:get_origin"
		     in
			 (case (!origin_info) of
			      BUILT str => str
			    | NEED_SELF => (let val orig = SELF (make_stamp())
					    in
						origin_info := BUILT orig;
						orig
					    end)
			    | NEED_SIMPLE env => 
				  let val orig = make_simple env
				  in
				      origin_info := BUILT orig;
				      orig
				  end)
		    end) handle Get_Origin => ERROR_STR

		(* Creates a structure node from the instance node found at
		 * position i and plugs it into the actual_subStrs array 
		 * for the current node.  If the structure is embedded, we 
		 * must recurse.
		 *)
		fun inst_to_struct (sym,i,_) =
		    (case (subs sub i) of
			 (inst as (Final_top _)) =>
			     let val str = 
				   instance_to_structure 
				   (sym::path,make_stamp) inst
			     in
				 update (actual_subStrs,i,str)
			     end
		       | (inst as (Final_embed 
				   {sign,subs,typs,origin_info,...})) =>
			 (let val orig = get_origin inst
			      val _ = app inst_to_struct (getSubSigs sign);
			      val str = INSTANCE {sign=sign,
						  origin=orig,
						  subStrs=actual_subStrs,
						  path=sym::path,
						  types=actual_types}
			  in
			      update(actual_subStrs,i,str)
			  end)
		       | ERRORinstance => (update (actual_subStrs,i,ERROR_STR))
		       | _ => impossible 
			        "Modules.Instantiate:inst_to_struct")
	    in
		app inst_to_struct (getSubSigs sign);
		let val str = INSTANCE {sign=sign,
					subStrs=actual_subStrs,
					types=actual_types,
					path=path,
					origin=get_origin instance}
		in
		    final_struct := SOME str;
		    str
		end
	    end)
  | instance_to_structure _ ERRORinstance = ERROR_STR
  | instance_to_structure _ _ = 
      impossible "Modules.instantiate:instance_to_structure"
    

fun instantiate (sign, path, scope, error_fn) : Structure =
    let val _ = error_found := false
	val makeStamp = newStamp scope
	fun err sev msg = (error_found := true; error_fn sev msg)
	val instance_graph = sig_to_instance(sign,path,makeStamp,err)
	val result = instance_to_structure (path,makeStamp) (instance_graph)
    in
	if DEBUG andalso !internals then
	    (print "<result of instantiate before equality analysis:>";
	     printStructure(Env.empty,result,0,100);
	     print "<doing equality analysis>\n")
	else ();
	EqTypes.eqAnalyze(result,Stamps.isBound scope,err);
	result
    end (* instantiate *)

end (* struct *)
