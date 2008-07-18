signature ENS_VAR2 = 
sig
    val add_var_def : 
	VarCon.var -> 
	int*int -> 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	unit
    val add_var_use : VarCon.var -> int*int -> Types.tyvar list -> unit
    val print_var : unit -> unit

    val add_ty_def : Types.tycon -> int*int -> unit
    val add_ty_use : Types.ty -> int*int -> unit
    val print_ty : unit -> unit
    val print_cons : unit -> unit

    val add_sig_def : Modules.Signature -> int*int -> unit
    val print_sig : unit -> unit

    val add_str_def : 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	Bindings.binding list -> 
	int*int -> 
	Access.access option -> 
	unit
    val add_str_alias : 
	{name: Symbol.symbol, str: Modules.Structure, def: Absyn.strexp} -> 
	Modules.Structure -> 
	int*int -> 
	Access.access option -> 
	unit
    val add_str_use : Modules.Structure -> int*int -> unit
    val print_str : unit -> unit

    val print_all : unit -> unit
    val set_source : string -> unit
    val add_lvar_ext : Access.access -> Access.access -> unit
    val set_eri : (Symbol.symbol -> string option) -> unit
    val set_pid : (PersStamps.persstamp -> unit)
    val clear : unit -> unit
    val save : unit -> unit
    val load : string -> unit
    val test : unit -> unit
end

structure Ens_var2 : ENS_VAR2 = struct
local 
    structure A = Access
    structure VC = VarCon
    structure T = Types
    structure S = Symbol
    structure M = Modules
    structure B = Bindings
    structure EP = Ens_print2
    open Ens_types2
    open Conversion
in
    val source = ref ""
    fun set_source s = source := s

    val lvar_ext = ref [] : (A.access * A.access) list ref
    fun add_lvar_ext lvar ext = 
	lvar_ext := (lvar, ext) :: (!lvar_ext)

    fun print_lvars () = 
	case !lvar_ext of
	    [] => print "No exports\n"
	  | l =>
	    ( List.app 
		  (fn (x, y) => print (A.prAcc x ^ "->" ^ A.prAcc y ^ ", ")) 
		  (!lvar_ext);
	      print "\n"
	    )

    val pid = ref NONE : PersStamps.persstamp option ref
    fun set_pid pid' = (lvar_ext := []; pid := SOME pid')
    fun print_pid () =
	case !pid of
	    NONE => print "No pid\n"
	  | (SOME p) => print ("pid : " ^ PersStamps.toHex p ^ "\n")

    val extRefInfo = ref (fn _ => NONE) : (Symbol.symbol -> string option) ref
    fun set_eri eri = 
	(extRefInfo := eri)

    fun bug msg = ErrorMsg.impossible("Bugs in Ens_var2: "^msg);

    fun loc_reg (r1, r2) = ((!source, r1, r2):location)


    fun is_accessible (A.EXTERN _) = SOME false
      | is_accessible (A.NO_ACCESS) = NONE
      | is_accessible (A.PATH (s, _)) = is_accessible s
      | is_accessible (A.LVAR _) = SOME true




    fun compare_acc (A.LVAR i, A.LVAR j) = Int.compare (i,j)
      | compare_acc (A.LVAR _, _) = LESS
      | compare_acc (_, A.LVAR _) = GREATER
      | compare_acc (A.EXTERN s1, A.EXTERN s2) = 
	PersStamps.compare (s1, s2)
      | compare_acc (A.EXTERN _, _) = LESS
      | compare_acc (_, A.EXTERN _) = GREATER
      | compare_acc (A.PATH (acc1', s1), A.PATH (acc2', s2)) = 
	( case compare_acc (acc1', acc2') of
	      EQUAL => Int.compare (s1, s2)
	    | ord => ord
	)
      | compare_acc (A.PATH _, _) = LESS
      | compare_acc (_, A.PATH _) = GREATER
      | compare_acc (A.NO_ACCESS, A.NO_ACCESS) = EQUAL

    fun compare_loc_acc (loc1, loc2) accs = 
	case String.compare (locFile loc1, locFile loc2) of
	    EQUAL => compare_acc accs
	  | ord => ord 

    (* external values *)
    structure ExtKey : ORD_KEY =
    struct
        type ord_key = ext_elem
	fun compare (ExtVar {access = acc1, ...}, ExtVar {access = acc2, ...})=
	    compare_acc (acc1, acc2)
	  | compare (ExtVar _, _) = LESS
	  | compare (_, ExtVar _) = GREATER
	  | compare (ExtStr {access = acc1, ...}, ExtStr {access = acc2, ...})=
	    compare_acc (acc1, acc2)
	  | compare (ExtStr _, _) = LESS
	  | compare (_, ExtStr _) = GREATER
	  | compare (ExtType {stamp = st1, ...}, ExtType {stamp = st2, ...}) =
	    Stamps.compare (st1, st2)
	  | compare (ExtType _, _) = LESS
	  | compare (_, ExtType _) = GREATER
	  | compare (ExtCons {stamp = st1,name = n1, ...}, 
		     ExtCons {stamp = st2,name = n2, ...})=
	    ( case String.compare (Symbol.name n1, Symbol.name n2) of
		  EQUAL => Stamps.compare (st1, st2)
		| ord => ord
	    )
	  | compare (ExtCons _, _) = LESS
	  | compare (_, ExtCons _) = GREATER
	  | compare (ExtSig {stamp = st1, ...}, ExtSig {stamp = st2, ...}) = 
	    Stamps.compare (st1, st2)
    end (* structure ExtKey *)

    (* external value set *)
    structure ExtSet = RedBlackSetFn(ExtKey)
    val ens_ext = (ref ExtSet.empty)

    (* variables *)
    structure VarKey : ORD_KEY =
    struct
        type ord_key = var_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key, 
		     {access = acc2, def = loc2, ...} : ord_key) = 
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure VarKey *)

    (* variable sets *)
    structure VarSet = RedBlackSetFn(VarKey)
    val ens_var = (ref VarSet.empty);

    (* structures *)
    structure StrKey : ORD_KEY =
    struct
        type ord_key = str_elem
	fun compare ({access = acc1, def = loc1, ...} : ord_key,
		     {access = acc2, def = loc2, ...} : ord_key) =
	    compare_loc_acc (loc1, loc2) (acc1, acc2)
    end (* structure StrKey *)
    
    (* structure sets *)
    structure StrSet = RedBlackSetFn(StrKey)
    val ens_str = (ref StrSet.empty);

    (* type *)
    structure TypeKey : ORD_KEY =
    struct
        type ord_key = type_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key) =
	    Stamps.compare (stamp1, stamp2)
    end (* structure TypeKey *)
    
    (* type sets *)
    structure TySet = RedBlackSetFn(TypeKey)
    val ens_ty = (ref TySet.empty);

    (* constructors *)
    structure ConsKey : ORD_KEY =
    struct
        type ord_key = cons_elem
	fun compare ({name = name1, dataty = s1, ...}:ord_key, 
		     {name = name2, dataty = s2, ...}:ord_key) =
	    case Stamps.compare (s1, s2) of
		EQUAL => String.compare (Symbol.name name1, Symbol.name name2)
	      | ord => ord
    end (* structure ConsKey *)
    
    (* constructor sets *)
    structure ConsSet = RedBlackSetFn(ConsKey)
    val ens_cons = (ref ConsSet.empty);

    (*signature*)
    structure SigKey : ORD_KEY =
    struct
        type ord_key = sig_elem
	fun compare ({stamp = stamp1, ...}:ord_key, 
		     {stamp = stamp2, ...}:ord_key)=
	    Stamps.compare (stamp1, stamp2)
    end (* structure SigKey *)
    
    (* signature sets *)
    structure SigSet = RedBlackSetFn(SigKey)
    val ens_sig = ref SigSet.empty;



    fun add_ext v = 
	ens_ext := ExtSet.add (!ens_ext, v)

    fun add_str v = 
	ens_str := StrSet.add (!ens_str, v)

    fun find_acc acc ({access, ...}:str_elem) = acc = access

    fun get_var_lvar acc slot = 
	case StrSet.find (find_acc acc) (!ens_str) of
	    NONE => bug ("get_var_lvar: " ^ A.prAcc acc)
	  | SOME {elements = Def l, ...} => 
	    ( case List.find (fn (x, _, _) => x = slot) l of
		  NONE => bug ("get_var_lvar2: " ^ A.prAcc acc)
		| SOME (_, _, Var access) => access
		| SOME _ => bug ("get_var_lvar3: " ^ A.prAcc acc)
	    )
	  | SOME {elements = Constraint (l, acc2), ...} => 
	    ( case List.find (fn (x, _, _) => x = slot) l of
		  NONE => bug ("get_var_lvar4: " ^ A.prAcc acc)
		| SOME (_, _, slot2) => get_var_lvar acc2 slot2
	    )
	  | SOME {elements = Alias a, ...} => get_var_lvar a slot

    fun get_str_lvar acc slot = 
	case StrSet.find (find_acc acc) (!ens_str) of
	    NONE => bug ("get_str_lvar: " ^ A.prAcc acc)
	  | SOME {elements = Def l, ...} => 
	    ( case List.find (fn (x, _, _) => x = slot) l of
		  NONE => bug ("get_str_lvar2: " ^ A.prAcc acc)
		| SOME (_, _, Str access) => access
		| SOME _ => bug ("get_str_lvar3: " ^ A.prAcc acc)
	    )
	  | SOME {elements = Constraint (l, acc2), ...} => 
	    ( case List.find (fn (x, _, _) => x = slot) l of
		  NONE => bug ("get_str_lvar4: " ^ A.prAcc acc)
		| SOME (_, _, slot2) => get_str_lvar acc2 slot2
	    )
	  | SOME {elements = Alias a, ...} => get_str_lvar a slot

    fun modify [] region = 
	([], region)
      | modify (h :: q) (r1, _) = 
	(q, (r1-1-String.size (Symbol.name h), r1-1))

    (* when saying  A.x, or open A ... x, the use of A is added here*)
    (* gives back the lvar corresponding to the structure or variable*)
    fun add_implicite_use (A.PATH (access, slot0)) rpath region is_str = 
	let fun add_uses (a as A.LVAR _) _ _ = a
	      | add_uses (a as (A.PATH (a', slot))) rpath region = 
		let val (rpath, region) = modify rpath region
		    val a' = add_uses a' rpath region
		    val a = get_str_lvar a' slot
		in
		    case StrSet.find (find_acc a) (!ens_str) of
			NONE => print "pb in add_implicite_use1\n"
		      | SOME {usage, ...} => 
			usage := loc_reg region :: !usage;
		    a
		end
	      | add_uses a _ _ = bug ("add_implicite_use3" ^ A.prAcc a)
	in
	    if is_str then
		get_str_lvar (add_uses access rpath region) slot0
	    else
		get_var_lvar (add_uses access rpath region) slot0
	end
      | add_implicite_use (access as A.LVAR _) _ _ _ = access
      | add_implicite_use _ _ _ _ = bug "add_implicite_use4"

    fun str_find access = 
	case 
	    ExtSet.find 
		(fn x => 
		    ExtKey.compare (ExtStr {access = access, usage = ref []},x)
		    = EQUAL)
		(!ens_ext)
	 of NONE => NONE
	  | SOME (ExtStr a) => SOME a
	  | SOME _ => bug "str_find"

    fun var_find access = 
	case 
	    ExtSet.find 
		(fn x => 
		    ExtKey.compare (ExtVar {access = access, usage = ref []},x)
		    = EQUAL)
		(!ens_ext)
	 of NONE => NONE
	  | SOME (ExtVar a) => SOME a
	  | SOME _ => bug "str_find"

    fun add_implicite_uses_ext (A.EXTERN _) _ _ = ()
      | add_implicite_uses_ext (a as A.PATH (a', _)) rpath (region as (r1, _))=
	let val (rpath, region) = modify rpath region
	in
	    case str_find a of
		NONE => 
		add_ext (ExtStr {access=a,
				 usage=ref[loc_reg region]})
	      | SOME {usage, ...} => 
		usage := loc_reg region :: !usage;
	    add_implicite_uses_ext a' rpath region
	end
      | add_implicite_uses_ext _ _ _ = bug "add_implicite_uses_ext"

    fun print_ext () = 
	ExtSet.app EP.print_ext (!ens_ext)


    fun add_var_def var 
		    (r1, _)
		    {str = M.STR {access = parent_acc, ...}, def, name} = 
	( case var of
	      (VC.VALvar {path = SymPath.SPATH [S.SYMBOL (_, "it")],...}) => ()
	    | VC.VALvar {access, typ, path = SymPath.SPATH path, ...} =>
		  ens_var := 
		  VarSet.add(!ens_var, 
			     {access = access, 
			      parent = parent_acc,
			      typ = ty_to_ty' (!typ),
			      name = List.last path,
			      def= loc_reg (r1,r1+String.size 
						      (Symbol.name 
							   (List.last path))), 
			      usage=ref []})
	    | _ => ()
	)
      | add_var_def _ _ _ = ()

    fun get_ty' typ typ' = 
	let
	    val typ'' = 
		case !typ of 
		    T.POLYty _ =>
		    TypesUtil.applyPoly 
			(!typ,List.map TypesUtil.pruneTyvar typ')
		  | _ => !typ
	in
	    ty_to_ty' typ''
	end

    fun add_var_use (VC.VALvar {access, path = SymPath.SPATH path, typ, ...})
		    region
		    (typ' : T.tyvar list) = 
	( case is_accessible access of
	      NONE => ()
	    | SOME false =>
	      ( case access of
		    (A.PATH (suite, _)) => 
		    let val rpath = rev path
			val toplevel = List.hd path 
		    in
			case !extRefInfo toplevel of
			    NONE => () (*print ("No sourcefile for " ^ 
					   Symbol.name toplevel ^ "\n")*)
			  | SOME _ =>
			    let val triplet = 
				(loc_reg region, get_ty' typ typ', access)
			    in
				case var_find access of
				    NONE => 
				    add_ext (ExtVar {access = access,
						     usage = ref [triplet]})
				  | SOME {usage, ...} => 
				    usage := triplet :: !usage;
				let val (rpath,region) = modify rpath region in
				    add_implicite_uses_ext suite rpath region
				end
			    end
		    end
		  | _ => bug "add_var_use"
	      )
	    | SOME true =>
	      let val new_acc=add_implicite_use access (rev path) region false
	      in
		  case VarSet.find
			   (fn {access, ...} => access = new_acc) 
			   (!ens_var) 
		   of NONE => ()
		    | SOME {usage, ...} => 
		      usage := (loc_reg region, get_ty' typ typ', access) ::
			       (!usage)
	      end
	)
      | add_var_use _ _ _ = ()

    fun print_var () = 
	VarSet.app EP.print_var (!ens_var)
			  



    fun add_ty_def tycon region = 
	let val (stamp, path, tycon'(*, dcons*)) = tycon_to_tycon' tycon in
	    ens_ty := TySet.add ( !ens_ty,
				  { tycon = tycon',
				    name = InvPath.last path,
				    stamp = stamp,
				    def = loc_reg region,
				    usage = ref []}
				)(*;
	    case (tycon', dcons) of
		((Datatype (_, sl) | Abstract sl), SOME dcons) =>
		List.foldl
		    ( fn (x, n) => (
			 ConsSet.add 
			     ( !ens_cons,
			       { name = x, 
				 dataty = stamp,
				 def = loc_reg region,
				 ty=ty_to_ty' 
					(Option.valOf 
					     (#domain
						  (#dcons 
						       (Vector.sub(members,n))
						  )
					     )
					),
				 usage = ref []
			       }
			     );
			 (n+1)
			 )
		    )
		    0
		    sl
	      | ((Deftyc | Primtyc _), _) => 0;
	    ()*)
	end

    fun add_ty_use ty region = 
	case ty_to_ty' ty of
	    Conty (General (st, p), _) => (*print (EP.rptoS p ^ " used\n")*)
	    ( case TySet.find 
		       (fn {stamp, ...} => Stamps.eq (stamp,st)) 
		       (!ens_ty) 
	       of NONE => () (*probablement un truc primitif, mais peut etre un
			      * type extern*)
		| SOME {usage, ...} => usage := loc_reg region :: (!usage)
	    )
	  | _ => ()

    fun print_ty () = 
	TySet.app EP.print_type (!ens_ty)

    fun print_cons () = 
	ConsSet.app EP.print_cons (!ens_cons)




    fun add_sig_def sign region = 
	let val {name, stamp, inferred, def, elements, alias, usage} = 
		sig_to_elem sign in
	    ens_sig := SigSet.add ( !ens_sig,
				    { name = name, 
				      stamp = stamp,
				      inferred = inferred,
				      def = loc_reg region,
				      elements = elements,
				      alias = alias,
				      usage = usage
				    }
				  )
	end
	
    fun print_sig () = 
	SigSet.app EP.print_sig (!ens_sig)






    fun add_str_def { name, 
		      str = 
		      M.STR { sign = (sign as M.SIG {stamp, elements, ...}), 
			      access, ...}, 
		      def} 
		    bl 
		    region 
		    parent_acc = 
	let 
	    val () = add_sig_def sign region
	    fun get_symbol (B.VALbind (VC.VALvar {path, ...})) =
		SOME (SymPath.last path)
	      | get_symbol (B.STRbind (M.STR {rlzn = {rpath, ...}, ...})) =
		SOME (InvPath.last rpath)
	      | get_symbol _ = NONE
			       
	    fun get_symbol' (x, _) = x
				     
	    fun get_acc b = 
		let val s = get_symbol b
		in
		    case s of
			NONE => NONE
		      | SOME symbol => 
			List.find (fn x => symbol = get_symbol' x) elements
		end
		
	    fun get_trip b = 
		case (b, get_acc b)
		 of (B.VALbind (VC.VALvar {access, ...}), 
		     SOME (s, M.VALspec {slot, ...})) => 
		    SOME (slot, s, Var access)
		  | (B.STRbind (M.STR {access, ...}), 
		     SOME (s, M.STRspec {slot, ...})) =>
		    SOME (slot, s, Str access)
		  | _ => NONE
			 
	    val elements' = 
		List.mapPartial get_trip bl
		
	    fun get_slot (A.PATH (_, s)) = s
					   
	    val elements'' = 
		case elements' of
		    (_, _, Var (A.PATH (acc, _)))::_ =>
		    Constraint 
			( List.map 
			      (fn ((x, y, Var z) | (x, y, Str z)) => 
				  (x, y, get_slot z)) 
			      elements',
			  acc
			)
		  | _ => 
		    Def elements'
		    
	in
	    (*en profiter pour mettre a jour le champ parent des enfants?*)
	    ens_str := StrSet.add(!ens_str, 
				  { name = name, 
				    access = access,
				    parent = parent_acc,
				    sign = SOME stamp,
				    def = loc_reg region,
				    elements = elements'',
				    usage = ref []}
				 )
	end
      | add_str_def _ _ _ _ = ()
			      
    fun add_str_alias {name, 
		       str = M.STR {sign = M.SIG {stamp, ...}, access, ...}, 
		       def}
		      (M.STR {access = access', ...}) (*structure on rhs*)
		      region 
		      parent_acc = 
	ens_str := StrSet.add(!ens_str, 
			      { name = name, 
				access = access,
				parent = parent_acc,
				sign = SOME stamp,
				def = loc_reg region,
				elements = Alias access',
				usage = ref []}
			     )
      | add_str_alias _ _ _ _ = ()

    fun get_path ({rpath = InvPath.IPATH rpath, ...}:M.strEntity) = 
	rpath

    fun add_str_use (M.STR {access, rlzn, ...}) region = 
	( case is_accessible access of
	      SOME false => (*extern*)
	      let val rpath = get_path rlzn
		  val toplevel = List.last rpath 
	      in
		  case !extRefInfo toplevel of
		      NONE => () (*print ("No sourcefile for " ^ 
				     Symbol.name toplevel ^ "\n")*)
		    | SOME _ =>
		      add_implicite_uses_ext access rpath region
	      end
	    | SOME true => (*local*)
	      let val new_acc = add_implicite_use access (get_path rlzn) 
						  region true
	      in
		  case StrSet.find (fn{access, ...}=>access=new_acc) (!ens_str)
		   of NONE => ()
		    | SOME {usage, ...} => usage := loc_reg region :: !usage
	      end
	    | NONE => (*NO_ACCESS*) ()
	)
      | add_str_use _ _ = ()
	  
	
    fun print_str () = 
	StrSet.app EP.print_str (!ens_str)





    fun print_all () = (
	print_var ();
	print_str ();
	print_ty ();
	print_cons ();
	print_sig ();
	print_ext ();
	print_lvars ();
	print_pid ()
    )


    fun clear () = (
	ens_var := VarSet.empty;
	ens_ty := TySet.empty;
	ens_cons := ConsSet.empty;
	ens_str := StrSet.empty;
	ens_sig := SigSet.empty;
	ens_ext := ExtSet.empty;
	source := "";
	(*lvar_ext := [];
	pid := NONE;*)
	extRefInfo := (fn _ => NONE)
    )

    fun get_strings (a, b, c, d, e, f, g) = 
	TyToString.varToString  (VarSet.listItems  a) ::
	TyToString.typeToString (TySet.listItems   b) ::
	TyToString.consToString (ConsSet.listItems c) ::
	TyToString.strToString  (StrSet.listItems  d) ::
	TyToString.sigToString  (SigSet.listItems  e) ::
	TyToString.extToString  (ExtSet.listItems  f) ::
	TyToString.lvarExtToString g ::
	nil

    fun get_sets (a, b, c, d, e, f, g) = 
	( VarSet.fromList  (StringToTy.stringToVar  a),
	  TySet.fromList   (StringToTy.stringToType b),
	  ConsSet.fromList (StringToTy.stringToCons c),
	  StrSet.fromList  (StringToTy.stringToStr  d),
	  SigSet.fromList  (StringToTy.stringToSig  e),
	  ExtSet.fromList  (StringToTy.stringToExt  f),
	  StringToTy.stringToLvarExt g
	)
	

    fun pickfile str = str ^ ".si"

    fun save_to_file sourcefile = 
	case sourcefile of
	     ("<instream>"|"stdIn") => ()
	   | _ =>
	     let val new_source = pickfile sourcefile
		 val os = TextIO.openOut new_source
		 val s = get_strings (!ens_var, !ens_ty, !ens_cons, !ens_str,
				      !ens_sig, !ens_ext, !lvar_ext)
	     in
		 List.app (fn x => (TextIO.output (os, x);
				    TextIO.output (os, "\n")))
			  s;
		 TextIO.flushOut os;
		 TextIO.closeOut os;
		 print ("Wrote to file " ^ new_source ^ "\n")
	     end

    fun save () = save_to_file (!source)

    fun load sourcefile =
	let val new_source = pickfile sourcefile
	    val os = TextIO.openIn new_source
	    fun get_val NONE = bug ("sourcefile "^new_source^":unexpected EOF")
	      | get_val (SOME s) = s
	    fun gs () = get_val (TextIO.inputLine os)
	    val (var,ty,cons,str,sign,ext,lvarext) =
		get_sets (gs(),gs(),gs(),gs(),gs(),gs(),gs())
	in
	    ens_var := var;
	    ens_ty := ty;
	    ens_cons := cons;
	    ens_str := str;
	    ens_sig := sign;
	    ens_ext := ext;
	    lvar_ext := lvarext;
	    pid := ( case !lvar_ext of
			 [] => NONE
		       | ((_, acc) :: _) =>
			 let fun f (A.PATH (a, _)) = f a
			       | f (A.EXTERN p) = p
			       | f _ = bug "load"
			 in
			     SOME (f acc)
			 end
		   )
	end

    fun test () =
	let val s = get_strings (!ens_var, !ens_ty, !ens_cons, !ens_str,
				 !ens_sig, !ens_ext, !lvar_ext)
	    val (a,b,c,d,e,f,g) = 
		case s of
		    [a, b, c, d, e, f, g] => get_sets (a, b, c, d, e, f, g)
		  | _ => bug "test"
	    val () = 
		if VarSet.numItems a <> VarSet.numItems (!ens_var) then
		    bug "test ().var length"
		else
		    ()
	    val () = 
		if TySet.numItems b <> TySet.numItems (!ens_ty) then
		    bug "test ().type length"
		else
		    ()
	    val () = 
		if ConsSet.numItems c <> ConsSet.numItems (!ens_cons) then
		    bug "test ().cons length"
		else
		    ()
	    val () = 
		if StrSet.numItems d <> StrSet.numItems (!ens_str) then
		    bug "test ().str length"
		else
		    ()
	    val () = 
		if SigSet.numItems e <> SigSet.numItems (!ens_sig) then
		    bug "test ().sig length"
		else
		    ()
	    val () = 
		if ExtSet.numItems f <> ExtSet.numItems (!ens_ext) then
		    bug "test ().ext length"
		else
		    ()
	    val () = 
		if List.length g <> List.length (!lvar_ext) then
		    bug "test ().lvar_ext length"
		else
		    ()
	in
	    VarSet.app EP.print_var a;
	    TySet.app EP.print_type b;
	    ConsSet.app EP.print_cons c;
	    StrSet.app EP.print_str d;
	    SigSet.app EP.print_sig e;
	    ExtSet.app EP.print_ext f;
	    print_lvars ();
	    print_pid ()
	end



end (*end local*)
end (*end struct*)
