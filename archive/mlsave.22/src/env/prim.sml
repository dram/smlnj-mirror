structure Prim : sig structure Access : ACCESS
		     structure Basics : BASICS
		     val primTypes : Basics.Structure
		     val inLine : Basics.Structure
		     val inLineName : Access.primop -> string
		     val pure : Access.primop -> bool
		     val special : Access.access -> bool
		 end = 
struct

   structure Access = Access
   structure Basics = Basics
   
   open Access Basics BasicTypes

   val symbol = Symbols.stringToSymbol

(* primTypes structure *)

   val typesTable = Table.new() : binding Table.table

   fun entercon(s: string, c: datacon) =
       Table.add(typesTable, (symbol s, CONbind(c)))

   fun entertyc(s : string, t : tycon ref) =
       Table.add(typesTable, (symbol s,TYCbind t))
   
   val primTypes = (
	entertyc("bool", boolTycon);
	entercon("true", trueDcon);
	entercon("false", falseDcon);
	
	entertyc("list", listTycon);
	entercon("::", consDcon);
	Table.add(typesTable, (symbol "::",FIXbind(infixright 5)));
	entercon("nil", nilDcon);
	
	entertyc("ref", refTycon);
	entercon("ref", refDcon);

	entertyc("unit", unitTycon);
	entertyc("int", intTycon);
	entertyc("real", realTycon);
	entertyc("array",arrayTycon);
	entertyc("string",stringTycon);

        entertyc("exn",exnTycon);

	mkSTR([symbol "PrimTypes"],NONE,typesTable,emptyStrenv))


(* inLine structure *)

   val inLineTable = Table.new() : binding Table.table

   val bottom = POLYty{sign=[{weakness=infinity,eq=false}],
		       tyfun=TYFUN{arity=1,body=VARty(mkTyvar(IBOUND 0))}}

   val primopNames = [
	("!",P.!),
	("*",P.*),
	("+",P.+),
	("-",P.-),
	(":=",P.:=),
	("<",P.<),
	("<=",P.<=),
	(">",P.>),
	(">=",P.>=),
	("alength",P.alength),
	("boxed",P.boxed),
	("create",P.create),
	("div",P.div),
	("cast",P.cast),
	("=",P.eql),
	("fadd",P.fadd),
	("fdiv",P.fdiv),
	("feql",P.feql),
	("fge",P.fge),
	("fgt",P.fgt),
	("fle",P.fle),
	("flt",P.flt),
	("fmul",P.fmul),
	("fneg",P.fneg),
	("fneq",P.fneq),
	("fsub",P.fsub),
	("gethdlr",P.gethdlr),
	("ieql",P.ieql),
	("ineq",P.ineq),
	("<>",P.neq),
	("makeref",P.makeref),
	("ordof",P.ordof),
	("profile",P.profile),
	("sethdlr",P.sethdlr),
	("sceql",P.sceql),
	("slength",P.slength),
	("store",P.store),
	("subscript",P.subscript),
	("unboxedassign",P.unboxedassign),
	("unboxedupdate",P.unboxedupdate),
	("update",P.update),
	("~",P.~) ]

   fun enter( s : string, p : primop ) =
       let val name = symbol s
        in Table.add(inLineTable, 
              (name, VARbind(VALvar{access=INLINE p,
				    name=name,vtype=ref bottom})))
       end

   val inLine =
       (app enter primopNames;
        mkSTR([symbol "InLine"],NONE,inLineTable,emptyStrenv))

   fun inLineName p =
       let fun find [] = ErrorMsg.impossible "Bad primop name"
	     | find ((s,p1)::rest) = if p1=p then s else find rest
       in find primopNames end

 val pure =
   fn P.! => false
    | P.:= => false
    | P.store => false
    | P.subscript => false
    | P.unboxedassign => false
    | P.unboxedupdate => false
    | P.update => false
    | _ => true

   fun special(INLINE P.eql) = true
     | special(INLINE P.neq) = true
     | special(INLINE P.:=) = true
     | special(INLINE P.update) = true
     | special _ = false

end (* structure Prim *)

