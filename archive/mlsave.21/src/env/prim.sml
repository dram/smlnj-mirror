structure Prim : sig structure Access : ACCESS
		     structure Basics : BASICS
		     val primTypes : Basics.Structure
		     val inLine : Basics.Structure
 		     val inLineNames : string list
		     val pure : int -> bool
		     val equalSlot : int
		     val notequalSlot : int
		     val makerefSlot : int
		     val fetchSlot : int
		     val assignSlot : int
		     val unboxedAssignSlot : int
		     val updateSlot : int
		     val unboxedUpdateSlot : int
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
   val slotnum = ref 0;
   val bottom = POLYty{sign=[{weakness=infinity,eq=false}],
		       tyfun=TYFUN{arity=1,body=VARty(mkTyvar(IBOUND 0))}}

   fun enter( s : string ) =
       let val name = symbol s
        in Table.add(inLineTable, 
              (name, VARbind(VALvar{access=INLINE(!slotnum),
				    name=name,vtype=ref bottom})));
	   inc slotnum
       end

   val inLineNames =
       Sort.sort (op > : (string*string->bool)) [
	"!", "*", "+", "-", ":=", "<", "<=", ">", ">=",
	"alength", "boxed", "create", "div", "cast", "=",
	"fadd","fdiv","feql","fge","fgt","fle","flt","fmul","fneg","fneq","fsub",
	"gethdlr", "ieql", "ineq", "<>", "makeref",
	"ordof", "profile", "sethdlr", "sceql", "slength", "store", "subscript",
	"unboxedassign", "unboxedupdate", "update", "~"
	]

   (* position of a string in the inLineNames list (first = 0) *)
   fun position(s:string) =
       let fun find(s'::rest,n) = 
	         if s = s'
		 then n
		 else find(rest,n+1)
        in find(inLineNames,0)
       end

   val inLine =
       (app enter inLineNames;
        mkSTR([symbol "InLine"],NONE,inLineTable,emptyStrenv))

   local val pureTable = array(1 + !slotnum, true);
         val _ = app (fn s => update(pureTable,position(s),false))
		["!",":=","store","subscript","unboxedassign","unboxedupdate",
		    "update"]
     in fun pure k = pureTable sub k
    end

   val equalSlot = position "="
   val notequalSlot = position "<>"
   val makerefSlot = position "makeref"
   val fetchSlot = position "!"
   val assignSlot = position ":="
   val updateSlot = position "update"
   val unboxedAssignSlot = position "unboxedassign"
   val unboxedUpdateSlot = position "unboxedupdate"

   fun special(INLINE i) =
	 i = equalSlot orelse i = notequalSlot
	 orelse i = assignSlot
	 orelse i = updateSlot
     | special _ = false

end (* structure Prim *)

