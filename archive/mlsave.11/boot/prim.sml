structure Prim : sig structure Basics : BASICS
		     val primTypes : Basics.Structure
		     val inLinePrim : Basics.Structure
		     val equalSlot : int
		     val notequalSlot : int
		     val makerefSlot : int
		     val fetchSlot : int
		     val assignSlot : int
		     val unboxedAssignSlot : int
		     val updateSlot : int
		     val unboxedUpdateSlot : int
		 end = 
struct

   structure Basics = Basics
   
   open Access Basics BasicTypes

   val symbol = SymbolTable.stringToSymbol

   val typesTable = Table.new()

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
	entercon("nil", nilDcon);
	
	entertyc("ref", refTycon);
	entercon("ref", refDcon);

	entertyc("unit", unitTycon);
	entertyc("int", intTycon);
	entertyc("real", realTycon);
	entertyc("array",arrayTycon);
	entertyc("string",stringTycon);

        entertyc("exn",exnTycon);

	mkSTR(NONE,typesTable,emptyStrenv))

   val inLineTable = Table.new()
   val slotnum = ref 0;
   val alpha = VARty(mkTyvar(symbol("'a"),BOUND))

   fun enter( s : string ) =
       let val name = symbol s
        in Table.add(inLineTable, 
              (name, VARbind(VALvar{access=INLINE(!slotnum),
				    name=name,vtype=ref alpha})));
	   inc slotnum
       end
   
   val inLinePrim = (
       app enter (Sort.sort (op > : (string*string->bool)) [
	"!",  "*",  "+",  "-",	":=",	"<",	"<=",	">",	">=",
	"alength",	"create",	"div",	"cast",	"=",
	"fadd","fdiv","feql","fge","fgt","fle","flt","fmul","fneg","fneq","fsub",
	"ieql",	"ineq", "<>","makeref",
	"ordof", 	"slength",	"store",	"subscript",
	"unboxedassign", "unboxedupdate", "update",	"~"
	]);
       mkSTR(NONE,inLineTable,emptyStrenv))

  val VALvar{access=INLINE(equalSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "=")

  val VALvar{access=INLINE(notequalSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "<>")

  val VALvar{access=INLINE(makerefSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "makeref")

  val VALvar{access=INLINE(fetchSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "!")

  val VALvar{access=INLINE(assignSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol ":=")

  val VALvar{access=INLINE(updateSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "update")

  val _ = (EnvAccess.equalref := equalSlot;
	   EnvAccess.notequalref := notequalSlot;
	   EnvAccess.assignref := assignSlot;
	   EnvAccess.updateref := updateSlot)

  val VALvar{access=INLINE(unboxedAssignSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "unboxedassign")

  val VALvar{access=INLINE(unboxedUpdateSlot),...} =
	   EnvAccess.lookVARinTable(inLineTable, symbol "unboxedupdate")
end (* structure Prim *)
