structure Prim : sig structure Basics : BASICS
		     val PrimTypes : Basics.Structure
		     val InLinePrim : Basics.Structure
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

   val symbol = SymbolTable.StringToSymbol

   val TypesTable = Table.new()

   fun entercon(s: string, c: datacon) =
       Table.add(TypesTable, (symbol s, CONbind(c)))

   fun entertyc(s : string, t : tycon ref) =
       Table.add(TypesTable, (symbol s,TYCbind t))
   
   val PrimTypes = (
	entertyc("bool", boolTycon);
	entercon("true", TRUEdcon);
	entercon("false", FALSEdcon);
	
	entertyc("list", listTycon);
	entercon("::", CONSdcon);
	entercon("nil", NILdcon);
	
	entertyc("ref", refTycon);
	entercon("ref", REFdcon);

	entertyc("unit", unitTycon);
	entertyc("int", intTycon);
	entertyc("real", realTycon);
	entertyc("array",arrayTycon);
	entertyc("string",stringTycon);

        entertyc("exn",exnTycon);

	mkSTR(NONE,TypesTable,emptyStrenv))

   val InLineTable = Table.new()
   val slotnum = ref 0;
   val alpha = VARty(mkTyvar(symbol("'a"),BOUND))

   fun enter( s : string ) =
       let val name = symbol s
        in Table.add(InLineTable, 
              (name, VARbind(VALvar{access=INLINE(!slotnum),
				    name=name,vtype=ref alpha})));
	   inc slotnum
       end
   
   val InLinePrim = (
       app enter (Sort.sort (op > : (string*string->bool)) [
	"!",  "*",  "+",  "-",	":=",	"<",	"<=",	">",	">=",
	"alength",	"create",	"div",	"cast",	"=",
	"fadd","fdiv","feql","fge","fgt","fle","flt","fmul","fneg","fneq","fsub",
	"ieql",	"ineq", "<>","makeref",
	"ordof", 	"slength",	"store",	"subscript",
	"unboxedassign", "unboxedupdate", "update",	"~"
	]);
       mkSTR(NONE,InLineTable,emptyStrenv))

  val VALvar{access=INLINE(equalSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "=")

  val VALvar{access=INLINE(notequalSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "<>")

  val VALvar{access=INLINE(makerefSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "makeref")

  val VALvar{access=INLINE(fetchSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "!")

  val VALvar{access=INLINE(assignSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol ":=")

  val VALvar{access=INLINE(updateSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "update")

  val _ = (EnvAccess.equalref := equalSlot;
	   EnvAccess.notequalref := notequalSlot;
	   EnvAccess.assignref := assignSlot;
	   EnvAccess.updateref := updateSlot)

  val VALvar{access=INLINE(unboxedAssignSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "unboxedassign")

  val VALvar{access=INLINE(unboxedUpdateSlot),...} =
	   EnvAccess.lookVARinTable(InLineTable, symbol "unboxedupdate")
end (* structure Prim *)
