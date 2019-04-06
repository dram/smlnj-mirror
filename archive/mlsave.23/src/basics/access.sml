(* access.sml *)

structure Access : ACCESS =
struct

  structure Symbol = Symbol
  structure P = 
    struct datatype primop = 
      ! | * | + | - | := | < | <= | > | >= | alength | boxed | create | div | cast |
      eql | fadd |fdiv |feql |fge |fgt |fle |flt |fmul |fneg |fneq |fsub | gethdlr |
      ieql | ineq | neq | makeref | ordof | profile | sethdlr | sceql | slength |
      store | subscript | unboxedassign | unboxedupdate | update | ~
    end

  type lvar = int      (* lambda variable id number *)
  type slot = int      (* position in structure record *)
  type path = int list (* slot chain terminated by lambda variable id number *)
  type primop = P.primop

  datatype access 
    = LVAR of lvar
    | SLOT of slot
    | PATH of path  
    | INLINE of primop

  (* local *)
    val varcount = ref 0
    exception NoLvarName
    val lvarNames : string Intmap.intmap = Intmap.new NoLvarName
    val name = Intmap.map lvarNames
    val giveLvarName = Intmap.add lvarNames

  val saveLvarNames = System.Control.saveLvarNames
  fun mkLvar () : lvar = (inc varcount; !varcount)
  fun sameName(v,w) =
      if !saveLvarNames
      then let val newname = ((name v) handle NoLvarName => makestring v) ^ "=" ^
			     ((name w) handle NoLvalName => makestring w)
           in giveLvarName(v,newname); giveLvarName(w,newname)
	   end
      else ()
  fun dupLvar v =
      (inc varcount;
       if !saveLvarNames then sameName (v,!varcount) else ();
       !varcount)
  fun namedLvar(id: Symbol.symbol) =
      (inc varcount;
       if !saveLvarNames then giveLvarName(!varcount,Symbol.name id) else ();
       !varcount)
  fun lvarName(lv : lvar) : string =
      (name lv ^ ":" ^ makestring lv) handle NoLvarName => makestring lv

end  (* structure Access *)


