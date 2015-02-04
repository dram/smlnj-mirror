(* access.sml *)

structure Access = struct

structure Symbol = Symbol

type lvar = int	      (* lambda variable id number *)
type slot = int	      (* position in structure record *)
type path = int list  (* slot chain terminated by lambda variable id number *)

datatype access 
  = LVAR of lvar
  | SLOT of slot
  | PATH of path  
  | INLINE of int

structure DSA = Dynamic(struct	type array = string array
				exceptionx subscript = subscript
				type elem = string
				val array = array
				val op sub = op sub
				val update = update
			        val length = Array.length
			   end)

local
  val varcount = ref 0
  val lvarNames = DSA.array("v")
in
  val saveLvarNames = ref false
  fun mkLvar () : lvar =
      (inc varcount;
       !varcount)

  fun resetLvars v = (varcount := v)

  fun namedLvar(id: Symbol.symbol) =
      (inc varcount;
       if !saveLvarNames then DSA.update(lvarNames,!varcount,Symbol.Name(id))
			 else ();
       !varcount)

  fun lvarName(lv : lvar) : string = 
      DSA.sub(lvarNames,lv) ^ makestring(lv)

  fun rootLvarName(lv : lvar) : string = DSA.sub(lvarNames,lv)

end

end  (* structure Access *)
