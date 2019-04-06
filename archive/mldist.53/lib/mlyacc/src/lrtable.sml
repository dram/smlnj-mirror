(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure LrTable : LR_TABLE = 
    struct
	datatype ('a,'b) entryList = EMPTY
				   | ENTRY of 'a * 'b * ('a,'b) entryList
	fun entryToList EMPTY = nil
	  | entryToList(ENTRY(key,data,rest)) = (key,data) :: (entryToList rest)
	fun listToEntry nil = EMPTY
	  | listToEntry ((key,data) :: b) = ENTRY(key,data,listToEntry b)
	datatype term = T of int
	datatype nonterm = NT of int
	datatype state = STATE of int
	datatype action = SHIFT of state
			| REDUCE of int (* rulenum from grammar *)
			| ACCEPT
			| ERROR
	exception Goto of state * nonterm
	type table = {states: int, initialState: state,
		      action: ((term,action) entryList * action) array,
		      goto :  (nonterm,state) entryList array}
	val numStates = fn ({states,...} : table) => states
	val describeActions =
	   fn ({action,...} : table) =>
	      fn STATE state =>
		  (fn (a,b) => (entryToList a,b)) (action sub state)
	val describeGoto =
	   fn ({goto,...} : table) =>
	      fn STATE state => entryToList(goto sub state)
	fun findTerm (T term,row,default) =
	    let fun find (ENTRY (T key,data,r)) =
		       if key < term then find r
		       else if key=term then data
		       else default
		   | find EMPTY = default
	    in find row
	    end
	fun findNonterm (NT nt,row) =
	    let fun find (ENTRY (NT key,data,r)) =
		       if key < nt then find r
		       else if key=nt then SOME data
		       else NONE
		   | find EMPTY = NONE
	    in find row
	    end
	val action = fn ({action,...} : table) =>
		fn (STATE state,term) =>
		  let val (row,default) = action sub state
		  in findTerm(term,row,default)
		  end
	val goto = fn ({goto,...} : table) =>
			fn (a as (STATE state,nonterm)) =>
			  case findNonterm(nonterm,goto sub state)
			  of SOME state => state
			   | NONE => raise (Goto a)
	val initialState = fn ({initialState,...} : table) => initialState

	val mkLrTable = fn {actions,gotos,initialState,numStates} =>
	     ({action=arrayoflist(map (fn (a,b) => (listToEntry a,b)) actions),
	       goto=arrayoflist(map listToEntry gotos),
	       states=numStates,
               initialState=initialState} : table)
end;
