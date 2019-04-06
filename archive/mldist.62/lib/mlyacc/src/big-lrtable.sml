(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* A space-efficient implementation of LR tables that should be used only
   with very large LR tables *)

structure LrTable : LR_TABLE = 
    struct
	datatype term = T of int
	datatype nonterm = NT of int
	datatype state = STATE of int
	datatype action = SHIFT of state
			| REDUCE of int (* rulenum from grammar *)
			| ACCEPT
			| ERROR
	exception Goto of state * nonterm
	type table = {states: int, initialState: state,
		      action: (int array * int) array,
		      goto :  int array array}

	val listofarray = fn a =>
	   let fun f(i,r) = if i >= 0 then 
				f(i-1,(a sub i) :: r)
			    else r
	   in f((Array.length a)-1,nil)
	   end

          (* We will encode term * action pairs into a single 30 bit
             nonnegative integer using the following scheme:
		bits 0-15:  the terminal number
		bits 16-17: the tag (in binary)
			00 : shift
			01 : reduce
			10 : accept
			11 : error
		bits 18-30: the integer value associated with the action,
			    if any
	   *)

	val termMask = 65536    (* use the mod function to get the
				     terminal # *)

	val encodeAction = fn (T t,action) =>
		case action
		of SHIFT (STATE s) => t + termMask * (4 * s)
		 | REDUCE s => t + termMask * (4 * s + 1)
	         | ERROR => t + termMask * 2
		 | ACCEPT => t + termMask * 3

	val decodeTerminal = fn i => T (i rem termMask)

	exception lrtable_decode
	val decodeAction = fn i =>
		let val actionInteger = i quot termMask
		    val data = actionInteger quot 4
		    val tag = actionInteger rem 4
		in case tag
		   of 0 => SHIFT (STATE data)
		    | 1 => REDUCE data
		    | 2 => ERROR
		    | 3 => ACCEPT
		    | _ => raise lrtable_decode
 		end

	val gotoMask = 65536
	val encodeGoto = fn (NT nt,STATE s) => nt + s * gotoMask
	val decodeNonterminal = fn i => NT (i rem gotoMask)
        val decodeGoto = fn i => STATE (i quot gotoMask)

	val numStates = fn ({states,...} : table) => states

	val describeActions =
	    let val mapRow = fn (actions,default) =>
               (map (fn entry => (decodeTerminal entry,decodeAction entry))
		    (listofarray actions),
                decodeAction default)
	    in fn ({action,...} : table) =>
		fn STATE state => mapRow (action sub state)
	    end

	val describeGoto =
	   fn ({goto,...} : table) =>
	      fn STATE state =>
		map (fn entry => (decodeNonterminal entry,decodeGoto entry))
		    (listofarray (goto sub state))

	val action = 
	  let val findAction = fn (term,(row,default)) =>
	      let fun binarySearch (l,r) =
		   if l > r then decodeAction default
		   else let val mid = (l + r ) quot 2
                            val entry = row sub mid
		            val key = case decodeTerminal entry
				      of T i => i
		        in if term < key then
			      binarySearch(l,mid-1)
			   else if term > key then
			      binarySearch(mid+1,r)
			   else decodeAction entry
                        end
    	      in binarySearch(0,Array.length row-1)
	      end
           in fn ({action,...} : table) =>
		fn (STATE state,T term) => findAction(term,action sub state)
	    end

	val goto = 
	  fn ({goto,...} : table) =>
	    fn (args as (STATE state,NT nonterm)) =>
	        let val row = goto sub state
                    fun binarySearch (l,r) =
		      if l > r then raise (Goto args)
		       else let val mid = (l + r ) quot 2
                                val entry = row sub mid
		                val key = case decodeNonterminal entry
					  of NT i => i
		             in if nonterm < key then
			          binarySearch(l,mid-1)
			        else if nonterm > key then
			          binarySearch(mid+1,r)
			        else decodeGoto entry
                             end
	        in binarySearch(0,Array.length row-1)
	        end

	val initialState = fn ({initialState,...} : table) => initialState

	val mkLrTable = fn {actions,gotos,initialState,numStates} =>
	   let val actionListToArray = fn l =>
		  arrayoflist(map encodeAction l)
	        val gotoListToArray = fn l=>
		  arrayoflist(map encodeGoto l)
	   in ({action=arrayoflist(map (fn (a,b) => (actionListToArray a,
						     encodeAction (T 0,b)))
                                  actions),
	       goto=arrayoflist(map gotoListToArray gotos),
	       states=numStates,
               initialState=initialState} : table)
           end
end;

