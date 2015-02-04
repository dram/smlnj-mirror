(* table.sml *)

signature TABLE = sig
    structure Symbol : SYMBOL
    type 'a table
    exception Notfound_Table
    exception Next
    val new : unit -> 'a table
    val add : 'a table * (Symbol.symbol * 'a) -> int
    val look : ('a -> 'b) -> ('a table * Symbol.symbol) -> 'b
    val push : 'a table * int * (Symbol.symbol * 'a) -> unit
    val pop : 'a table * int -> (Symbol.symbol * 'a)
    val merge : 'a table * 'a table -> unit
    val app: 'a table * ((Symbol.symbol * 'a) -> unit) -> unit
end

structure Table : TABLE = struct

    (* table -- symbol table hash tables *)

    structure Symbol = Symbol

    type 'a table = (Symbol.symbol * 'a) list array

    exception Notfound_Table
	  and Next

    val tableSize = 103  (* size of hash array *)

    fun new () =
	array(tableSize,nil)

    fun add (s: 'a table, binder as (id,_)): int =
	let val index = Symbol.number id mod tableSize
	 in update(s, index, binder::(s sub index));
	    index
	end

    fun look test (s, id) =
	let fun look1 ((i,b)::e) =
		  if Symbol.eq(i,id)
		  then (test b handle Next => look1 e)
		  else look1 e
	      | look1 nil = raise Notfound_Table
	 in look1(s sub (Symbol.number id mod tableSize))
	end

    fun push(s,i,x) =
	update(s,i,x::(s sub i))

    fun pop(s,i) =
	let val x::b = s sub i
	 in update(s,i,b); x
	end

    fun merge(s,t) =
	   let fun index i = (update(t,i,(s sub i)@(t sub i)); index(i+1))
	    in index 0 handle Subscript => ()
	   end

    fun app(s,f) =
	(* applies f in the order in which elements were entered into bucket *)
        let fun bucket (elem::rest) = (bucket rest; f(elem))
	      | bucket nil = ()
	    fun table i = (bucket(s sub i); table(i+1))
         in table 0 handle Subscript => ()
        end

end;
