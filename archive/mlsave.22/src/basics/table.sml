(* table.sml *)

signature TABLE = sig
    structure Symbol : SYMBOL
    type 'a table
    exception Notfound_Table
    exception Next
    val new : unit -> '1a table
    val add : '2a table * (Symbol.symbol * '2a) -> unit
    val look : ('a -> 'b) -> ('a table * Symbol.symbol) -> 'b
    val pop : 'a table * Symbol.symbol -> (Symbol.symbol * 'a)
    val app: 'a table * ((Symbol.symbol * 'a) -> unit) -> unit
end

structure Table : TABLE = struct (* table -- symbol table hash tables *) 

    structure Symbol = Symbol

    type 'a table = (Symbol.symbol * 'a list ref) Intmap.intmap

    exception Notfound_Table
	  and Next

    fun new () = Intmap.new Notfound_Table

    fun add (s, binder as (id,v)) = 
	let val num = Symbol.number id
	in let val (_,r as ref l) = Intmap.map s num
	   in r := v::l
	   end
	   handle Notfound_Table => Intmap.add s (num,(id,ref [v]))
	end

    fun look test (s, id) =
	let val (_,ref l) = Intmap.map s (Symbol.number id)
	    fun look1 nil = raise Notfound_Table
	      | look1 (b::l) = test b handle Next => look1 l
	in look1 l
	end

    fun pop(s,i) =
	let val (_,r as ref (x::l)) = Intmap.map s (Symbol.number i)
	in (r := l;(i,x))
	end

    fun app(s,f) = Intmap.app (fn (_,(id,ref l)) => revapp (fn x => f(id,x)) l) s

end;
