(* symbol.sml *)

signature SYMBOL = sig
    type symbol
    val eq: symbol * symbol -> bool
    and new: string -> symbol
    and name: symbol -> string
    and number: symbol -> int
end;

structure Symbol : SYMBOL = 
struct
    datatype symbol = symbol of {name: string, number: int};
    val nextNumber = ref (~1);	(* symbol counter *)
    fun eq (symbol{number=n1,...}, symbol{number=n2,...}) = (n1 = n2)
    fun new str = (inc nextNumber; symbol{name = str, number = !nextNumber})
    fun name (symbol{name,...}) = name
    fun number (symbol{number,...}) = number
end;
