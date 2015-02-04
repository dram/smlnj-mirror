(* symbol.sml *)

signature SYMBOL = sig
    type symbol
    val Eq: symbol * symbol -> bool
    and New: string -> symbol
    and Name: symbol -> string
    and Number: symbol -> int
end;

structure Symbol : SYMBOL = 
struct
    datatype symbol = symbol of {name: string, number: int};
    val NextNumber = ref (~1);	(* symbol counter *)
    fun Eq (symbol{number=n1,...}, symbol{number=n2,...}) = (n1 = n2)
    fun New (str: string) =
	(NextNumber := !NextNumber + 1;
	 symbol{name = str, number = !NextNumber})
    fun Name (symbol{name,...}) = name
    fun Number (symbol{number,...}) = number
end;
