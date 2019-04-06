structure Symbol : SYMBOL = 
struct
    datatype symbol = Symbol of {name: string, number: int}
    fun eq(s1:symbol,s2) = (s1 = s2)
    fun new (name: string, number: int) =
	Symbol{name = name, number = number}
    fun name (Symbol{name,...}) = name
    fun number (Symbol{number,...}) = number
end

