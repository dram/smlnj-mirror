(* Copyright 1989 by AT&T Bell Laboratories *)
structure Symbol : SYMBOL = 
struct
    datatype symbol = SYMBOL of {name: string, number: int}
    fun eq(SYMBOL{name=a1,number=b1},SYMBOL{name=a2,number=b2}) = b1=b2 andalso a1=a2
    fun varSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name}
    fun tycSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 1}
    fun sigSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 2}
    fun strSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 3}
    fun fctSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 4}
    fun fixSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 5}
    fun labSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 6}
    fun tyvSymbol (name: string) =
	SYMBOL{name = name, number = StrgHash.hashString name + 7}
    fun var'n'fix name =
        let val h = StrgHash.hashString name
	 in (SYMBOL{name=name,number=h},SYMBOL{name=name,number=h+5})
	end
    fun name (SYMBOL{name,...}) = name
    fun number (SYMBOL{number,...}) = number
    fun clumsySymbol(number,name) = SYMBOL{name=name,number=number}
end
