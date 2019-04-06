signature SYMBOLS = sig
    structure Symbol : SYMBOL 
    structure Token : TOKEN
    val stringToSymbol : string -> Symbol.symbol
    val ASTERISKsym : Symbol.symbol
    val EQUALsym : Symbol.symbol
end

structure Symbols: SYMBOLS = struct

    structure Symbol = Symbol
    structure Token = Token

    val tablesize = 1597

    val table : Symbol.symbol list array = array(tablesize, nil)

    fun stringToSymbol(str: string) : Symbol.symbol =
	let val n = StrgHash.hashString str
	    val i = n - tablesize * (n div tablesize)
	    val bucket = (table sub i)
	    fun search nil =
		  let val s = Symbol.new(str,n)
		   in update(table, i, s :: bucket); s
		  end
	      | search (sym::symbols) =
		if str = Symbol.name sym then sym else search symbols
	 in search bucket
	end

    val ASTERISKsym = stringToSymbol "*"
    val EQUALsym = stringToSymbol "="

end (* structure Symbols *)
