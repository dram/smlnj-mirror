signature SYMBOLS = sig
    structure Symbol : SYMBOL 
    structure Token : TOKEN
    val stringToSymbol : string -> Symbol.symbol
    val lexClass : Symbol.symbol -> Token.token
    val ASTERISKsym : Symbol.symbol
    val EQUALsym : Symbol.symbol
end

structure Symbols: SYMBOLS = struct

    structure Symbol = Symbol
    structure Token = Token

    val hash = 256
    and size = 1597

    val table : Symbol.symbol list array = array(size, nil)

    fun hashString(str: string) : int =
        let fun loop (0,n) = n
	      | loop (i,n) = let val i = i-1
			     in loop (i,(hash * n + ordof(str,i)) mod size)
			     end
	in loop (length str,0)
	end

    fun stringToSymbol(str: string) : Symbol.symbol =
	let val n = hashString str
	    val bucket = (table sub n)
	    fun search nil = let val s = Symbol.new str
			     in update(table, n, s :: bucket); s
			     end
	      | search (s::symbols) =
		if str = Symbol.name s then s else search symbols
	in search bucket
	end

    val reservedList =
       [("and", Token.AND),
        ("abstraction", Token.ABSTRACTION),
	("abstype", Token.ABSTYPE),
	("->", Token.ARROW),
	("as", Token.AS),
	("||", Token.BARBAR),
	("case", Token.CASE),
	("datatype", Token.DATATYPE),
	("...", Token.DOTDOTDOT),
	("else", Token.ELSE),
	("end", Token.END),
	("eqtype", Token.EQTYPE),
	("exception", Token.EXCEPTION),
	("do", Token.DO),
	("=>", Token.DARROW),
	("fn", Token.FN),
	("fun", Token.FUN),
	("functor", Token.FUNCTOR),
	("handle", Token.HANDLE),
	("if", Token.IF),
	("in", Token.IN),
	("include", Token.INCLUDE),
	("infix", Token.INFIX),
	("infixr", Token.INFIXR),
	("let", Token.LET),
	("local", Token.LOCAL),
	("nonfix", Token.NONFIX),
	("of", Token.OF),
	("op", Token.OP),
	("open", Token.OPEN),
	("overload", Token.OVERLOAD),
	("raise", Token.RAISE),
	("rec", Token.REC),
	("sharing", Token.SHARING),
	("sig", Token.SIG),
	("signature", Token.SIGNATURE),
	("struct", Token.STRUCT),
	("structure", Token.STRUCTURE),
	("then", Token.THEN),
	("type", Token.TYPE),
	("val", Token.VAL),
	("while", Token.WHILE),
	("with", Token.WITH),
	("withtype", Token.WITHTYPE),
        ("orelse", Token.ORELSE),
	("andalso", Token.ANDALSO)]

    val numres = length reservedList

    val reserved = array(numres, Token.AND)

    val _ = app (fn (name,tok) => 
                   update(reserved,Symbol.number(stringToSymbol name), tok))
                reservedList
     handle Subscript => ErrorMsg.impossible "symbols initialization"

    fun lexClass(s) =
	let val num = Symbol.number s
	in if num < numres then reserved sub num else Token.ID s
	end
 
    val ASTERISKsym = stringToSymbol "*"
    val EQUALsym = stringToSymbol "="

end (* structure Symbols *)
