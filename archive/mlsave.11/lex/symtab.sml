(* symtab.sml *)

signature LEX_SYMBOL_TABLE = sig
    structure Symbol: SYMBOL 
    structure Token: TOKEN
    val lookupSymbol: byte_array * int -> Symbol.symbol
    val stringToSymbol: string -> Symbol.symbol
    val lexClass : Symbol.symbol -> Token.token
end

structure SymbolTable: LEX_SYMBOL_TABLE = struct

    structure Symbol = Symbol
    structure Token = Token

    open Byte_array

    val hashFactor = 5
    and tableSize = 211

    val table: Symbol.symbol list array = array(tableSize, nil)

    fun eqByte_arrayString (buf: byte_array, count: int, str: string) =
	(count = length str) andalso
	let fun compare (i) =
		if i = count then true
		else if fetch(buf,i) <> ordof(str,i) then false
		else compare(i+1)
	in  compare 0
	end

    fun hashBuffer(buffer: byte_array, nchars: int) : int =
	let val i = ref 0
	    and n = ref 0
	in  while !i < nchars do
		(n := (hashFactor * !n + fetch(buffer, !i)) mod tableSize;
		 i := !i + 1);
	    !n
	end

    fun hashString(str: string) : int =
	let val i = ref 0
	    and n = ref 0
	    and nchars = length str
	in  while !i < nchars do
		(n := (hashFactor * !n + ordof(str, !i)) mod tableSize;
		 i := !i + 1);
	    !n
	end

    fun lookupSymbol(buffer: byte_array, nchars: int) : Symbol.symbol =
	let val n = hashBuffer(buffer, nchars)
	    val bucket = table sub n
	    fun search nil = 
		let val new = Symbol.new(extract(buffer,0,nchars))
		in  update(table, n, new::bucket);
		    new
		end
	      | search (s::symbols) = 
		if eqByte_arrayString(buffer, nchars, Symbol.name s)
		    then s
		    else search symbols
	in  search bucket
	end


  (* debugging aid -- find symbol given string *)
    fun stringToSymbol(str: string) : Symbol.symbol =
	let val n = hashString str
	    fun search nil = 
		 let val s = Symbol.new(str)
		  in update(table, n, s :: (table sub n));
		     s
		 end
	      | search (s::symbols) = 
		if str = Symbol.name s
		    then s
		    else search symbols
	in  search (table sub n)
	end

  (* symbol table initialization *)

    val reserved = array(80,Token.ID)


    fun initSymbol(str, lexclass) =
	update(reserved, Symbol.number(stringToSymbol(str)), lexclass)

    fun lexClass(s) = 
	    reserved sub Symbol.number(s)
	   handlex subscript => Token.ID

    val _ =
       (initSymbol("and", Token.AND);
        initSymbol("abstraction", Token.ABSTRACTION);
	initSymbol("abstype", Token.ABSTYPE);
	initSymbol("->", Token.ARROW);
	initSymbol("as", Token.AS);
	initSymbol("*", Token.ASTERISK);
	initSymbol("|", Token.BAR);
	initSymbol("||", Token.BARBAR);
	initSymbol(":", Token.COLON);
	initSymbol("case", Token.CASE);
	initSymbol("datatype", Token.DATATYPE);
	initSymbol("...", Token.DOTDOTDOT);
	initSymbol("else", Token.ELSE);
	initSymbol("end", Token.END);
	initSymbol("=", Token.EQUAL);
	initSymbol("exceptionx", Token.EXCEPTIONX);
	initSymbol("exception", Token.EXCEPTION);
	initSymbol("do", Token.DO);
	initSymbol("=>", Token.DARROW);
	initSymbol("fn", Token.FN);
	initSymbol("fun", Token.FUN);
	initSymbol("functor", Token.FUNCTOR);
	initSymbol("handle", Token.HANDLE);
	initSymbol("handlex", Token.HANDLEX);
	initSymbol("#",Token.HASH);
	initSymbol("if", Token.IF);
	initSymbol("in", Token.IN);
	initSymbol("infix", Token.INFIX);
	initSymbol("infixr", Token.INFIXR);
	initSymbol("let", Token.LET);
	initSymbol("local", Token.LOCAL);
	initSymbol("nonfix", Token.NONFIX);
	initSymbol("of", Token.OF);
	initSymbol("op", Token.OP);
	initSymbol("open", Token.OPEN);
	initSymbol("overload", Token.OVERLOAD);
	initSymbol("?", Token.QUERY);
	initSymbol("raise", Token.RAISE);
	initSymbol("raisex", Token.RAISEX);
	initSymbol("rec", Token.REC);
	initSymbol("sharing", Token.SHARING);
	initSymbol("sig", Token.SIG);
	initSymbol("signature", Token.SIGNATURE);
	initSymbol("struct", Token.STRUCT);
	initSymbol("structure", Token.STRUCTURE);
	initSymbol("then", Token.THEN);
	initSymbol("type", Token.TYPE);
	initSymbol("val", Token.VAL);
	initSymbol("while", Token.WHILE);
	initSymbol("_", Token.WILD);
	initSymbol("with", Token.WITH);
	initSymbol("withtype", Token.WITHTYPE);
        initSymbol("orelse", Token.ORELSE);
	initSymbol("andalso", Token.ANDALSO);
	())

end (* structure SymbolTable *)
