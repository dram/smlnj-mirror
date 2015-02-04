(* symtab.sml *)

signature LEX_SYMBOL_TABLE = sig
    structure S: SYMBOL and T: TOKEN and B:BYTE_ARRAY
    val LookupSymbol: B.byte_array * int -> S.symbol
    val StringToSymbol: string -> S.symbol
    val LexClass : S.symbol -> T.token
end;

structure SymbolTable: LEX_SYMBOL_TABLE = struct

    structure S = Symbol;
    structure T = Token;
    structure B = Byte_array;
  local
    open Byte_array;

  in

    val HashFactor = 5
    and TableSize = 211

    val Table: S.symbol list array = array(TableSize, nil)

    fun EqByte_arrayString (buf: byte_array, count: int, str: string) =
	(count = length str) andalso
	let fun compare (i) =
		if i = count then true
		else if fetch(buf,i) <> ordof(str,i) then false
		else compare(i+1)
	in  compare 0 end;

    fun HashBuffer(buffer: byte_array, nchars: int) : int =
	let val i = ref 0
	    and n = ref 0
	in  while !i < nchars do
		(n := (HashFactor * !n + fetch(buffer, !i)) mod TableSize;
		 i := !i + 1);
	    !n
	end;

    fun HashString(str: string) : int =
	let val i = ref 0
	    and n = ref 0
	    and nchars = length str
	in  while !i < nchars do
		(n := (HashFactor * !n + ordof(str, !i)) mod TableSize;
		 i := !i + 1);
	    !n
	end;

    fun LookupSymbol(buffer: byte_array, nchars: int) : S.symbol =
	let val n = HashBuffer(buffer, nchars)
	    val bucket = Table sub n
	    fun search nil = 
		let val new = S.New(extract(buffer,0,nchars))
		in  update(Table, n, new::bucket);
		    new
		end
	      | search (s::symbols) = 
		if EqByte_arrayString(buffer, nchars, S.Name s)
		    then s
		    else search symbols
	in  search bucket end;


  (* debugging aid -- find symbol given string *)


    fun StringToSymbol(str: string) : S.symbol =
	let val n = HashString str
	    fun search nil = 
		 let val s = S.New(str)
		  in update(Table, n, s :: (Table sub n));
		     s
		 end
	      | search (s::symbols) = 
		if str = S.Name s
		    then s
		    else search symbols
	in  search (Table sub n)
	end;

  (* symbol table initialization *)

    val reserved = array(50,T.ID);


    fun InitSymbol(str, lexclass) =
	update(reserved, S.Number(StringToSymbol(str)), lexclass)

    fun LexClass(s) = 
	    reserved sub S.Number(s)
	   handlex subscript => T.ID

    val _ =
       (InitSymbol("and", T.AND);
	InitSymbol("->", T.ARROW);
	InitSymbol("as", T.AS);
	InitSymbol("*", T.ASTERISK);
	InitSymbol("|", T.BAR);
	InitSymbol("||", T.BARBAR);
	InitSymbol(":", T.COLON);
	InitSymbol("case", T.CASE);
	InitSymbol("datatype", T.DATATYPE);
	InitSymbol("...", T.DOTDOTDOT);
	InitSymbol("else", T.ELSE);
	InitSymbol("end", T.END);
	InitSymbol("=", T.EQUAL);
	InitSymbol("exceptionx", T.EXCEPTIONX);
	InitSymbol("exception", T.EXCEPTION);
	InitSymbol("do", T.DO);
	InitSymbol("=>", T.DARROW);
	InitSymbol("fn", T.FN);
	InitSymbol("fun", T.FUN);
	InitSymbol("functor", T.FUNCTOR);
	InitSymbol("handle", T.HANDLE);
	InitSymbol("handlex", T.HANDLEX);
	InitSymbol("if", T.IF);
	InitSymbol("in", T.IN);
	InitSymbol("infix", T.INFIX);
	InitSymbol("infixr", T.INFIXR);
	InitSymbol("let", T.LET);
	InitSymbol("local", T.LOCAL);
	InitSymbol("nonfix", T.NONFIX);
	InitSymbol("of", T.OF);
	InitSymbol("op", T.OP);
	InitSymbol("open", T.OPEN);
	InitSymbol("overload", T.OVERLOAD);
	InitSymbol("?", T.QUERY);
	InitSymbol("raise", T.RAISE);
	InitSymbol("raisex", T.RAISEX);
	InitSymbol("rec", T.REC);
	InitSymbol("sharing", T.SHARING);
	InitSymbol("sig", T.SIG);
	InitSymbol("signature", T.SIGNATURE);
	InitSymbol("struct", T.STRUCT);
	InitSymbol("structure", T.STRUCTURE);
	InitSymbol("then", T.THEN);
	InitSymbol("type", T.TYPE);
	InitSymbol("val", T.VAL);
	InitSymbol("while", T.WHILE);
	InitSymbol("_", T.WILD);
	InitSymbol("with", T.WITH);
        InitSymbol("orelse", T.ORELSE);
	InitSymbol("andalso", T.ANDALSO);
	())

end; (* local *)

end; (* SymbolTable *)


(* following dummy structure declaration is necessary to work around
   a bug in structures 

structure SymbolTable = struct
    structure S = Symbol
    val LookupSymbol = LookupSymbol
    val StringToSymbol = StringToSymbol
end;

*)
