structure MlLrVals = MlLrValsFun(structure Token = LrParser.Token)
structure MlLex = MlLexFun(structure Tokens = MlLrVals.Tokens)
structure MlParser = JoinWithArg(structure Lex= MlLex
		                 structure LrParser = LrParser
		                 structure ParserData = MlLrVals.ParserData)

val parse = fn s =>
  let val dev = open_in s
      val stream = MlParser.makeLexer(fn i => input_line dev) ()
      val _ = MlLex.UserDeclarations.lineNum := 1
      val error = fn (e,i:int,_) => output std_out (s ^ "," ^
		  " line " ^ (makestring i) ^ ", Error: " ^ e ^ "\n")
  in MlParser.parse(30,stream,error,()) before close_in dev
  end

val keybd = fn () =>
  let val dev = std_in
      val stream = MlParser.makeLexer(fn i => input_line dev) ()
      val _ = MlLex.UserDeclarations.lineNum := 1
      val error = fn (e,i:int,_) => output std_out ("std_in," ^
		  " line " ^ (makestring i) ^ ", Error: " ^ e ^ "\n")
  in MlParser.parse(0,stream,error,())
  end
