(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.1.1.7  1999/04/17 16:22:28  monnier
 * version 110.11
 *
 * Revision 1.1.1.1  1998/04/08 18:40:16  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:38:05  george
 *   Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)
local

(* create parser *)

   structure LrVals = MlyaccLrValsFun(structure Token = LrParser.Token
				      structure Hdr = Header)
   structure Lex = LexMLYACC(structure Tokens = LrVals.Tokens
			     structure Hdr = Header)
   structure Parser = JoinWithArg(structure Lex=Lex
			         structure ParserData = LrVals.ParserData
			         structure LrParser= LrParser)
   structure ParseGenParser =
	   ParseGenParserFun(structure Parser = Parser
	                     structure Header = Header)

(* create structure for computing LALR table from a grammar *)

   structure MakeLrTable = mkMakeLrTable(structure IntGrammar =IntGrammar
				     structure LrTable = LrTable)

(* create structures for printing LALR tables:

   Verbose prints a verbose description of an lalr table
   PrintStruct prints an ML structure representing that is an lalr table *)

   structure Verbose = mkVerbose(structure Errs = MakeLrTable.Errs)
   structure PrintStruct =
       mkPrintStruct(structure LrTable = MakeLrTable.LrTable
		     structure ShrinkLrTable =
                          ShrinkLrTableFun(structure LrTable=LrTable))
in

(* returns function which takes a file name, invokes the parser on the file,
  does semantic checks, creates table, and prints it *)

   structure ParseGen = ParseGenFun(structure ParseGenParser = ParseGenParser
			            structure MakeTable = MakeLrTable
			            structure Verbose = Verbose
			            structure PrintStruct = PrintStruct
				    structure Absyn = Absyn)
end

