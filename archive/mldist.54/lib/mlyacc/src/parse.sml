(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure ParseGenParser = 
 struct
      structure LrVals = MlyaccLrValsFun(structure Token = LrParser.Token)
      structure Lex = LexMLYACC(structure Tokens = LrVals.Tokens)
      structure Mlyacc = Join(structure Lex=Lex
			      structure ParserData = LrVals.ParserData
			      structure LrParser= LrParser)

      val parse = fn file =>
          let
	      val say = output std_out
	      val error = fn (s,i:int,_) =>
		 (say file; say ", line "; say (makestring i);
		  say ": Error: "; say s; say "\n")
	      val in_str = open_in file
	      val stream =  Mlyacc.makeLexer (input in_str)
	      val p = (Header.lineno := 1;
		       Lex.UserDeclarations.text :=[""];
		       Mlyacc.parse(15,stream,error,()))
	   in (close_in in_str; p)
	   end
  end;
