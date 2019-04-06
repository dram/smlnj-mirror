(* Copyright 1989 by AT&T Bell Laboratories *)
signature NEWPARSE = sig
  datatype parseResult = EOF (* end of file reached *)
		       | ERROR (* parsed successfully, but with syntactic
				  or semantic errors *)
		       | ABORT (* could not even parse to end of declaration *)
		       | PARSE of BareAbsyn.dec
  val parse : ErrorMsg.inputSource -> unit -> parseResult
end
structure NewParse : NEWPARSE =
struct 
    structure MLLrVals = MLLrValsFun(structure Token = LrParser.Token)
    structure Lex = MLLexFun(structure Tokens = MLLrVals.Tokens)
    structure MLP = JoinWithArg(structure ParserData = MLLrVals.ParserData
	       			structure Lex=Lex
	       			structure LrParser = LrParser)
    structure BareAbsyn = BareAbsyn

    open ErrorMsg

    fun debugmsg  (msg : string) =
	let val printit = !System.Control.debugging
	in  if printit then (print msg; print "\n")
	    else ();
	    printit
	end

    datatype parseResult = EOF (* end of file reached *)
		       | ERROR (* parsed successfully, but with syntactic
				  or semantic errors *)
		       | ABORT (* could not even parse to end of declaration *)
		       | PARSE of BareAbsyn.dec

    fun subtract_time (System.Timer.TIME{sec=sa,usec=ua},
		       System.Timer.TIME{sec=sb,usec=ub}) =
	System.Timer.TIME
       let val sc = sa - sb and uc = ua - ub
	in if uc < 0 then {sec=sc-1,usec=uc+1000000} else {sec=sc,usec=uc}
       end
	
  fun timemsg (s : string) =
      let val printit = !System.Control.timings
       in if printit then (print s; print "\n"; flush_out std_out) else ();
	  printit
      end

  val dummyEOF = MLLrVals.Tokens.EOF(0,0)
  val dummySEMI = MLLrVals.Tokens.SEMICOLON(0,0)

 fun parse (context as {sourceStream,errStream,interactive,
				linePos,lineNum,anyErrors,...}:inputSource) =
  let val lastLineNum = ref(!lineNum-1)

      val complain = ErrorMsg.error context

      fun parseerror(s,p1,p2) = complain (p1,p2) COMPLAIN s

      val lexarg = {comLevel = ref 0, lineNum = lineNum,
			linePos = linePos,
			charlist = ref (nil : string list),
			stringstart = ref 0,
                        err = complain}

      val doprompt = ref true
      val prompt = ref (!System.Control.primaryPrompt)

      exception Abort
      fun getline _ =(if !doprompt then (if !anyErrors then raise Abort
						       else ();
					 output errStream 
					  (if !(#comLevel lexarg) > 0
					     orelse !(#charlist lexarg) <> nil
						then !System.Control.secondaryPrompt
						else !prompt);
					 flush_out errStream;
					 doprompt := false)
				   else ();
		      let val s = input_line sourceStream
		      in doprompt := (ordof(s,size s - 1)=ord("\n")
				     handle Ord => false);
		         s
		      end)
     val lexer = Lex.makeLexer(if interactive then getline else input sourceStream)
		 lexarg
     val lexer' = ref(LrParser.Stream.streamify lexer)
     val lookahead = if interactive then 0 else 30

     fun oneparse() =
	 let val _ = prompt := !System.Control.primaryPrompt
	     val (nextToken,rest) = LrParser.Stream.get(!lexer')
	  in if MLP.sameToken(nextToken,dummySEMI) 
				then (lexer' := rest; oneparse())
	     else if MLP.sameToken(nextToken,dummyEOF) then EOF
	     else 
		let val _ = prompt := !System.Control.secondaryPrompt;
		    open System.Timer
		    val t1 = start_timer()
		    val (f, lexer'') = MLP.parse(lookahead, !lexer', parseerror,complain)
		    val t2 = check_timer t1
		    val lines = !lineNum - !lastLineNum
		    val _ = System.Stats.lines := !System.Stats.lines + lines;
		    val _ = timemsg("parse, " ^ 
			    	Integer.makestring lines
			    	^ " lines, " ^ makestring t2 ^"s")
			    orelse debugmsg "parse"
		    val _ = lexer' := lexer'';
		    val absyn = f()
		    val t3 = check_timer t1
		 in System.Stats.update(System.Stats.parse,t3);
		    timemsg("semantics, "^makestring(subtract_time(t3,t2))^"s")
			orelse debugmsg "semantics";
		    if !anyErrors then ERROR else PARSE absyn
	        end handle LrParser.ParseError => ABORT
			 | Abort => ABORT
			 | ErrorMsg.Syntax => ABORT
	  end

  in fn() => (lastLineNum := !lineNum; anyErrors := false; oneparse())
 end

end
