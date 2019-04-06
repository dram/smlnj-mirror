(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* base.sig: Base signature file for SML-Yacc.  This file contains signatures
   that must be loaded before any of the files produced by ML-Yacc are loaded
*)

(* STREAM: signature for a lazy stream.*)

signature STREAM =
 sig type 'xa stream
     val streamify : (unit -> '_a) -> '_a stream
     val cons : '_a * '_a stream -> '_a stream
     val get : '_a stream -> '_a * '_a stream
 end

(* LR_TABLE: signature for an LR Table.

   The list of actions and gotos passed to mkLrTable must be ordered by state
   number. The values for state 0 are the first in the list, the values for
    state 1 are next, etc.
*)

signature LR_TABLE =
    sig
	datatype state = STATE of int
	datatype term = T of int
	datatype nonterm = NT of int
	datatype action = SHIFT of state
			| REDUCE of int
			| ACCEPT
			| ERROR
	type table
	
	val numStates : table -> int
	val describeActions : table -> state ->
				((term * action) list) * action
	val describeGoto : table -> state -> (nonterm * state) list
	val action : table -> state * term -> action
	val goto : table -> state * nonterm -> state
	val initialState : table -> state
	exception Goto of state * nonterm

	val mkLrTable : {actions : (((term * action) list) * action) list,
			 gotos : (nonterm * state) list list,
			 numStates : int,
			 initialState : state} -> table
    end

(* TOKEN: signature revealing the internal structure of a token. This signature
   TOKEN distinct from the signature {parser name}_TOKENS produced by ML-Yacc.
   The {parser name}_TOKENS structures contain some types and functions to
    construct tokens from values and positions.

   The representation of token was very carefully chosen here to allow the
   polymorphic parser to work without knowing the types of semantic values
   or line numbers.

   This has had an impact on the TOKENS structure produced by SML-Yacc, which
   is a structure parameter to lexer functors.  We would like to have some
   type 'a token which functions to construct tokens would create.  A
   constructor function for a integer token might be

	  INT: int * 'a * 'a -> 'a token.
 
   This is not possible because we need to have tokens with the representation
   given below for the polymorphic parser.

   Thus our constructur functions for tokens have the form:

	  INT: int * 'a * 'a -> (svalue,'a) token

   This in turn has had an impact on the signature that lexers for SML-Yacc
   must match and the types that a user must declare in the user declarations
   section of lexers.
*)

signature TOKEN =
    sig
	structure LrTable : LR_TABLE
        datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	val sameToken : ('a,'b) token * ('a,'b) token -> bool
    end

(* LR_PARSER: signature for a polymorphic LR parser *)

signature LR_PARSER =
    sig
	structure Stream: STREAM
	structure LrTable : LR_TABLE
	structure Token : TOKEN

	sharing LrTable = Token.LrTable

	exception ParseError

	val parse : {table : LrTable.table,
		     lexer : ('_b,'_c) Token.token Stream.stream,
		     arg: 'arg,
		     saction : int *
			       '_c *
				(LrTable.state * ('_b * '_c * '_c)) list * 
				'arg ->
				     LrTable.nonterm *
				     ('_b * '_c * '_c) *
				     ((LrTable.state *('_b * '_c * '_c)) list),
		     void : '_b,
		     ec : { is_keyword : LrTable.term -> bool,
			    noShift : LrTable.term -> bool,
			    preferred_subst : LrTable.term -> LrTable.term list,
			    preferred_insert : LrTable.term -> bool,
			    errtermvalue : LrTable.term -> '_b,
			    showTerminal : LrTable.term -> string,
			    terms: LrTable.term list,
			    error : string * '_c * '_c -> unit
			   },
		     lookahead : int  (* max amount of lookahead used in *)
				      (* error correction *)
			} -> '_b *
			     (('_b,'_c) Token.token Stream.stream)
    end

(* LEXER: a signature that most lexers produced for use with SML-Yacc's
   output will match.  The user is responsible for declaring type token,
   type pos, and type svalue in the UserDeclarations section of a lexer.

   Note that type token is abstract in the lexer.  This allows SML-Yacc to
   create a TOKENS signature for use with lexers produced by ML-Lex that
   treats the type token abstractly.  Lexers that are functors parametrized by
   a Tokens structure matching a TOKENS signature cannot examine the structure
   of tokens.
*)

signature LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
	   end
	val makeLexer : (int -> string) -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* ARG_LEXER: the %arg option of ML-Lex allows users to produce lexers which
   also take an argument before yielding a function from unit to a token
*)

signature ARG_LEXER =
   sig
       structure UserDeclarations :
	   sig
	        type ('a,'b) token
		type pos
		type svalue
		type arg
	   end
	val makeLexer : (int -> string) -> UserDeclarations.arg -> unit -> 
         (UserDeclarations.svalue,UserDeclarations.pos) UserDeclarations.token
   end

(* PARSER_DATA: the signature of ParserData structures in {parser name}LrValsFun
   produced by  SML-Yacc.  All such structures match this signature.  

   The {parser name}LrValsFun produces a structure which contains all the values
   except for the lexer needed to call the polymorphic parser mentioned
   before.

*)

signature PARSER_DATA =
   sig
        (* the type of line numbers *)

	type pos

	(* the type of semantic values *)

	type svalue

         (* the type of the user-supplied argument to the parser *)
 	type arg
 
	(* the intended type of the result of the parser.  This value is
	   produced by applying extract from the structure Actions to the
	   final semantic value resultiing from a parse.
	 *)

	type result

	structure LrTable : LR_TABLE
	structure Token : TOKEN
	sharing Token.LrTable = LrTable

	(* structure Actions contains the functions which mantain the
	   semantic values stack in the parser.  Void is used to provide
	   a default value for the semantic stack.
	 *)

	structure Actions : 
	  sig
	      val actions : int * pos *
		   (LrTable.state * (svalue * pos * pos)) list * arg->
		         LrTable.nonterm * (svalue * pos * pos) *
			 ((LrTable.state *(svalue * pos * pos)) list)
	      val void : svalue
	      val extract : svalue -> result
	  end

	(* structure EC contains information used to improve error
	   recovery in an error-correcting parser *)

	structure EC :
	   sig
     val is_keyword : LrTable.term -> bool
	     val noShift : LrTable.term -> bool
	     val preferred_subst : LrTable.term -> LrTable.term list
	     val preferred_insert : LrTable.term -> bool
	     val errtermvalue : LrTable.term -> svalue
	     val showTerminal : LrTable.term -> string
	     val terms: LrTable.term list
	   end

	(* table is the LR table for the parser *)

	val table : LrTable.table
    end

(* signature PARSER is the signature that most user parsers created by 
   SML-Yacc will match.
*)

signature PARSER =
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	(* type pos is the type of line numbers *)

	type pos

	(* type result is the type of the result from the parser *)

	type result

         (* the type of the user-supplied argument to the parser *)
 	type arg
	
	(* type svalue is the type of semantic values for the semantic value
	   stack
	 *)

	type svalue

	(* val makeLexer is used to create a stream of tokens for the parser *)

	val makeLexer : (int -> string) ->
			 (svalue,pos) Token.token Stream.stream

	(* val parse takes a stream of tokens and a function to print
	   errors and returns a value of type result and a stream containing
	   the unused tokens
	 *)

	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end

(* signature ARG_PARSER is the signature that will be matched by parsers whose
    lexer takes an additional argument.
*)

signature ARG_PARSER = 
    sig
        structure Token : TOKEN
	structure Stream : STREAM
	exception ParseError

	type arg
	type lexarg
	type pos
	type result
	type svalue

	val makeLexer : (int -> string) -> lexarg ->
			 (svalue,pos) Token.token Stream.stream
	val parse : int * ((svalue,pos) Token.token Stream.stream) *
		    (string * pos * pos -> unit) * arg ->
				result * (svalue,pos) Token.token Stream.stream

	val sameToken : (svalue,pos) Token.token * (svalue,pos) Token.token ->
				bool
     end

(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* Stream: a structure implementing a lazy stream.  The signature STREAM
   is found in base.sig *)

abstraction Stream : STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
	    let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure LrTable : LR_TABLE = 
    struct
	datatype ('a,'b) entryList = EMPTY
				   | ENTRY of 'a * 'b * ('a,'b) entryList
	fun entryToList EMPTY = nil
	  | entryToList(ENTRY(key,data,rest)) = (key,data) :: (entryToList rest)
	fun listToEntry nil = EMPTY
	  | listToEntry ((key,data) :: b) = ENTRY(key,data,listToEntry b)
	datatype term = T of int
	datatype nonterm = NT of int
	datatype state = STATE of int
	datatype action = SHIFT of state
			| REDUCE of int (* rulenum from grammar *)
			| ACCEPT
			| ERROR
	exception Goto of state * nonterm
	type table = {states: int, initialState: state,
		      action: ((term,action) entryList * action) array,
		      goto :  (nonterm,state) entryList array}
	val numStates = fn ({states,...} : table) => states
	val describeActions =
	   fn ({action,...} : table) =>
	      fn STATE state =>
		  (fn (a,b) => (entryToList a,b)) (action sub state)
	val describeGoto =
	   fn ({goto,...} : table) =>
	      fn STATE state => entryToList(goto sub state)
	fun findTerm (T term,row,default) =
	    let fun find (ENTRY (T key,data,r)) =
		       if key < term then find r
		       else if key=term then data
		       else default
		   | find EMPTY = default
	    in find row
	    end
	fun findNonterm (NT nt,row) =
	    let fun find (ENTRY (NT key,data,r)) =
		       if key < nt then find r
		       else if key=nt then SOME data
		       else NONE
		   | find EMPTY = NONE
	    in find row
	    end
	val action = fn ({action,...} : table) =>
		fn (STATE state,term) =>
		  let val (row,default) = action sub state
		  in findTerm(term,row,default)
		  end
	val goto = fn ({goto,...} : table) =>
			fn (a as (STATE state,nonterm)) =>
			  case findNonterm(nonterm,goto sub state)
			  of SOME state => state
			   | NONE => raise (Goto a)
	val initialState = fn ({initialState,...} : table) => initialState

	val mkLrTable = fn {actions,gotos,initialState,numStates} =>
	     ({action=arrayoflist(map (fn (a,b) => (listToEntry a,b)) actions),
	       goto=arrayoflist(map listToEntry gotos),
	       states=numStates,
               initialState=initialState} : table)
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* functor Join creates a user parser by putting together a Lexer structure,
   an LrValues structure, and a polymorphic parser structure.  Note that
   the Lexer and LrValues structure must share the type pos (i.e. the type
   of line numbers), the type svalues for semantic values, and the type
   of tokens.
*)

functor Join(structure Lex : LEXER
	     structure ParserData: PARSER_DATA
	     structure LrParser : LR_PARSER
	     sharing ParserData.LrTable = LrParser.LrTable
	     sharing ParserData.Token = LrParser.Token
	     sharing type Lex.UserDeclarations.svalue = ParserData.svalue
	     sharing type Lex.UserDeclarations.pos = ParserData.pos
	     sharing type Lex.UserDeclarations.token = ParserData.Token.token)
		 : PARSER =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream
 
    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue
    val makeLexer = LrParser.Stream.streamify o Lex.makeLexer
    val parse = fn (lookahead,lexer,error,arg) =>
	(fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
	        lexer=lexer,
		lookahead=lookahead,
		saction = ParserData.Actions.actions,
		arg=arg,
		void= ParserData.Actions.void,
	        ec = {is_keyword = ParserData.EC.is_keyword,
		      noShift = ParserData.EC.noShift,
		      preferred_subst = ParserData.EC.preferred_subst,
		      preferred_insert= ParserData.EC.preferred_insert,
		      errtermvalue = ParserData.EC.errtermvalue,
		      error=error,
		      showTerminal = ParserData.EC.showTerminal,
		      terms = ParserData.EC.terms}}
      )
     val sameToken = Token.sameToken
end

(* functor JoinWithArg creates a variant of the parser structure produced 
   above.  In this case, the makeLexer take an additional argument before
   yielding a value of type unit -> (svalue,pos) token
 *)

functor JoinWithArg(structure Lex : ARG_LEXER
	     structure ParserData: PARSER_DATA
	     structure LrParser : LR_PARSER
	     sharing ParserData.LrTable = LrParser.LrTable
	     sharing ParserData.Token = LrParser.Token
	     sharing type Lex.UserDeclarations.svalue = ParserData.svalue
	     sharing type Lex.UserDeclarations.pos = ParserData.pos
	     sharing type Lex.UserDeclarations.token = ParserData.Token.token)
		 : ARG_PARSER  =
struct
    structure Token = ParserData.Token
    structure Stream = LrParser.Stream

    exception ParseError = LrParser.ParseError

    type arg = ParserData.arg
    type lexarg = Lex.UserDeclarations.arg
    type pos = ParserData.pos
    type result = ParserData.result
    type svalue = ParserData.svalue

    val makeLexer = fn s => fn arg =>
		 LrParser.Stream.streamify (Lex.makeLexer s arg)
    val parse = fn (lookahead,lexer,error,arg) =>
	(fn (a,b) => (ParserData.Actions.extract a,b))
     (LrParser.parse {table = ParserData.table,
	        lexer=lexer,
		lookahead=lookahead,
		saction = ParserData.Actions.actions,
		arg=arg,
		void= ParserData.Actions.void,
	        ec = {is_keyword = ParserData.EC.is_keyword,
		      noShift = ParserData.EC.noShift,
		      preferred_subst = ParserData.EC.preferred_subst,
		      preferred_insert= ParserData.EC.preferred_insert,
		      errtermvalue = ParserData.EC.errtermvalue,
		      error=error,
		      showTerminal = ParserData.EC.showTerminal,
		      terms = ParserData.EC.terms}}
      )
    val sameToken = Token.sameToken
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser.sml:  This is a parser driver for LR tables with an error-recovery
   routine added to it.  The routine used is described in detail in this
   article:

	'A Practical Method for LR and LL Syntactic Error Diagnosis and
	 Recovery', by M. Burke and G. Fisher, ACM Transactions on
	 Programming Langauges and Systems, Vol. 9, No. 2, April 1987,
	 pp. 164-197.

    This program is an implementation is the partial, deferred method discussed
    in the article.  The algorithm and data structures used in the program
    are described below.  

    This program assumes that all semantic actions are delayed.  A semantic
    action should produce a function from unit -> value instead of producing the
    normal value.  The parser returns the semantic value on the top of the
    stack when accept is encountered.  The user can deconstruct this value
    and apply the unit -> value function in it to get the answer.

    It also assumes that the lexer is a lazy stream.

    Data Structures:
    ----------------
	
	* The parser:

	   The state stack has the type

		 (state * (semantic value * line # * line #)) list

	   The parser keeps a queue of (state stack * lexer pair).  A lexer pair
	 consists of a terminal * value pair and a lexer.  This allows the 
	 parser to reconstruct the states for terminals to the left of a
	 syntax error, and attempt to make error corrections there.

	   The queue consists of a pair of lists (x,y).  New additions to
	 the queue are cons'ed onto y.  The first element of x is the top
	 of the queue.  If x is nil, then y is reversed and used
	 in place of x.

    Algorithm:
    ----------

	* The steady-state parser:  

	    This parser keeps the length of the queue of state stacks at
	a steady state by always removing an element from the front when
	another element is placed on the end.

	    It has these arguments:

	   stack: current stack
	   queue: value of the queue
	   lexPair ((terminal,value),lex stream)

	When SHIFT is encountered, the state to shift to and the value are
	are pushed onto the state stack.  The state stack and lexPair are
	placed on the queue.  The front element of the queue is removed.

	When REDUCTION is encountered, the rule is applied to the current
	stack to yield a triple (nonterm,value,new stack).  A new
	stack is formed by adding (goto(top state of stack,nonterm),value)
	to the stack.

	When ACCEPT is encountered, the top value from the stack and the
	lexer are returned.

	When an ERROR is encountered, fixError is called.  FixError
	takes the arguments to the parser, fixes the error if possible and
        returns a new set of arguments.

	* The distance-parser:

	This parser includes an additional argument distance.  It pushes
	elements on the queue until it has parsed distance tokens, or an
	ACCEPT or ERROR occurs.  It returns a stack, lexer, the number of
	tokens left unparsed, a queue, and an action option.
*)

signature FIFO = 
  sig type 'a queue
      val empty : 'a queue
      exception Empty
      val get : 'a queue -> 'a * 'a queue
      val put : 'a * 'a queue -> 'a queue
  end

(* drt (12/15/89) -- the functor should be used in development work, but
   it wastes space in the release version.

functor ParserGen(structure LrTable : LR_TABLE
		  structure Stream : STREAM) : LR_PARSER =
*)

abstraction LrParser : LR_PARSER =
   struct
      structure LrTable = LrTable
      structure Stream = Stream

      structure Token : TOKEN =
	struct
	    structure LrTable = LrTable
	    datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
	    val sameToken = fn (TOKEN(t,_),TOKEN(t',_)) => t=t'
        end

      open LrTable
      open Token

      val DEBUG1 = false
      val DEBUG2 = false
      exception ParseError
      exception ParseImpossible of int

      abstraction Fifo : FIFO =
        struct
	  type 'a queue = ('a list * 'a list)
	  val empty = (nil,nil)
	  exception Empty
	  fun get(a::x, y) = (a, (x,y))
	    | get(nil, nil) = raise Empty
	    | get(nil, y) = get(rev y, nil)
 	  fun put(a,(x,y)) = (x,a::y)
        end

      type ('a,'b) elem = (state * ('a * 'b * 'b))
      type ('a,'b) stack = ('a,'b) elem list
      type ('a,'b) lexv = ('a,'b) token
      type ('a,'b) lexpair = ('a,'b) lexv * (('a,'b) lexv Stream.stream)
      type ('a,'b) distanceParse =
		 ('a,'b) lexpair *
		 ('a,'b) stack * 
		 (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		 int ->
		   ('a,'b) lexpair *
		   ('a,'b) stack * 
		   (('a,'b) stack * ('a,'b) lexpair) Fifo.queue *
		   int *
		   action option

      type ('a,'b) ecRecord =
	 {is_keyword : term -> bool,
          preferred_subst : term -> term list,
	  preferred_insert : term -> bool,
	  error : string * 'b * 'b -> unit,
	  errtermvalue : term -> 'a,
	  terms : term list,
	  showTerminal : term -> string,
	  noShift : term -> bool}

      local 
	 val print = fn s => output(std_out,s)
	 val println = fn s => (print s; print "\n")
	 val showState = fn (STATE s) => "STATE " ^ (makestring s)
      in
        fun printStack(stack: ('a,'b) stack, n: int) =
         case stack
           of (state,_) :: rest =>
                 (print("\t" ^ makestring n ^ ": ");
                  println(showState state);
                  printStack(rest, n+1))
            | nil => ()
                
        fun prAction showTerminal
		 (stack as (state,_) :: _, next as (TOKEN (term,_),_), action) =
             (println "Parse: state stack:";
              printStack(stack, 0);
              print("       state="
                         ^ showState state	
                         ^ " next="
                         ^ showTerminal term
                         ^ " action="
                        );
              case action
                of SHIFT state => println ("SHIFT " ^ (showState state))
                 | REDUCE i => println ("REDUCE " ^ (makestring i))
                 | ERROR => println "ERROR"
		 | ACCEPT => println "ACCEPT")
        | prAction _ (_,_,action) = ()
     end

    (* ssParse: parser which maintains the queue of (state * lexvalues) in a
	steady-state.  It takes a table, showTerminal function, saction
	function, and fixError function.  It parses until an ACCEPT is
	encountered, or an exception is raised.  When an error is encountered,
	fixError is called with the arguments of parseStep (lexv,stack,and
	queue).  It returns the lexv, and a new stack and queue adjusted so
	that the lexv can be parsed *)
	
    val ssParse =
      fn (table,showTerminal,saction,fixError,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(args as
			 (lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				      ),
			  stack as (state,_) :: _,
			  queue)) =
	      let val nextAction = action (state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		      val (_,newQueue) =Fifo.get(Fifo.put((newStack,newLexPair),
							    queue))
		  in parseStep(newLexPair,(s,value)::stack,newQueue)
		  end
		 | REDUCE i =>
		     (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		          parseStep(lexPair,(goto(state,nonterm),value)::stack,
				    queue)
		       | _ => raise (ParseImpossible 197))
		 | ERROR => parseStep(fixError args)
		 | ACCEPT => 
			(case stack
			 of (_,(topvalue,_,_)) :: _ =>
				let val (token,restLexer) = lexPair
				in (topvalue,Stream.cons(token,restLexer))
				end
			  | _ => raise (ParseImpossible 202))
	      end
	    | parseStep _ = raise (ParseImpossible 204)
	in parseStep
	end

    (*  distanceParse: parse until n tokens are shifted, or accept or
	error are encountered.  Takes a table, showTerminal function, and
	semantic action function.  Returns a parser which takes a lexPair
	(lex result * lexer), a state stack, a queue, and a distance
	(must be > 0) to parse.  The parser returns a new lex-value, a stack
	with the nth token shifted on top, a queue, a distance, and action
	option. *)

    val distanceParse =
      fn (table,showTerminal,saction,arg) =>
	let val prAction = prAction showTerminal
	    val action = LrTable.action table
	    val goto = LrTable.goto table
	    fun parseStep(lexPair,stack,queue,0) = (lexPair,stack,queue,0,NONE)
	      | parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),
				      lexer
				     ),
			  stack as (state,_) :: _,
			  queue,distance) =
	      let val nextAction = action(state,terminal)
	          val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
			  else ()
	      in case nextAction
		 of SHIFT s =>
		  let val newStack = (s,value) :: stack
		      val newLexPair = Stream.get lexer
		  in parseStep(newLexPair,(s,value)::stack,
			       Fifo.put((newStack,newLexPair),queue),distance-1)
		  end
		 | REDUCE i =>
		    (case saction(i,leftPos,stack,arg)
		      of (nonterm,value,stack as (state,_) :: _) =>
		         parseStep(lexPair,(goto(state,nonterm),value)::stack,
				 queue,distance)
		      | _ => raise (ParseImpossible 240))
		 | ERROR => (lexPair,stack,queue,distance,SOME nextAction)
		 | ACCEPT => (lexPair,stack,queue,distance,SOME nextAction)
	      end
	   | parseStep _ = raise (ParseImpossible 242)
	in parseStep : ('_a,'_b) distanceParse 
	end

(* mkFixError: function to create fixError function which adjusts parser state
   so that parse may continue in the presence of an error *)

val mkFixError = fn ({is_keyword,preferred_subst,terms,errtermvalue,
		      preferred_insert,noShift,
		      showTerminal,error,...} : ('_a,'_b) ecRecord,
		      distanceParse : ('_a,'_b) distanceParse,
		      minAdvance,maxAdvance) =>
  let fun FixError(lexv as (TOKEN (term,value as (_,leftPos,_)),_),
		   stack,queue) =
    let val lexVList = map (fn t => TOKEN (t,(errtermvalue t,leftPos,leftPos)))
		       terms
	val _ = if DEBUG2 then
			error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos)
		else ()

	val minDelta = 3

	(* pull all the state * lexv elements from the queue *)

	val stateList = 
	   let fun f q = let val (elem,newQueue) = Fifo.get q
			 in elem :: (f newQueue)
			 end handle Fifo.Empty => nil
	   in f queue
	   end

	(* now number elements of stateList, giving distance from
	   error token *)

	val (_,numStateList) = List.fold (fn (a,(num,r)) => (num+1,(a,num)::r))
				stateList (0,nil)

	(* Represent the set of potential changes as a linked list.

	   Values of datatype Change hold information about a potential change.

	   oper = oper to be applied
	   pos = the # of the element in stateList that would be altered.
	   distance = the number of tokens beyond the error token which the
	     change allows us to parse.
	   new = new terminal * value pair at that point
	   orig = original terminal * value pair at the point being changed.
	 *)

	datatype oper = INSERT | DELETE  | SUBST
	datatype ('a,'b) change = CHANGE of
	   {oper : oper, pos : int, distance : int,
	    new : ('a,'b) lexv, orig : ('a,'b) lexv}

	val operToString = 
	       fn INSERT => "INSERT "
		| SUBST  => "SUBST "
		| DELETE => "DELETE "

	 val printChange = fn c =>
	  let val CHANGE {oper,distance,new=TOKEN (t,_),
			  orig=TOKEN (t',_),pos,...} = c
	  in (print ("{distance= " ^ (makestring distance));
	      print (",orig = " ^ (showTerminal t'));
	      print (",new = " ^ (showTerminal t));
	      print (",oper= " ^ (operToString oper));
	      print (",pos= " ^ (makestring pos));
	      print "}\n")
	  end

	val printChangeList = app printChange

(* parse: given a lexPair, a stack, and the distance from the error
   token, return the distance past the error token that we are able to parse.*)

	fun parse (lexPair,stack,queuePos : int) =
	    let val (_,_,_,distance,action) =
		  distanceParse(lexPair,stack,Fifo.empty,queuePos+maxAdvance+1)
	    in maxAdvance - distance - 1
	    end

(* foldStateList: accumulates results while scanning numStateList *)


	fun foldStateList f start = List.fold f numStateList start

(* foldLexVList: accumulates results while scanning lexVList *)

	fun foldLexVList f start = List.fold f lexVList start

(* deleteFold: function which accumulates results of deleting the
   current terminal.  Does not delete the current terminal if that terminal
   cannot be shifted *)

	val deleteFold =
		fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),
			queuePos),r) =>
		 if noShift term then r
		 else
		   let val newLexPair as (new,_) = Stream.get lexer
		       val distance = parse(newLexPair,stack,queuePos-1)
		   in if distance >= minAdvance then
			CHANGE {pos=queuePos,distance=distance,orig=orig,
				new=new,oper=DELETE} :: r
		      else r
		   end


(* insertFold: accumulate results of trying to insert tokens before
   the current terminal *)

	val insertFold =
	   fn (((stack,lexPair as (orig,lexer)),queuePos),r) =>
	    let val lexer = Stream.cons lexPair
	    in foldLexVList (fn (newLexV,r) =>
		let val distance = parse((newLexV,lexer),stack,queuePos+1)
		in if distance >= minAdvance
			 then CHANGE{pos=queuePos,distance=distance,orig=orig,
					new=newLexV,oper=INSERT} :: r
			 else r
		end) r
	    end

(* substFold: accumulate results of deleting the current terminal
   and then trying to insert tokens *)

	val substFold =
	    fn (((stack,lexPair as (orig as TOKEN (term,_),lexer)),queuePos),
		r) =>
	      if noShift term then r
	      else
		  foldLexVList (fn (newLexV,r) =>
		   let val distance = parse((newLexV,lexer),stack,queuePos)
		   in if distance >= minAdvance then
			   CHANGE{pos=queuePos,distance=distance,orig=orig,
				  new=newLexV,oper=SUBST} :: r
		     else r
		   end) r

	val changes = (foldStateList insertFold nil) @
			  (foldStateList substFold nil) @
				(foldStateList deleteFold nil)

	val findMaxDist = fn l => 
	  fold (fn (CHANGE {distance,...},high) => max(distance,high)) l 0

(* maxDist: max distance past error taken that we could parse *)

	val maxDist = findMaxDist changes

(* sieve: keep only the elements of a list for which pred is true *)

	val sieve = fn pred => fn l => 
	  fold (fn (elem,rest) => if pred elem then elem::rest else rest) l nil

(* remove changes which did not parse maxDist tokens past the error token *)

	val changes = sieve (fn CHANGE{distance=a,...} => a = maxDist) changes

(* Find preferred elements *)

        val preferredInsertChanges =
		sieve (fn CHANGE {new=TOKEN (term,_),oper=INSERT,...} => 
				 preferred_insert term
		        | _ => false) changes

        val preferredSubstChanges =
		sieve
		    (fn CHANGE {new=TOKEN(t,_),orig=TOKEN (t',_),
				oper=SUBST,...} =>
			  List.exists (fn a => a =t) (preferred_subst t')
		      | _ => false) changes

        val _ = if DEBUG2 then
	    (print "preferred insert:\n";
	     printChangeList preferredInsertChanges;
	     print "preferred subst:\n";
	     printChangeList preferredSubstChanges
	    ) else ()

(* Remove keywords which don't meet the long parse check
   (minAdvance+minDelta) *)

         val changes =
	    sieve (fn CHANGE {new=TOKEN (term,_),distance,...} =>
		(not (is_keyword term) orelse distance >= minAdvance+minDelta))
			changes


         val changes =
	       preferredInsertChanges @ (preferredSubstChanges @ changes)

         in case changes 
	     of (l as _ :: _) =>
	        let fun print_msg (CHANGE {new=TOKEN (term,_),oper,
					   orig=TOKEN (t',(_,leftPos,rightPos)),
					   ...}) =
		     let val s = 
		       case oper
			 of DELETE => "deleting " ^ (showTerminal t')
			  | INSERT => "inserting " ^ (showTerminal term)
		          | SUBST => "replacing " ^ (showTerminal t') ^
				   " with " ^ (showTerminal term)
		     in error ("syntax error: " ^ s,leftPos,rightPos)
		     end
		   
		   val a = 
		     (if length l > 1 andalso DEBUG2 then
			(print "multiple fixes possible; could fix it by:\n";
		 	 map print_msg l;
		 	 print "chosen correction:\n")
		      else ();
		      print_msg (hd l); (hd l))

		    (* findNth: find nth queue entry from the error
		       entry.  Returns the Nth queue entry and the  portion of
		       the queue from the beginning to the nth-1 entry.  The
		       error entry is at the end of the queue.

			Examples:

			queue = a b c d e
		        findNth 0 = (e,a b c d)
			findNth 1 =  (d,a b c)
		    *)

		    val findNth = fn n =>
		     let fun f (h::t,0) = (h,rev t)
			   | f (h::t,n) = f(t,n-1)
			   | f (nil,_) = let exception FindNth
					   in raise FindNth
					   end
		     in f (rev stateList,n)
		     end
		
		    val CHANGE {pos,oper,new=TOKEN (term,(value,_,_)),...} = a
		    val (last,queueFront) = findNth pos
		    val (stack,lexPair as (orig,lexer)) = last
		    val TOKEN (_,(_,leftPos,rightPos)) = orig
 		    val newLexV = TOKEN (term,(value,leftPos,rightPos))

		    val newLexPair =
			case oper
			of DELETE => Stream.get lexer
			 | SUBST => (newLexV,lexer)
			 | INSERT => (newLexV,Stream.cons lexPair)

		    val restQueue = 
		     Fifo.put((stack,newLexPair),
			      revfold Fifo.put queueFront Fifo.empty)

	 	   val (lexPair,stack,queue,_,_) =
			distanceParse(newLexPair,stack,restQueue,pos)

	      in (lexPair,stack,queue)
	      end
	  | nil => (error("syntax error found at " ^ (showTerminal term),
			      leftPos,leftPos); raise ParseError)
	end
     in FixError
     end

   val parse = fn {arg,table,lexer,saction,void,lookahead,
		   ec=ec as {showTerminal,...} : ('_a,'_b) ecRecord} =>
	let val distance = 15   (* defer distance tokens *)
	    val minAdvance = 1  (* must parse at least 1 token past error *)
	    val maxAdvance = max(lookahead,0)(* max distance for parse check *)
	    val lexPair = Stream.get lexer
	    val (TOKEN (_,(_,leftPos,_)),_) = lexPair
	    val startStack = [(initialState table,(void,leftPos,leftPos))]
	    val startQueue = Fifo.put((startStack,lexPair),Fifo.empty)
	    val distanceParse = distanceParse(table,showTerminal,saction,arg)
	    val fixError = mkFixError(ec,distanceParse,minAdvance,maxAdvance)
	    val ssParse = ssParse(table,showTerminal,saction,fixError,arg)
	    fun loop (lexPair,stack,queue,_,SOME ACCEPT) =
		   ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,0,_) = ssParse(lexPair,stack,queue)
	      | loop (lexPair,stack,queue,distance,SOME ERROR) =
		 let val (lexPair,stack,queue) = fixError(lexPair,stack,queue)
		 in loop (distanceParse(lexPair,stack,queue,distance))
		 end
	      | loop _ = let exception ParseInternal
			 in raise ParseInternal
			 end
	in loop (distanceParse(lexPair,startStack,startQueue,distance))
	end
 end;

(* drt (12/15/89) -- needed only when the code above is functorized

structure LrParser = ParserGen(structure LrTable=LrTable
			     structure Stream=Stream);
*)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure Header =
  struct

	val DEBUG = true

	type Lineno = int
	val lineno = ref 1
	val infile = ref ""
	val errflag = ref false

	val pr = fn s => output(std_out,s)

	val error = fn t => fn (l:int) =>
	   (pr (!infile); pr ", line "; pr (makestring l); pr ": Error: ";
	      pr t; pr "\n"; errflag := true)

	val warn = fn t => fn (l:int) =>
	   (pr (!infile); pr ", line "; pr (makestring l); pr ": Warning: ";
	    pr t; pr "\n")

	val cerror = fn t =>
	     (pr (!infile); pr ", line ";
	      pr (makestring (!lineno)); pr ": Error: ";
	      pr t; pr "\n"; errflag := true)

	exception SemanticError

	type symbol = string*int
	type ty = string option

	datatype lexvalue = LEFT | RIGHT | NONASSOC
	datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
			   FUNCTOR of string  | START_SYM of symbol |
			   NSHIFT of symbol list | POS of string | PURE |
			   PARSE_ARG of string * string
			   
	type declData = {eop : symbol list,
			 keyword : symbol list,
			 nonterm : (symbol*ty) list option,
			 prec : (lexvalue * (symbol list)) list,
			 prefer : symbol list,
			 subst: (symbol*symbol) list,
			 term : (symbol*ty) list option,
			 control : control list,
			 value : (symbol * string) list}

	type rhsData = {rhs:symbol list,code:string, prec:symbol option} list
	type rule = {lhs : symbol, rhs : symbol list,
		     code : string, prec : symbol option}

	fun join_decls
	      ({eop=e,control=c,keyword=k,nonterm=n,prec, prefer=p,
		subst=su,term=t,value=v}:declData,
	       {eop=e',control=c',keyword=k',nonterm=n',prec=prec', prefer=p',
		subst=su',term=t',value=v'} : declData) =
	  let val ignore_dup = fn s => (cerror ("ignoring duplicate " ^ s ^
						" declaration"))
	      val join = fn (e,NONE,NONE) => NONE
			  | (e,NONE,a) => a
			  | (e,a,NONE) => a
			  | (e,a,b) => (ignore_dup e; a)
	      fun mergeControl (nil,a) = [a]
		| mergeControl (l as h::t,a) =
		     case (h,a)
	  	     of (PARSER_NAME _,PARSER_NAME _) => (ignore_dup "%name"; l)
		      | (FUNCTOR _,FUNCTOR _) => (ignore_dup "%header"; l)
		      | (PARSE_ARG _,PARSE_ARG _) => (ignore_dup "%arg"; l)
		      | (START_SYM _,START_SYM _) => (ignore_dup "%start"; l)
		      | (POS _,POS _) => (ignore_dup "%pos"; l)
		      | (NSHIFT a,NSHIFT b) => (NSHIFT (a@b)::t)
		      | _ => h :: mergeControl(t,a)
	      fun loop (nil,r) = r
		| loop (h::t,r) = mergeControl(r,h)
	 in {eop=e@e',control=loop(c',c),keyword=k'@k,
	    nonterm=join("%nonterm",n,n'), prec=prec@prec',
	    prefer = p@p', subst=su@su', term=join("%term",t,t'),value=v@v'} : declData
	end
end;
signature Mlyacc_TOKENS =
sig
type ('a,'b) token
type svalue
val BOGUS_VALUE: ('a * 'a) ->(svalue,'a) token
val UNKNOWN: ((string) * 'a * 'a) ->(svalue,'a) token
val VALUE: ('a * 'a) ->(svalue,'a) token
val VERBOSE: ('a * 'a) ->(svalue,'a) token
val TYVAR: ((string) * 'a * 'a) ->(svalue,'a) token
val TERM: ('a * 'a) ->(svalue,'a) token
val START: ('a * 'a) ->(svalue,'a) token
val SUBST: ('a * 'a) ->(svalue,'a) token
val RPAREN: ('a * 'a) ->(svalue,'a) token
val RBRACE: ('a * 'a) ->(svalue,'a) token
val PROG: ((string) * 'a * 'a) ->(svalue,'a) token
val PREFER: ('a * 'a) ->(svalue,'a) token
val PREC_TAG: ('a * 'a) ->(svalue,'a) token
val PREC: ((Header.lexvalue) * 'a * 'a) ->(svalue,'a) token
val PERCENT_ARG: ('a * 'a) ->(svalue,'a) token
val PERCENT_POS: ('a * 'a) ->(svalue,'a) token
val PERCENT_PURE: ('a * 'a) ->(svalue,'a) token
val PERCENT_EOP: ('a * 'a) ->(svalue,'a) token
val OF: ('a * 'a) ->(svalue,'a) token
val NOSHIFT: ('a * 'a) ->(svalue,'a) token
val NONTERM: ('a * 'a) ->(svalue,'a) token
val NODEFAULT: ('a * 'a) ->(svalue,'a) token
val NAME: ('a * 'a) ->(svalue,'a) token
val LPAREN: ('a * 'a) ->(svalue,'a) token
val LBRACE: ('a * 'a) ->(svalue,'a) token
val KEYWORD: ('a * 'a) ->(svalue,'a) token
val INT: ((string) * 'a * 'a) ->(svalue,'a) token
val PERCENT_HEADER: ('a * 'a) ->(svalue,'a) token
val IDDOT: ((string) * 'a * 'a) ->(svalue,'a) token
val ID: ((string*int) * 'a * 'a) ->(svalue,'a) token
val HEADER: ((string) * 'a * 'a) ->(svalue,'a) token
val FOR: ('a * 'a) ->(svalue,'a) token
val EQUAL: ('a * 'a) ->(svalue,'a) token
val EOF: ('a * 'a) ->(svalue,'a) token
val DELIMITER: ('a * 'a) ->(svalue,'a) token
val COMMA: ('a * 'a) ->(svalue,'a) token
val COLON: ('a * 'a) ->(svalue,'a) token
val BAR: ('a * 'a) ->(svalue,'a) token
val BLOCK: ('a * 'a) ->(svalue,'a) token
val ASTERISK: ('a * 'a) ->(svalue,'a) token
val ARROW: ('a * 'a) ->(svalue,'a) token
end
signature Mlyacc_LRVALS =
sig
structure Tokens : Mlyacc_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
functor MlyaccLrValsFun(structure Token : TOKEN)
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser for the ML parser generator *)
open Header

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionT =
"\
\\011\000\003\000\000\000\001\000\
\\000\000\098\000\
\\007\000\023\000\014\000\022\000\016\000\021\000\
\\019\000\020\000\020\000\019\000\021\000\018\000\022\000\017\000\
\\024\000\016\000\025\000\015\000\026\000\014\000\027\000\013\000\
\\028\000\012\000\030\000\011\000\034\000\010\000\035\000\009\000\
\\036\000\008\000\038\000\007\000\039\000\006\000\000\000\001\000\
\\000\000\097\000\
\\012\000\024\000\000\000\001\000\
\\000\000\111\000\
\\012\000\026\000\000\000\001\000\
\\012\000\027\000\000\000\001\000\
\\012\000\029\000\000\000\001\000\
\\000\000\126\000\
\\000\000\126\000\
\\031\000\032\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\000\000\113\000\
\\000\000\126\000\
\\000\000\126\000\
\\012\000\026\000\000\000\001\000\
\\000\000\112\000\
\\012\000\043\000\000\000\001\000\
\\000\000\126\000\
\\031\000\045\000\000\000\001\000\
\\012\000\048\000\000\000\001\000\
\\031\000\049\000\000\000\001\000\
\\004\000\050\000\000\000\099\000\
\\023\000\051\000\000\000\121\000\
\\000\000\102\000\
\\004\000\052\000\000\000\106\000\
\\010\000\053\000\000\000\001\000\
\\012\000\054\000\000\000\105\000\
\\012\000\054\000\000\000\101\000\
\\005\000\055\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\114\000\
\\000\000\134\000\
\\000\000\129\000\
\\000\000\132\000\
\\012\000\063\000\015\000\062\000\032\000\061\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\000\000\001\000\
\\000\000\139\000\
\\012\000\054\000\000\000\103\000\
\\012\000\054\000\000\000\107\000\
\\004\000\050\000\000\000\100\000\
\\000\000\109\000\
\\012\000\054\000\000\000\104\000\
\\000\000\108\000\
\\012\000\048\000\000\000\096\000\
\\000\000\124\000\
\\005\000\066\000\000\000\001\000\
\\000\000\115\000\
\\012\000\067\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\069\000\000\000\001\000\
\\012\000\070\000\000\000\001\000\
\\000\000\125\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\000\000\133\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\006\000\075\000\032\000\074\000\000\000\001\000\
\\005\000\076\000\000\000\001\000\
\\000\000\131\000\
\\000\000\142\000\
\\000\000\141\000\
\\000\000\140\000\
\\000\000\123\000\
\\000\000\126\000\
\\023\000\079\000\000\000\119\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\120\000\
\\010\000\080\000\000\000\001\000\
\\000\000\117\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\110\000\
\\012\000\039\000\013\000\038\000\000\000\135\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\136\000\
\\000\000\130\000\
\\012\000\063\000\015\000\062\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\004\000\083\000\000\000\122\000\
\\012\000\054\000\029\000\085\000\000\000\144\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\087\000\000\000\001\000\
\\005\000\088\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\138\000\
\\000\000\126\000\
\\031\000\090\000\000\000\001\000\
\\012\000\091\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\118\000\
\\000\000\116\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\054\000\029\000\085\000\000\000\144\000\
\\000\000\127\000\
\\000\000\143\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\137\000\
\\031\000\094\000\000\000\001\000\
\\000\000\128\000\
\\008\000\000\000\000\000\001\000\
\"
val gotoT =
"\
\\001\000\093\000\000\000\000\000\
\\006\000\002\000\000\000\000\000\
\\005\000\003\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\002\000\023\000\000\000\000\000\
\\000\000\000\000\
\\013\000\026\000\000\000\000\000\
\\003\000\028\000\000\000\000\000\
\\003\000\029\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\031\000\000\000\000\000\
\\000\000\000\000\
\\003\000\038\000\000\000\000\000\
\\003\000\039\000\000\000\000\000\
\\002\000\040\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\003\000\042\000\000\000\000\000\
\\000\000\000\000\
\\010\000\045\000\011\000\044\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\004\000\058\000\008\000\057\000\000\000\000\000\
\\007\000\062\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\010\000\063\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\066\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\069\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\070\000\000\000\000\000\
\\007\000\032\000\014\000\071\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\003\000\076\000\009\000\075\000\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\004\000\079\000\000\000\000\000\
\\007\000\032\000\014\000\080\000\000\000\000\000\
\\000\000\000\000\
\\012\000\082\000\000\000\000\000\
\\007\000\032\000\014\000\084\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\003\000\087\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\090\000\000\000\000\000\
\\012\000\091\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\"
val numstates = 94
val string_to_int = fn(s,index) => (ordof(s,index) + 
			ordof(s,index+1)*256,index+2)
	val convert_string_to_row = fn (conv_key,conv_entry) =>
	     fn(s,index) =>
		let fun f (r,index) =
			let val (num,index) = string_to_int(s,index)
			    val (i,index) = string_to_int(s,index)
			in if num=0 then ((rev r,conv_entry i),index)
			   else f((conv_key (num-1),conv_entry i)::r,index)
			end
		in f(nil,index)
		end
	 val convert_string_to_row_list = fn conv_funcs => fn s =>
		    let val convert_row =convert_string_to_row conv_funcs
		 	fun f(r,index) =
			  if index < String.length s then
			    let val (newlist,index) = convert_row (s,index)
			    in f(newlist::r,index)
			    end
			  else rev r
		    in f(nil,0)
		    end
	 val entry_to_action = fn j =>
		       if j=0 then ACCEPT
		       else if j=1 then ERROR
		       else if j >= (numstates+2) then REDUCE (j-numstates-2)
		       else SHIFT (STATE (j-2))
	 val make_goto_table = convert_string_to_row_list(NT,STATE)
	 val make_action_table=convert_string_to_row_list(T,entry_to_action)
	 val gotoT = map (fn (a,b) => a) (make_goto_table gotoT)
	 val actionT = make_action_table actionT
     in LrTable.mkLrTable {actions=actionT,gotos=gotoT,
	  numStates=numstates,initialState=STATE 0}
     end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | UNKNOWN of unit ->  (string) | TYVAR of unit ->  (string)
 | PROG of unit ->  (string) | PREC of unit ->  (Header.lexvalue)
 | INT of unit ->  (string) | IDDOT of unit ->  (string)
 | ID of unit ->  (string*int) | HEADER of unit ->  (string)
 | TY of unit ->  (string)
 | SUBST_DECL of unit ->  ( ( Header.symbol*Header.symbol )  list)
 | RULE_PREC of unit ->  (Header.symbol option)
 | RULE_LIST of unit ->  (Header.rule list)
 | RULE of unit ->  (Header.rule list)
 | RHS_LIST of unit ->  (Header.rhsData)
 | RECORD_LIST of unit ->  (string) | QUAL_ID of unit ->  (string)
 | MPC_DECLS of unit ->  (Header.declData)
 | MPC_DECL of unit ->  (Header.declData) | LABEL of unit ->  (string)
 | ID_LIST of unit ->  (Header.symbol list)
 | CONSTR_LIST of unit ->  ( ( Header.symbol*Header.ty )  list)
 | BEGIN of unit ->  (string*Header.declData* ( Header.rule list ) )
end
type svalue = MlyValue.svalue
type result = string*Header.declData* ( Header.rule list ) 
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_insert =
fn _ => false
val preferred_subst =
fn  _ => nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "ARROW"
  | (T 1) => "ASTERISK"
  | (T 2) => "BLOCK"
  | (T 3) => "BAR"
  | (T 4) => "COLON"
  | (T 5) => "COMMA"
  | (T 6) => "DELIMITER"
  | (T 7) => "EOF"
  | (T 8) => "EQUAL"
  | (T 9) => "FOR"
  | (T 10) => "HEADER"
  | (T 11) => "ID"
  | (T 12) => "IDDOT"
  | (T 13) => "PERCENT_HEADER"
  | (T 14) => "INT"
  | (T 15) => "KEYWORD"
  | (T 16) => "LBRACE"
  | (T 17) => "LPAREN"
  | (T 18) => "NAME"
  | (T 19) => "NODEFAULT"
  | (T 20) => "NONTERM"
  | (T 21) => "NOSHIFT"
  | (T 22) => "OF"
  | (T 23) => "PERCENT_EOP"
  | (T 24) => "PERCENT_PURE"
  | (T 25) => "PERCENT_POS"
  | (T 26) => "PERCENT_ARG"
  | (T 27) => "PREC"
  | (T 28) => "PREC_TAG"
  | (T 29) => "PREFER"
  | (T 30) => "PROG"
  | (T 31) => "RBRACE"
  | (T 32) => "RPAREN"
  | (T 33) => "SUBST"
  | (T 34) => "START"
  | (T 35) => "TERM"
  | (T 36) => "TYVAR"
  | (T 37) => "VERBOSE"
  | (T 38) => "VALUE"
  | (T 39) => "UNKNOWN"
  | (T 40) => "BOGUS_VALUE"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 13) :: (T 15) :: (T 16) :: (T 17)
 :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22) :: (T 23) :: (T 24)
 :: (T 25) :: (T 26) :: (T 28) :: (T 29) :: (T 31) :: (T 32) :: (T 33)
 :: (T 34) :: (T 35) :: (T 37) :: (T 38) :: (T 40) :: nil
end
structure Actions =
struct 
exception mlyAction of int
val actions = 
let open Header
in
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.RULE_LIST (RULE_LIST1),RULE_LISTleft as 
RULE_LIST1left,RULE_LISTright as RULE_LIST1right)) :: (_,(_,
DELIMITERleft as DELIMITER1left,DELIMITERright as DELIMITER1right
)) :: (_,(MlyValue.MPC_DECLS (MPC_DECLS1),MPC_DECLSleft as 
MPC_DECLS1left,MPC_DECLSright as MPC_DECLS1right)) :: (_,(MlyValue.
HEADER (HEADER1),HEADERleft as HEADER1left,HEADERright as HEADER1right
)) :: rest671) =>
let val result = 
MlyValue.BEGIN (fn () => (let 
val HEADER as HEADER1 = HEADER1()
val MPC_DECLS as MPC_DECLS1 = MPC_DECLS1()
val RULE_LIST as RULE_LIST1 = RULE_LIST1()
 in (HEADER,MPC_DECLS,rev RULE_LIST) end ))
in (LrTable.NT 0,(result,HEADER1left,RULE_LIST1right),rest671)
end
| (1,(_,(MlyValue.MPC_DECL (MPC_DECL1),MPC_DECLleft as MPC_DECL1left,
MPC_DECLright as MPC_DECL1right)) :: (_,(MlyValue.MPC_DECLS (
MPC_DECLS1),MPC_DECLSleft as MPC_DECLS1left,MPC_DECLSright as 
MPC_DECLS1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECLS (fn () => (let 
val MPC_DECLS as MPC_DECLS1 = MPC_DECLS1()
val MPC_DECL as MPC_DECL1 = MPC_DECL1()
 in (join_decls(MPC_DECLS,MPC_DECL)) end ))
in (LrTable.NT 5,(result,MPC_DECLS1left,MPC_DECL1right),rest671)
end
| (2,rest671) =>
let val result = 
MlyValue.MPC_DECLS ( fn () => ((
{prec=nil,nonterm=NONE,term=NONE,eop=nil,control=nil,
		prefer=nil,keyword=nil,subst=nil,
		value=nil}
)))
in (LrTable.NT 5,(result,defaultPos,defaultPos),rest671)
end
| (3,(_,(MlyValue.CONSTR_LIST (CONSTR_LIST1),CONSTR_LISTleft as 
CONSTR_LIST1left,CONSTR_LISTright as CONSTR_LIST1right)) :: (_,(_,
TERMleft as TERM1left,TERMright as TERM1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val CONSTR_LIST as CONSTR_LIST1 = CONSTR_LIST1()
 in (
{ prec=nil,nonterm=NONE,
	       term = SOME CONSTR_LIST, eop =nil,control=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,TERM1left,CONSTR_LIST1right),rest671)
end
| (4,(_,(MlyValue.CONSTR_LIST (CONSTR_LIST1),CONSTR_LISTleft as 
CONSTR_LIST1left,CONSTR_LISTright as CONSTR_LIST1right)) :: (_,(_,
NONTERMleft as NONTERM1left,NONTERMright as NONTERM1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val CONSTR_LIST as CONSTR_LIST1 = CONSTR_LIST1()
 in (
{ prec=nil,control=nil,nonterm= SOME CONSTR_LIST,
	       term = NONE, eop=nil,prefer=nil,subst=nil,keyword=nil,
	       value=nil}
) end ))
in (LrTable.NT 4,(result,NONTERM1left,CONSTR_LIST1right),rest671)
end
| (5,(_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: (_,(MlyValue.PREC (PREC1),
PRECleft as PREC1left,PRECright as PREC1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val PREC as PREC1 = PREC1()
val ID_LIST as ID_LIST1 = ID_LIST1()
 in (
{prec= [(PREC,ID_LIST)],control=nil,
	      nonterm=NONE,term=NONE,eop=nil,prefer=nil,subst=nil,
	      keyword=nil,value=nil}
) end ))
in (LrTable.NT 4,(result,PREC1left,ID_LIST1right),rest671)
end
| (6,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right)) :: 
(_,(_,STARTleft as START1left,STARTright as START1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID as ID1 = ID1()
 in (
{prec=nil,control=[START_SYM ID],nonterm=NONE,
	       term = NONE, eop = nil,prefer=nil,subst=nil,keyword=nil,
	       value=nil}
) end ))
in (LrTable.NT 4,(result,START1left,ID1right),rest671)
end
| (7,(_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: (_,(_,PERCENT_EOPleft as 
PERCENT_EOP1left,PERCENT_EOPright as PERCENT_EOP1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
 in (
{prec=nil,control=nil,nonterm=NONE,term=NONE,
		eop=ID_LIST, prefer=nil,subst=nil,keyword=nil,
	 	value=nil}
) end ))
in (LrTable.NT 4,(result,PERCENT_EOP1left,ID_LIST1right),rest671)
end
| (8,(_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: (_,(_,KEYWORDleft as KEYWORD1left,
KEYWORDright as KEYWORD1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
 in (
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=ID_LIST,
	 	value=nil}
) end ))
in (LrTable.NT 4,(result,KEYWORD1left,ID_LIST1right),rest671)
end
| (9,(_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: (_,(_,PREFERleft as PREFER1left,
PREFERright as PREFER1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
 in (
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=ID_LIST, subst=nil,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,PREFER1left,ID_LIST1right),rest671)
end
| (10,(_,(MlyValue.SUBST_DECL (SUBST_DECL1),SUBST_DECLleft as 
SUBST_DECL1left,SUBST_DECLright as SUBST_DECL1right)) :: (_,(_,
SUBSTleft as SUBST1left,SUBSTright as SUBST1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val SUBST_DECL as SUBST_DECL1 = SUBST_DECL1()
 in (
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=SUBST_DECL,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,SUBST1left,SUBST_DECL1right),rest671)
end
| (11,(_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: (_,(_,NOSHIFTleft as NOSHIFT1left,
NOSHIFTright as NOSHIFT1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
 in (
{prec=nil,control=[NSHIFT ID_LIST],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,NOSHIFT1left,ID_LIST1right),rest671)
end
| (12,(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: (_,(_,PERCENT_HEADERleft as PERCENT_HEADER1left,
PERCENT_HEADERright as PERCENT_HEADER1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val PROG as PROG1 = PROG1()
 in (
{prec=nil,control=[FUNCTOR PROG],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,PERCENT_HEADER1left,PROG1right),rest671)
end
| (13,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: (_,(_,NAMEleft as NAME1left,NAMEright as NAME1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID as ID1 = ID1()
 in (
{prec=nil,control=[PARSER_NAME ID],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil, value=nil}
) end ))
in (LrTable.NT 4,(result,NAME1left,ID1right),rest671)
end
| (14,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right)) :: 
(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: (_,(_,PERCENT_ARGleft as PERCENT_ARG1left,
PERCENT_ARGright as PERCENT_ARG1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val PROG as PROG1 = PROG1()
val TY as TY1 = TY1()
 in (
{prec=nil,control=[PARSE_ARG(PROG,TY)],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil, value=nil}
) end ))
in (LrTable.NT 4,(result,PERCENT_ARG1left,TY1right),rest671)
end
| (15,(_,(_,VERBOSEleft as VERBOSE1left,VERBOSEright as VERBOSE1right
)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL ( fn () => ((
 {prec=nil,control=[Header.VERBOSE],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,VERBOSE1left,VERBOSE1right),rest671)
end
| (16,(_,(_,NODEFAULTleft as NODEFAULT1left,NODEFAULTright as 
NODEFAULT1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL ( fn () => ((
 {prec=nil,control=[Header.NODEFAULT],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,NODEFAULT1left,NODEFAULT1right),rest671)
end
| (17,(_,(_,PERCENT_PUREleft as PERCENT_PURE1left,
PERCENT_PUREright as PERCENT_PURE1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL ( fn () => ((
 {prec=nil,control=[Header.PURE],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_PURE1left,PERCENT_PURE1right),rest671)
end
| (18,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,PERCENT_POSleft as PERCENT_POS1left,PERCENT_POSright as 
PERCENT_POS1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val TY as TY1 = TY1()
 in (
 {prec=nil,control=[Header.POS TY],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
) end ))
in (LrTable.NT 4,(result,PERCENT_POS1left,TY1right),rest671)
end
| (19,(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: (_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as 
ID1right)) :: (_,(_,VALUEleft as VALUE1left,VALUEright as VALUE1right
)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (fn () => (let 
val ID as ID1 = ID1()
val PROG as PROG1 = PROG1()
 in (
 {prec=nil,control=[Header.NODEFAULT],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=[(ID,PROG)]}
) end ))
in (LrTable.NT 4,(result,VALUE1left,PROG1right),rest671)
end
| (20,(_,(MlyValue.ID (ID2),ID2left,ID2right)) :: (_,(_,FORleft as 
FOR1left,FORright as FOR1right)) :: (_,(MlyValue.ID (ID1),IDleft as 
ID1left,IDright as ID1right)) :: (_,(_,BARleft as BAR1left,
BARright as BAR1right)) :: (_,(MlyValue.SUBST_DECL (SUBST_DECL1),
SUBST_DECLleft as SUBST_DECL1left,SUBST_DECLright as SUBST_DECL1right
)) :: rest671) =>
let val result = 
MlyValue.SUBST_DECL (fn () => (let 
val SUBST_DECL as SUBST_DECL1 = SUBST_DECL1()
val ID as ID1 = ID1()
val ID2 = ID2()
 in ((ID1,ID2)::SUBST_DECL) end ))
in (LrTable.NT 12,(result,SUBST_DECL1left,ID2right),rest671)
end
| (21,(_,(MlyValue.ID (ID2),ID2left,ID2right)) :: (_,(_,FORleft as 
FOR1left,FORright as FOR1right)) :: (_,(MlyValue.ID (ID1),IDleft as 
ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.SUBST_DECL (fn () => (let 
val ID as ID1 = ID1()
val ID2 = ID2()
 in ([(ID1,ID2)]) end ))
in (LrTable.NT 12,(result,ID1left,ID2right),rest671)
end
| (22,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,OFleft as OF1left,OFright as OF1right)) :: (_,(MlyValue.ID
 (ID1),IDleft as ID1left,IDright as ID1right)) :: (_,(_,BARleft as 
BAR1left,BARright as BAR1right)) :: (_,(MlyValue.CONSTR_LIST (
CONSTR_LIST1),CONSTR_LISTleft as CONSTR_LIST1left,CONSTR_LISTright as 
CONSTR_LIST1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST (fn () => (let 
val CONSTR_LIST as CONSTR_LIST1 = CONSTR_LIST1()
val ID as ID1 = ID1()
val TY as TY1 = TY1()
 in ((ID,SOME TY)::CONSTR_LIST) end ))
in (LrTable.NT 1,(result,CONSTR_LIST1left,TY1right),rest671)
end
| (23,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: (_,(_,BARleft as BAR1left,BARright as BAR1right)) :: (_,(
MlyValue.CONSTR_LIST (CONSTR_LIST1),CONSTR_LISTleft as 
CONSTR_LIST1left,CONSTR_LISTright as CONSTR_LIST1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST (fn () => (let 
val CONSTR_LIST as CONSTR_LIST1 = CONSTR_LIST1()
val ID as ID1 = ID1()
 in ((ID,NONE)::CONSTR_LIST) end ))
in (LrTable.NT 1,(result,CONSTR_LIST1left,ID1right),rest671)
end
| (24,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,OFleft as OF1left,OFright as OF1right)) :: (_,(MlyValue.ID
 (ID1),IDleft as ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST (fn () => (let 
val ID as ID1 = ID1()
val TY as TY1 = TY1()
 in ([(ID,SOME TY)]) end ))
in (LrTable.NT 1,(result,ID1left,TY1right),rest671)
end
| (25,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST (fn () => (let 
val ID as ID1 = ID1()
 in ([(ID,NONE)]) end ))
in (LrTable.NT 1,(result,ID1left,ID1right),rest671)
end
| (26,(_,(MlyValue.RHS_LIST (RHS_LIST1),RHS_LISTleft as RHS_LIST1left,
RHS_LISTright as RHS_LIST1right)) :: (_,(_,COLONleft as COLON1left,
COLONright as COLON1right)) :: (_,(MlyValue.ID (ID1),IDleft as ID1left
,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.RULE (fn () => (let 
val ID as ID1 = ID1()
val RHS_LIST as RHS_LIST1 = RHS_LIST1()
 in (
map (fn {rhs,code,prec} => {lhs=ID,rhs=rev rhs,code=code,prec=prec})
	 RHS_LIST
) end ))
in (LrTable.NT 9,(result,ID1left,RHS_LIST1right),rest671)
end
| (27,(_,(MlyValue.RULE (RULE1),RULEleft as RULE1left,RULEright as 
RULE1right)) :: (_,(MlyValue.RULE_LIST (RULE_LIST1),RULE_LISTleft as 
RULE_LIST1left,RULE_LISTright as RULE_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RULE_LIST (fn () => (let 
val RULE_LIST as RULE_LIST1 = RULE_LIST1()
val RULE as RULE1 = RULE1()
 in (RULE@RULE_LIST) end ))
in (LrTable.NT 10,(result,RULE_LIST1left,RULE1right),rest671)
end
| (28,(_,(MlyValue.RULE (RULE1),RULEleft as RULE1left,RULEright as 
RULE1right)) :: rest671) =>
let val result = 
MlyValue.RULE_LIST (fn () => (let 
val RULE as RULE1 = RULE1()
 in (RULE) end ))
in (LrTable.NT 10,(result,RULE1left,RULE1right),rest671)
end
| (29,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: (_,(MlyValue.ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,
ID_LISTright as ID_LIST1right)) :: rest671) =>
let val result = 
MlyValue.ID_LIST (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
val ID as ID1 = ID1()
 in (ID::ID_LIST) end ))
in (LrTable.NT 2,(result,ID_LIST1left,ID1right),rest671)
end
| (30,rest671) =>
let val result = 
MlyValue.ID_LIST ( fn () => ((nil)))
in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671)
end
| (31,(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: (_,(MlyValue.RULE_PREC (RULE_PREC1),RULE_PRECleft as 
RULE_PREC1left,RULE_PRECright as RULE_PREC1right)) :: (_,(MlyValue.
ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,ID_LISTright as 
ID_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RHS_LIST (fn () => (let 
val ID_LIST as ID_LIST1 = ID_LIST1()
val RULE_PREC as RULE_PREC1 = RULE_PREC1()
val PROG as PROG1 = PROG1()
 in ([{rhs=ID_LIST,code=PROG,prec=RULE_PREC}]) end ))
in (LrTable.NT 8,(result,ID_LIST1left,PROG1right),rest671)
end
| (32,(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: (_,(MlyValue.RULE_PREC (RULE_PREC1),RULE_PRECleft as 
RULE_PREC1left,RULE_PRECright as RULE_PREC1right)) :: (_,(MlyValue.
ID_LIST (ID_LIST1),ID_LISTleft as ID_LIST1left,ID_LISTright as 
ID_LIST1right)) :: (_,(_,BARleft as BAR1left,BARright as BAR1right
)) :: (_,(MlyValue.RHS_LIST (RHS_LIST1),RHS_LISTleft as RHS_LIST1left,
RHS_LISTright as RHS_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RHS_LIST (fn () => (let 
val RHS_LIST as RHS_LIST1 = RHS_LIST1()
val ID_LIST as ID_LIST1 = ID_LIST1()
val RULE_PREC as RULE_PREC1 = RULE_PREC1()
val PROG as PROG1 = PROG1()
 in ({rhs=ID_LIST,code=PROG,prec=RULE_PREC}::RHS_LIST) end ))
in (LrTable.NT 8,(result,RHS_LIST1left,PROG1right),rest671)
end
| (33,(_,(MlyValue.TYVAR (TYVAR1),TYVARleft as TYVAR1left,
TYVARright as TYVAR1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val TYVAR as TYVAR1 = TYVAR1()
 in (TYVAR) end ))
in (LrTable.NT 13,(result,TYVAR1left,TYVAR1right),rest671)
end
| (34,(_,(_,RBRACEleft as RBRACE1left,RBRACEright as RBRACE1right
)) :: (_,(MlyValue.RECORD_LIST (RECORD_LIST1),RECORD_LISTleft as 
RECORD_LIST1left,RECORD_LISTright as RECORD_LIST1right)) :: (_,(_,
LBRACEleft as LBRACE1left,LBRACEright as LBRACE1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val RECORD_LIST as RECORD_LIST1 = RECORD_LIST1()
 in ("{ "^RECORD_LIST^" } ") end ))
in (LrTable.NT 13,(result,LBRACE1left,RBRACE1right),rest671)
end
| (35,(_,(_,RBRACEleft as RBRACE1left,RBRACEright as RBRACE1right
)) :: (_,(_,LBRACEleft as LBRACE1left,LBRACEright as LBRACE1right
)) :: rest671) =>
let val result = 
MlyValue.TY ( fn () => (("{}")))
in (LrTable.NT 13,(result,LBRACE1left,RBRACE1right),rest671)
end
| (36,(_,(MlyValue.PROG (PROG1),PROGleft as PROG1left,PROGright as 
PROG1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val PROG as PROG1 = PROG1()
 in (" ( "^PROG^" ) ") end ))
in (LrTable.NT 13,(result,PROG1left,PROG1right),rest671)
end
| (37,(_,(MlyValue.QUAL_ID (QUAL_ID1),QUAL_IDleft as QUAL_ID1left,
QUAL_IDright as QUAL_ID1right)) :: (_,(MlyValue.TY (TY1),TYleft as 
TY1left,TYright as TY1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val TY as TY1 = TY1()
val QUAL_ID as QUAL_ID1 = QUAL_ID1()
 in (TY^" "^QUAL_ID) end ))
in (LrTable.NT 13,(result,TY1left,QUAL_ID1right),rest671)
end
| (38,(_,(MlyValue.QUAL_ID (QUAL_ID1),QUAL_IDleft as QUAL_ID1left,
QUAL_IDright as QUAL_ID1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val QUAL_ID as QUAL_ID1 = QUAL_ID1()
 in (QUAL_ID) end ))
in (LrTable.NT 13,(result,QUAL_ID1left,QUAL_ID1right),rest671)
end
| (39,(_,(MlyValue.TY (TY2),TY2left,TY2right)) :: (_,(_,
ASTERISKleft as ASTERISK1left,ASTERISKright as ASTERISK1right)) :: 
(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val TY as TY1 = TY1()
val TY2 = TY2()
 in (TY1^"*"^TY2) end ))
in (LrTable.NT 13,(result,TY1left,TY2right),rest671)
end
| (40,(_,(MlyValue.TY (TY2),TY2left,TY2right)) :: (_,(_,ARROWleft as 
ARROW1left,ARROWright as ARROW1right)) :: (_,(MlyValue.TY (TY1),
TYleft as TY1left,TYright as TY1right)) :: rest671) =>
let val result = 
MlyValue.TY (fn () => (let 
val TY as TY1 = TY1()
val TY2 = TY2()
 in (TY1 ^ " -> " ^ TY2) end ))
in (LrTable.NT 13,(result,TY1left,TY2right),rest671)
end
| (41,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right)) :: 
(_,(MlyValue.LABEL (LABEL1),LABELleft as LABEL1left,LABELright as 
LABEL1right)) :: (_,(_,COMMAleft as COMMA1left,COMMAright as 
COMMA1right)) :: (_,(MlyValue.RECORD_LIST (RECORD_LIST1),
RECORD_LISTleft as RECORD_LIST1left,RECORD_LISTright as 
RECORD_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RECORD_LIST (fn () => (let 
val RECORD_LIST as RECORD_LIST1 = RECORD_LIST1()
val LABEL as LABEL1 = LABEL1()
val TY as TY1 = TY1()
 in (RECORD_LIST^","^LABEL^":"^TY) end ))
in (LrTable.NT 7,(result,RECORD_LIST1left,TY1right),rest671)
end
| (42,(_,(MlyValue.TY (TY1),TYleft as TY1left,TYright as TY1right
)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right)) :: 
(_,(MlyValue.LABEL (LABEL1),LABELleft as LABEL1left,LABELright as 
LABEL1right)) :: rest671) =>
let val result = 
MlyValue.RECORD_LIST (fn () => (let 
val LABEL as LABEL1 = LABEL1()
val TY as TY1 = TY1()
 in (LABEL^":"^TY) end ))
in (LrTable.NT 7,(result,LABEL1left,TY1right),rest671)
end
| (43,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: rest671) =>
let val result = 
MlyValue.QUAL_ID (fn () => (let 
val ID as ID1 = ID1()
 in ((fn (a,_) => a) ID) end ))
in (LrTable.NT 6,(result,ID1left,ID1right),rest671)
end
| (44,(_,(MlyValue.QUAL_ID (QUAL_ID1),QUAL_IDleft as QUAL_ID1left,
QUAL_IDright as QUAL_ID1right)) :: (_,(MlyValue.IDDOT (IDDOT1),
IDDOTleft as IDDOT1left,IDDOTright as IDDOT1right)) :: rest671) =>
let val result = 
MlyValue.QUAL_ID (fn () => (let 
val IDDOT as IDDOT1 = IDDOT1()
val QUAL_ID as QUAL_ID1 = QUAL_ID1()
 in (IDDOT^QUAL_ID) end ))
in (LrTable.NT 6,(result,IDDOT1left,QUAL_ID1right),rest671)
end
| (45,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: rest671) =>
let val result = 
MlyValue.LABEL (fn () => (let 
val ID as ID1 = ID1()
 in ((fn (a,_) => a) ID) end ))
in (LrTable.NT 3,(result,ID1left,ID1right),rest671)
end
| (46,(_,(MlyValue.INT (INT1),INTleft as INT1left,INTright as 
INT1right)) :: rest671) =>
let val result = 
MlyValue.LABEL (fn () => (let 
val INT as INT1 = INT1()
 in (INT) end ))
in (LrTable.NT 3,(result,INT1left,INT1right),rest671)
end
| (47,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right
)) :: (_,(_,PREC_TAGleft as PREC_TAG1left,PREC_TAGright as 
PREC_TAG1right)) :: rest671) =>
let val result = 
MlyValue.RULE_PREC (fn () => (let 
val ID as ID1 = ID1()
 in (SOME ID) end ))
in (LrTable.NT 11,(result,PREC_TAG1left,ID1right),rest671)
end
| (48,rest671) =>
let val result = 
MlyValue.RULE_PREC ( fn () => ((NONE)))
in (LrTable.NT 11,(result,defaultPos,defaultPos),rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.BEGIN x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Mlyacc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ASTERISK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BLOCK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DELIMITER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun HEADER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.HEADER (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun IDDOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.IDDOT (fn () => i),p1,p2))
fun PERCENT_HEADER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun KEYWORD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NODEFAULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NONTERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOSHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_EOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_PURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_POS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_ARG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PREC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.PREC (fn () => i),p1,p2))
fun PREC_TAG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun PREFER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.PROG (fn () => i),p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TYVAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.TYVAR (fn () => i),p1,p2))
fun VERBOSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun UNKNOWN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.UNKNOWN (fn () => i),p1,p2))
fun BOGUS_VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
end
end
functor LexMLYACC(structure Tokens : Mlyacc_TOKENS)=
   struct
    structure UserDeclarations =
      struct
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
val lineno = Header.lineno

type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) token

open Tokens
val error = Header.error

val pcount = ref 0;
val commentLevel = ref 0
val actionstart = ref 0
val eof = fn () => (if (!pcount)>0 then
			 error " eof encountered in action beginning here !"
				(!actionstart)
		      else (); EOF(!lineno,!lineno))

val text = ref (nil : string list)
val Add = fn s => (text := s::(!text))

val lookup =
   let val dict = [("%prec",PREC_TAG),("%term",TERM),
		  ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
		  ("%prefer",PREFER),("%subst",SUBST),
		  ("%keyword",KEYWORD),("%name",NAME),
		  ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
		  ("%value",VALUE), ("%noshift",NOSHIFT),
		  ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
		  ("%arg",PERCENT_ARG),
		  ("%pos",PERCENT_POS)]
   in fn (s,left,right) =>
	    let fun f ((a,d)::b) = if a=s then d(left,right) else f b
	 	  | f nil = UNKNOWN(s,left,right)
	    in f dict
    	    end
   end
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s0 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s1 =
"\015\015\015\015\015\015\015\015\015\015\021\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\019\015\015\017\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015"
val s3 =
"\022\022\022\022\022\022\022\022\022\066\068\022\022\022\022\022\
\\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\022\
\\066\022\022\022\022\046\022\044\042\022\041\022\040\038\022\022\
\\036\036\036\036\036\036\036\036\036\036\035\022\022\034\022\022\
\\022\026\026\026\026\026\026\026\026\026\026\026\026\026\026\026\
\\026\026\026\026\026\026\026\026\026\026\026\022\022\022\022\022\
\\022\026\026\026\026\026\031\026\026\026\026\026\026\026\026\029\
\\026\026\026\026\026\026\026\026\026\026\026\025\024\023\022\022"
val s5 =
"\069\069\069\069\069\069\069\069\069\069\021\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\073\069\069\069\069\069\071\070\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069"
val s7 =
"\074\074\074\074\074\074\074\074\074\076\021\074\074\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\
\\076\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\075\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\
\\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074\074"
val s9 =
"\078\078\078\078\078\078\078\078\078\078\021\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\082\081\079\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078"
val s11 =
"\084\084\084\084\084\084\084\084\084\084\089\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\088\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\085\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084"
val s13 =
"\090\090\090\090\090\090\090\090\090\090\021\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\094\093\091\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090"
val s15 =
"\016\016\016\016\016\016\016\016\016\016\000\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\000\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016"
val s17 =
"\016\016\016\016\016\016\016\016\016\016\000\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\000\016\016\016\016\018\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016"
val s19 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\020\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s26 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000"
val s29 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\030\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000"
val s31 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\032\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000"
val s32 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\027\000\000\000\000\000\000\028\000\
\\027\027\027\027\027\027\027\027\027\027\000\000\000\000\000\000\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\027\027\027\027\027\027\027\027\027\000\000\000\000\027\
\\000\027\027\027\027\027\027\027\027\027\027\027\027\027\027\027\
\\027\027\033\027\027\027\027\027\027\027\027\000\000\000\000\000"
val s36 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\037\037\037\037\037\037\037\037\037\037\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s38 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\039\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s42 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\043\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s44 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\
\\045\045\045\045\045\045\045\045\045\045\000\000\000\000\000\000\
\\000\045\045\045\045\045\045\045\045\045\045\045\045\045\045\045\
\\045\045\045\045\045\045\045\045\045\045\045\000\000\000\000\045\
\\000\045\045\045\045\045\045\045\045\045\045\045\045\045\045\045\
\\045\045\045\045\045\045\045\045\045\045\045\000\000\000\000\000"
val s46 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\065\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\061\047\053\047\
\\047\047\048\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s47 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s48 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\049\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s49 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\050\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s50 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\051\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s51 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\052\047\047\047\047\047\047\000\000\000\000\000"
val s53 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\054\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s54 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\055\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s55 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\056\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s56 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\057\047\047\047\047\047\047\047\000\000\000\000\000"
val s57 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\058\047\047\047\047\047\047\047\000\000\000\000\000"
val s58 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\059\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s59 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\060\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s61 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\062\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s62 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\063\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\047\047\047\047\047\047\047\000\000\000\000\000"
val s63 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\047\
\\000\047\047\047\047\047\047\047\047\047\047\047\047\047\047\047\
\\047\047\047\047\064\047\047\047\047\047\047\000\000\000\000\000"
val s66 =
"\000\000\000\000\000\000\000\000\000\067\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\067\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s69 =
"\069\069\069\069\069\069\069\069\069\069\000\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\000\069\069\069\069\069\000\000\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\
\\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069\069"
val s71 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\072\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s76 =
"\000\000\000\000\000\000\000\000\000\077\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\077\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s78 =
"\078\078\078\078\078\078\078\078\078\078\000\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\000\000\000\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\
\\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078\078"
val s79 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\080\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s82 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\083\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s84 =
"\084\084\084\084\084\084\084\084\084\084\000\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\000\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\000\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\
\\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084\084"
val s85 =
"\000\000\000\000\000\000\000\000\000\087\087\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\087\000\086\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s90 =
"\090\090\090\090\090\090\090\090\090\090\000\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\000\000\000\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\
\\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090\090"
val s91 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\092\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
val s94 =
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\095\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
in arrayoflist
[{fin = [], trans = s0},
{fin = [], trans = s1},
{fin = [], trans = s1},
{fin = [], trans = s3},
{fin = [], trans = s3},
{fin = [], trans = s5},
{fin = [], trans = s5},
{fin = [], trans = s7},
{fin = [], trans = s7},
{fin = [], trans = s9},
{fin = [], trans = s9},
{fin = [], trans = s11},
{fin = [], trans = s11},
{fin = [], trans = s13},
{fin = [], trans = s13},
{fin = [(N 11),(N 18)], trans = s15},
{fin = [(N 11)], trans = s15},
{fin = [(N 11),(N 18)], trans = s17},
{fin = [(N 2),(N 11)], trans = s15},
{fin = [(N 18)], trans = s19},
{fin = [(N 14)], trans = s0},
{fin = [(N 16)], trans = s0},
{fin = [(N 96)], trans = s0},
{fin = [(N 38),(N 96)], trans = s0},
{fin = [(N 89),(N 96)], trans = s0},
{fin = [(N 36),(N 96)], trans = s0},
{fin = [(N 92),(N 96)], trans = s26},
{fin = [(N 92)], trans = s26},
{fin = [(N 79)], trans = s0},
{fin = [(N 92),(N 96)], trans = s29},
{fin = [(N 28),(N 92)], trans = s26},
{fin = [(N 92),(N 96)], trans = s31},
{fin = [(N 92)], trans = s32},
{fin = [(N 32),(N 92)], trans = s26},
{fin = [(N 34),(N 96)], trans = s0},
{fin = [(N 87),(N 96)], trans = s0},
{fin = [(N 82),(N 96)], trans = s36},
{fin = [(N 82)], trans = s36},
{fin = [(N 96)], trans = s38},
{fin = [(N 45)], trans = s0},
{fin = [(N 40),(N 96)], trans = s0},
{fin = [(N 42),(N 96)], trans = s0},
{fin = [(N 94),(N 96)], trans = s42},
{fin = [(N 5)], trans = s0},
{fin = [(N 75),(N 96)], trans = s44},
{fin = [(N 75)], trans = s44},
{fin = [(N 96)], trans = s46},
{fin = [(N 72)], trans = s47},
{fin = [(N 72)], trans = s48},
{fin = [(N 72)], trans = s49},
{fin = [(N 72)], trans = s50},
{fin = [(N 72)], trans = s51},
{fin = [(N 58),(N 72)], trans = s47},
{fin = [(N 72)], trans = s53},
{fin = [(N 72)], trans = s54},
{fin = [(N 72)], trans = s55},
{fin = [(N 72)], trans = s56},
{fin = [(N 72)], trans = s57},
{fin = [(N 72)], trans = s58},
{fin = [(N 72)], trans = s59},
{fin = [(N 68),(N 72)], trans = s47},
{fin = [(N 72)], trans = s61},
{fin = [(N 72)], trans = s62},
{fin = [(N 72)], trans = s63},
{fin = [(N 51),(N 72)], trans = s47},
{fin = [(N 85)], trans = s0},
{fin = [(N 25),(N 96)], trans = s66},
{fin = [(N 25)], trans = s66},
{fin = [(N 20)], trans = s0},
{fin = [(N 105)], trans = s69},
{fin = [(N 100)], trans = s0},
{fin = [(N 98)], trans = s71},
{fin = [(N 8)], trans = s0},
{fin = [(N 102)], trans = s0},
{fin = [(N 149)], trans = s0},
{fin = [(N 147),(N 149)], trans = s0},
{fin = [(N 145),(N 149)], trans = s76},
{fin = [(N 145)], trans = s76},
{fin = [(N 116)], trans = s78},
{fin = [(N 107)], trans = s79},
{fin = [(N 110)], trans = s0},
{fin = [(N 107)], trans = s0},
{fin = [(N 107)], trans = s82},
{fin = [(N 113)], trans = s0},
{fin = [(N 136)], trans = s84},
{fin = [(N 131)], trans = s85},
{fin = [(N 139)], trans = s0},
{fin = [(N 142)], trans = s0},
{fin = [(N 129)], trans = s0},
{fin = [(N 133)], trans = s0},
{fin = [(N 127)], trans = s90},
{fin = [(N 118)], trans = s91},
{fin = [(N 121)], trans = s0},
{fin = [(N 118)], trans = s0},
{fin = [(N 118)], trans = s94},
{fin = [(N 124)], trans = s0}]
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val A = STARTSTATE 3;
val CODE = STARTSTATE 5;
val COMMENT = STARTSTATE 9;
val EMPTYCOMMENT = STARTSTATE 13;
val F = STARTSTATE 7;
val INITIAL = STARTSTATE 1;
val STRING = STARTSTATE 11;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput = 
let 
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yypos = ref 1		(* location of next character to use *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let val yytext = substring(!yyb,i0,i-i0)
			open UserDeclarations Internal.StartStates
 in (yypos := i; case yyk of 

			(* Application actions *)

  100 => (dec pcount;
		    if !pcount = 0 then
			 PROG (implode (rev (!text)),!lineno,!lineno)
		    else (Add yytext; lex()))
| 102 => (Add yytext; YYBEGIN STRING; lex())
| 105 => (Add yytext; lex())
| 107 => (Add yytext; lex())
| 11 => (Add yytext; lex())
| 110 => (Add yytext; dec commentLevel;
		    if !commentLevel=0
			 then BOGUS_VALUE(!lineno,!lineno)
			 else lex()
		   )
| 113 => (Add yytext; inc commentLevel; lex())
| 116 => (Add yytext; lex())
| 118 => (lex())
| 121 => (dec commentLevel;
		          if !commentLevel=0 then YYBEGIN A else ();
			  lex ())
| 124 => (inc commentLevel; lex())
| 127 => (lex())
| 129 => (Add yytext; YYBEGIN CODE; lex())
| 131 => (Add yytext; lex())
| 133 => (Add yytext; inc lineno; error "unclosed string" (!lineno);
		    YYBEGIN CODE; lex())
| 136 => (Add yytext; lex())
| 139 => (Add yytext; lex())
| 14 => (YYBEGIN A; HEADER (implode (rev (!text)),!lineno,!lineno))
| 142 => (Add yytext;
			if substring(yytext,1,1)="\n" then inc lineno else ();
		     	YYBEGIN F; lex())
| 145 => (Add yytext; lex())
| 147 => (Add yytext; YYBEGIN STRING; lex())
| 149 => (Add yytext; error "unclosed string" (!lineno);
		    YYBEGIN CODE; lex())
| 16 => (Add yytext; inc lineno; lex())
| 18 => (Add yytext; lex())
| 2 => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    lex() before YYBEGIN INITIAL)
| 20 => (inc lineno; lex ())
| 25 => (lex())
| 28 => (OF(!lineno,!lineno))
| 32 => (FOR(!lineno,!lineno))
| 34 => (EQUAL(!lineno,!lineno))
| 36 => (LBRACE(!lineno,!lineno))
| 38 => (RBRACE(!lineno,!lineno))
| 40 => (COMMA(!lineno,!lineno))
| 42 => (ASTERISK(!lineno,!lineno))
| 45 => (ARROW(!lineno,!lineno))
| 5 => (YYBEGIN EMPTYCOMMENT; commentLevel := 1; lex())
| 51 => (PREC(Header.LEFT,!lineno,!lineno))
| 58 => (PREC(Header.RIGHT,!lineno,!lineno))
| 68 => (PREC(Header.NONASSOC,!lineno,!lineno))
| 72 => (lookup(yytext,!lineno,!lineno))
| 75 => (TYVAR(yytext,!lineno,!lineno))
| 79 => (IDDOT(yytext,!lineno,!lineno))
| 8 => (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    lex() before YYBEGIN CODE)
| 82 => (INT (yytext,!lineno,!lineno))
| 85 => (DELIMITER(!lineno,!lineno))
| 87 => (COLON(!lineno,!lineno))
| 89 => (BAR(!lineno,!lineno))
| 92 => (ID ((yytext,!lineno),!lineno,!lineno))
| 94 => (pcount := 1; actionstart := (!lineno);
		    text := nil; YYBEGIN CODE; lex() before YYBEGIN A)
| 96 => (UNKNOWN(yytext,!lineno,!lineno))
| 98 => (inc pcount; Add yytext; lex())
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Internal.tab sub s
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = ordof(!yyb,l)
		val NewState = ordof(trans,NewChar)
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yypos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yypos,!yypos)
    end
end
  in lex
  end
end
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
	      val say = fn s => output(std_out,s)
              val lex_input = fn s => 
			let val done = ref false
  			in fn i =>
			    if (!done) then ""
			    else let val result = input(s,i)
				 in (if String.size result < i
					 then done := true
				         else ();
				     result)
				 end
			end
	      val error = fn (s,i:int,_) =>
		 (say file; say ", line "; say (makestring i);
		  say ": Error: "; say s; say "\n")
	      val in_str = open_in file
	      val stream =  Mlyacc.makeLexer (lex_input in_str)
	      val p = (Header.lineno := 1;
		       Lex.UserDeclarations.text :=[""];
		       Mlyacc.parse(15,stream,error,()))
	   in (close_in in_str; p)
	   end
  end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

signature ORDSET =
   sig
      type set
      type elem
      exception Select_arb
      val app : (elem -> 'b) -> set -> unit
	  and card: set -> int
          and closure: set * (elem -> set) -> set
          and difference: set * set -> set
          and elem_eq: (elem * elem -> bool)
	  and elem_gt : (elem * elem -> bool)
          and empty: set
	  and exists: (elem * set) -> bool
	  and find : (elem * set)  ->  elem option
	  and fold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and insert: (elem * set) -> set
          and is_empty: set -> bool
          and make_list: set -> elem list
          and make_set: (elem list -> set)
          and partition: (elem -> bool) -> (set -> set * set)
          and remove: (elem * set) -> set
	  and revfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and select_arb: set -> elem
	  and set_eq: (set * set) -> bool
	  and set_gt: (set * set) -> bool
          and singleton: (elem -> set)
          and union: set * set -> set
   end

signature TABLE =
   sig
	type 'a table
	type key
	val size : 'a table -> int
	val empty: 'a table
	val exists: (key * 'a table) -> bool
	val find : (key * 'a table)  ->  'a option
	val insert: ((key * 'a) * 'a table) -> 'a table
	val make_table : (key * 'a ) list -> 'a table
	val make_list : 'a table -> (key * 'a) list
	val fold : ((key * 'a) * 'b -> 'b) -> 'a table -> 'b -> 'b
   end

signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* Implementation of ordered sets using ordered lists and red-black trees.  The
   code for red-black trees was originally written by Norris Boyd, which was
   modified for use here.
*)   

(* ordered sets implemented using ordered lists.

   Upper bound running times for functions implemented here:

   app  = O(n)
   card = O(n)
   closure = O(n^2)
   difference = O(n+m), where n,m = the size of the two sets used here.
   empty = O(1)
   exists = O(n)
   find = O(n)
   fold = O(n)
   insert = O(n)
   is_empty = O(1)
   make_list = O(1)
   make_set = O(n^2)
   partition = O(n)
   remove = O(n)
   revfold = O(n)
   select_arb = O(1)
   set_eq = O(n), where n = the cardinality of the smaller set
   set_gt = O(n), ditto
   singleton = O(1)
   union = O(n+m)
*)

functor ListOrdSet(B : sig type elem
		  	val gt : elem * elem -> bool
			val eq : elem * elem -> bool
		    end ) : ORDSET =

struct
 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 type set = elem list
 exception Select_arb
 val empty = nil

 val insert = fn (key,s) =>
	let fun f (l as (h::t)) =
		 if elem_gt(key,h) then h::(f t)
		 else if elem_eq(key,h) then key::t
		 else key::l
 	      | f nil = [key]
	in f s
	end
		
 val select_arb = fn nil => raise Select_arb
 		   | a::b => a

 val exists = fn (key,s) =>
	let fun f (h::t) = if elem_gt(key,h) then f t
			   else elem_eq(h,key) 
 	      | f nil = false
	in f s
	end

 val find = fn (key,s) =>
	let fun f (h::t) = if elem_gt(key,h) then f t
			   else if elem_eq(h,key) then SOME h
			   else NONE
 	      | f nil = NONE
	in f s
	end
   
 val revfold = List.revfold
 val fold = List.fold
 val app = List.app

fun set_eq(h::t,h'::t') = 
	(case elem_eq(h,h')
	  of true => set_eq(t,t')
	   | a => a)
  | set_eq(nil,nil) = true
  | set_eq _ = false

fun set_gt(h::t,h'::t') =
	(case elem_gt(h,h')
	  of false => (case (elem_eq(h,h'))
			of true => set_gt(t,t')
			 | a => a)
	   |  a => a)
  | set_gt(_::_,nil) = true
  | set_gt _ = false
		
fun union(a as (h::t),b as (h'::t')) =
	  if elem_gt(h',h) then h::union(t,b)
	  else if elem_eq(h,h') then h::union(t,t')
	  else h'::union(a,t')
  | union(nil,s) = s
  | union(s,nil) = s

val make_list = fn s => s

val is_empty = fn nil => true | _ => false

val make_set = fn l => List.fold insert l nil

val partition = fn f => fn s =>
    fold (fn (e,(yes,no)) =>
	    if (f e) then (e::yes,no) else (e::no,yes)) s (nil,nil)

val remove = fn (e,s) =>
    let fun f (l as (h::t)) = if elem_gt(h,e) then l
			      else if elem_eq(h,e) then t
			      else h::(f t)
	  | f nil = nil
    in f s
    end

 (* difference: X-Y *)

 fun difference (nil,_) = nil
   | difference (r,nil) = r
   | difference (a as (h::t),b as (h'::t')) =
	  if elem_gt (h',h) then h::difference(t,b)
	  else if elem_eq(h',h) then difference(t,t')
	  else difference(a,t')

 fun singleton X = [X]

 fun card(S) = fold (fn (a,count) => count+1) S 0

      local
	    fun closure'(from, f, result) =
	      if is_empty from then result
	      else
		let val (more,result) =
			fold (fn (a,(more',result')) =>
				let val more = f a
				    val new = difference(more,result)
				in (union(more',new),union(result',new))
				end) from
				 (empty,result)
		in closure'(more,f,result)
		end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
end

(* ordered set implemented using red-black trees:

   Upper bound running time of the functions below:

   app: O(n)
   card: O(n)
   closure: O(n^2 ln n)
   difference: O(n ln n)
   empty: O(1)
   exists: O(ln n)
   find: O(ln n)
   fold: O(n)
   insert: O(ln n)
   is_empty: O(1)
   make_list: O(n)
   make_set: O(n ln n)
   partition: O(n ln n)
   remove: O(n ln n)
   revfold: O(n)
   select_arb: O(1)
   set_eq: O(n)
   set_gt: O(n)
   singleton: O(1)
   union: O(n ln n)
*)

functor RbOrdSet (B : sig type elem
			 val eq : (elem*elem) -> bool
		 	 val gt : (elem*elem) -> bool
		     end
		) : ORDSET =
struct

 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 datatype Color = RED | BLACK

 abstype set = EMPTY | TREE of (B.elem * Color * set * set)
 with exception Select_arb
      val empty = EMPTY

 fun insert(key,t) =
  let fun f EMPTY = TREE(key,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
	    if elem_gt (key,k)
	    then case f r
		 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
			(case l
			 of TREE(lk,RED,ll,lr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
						TREE(rk,RED,rlr,rr)))
		  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
			(case l
			 of TREE(lk,RED,ll,lr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
	          | r => TREE(k,BLACK,l,r)
	    else if elem_gt(k,key)
	    then case f l
	         of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
			(case r
			 of TREE(rk,RED,rl,rr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
						TREE(k,RED,lrr,r)))
		  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
			(case r
			 of TREE(rk,RED,rl,rr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
	          | l => TREE(k,BLACK,l,r)
	    else TREE(key,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
	    if elem_gt(key,k) then TREE(k,RED,l, f r)
	    else if elem_gt(k,key) then TREE(k,RED, f l, r)
	    else TREE(key,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun select_arb (TREE(k,_,l,r)) = k
   | select_arb EMPTY = raise Select_arb
   
 fun exists(key,t) =
  let fun look EMPTY = false
	| look (TREE(k,_,l,r)) =
		if elem_gt(k,key) then look l
		else if elem_gt(key,k) then look r
		else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
	| look (TREE(k,_,l,r)) =
		if elem_gt(k,key) then look l
		else if elem_gt(key,k) then look r
		else SOME k
   in look t
  end

  fun revfold f t start =
     let fun scan (EMPTY,value) = value
	   | scan (TREE(k,_,l,r),value) = scan(r,f(k,scan(l,value)))
     in scan(t,start)
     end

   fun fold f t start =
	let fun scan(EMPTY,value) = value
	      | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
	in scan(t,start)
	end

   fun app f t =
      let fun scan EMPTY = ()
            | scan(TREE(k,_,l,r)) = (scan l; f k; scan r)
      in scan t
      end

(* equal_tree : test if two trees are equal.  Two trees are equal if
   the set of leaves are equal *)

   fun set_eq (tree1 as (TREE _),tree2 as (TREE _)) =
     let datatype pos = L | R | M
	 exception Done
	 fun getvalue(stack as ((a,position)::b)) =
	    (case a
	     of (TREE(k,_,l,r)) =>
		(case position
		 of L => getvalue ((l,L)::(a,M)::b)
		  | M => (k,case r of  EMPTY => b | _ => (a,R)::b)
		  | R => getvalue ((r,L)::b)
		 )
	      | EMPTY => getvalue b
	     )
	    | getvalue(nil) = raise Done
	  fun f (nil,nil) = true
	    | f (s1 as (_ :: _),s2 as (_ :: _ )) =
			  let val (v1,news1) = getvalue s1
			      and (v2,news2) = getvalue s2
			  in (elem_eq(v1,v2)) andalso f(news1,news2)
			  end
	    | f _ = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end
    | set_eq (EMPTY,EMPTY) = true
    | set_eq _ = false

   (* gt_tree : Test if tree1 is greater than tree 2 *)

   fun set_gt (tree1,tree2) =
     let datatype pos = L | R | M
	 exception Done
	 fun getvalue(stack as ((a,position)::b)) =
	    (case a
	     of (TREE(k,_,l,r)) =>
		(case position
		 of L => getvalue ((l,L)::(a,M)::b)
		  | M => (k,case r of EMPTY => b | _ => (a,R)::b)
		  | R => getvalue ((r,L)::b)
		 )
	      | EMPTY => getvalue b
	     )
	    | getvalue(nil) = raise Done
	  fun f (nil,nil) = false
	    | f (s1 as (_ :: _),s2 as (_ :: _ )) =
			  let val (v1,news1) = getvalue s1
			      and (v2,news2) = getvalue s2
			  in (elem_gt(v1,v2)) orelse (elem_eq(v1,v2) andalso f(news1,news2))
			  end
	    | f (_,nil) = true
	    | f (nil,_) = false
      in f ((tree1,L)::nil,(tree2,L)::nil) handle Done => false
      end

      fun is_empty S = (let val _ = select_arb S in false end
                         handle Select_arb => true)

      fun make_list S = fold (op ::) S nil

      fun make_set l = List.fold insert l empty

      fun partition F S = fold (fn (a,(Yes,No)) =>
				if F(a) then (insert(a,Yes),No)
				else (Yes,insert(a,No)))
			     S (empty,empty)

      fun remove(X, XSet) =
             let val (YSet, _) =
                        partition (fn a => not (elem_eq (X, a))) XSet
             in  YSet
             end

      fun difference(Xs, Ys) =
	   fold (fn (p as (a,Xs')) =>
		      if exists(a,Ys) then Xs' else insert p)
	   Xs empty

      fun singleton X = insert(X,empty)

      fun card(S) = fold (fn (_,count) => count+1) S 0

      fun union(Xs,Ys)= fold insert Ys Xs

      local
	    fun closure'(from, f, result) =
	      if is_empty from then result
	      else
		let val (more,result) =
			fold (fn (a,(more',result')) =>
				let val more = f a
				    val new = difference(more,result)
				in (union(more',new),union(result',new))
				end) from
				 (empty,result)
		in closure'(more,f,result)
		end
      in
         fun closure(start, f) = closure'(start, f, start)
      end
   end
end

signature TABLE =
   sig
	type 'a table
	type key
	val size : 'a table -> int
	val empty: 'a table
	val exists: (key * 'a table) -> bool
	val find : (key * 'a table)  ->  'a option
	val insert: ((key * 'a) * 'a table) -> 'a table
	val make_table : (key * 'a ) list -> 'a table
	val make_list : 'a table -> (key * 'a) list
	val fold : ((key * 'a) * 'b -> 'b) -> 'a table -> 'b -> 'b
   end

functor Table (B : sig type key
		      val gt : (key * key) -> bool
		     end
		) : TABLE =
struct

 datatype Color = RED | BLACK
 type key = B.key

 abstype 'a table = EMPTY
		  | TREE of ((B.key * 'a ) * Color * 'a table * 'a table)
 with

 val empty = EMPTY

 fun insert(elem as (key,data),t) =
  let val key_gt = fn (a,_) => B.gt(key,a)
      val key_lt = fn (a,_) => B.gt(a,key)
	fun f EMPTY = TREE(elem,RED,EMPTY,EMPTY)
        | f (TREE(k,BLACK,l,r)) =
	    if key_gt k
	    then case f r
		 of r as TREE(rk,RED, rl as TREE(rlk,RED,rll,rlr),rr) =>
			(case l
			 of TREE(lk,RED,ll,lr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(rlk,BLACK,TREE(k,RED,l,rll),
						TREE(rk,RED,rlr,rr)))
		  | r as TREE(rk,RED,rl, rr as TREE(rrk,RED,rrl,rrr)) =>
			(case l
			 of TREE(lk,RED,ll,lr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(rk,BLACK,TREE(k,RED,l,rl),rr))
	          | r => TREE(k,BLACK,l,r)
	    else if key_lt k
	    then case f l
	         of l as TREE(lk,RED,ll, lr as TREE(lrk,RED,lrl,lrr)) =>
			(case r
			 of TREE(rk,RED,rl,rr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(lrk,BLACK,TREE(lk,RED,ll,lrl),
						TREE(k,RED,lrr,r)))
		  | l as TREE(lk,RED, ll as TREE(llk,RED,lll,llr), lr) =>
			(case r
			 of TREE(rk,RED,rl,rr) =>
				TREE(k,RED,TREE(lk,BLACK,ll,lr),
					   TREE(rk,BLACK,rl,rr))
			  | _ => TREE(lk,BLACK,ll,TREE(k,RED,lr,r)))
	          | l => TREE(k,BLACK,l,r)
	    else TREE(elem,BLACK,l,r)
        | f (TREE(k,RED,l,r)) =
	    if key_gt k then TREE(k,RED,l, f r)
	    else if key_lt k then TREE(k,RED, f l, r)
	    else TREE(elem,RED,l,r)
   in case f t
      of TREE(k,RED, l as TREE(_,RED,_,_), r) => TREE(k,BLACK,l,r)
       | TREE(k,RED, l, r as TREE(_,RED,_,_)) => TREE(k,BLACK,l,r)
       | t => t
  end

 fun exists(key,t) =
  let fun look EMPTY = false
	| look (TREE((k,_),_,l,r)) =
		if B.gt(k,key) then look l
		else if B.gt(key,k) then look r
		else true
   in look t
   end

 fun find(key,t) =
  let fun look EMPTY = NONE
	| look (TREE((k,data),_,l,r)) =
		if B.gt(k,key) then look l
		else if B.gt(key,k) then look r
		else SOME data
   in look t
  end

  fun fold f t start =
	let fun scan(EMPTY,value) = value
	      | scan(TREE(k,_,l,r),value) = scan(l,f(k,scan(r,value)))
	in scan(t,start)
	end

  fun make_table l = List.fold insert l empty

  fun size S = fold (fn (_,count) => count+1) S 0

  fun make_list table = fold (op ::) table nil

  end
end;

(* assumes that a functor Table with signature TABLE from table.sml is
   in the environment *)

signature HASH =
  sig
    type table
    type elem

    val size : table -> int
    val add : elem * table -> table
    val find : elem * table -> int option
    val exists : elem * table -> bool
    val empty : table
  end

(* hash: creates a hash table of size n which assigns each distinct member
   a unique integer between 0 and n-1 *)

functor Hash(B : sig type elem
		     val gt : elem * elem -> bool
		 end) : HASH =
struct
    type elem=B.elem
    structure HashTable = Table(type key=B.elem
				val gt = B.gt)

    type table = {count : int, table : int HashTable.table}

    val empty = {count=0,table=HashTable.empty}
    val size = fn {count,table} => count
    val add = fn (e,{count,table}) =>
		{count=count+1,table=HashTable.insert((e,count),table)}
    val find = fn (e,{table,count}) => HashTable.find(e,table)
    val exists = fn (e,{table,count}) => HashTable.exists(e,table)
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

signature GRAMMAR =
    sig
	
	datatype term = T of int
	datatype nonterm = NT of int
	datatype symbol = TERM of term | NONTERM of nonterm

	(* grammar:
	     terminals should be numbered from 0 to terms-1,
	     nonterminals should be numbered from 0 to nonterms-1,
	     rules should be numbered between 0 and (length rules) - 1,
	     higher precedence binds tighter,
	     start nonterminal should not occur on the rhs of any rule
	*)

	datatype grammar = GRAMMAR of
			{rules: {lhs : nonterm, rhs : symbol list,
				 precedence : int option, rulenum : int } list,
			terms: int,
			nonterms: int,
			start : nonterm,
			eop : term list,
			noshift : term list,
			precedence : term -> int option,
			termToString : term -> string,
			nontermToString : nonterm -> string} 
   end

(* signature for internal version of grammar *)

signature INTGRAMMAR =
    sig
	structure Grammar  : GRAMMAR
	structure SymbolAssoc : TABLE
	structure NontermAssoc : TABLE

	sharing type SymbolAssoc.key = Grammar.symbol
	sharing type NontermAssoc.key = Grammar.nonterm

	datatype rule = RULE of
		{lhs : Grammar.nonterm,
		 rhs : Grammar.symbol list,

	(* internal number of rule - convenient for producing LR graph *)

		 num : int,	
		 rulenum : int,
		 precedence : int option}

	val gtTerm : Grammar.term * Grammar.term -> bool
	val eqTerm : Grammar.term * Grammar.term -> bool

	val gtNonterm : Grammar.nonterm * Grammar.nonterm -> bool
	val eqNonterm : Grammar.nonterm * Grammar.nonterm -> bool

	val gtSymbol : Grammar.symbol * Grammar.symbol -> bool
	val eqSymbol : Grammar.symbol * Grammar.symbol -> bool

	(* Debugging information will be generated only if DEBUG is true. *)

	val DEBUG : bool

	val prRule : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
				(string -> 'b) -> rule -> unit
	val prGrammar : (Grammar.symbol -> string)*(Grammar.nonterm -> string) *
				(string -> 'b) -> Grammar.grammar -> unit
    end

signature CORE =
    sig
	structure Grammar : GRAMMAR
	structure IntGrammar : INTGRAMMAR
	sharing Grammar = IntGrammar.Grammar

	datatype item = ITEM of
			{ rule : IntGrammar.rule,
			  dot : int,

(* rhsAfter: The portion of the rhs of a rule that lies after the dot *)

			  rhsAfter: Grammar.symbol list }

(* eqItem and gtItem compare items *)

	val eqItem : item * item -> bool
	val gtItem : item * item -> bool

(* functions for maintaining ordered item lists *)

	val insert : item * item list -> item list
	val union : item list * item list -> item list

(* core:  a set of items.  It is represented by an ordered list of items. 
   The list is in ascending order The rule numbers and the positions of the
   dots are used to order the items. *)

	datatype core = CORE of item list * int (* state # *)

(* gtCore and eqCore compare the lists of items *)

	val gtCore : core * core -> bool
	val eqCore : core * core -> bool

(* functions for debugging *)

	val prItem : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
				(string -> unit) -> item -> unit
	val prCore : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
				(string -> unit) -> core -> unit
end

signature CORE_UTILS =
    sig

	structure Grammar : GRAMMAR
	structure IntGrammar : INTGRAMMAR
	structure Core : CORE

	sharing Grammar = IntGrammar.Grammar = Core.Grammar
	sharing IntGrammar = Core.IntGrammar

(* mkFuncs: create functions for the set of productions derived from a
   nonterminal, the cores that result from shift/gotos from a core,
    and return a list of rules *)

	val mkFuncs : Grammar.grammar ->
		{ produces : Grammar.nonterm -> IntGrammar.rule list,

(* shifts: take a core and compute all the cores that result from shifts/gotos
   on symbols *)

		  shifts : Core.core -> (Grammar.symbol*Core.item list) list,
		  rules: IntGrammar.rule list,

(* epsProds: take a core compute epsilon productions for it *)

		  epsProds : Core.core -> IntGrammar.rule list}
	end

signature LRGRAPH =
    sig
	structure Grammar : GRAMMAR
	structure IntGrammar : INTGRAMMAR
	structure Core : CORE

	sharing Grammar = IntGrammar.Grammar = Core.Grammar
	sharing IntGrammar = Core.IntGrammar

	type graph
	val edges : Core.core * graph -> {edge:Grammar.symbol,to:Core.core} list
	val nodes : graph -> Core.core list
	val shift : graph -> int * Grammar.symbol -> int (* int = state # *)
	val core : graph -> int -> Core.core (* get core for a state *)

(* mkGraph: compute the LR(0) sets of items *)

	val mkGraph :  Grammar.grammar ->
			 {graph : graph,
			  produces : Grammar.nonterm -> IntGrammar.rule list,
			  rules : IntGrammar.rule list,
			  epsProds: Core.core -> IntGrammar.rule list}

	val prGraph: (Grammar.symbol -> string)*(Grammar.nonterm -> string) *
				(string -> unit) -> graph -> unit
    end

signature LOOK =
    sig
 	structure Grammar : GRAMMAR
	structure IntGrammar : INTGRAMMAR
	sharing Grammar = IntGrammar.Grammar

	val union : Grammar.term list * Grammar.term list -> Grammar.term list
	val make_set : Grammar.term list -> Grammar.term list

	val mkFuncs :  {rules : IntGrammar.rule list, nonterms : int,
			produces : Grammar.nonterm -> IntGrammar.rule list} ->
		 	    {nullable: Grammar.nonterm -> bool,
			     first : Grammar.symbol list -> Grammar.term list}

	val prLook : (Grammar.term -> string) * (string -> unit) -> 
			Grammar.term list -> unit
   end

signature LALR_GRAPH =
    sig
	structure Grammar : GRAMMAR
	structure IntGrammar : INTGRAMMAR
	structure Core : CORE
	structure Graph : LRGRAPH

	sharing Grammar = IntGrammar.Grammar = Core.Grammar = Graph.Grammar
	sharing IntGrammar = Core.IntGrammar = Graph.IntGrammar
	sharing Core = Graph.Core

	datatype lcore = LCORE of (Core.item * Grammar.term list) list * int
	val addLookahead : {graph : Graph.graph,
			    first : Grammar.symbol list -> Grammar.term list,
			    eop : Grammar.term list,
			    nonterms : int,
			    nullable: Grammar.nonterm -> bool,
			    produces : Grammar.nonterm -> IntGrammar.rule list,
			    rules : IntGrammar.rule list,
			    epsProds : Core.core -> IntGrammar.rule list,
			    print : string -> unit,  (* for debugging *)
			    termToString : Grammar.term -> string,
			    nontermToString : Grammar.nonterm -> string} ->
				lcore list
	val prLcore : (Grammar.symbol -> string) * (Grammar.nonterm -> string) *
		      (Grammar.term -> string) * (string -> unit) ->
					 lcore -> unit
    end

(* LR_ERRS: errors found while constructing an LR table *)

signature LR_ERRS =
  sig
    structure LrTable : LR_TABLE

    (* RR = reduce/reduce,
       SR = shift/reduce
       NONASSOC: a rule and a nonassociative terminal in a s/r conflict have
       the same precedence.  Since the terminal is nonassociative, we cannot
       use whether it is left or right associative to decide whether to reduce
       or shift.
       NS: non-shiftable terminal found on the rhs of a rule
       NOT_REDUCED n: rule number n was not reduced
       START n : start symbol found on the rhs of rule n *)

    datatype err = RR of LrTable.term * LrTable.state * int * int
		 | SR of LrTable.term * LrTable.state * int
		 | NONASSOC of LrTable.term * LrTable.state * int
		 | NS of LrTable.term * int  
		 | NOT_REDUCED of int
	         | START of int

     val summary : err list -> {rr : int, sr: int,nonassoc:int,
			    not_reduced : int, start : int,nonshift : int}

     val printSummary : (string -> unit) -> err list -> unit
				      
  end

(* MAKE_STRUCT: prints a structure which includes a value 'table' and a
   structure Table whose signature matches LR_TABLE.  The table in the printed
   structure will contain the same information as the one passed to makeStruct,
   although the representation may be different.
*)
  
signature MAKE_STRUCT =
  sig
	structure LrTable : LR_TABLE
	val makeStruct :
		{table : LrTable.table,
		 name : string,
		 print: string -> unit
		} -> unit
  end

(* VERBOSE: signature for a structure which takes a table and creates a
   verbose description of it *)

signature VERBOSE =
  sig
	structure Errs : LR_ERRS
	val printVerbose :
		{table : Errs.LrTable.table,
	         termToString : Errs.LrTable.term -> string,
	         nontermToString : Errs.LrTable.nonterm -> string,
		 stateErrs : Errs.LrTable.state -> Errs.err list,
		 errs : Errs.err list,
		 print: string -> unit,
		 printCores : (string -> unit) -> Errs.LrTable.state -> unit,
		 printRule : (string -> unit) -> int -> unit} -> unit
  end

(* MAKE_LR_TABLE: signature for a structure which includes a structure
   matching the signature LR_TABLE and a function which maps grammars
   to tables *)

signature MAKE_LR_TABLE =
   sig
	structure Grammar : GRAMMAR
	structure Errs : LR_ERRS
	structure LrTable : LR_TABLE
	sharing Errs.LrTable = LrTable

	sharing type LrTable.term = Grammar.term
	sharing type LrTable.nonterm = Grammar.nonterm

	(* boolean value determines whether default reductions will be used.
	   If it is true, reductions will be used. *)

	val mkTable : Grammar.grammar * bool ->
	       LrTable.table *
	      (LrTable.state -> Errs.err list) *   (* errors in a state *)
	      ((string -> unit) -> LrTable.state -> unit) *
	       Errs.err list	(* list of all errors *)
   end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure Grammar : GRAMMAR =
	struct

		(* define types term and nonterm using those in LrTable
		   datatype term = T of int 
		   datatype nonterm = NT of int *)

		open LrTable
		datatype symbol = TERM of term | NONTERM of nonterm
		datatype grammar = GRAMMAR of
				{rules: {lhs: nonterm,
				 	 rhs: symbol list, 
				 	 precedence: int option,
				 	 rulenum: int} list,
				noshift : term list,
				eop : term list,
				terms: int,
				nonterms: int,
				start : nonterm,
				precedence : term -> int option,
				termToString : term -> string,
				nontermToString : nonterm -> string}
end;

structure IntGrammar : INTGRAMMAR =
	struct
		structure Grammar = Grammar
		open Grammar

		datatype rule = RULE of
			 	{lhs: nonterm,
				 rhs: symbol list,
				 num: int,(* internal # assigned by coreutils *)
				 rulenum: int,
				 precedence: int option}
		
		val eqTerm = (op =)
		val gtTerm = fn (T i,T j) => i>j

		val eqNonterm = (op =)
		val gtNonterm = fn (NT i,NT j) => i>j

		val eqSymbol = (op =)
		val gtSymbol = fn (TERM (T i),TERM (T j)) => i>j
				| (NONTERM (NT i),NONTERM (NT j)) => i>j
				| (TERM _,NONTERM _) => false
				| (NONTERM _,TERM _) => true


		structure SymbolAssoc = Table(type key = symbol
					      val gt = gtSymbol)

		structure NontermAssoc = Table(type key =  nonterm
					       val gt = gtNonterm)

		val DEBUG = false

		val prRule = fn (a as symbolToString,nontermToString,print) =>
		   let val printSymbol = print o symbolToString
		       fun printRhs (h::t) = (printSymbol h; print " ";
					      printRhs t)
			 | printRhs nil = ()
		   in fn (RULE {lhs,rhs,num,rulenum,precedence,...}) =>
			((print o nontermToString) lhs; print " : ";
			 printRhs rhs;
			 if DEBUG then (print " num = ";
					print (makestring num);
					print " rulenum = ";
					print (makestring rulenum);
					print " precedence = ";
					case precedence
					    of NONE => print " none"
					     | (SOME i) =>
						 print (makestring i);
					())
			else ())
		   end
			
		val prGrammar =
			 fn (a as (symbolToString,nontermToString,print)) =>
			     fn (GRAMMAR {rules,terms,nonterms,start,...}) =>
		 let val printRule =
			let val prRule = prRule a
			in  fn {lhs,rhs,precedence,rulenum} =>
		   	     (prRule (RULE {lhs=lhs,rhs=rhs,num=0,
				      rulenum=rulenum, precedence=precedence});
			      print "\n")
			end
		 in print "grammar = \n";
		    List.app printRule rules;
		    print "\n";
		    print (" terms = " ^ (makestring terms) ^
			     " nonterms = " ^ (makestring nonterms) ^
			     " start = ");
		    (print o nontermToString) start;
		    ()
		 end
	end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkCore(structure IntGrammar : INTGRAMMAR) : CORE =
	struct
		open IntGrammar
		open  Grammar
		structure IntGrammar = IntGrammar
		structure Grammar = Grammar

		datatype item = ITEM of
				{ rule : rule,
				  dot : int,
				  rhsAfter : symbol list
				}

		val eqItem = fn (ITEM{rule=RULE{num=n,...},dot=d,...},
				 ITEM{rule=RULE{num=m,...},dot=e,...}) =>
					n=m andalso d=e

		val gtItem =  fn (ITEM{rule=RULE{num=n,...},dot=d,...},
				  ITEM{rule=RULE{num=m,...},dot=e,...}) =>
					n>m orelse (n=m andalso d>e)

		structure ItemList = ListOrdSet
			(struct
				type elem = item
				val eq = eqItem
				val gt = gtItem
			end)
		
		open ItemList
		datatype core = CORE of item list * int

		val gtCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_gt(a,b)
		val eqCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_eq(a,b)

		(* functions for printing and debugging *)

		 val prItem = fn (symbolToString,nontermToString,print) =>
		   let val printInt = print o (makestring : int -> string)
		       val prSymbol = print o symbolToString
		       val prNonterm = print o nontermToString
		       fun showRest nil = ()
			 | showRest (h::t) = (prSymbol h; print " "; showRest t)
		       fun showRhs (l,0) = (print ". "; showRest l)
			 | showRhs (nil,_) = ()
			 | showRhs (h::t,n) = (prSymbol h;
					       print " ";
					       showRhs(t,n-1))
		   in fn (ITEM {rule=RULE {lhs,rhs,rulenum,num,...},
				dot,rhsAfter,...}) =>
			(prNonterm lhs; print " : "; showRhs(rhs,dot);
		 	 case rhsAfter 
			 of nil => (print " (reduce by rule "; 
				    printInt rulenum;
				    print ")")
			  | _ => ();
			  if DEBUG then 
			     (print " (num "; printInt num; print ")")
			  else ())
		   end

	         val prCore = fn a as (_,_,print) =>
		    let val prItem = prItem a
		    in fn (CORE (items,state)) =>
			  (print "state ";
			   print (makestring state);
		   	   print ":\n\n";
		   	   app (fn i => (print "\t";
					 prItem i; print "\n")) items;
			   print "\n")
		    end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure Grammar : GRAMMAR =
	struct

		(* define types term and nonterm using those in LrTable
		   datatype term = T of int 
		   datatype nonterm = NT of int *)

		open LrTable
		datatype symbol = TERM of term | NONTERM of nonterm
		datatype grammar = GRAMMAR of
				{rules: {lhs: nonterm,
				 	 rhs: symbol list, 
				 	 precedence: int option,
				 	 rulenum: int} list,
				noshift : term list,
				eop : term list,
				terms: int,
				nonterms: int,
				start : nonterm,
				precedence : term -> int option,
				termToString : term -> string,
				nontermToString : nonterm -> string}
end;

structure IntGrammar : INTGRAMMAR =
	struct
		structure Grammar = Grammar
		open Grammar

		datatype rule = RULE of
			 	{lhs: nonterm,
				 rhs: symbol list,
				 num: int,(* internal # assigned by coreutils *)
				 rulenum: int,
				 precedence: int option}
		
		val eqTerm = (op =)
		val gtTerm = fn (T i,T j) => i>j

		val eqNonterm = (op =)
		val gtNonterm = fn (NT i,NT j) => i>j

		val eqSymbol = (op =)
		val gtSymbol = fn (TERM (T i),TERM (T j)) => i>j
				| (NONTERM (NT i),NONTERM (NT j)) => i>j
				| (TERM _,NONTERM _) => false
				| (NONTERM _,TERM _) => true


		structure SymbolAssoc = Table(type key = symbol
					      val gt = gtSymbol)

		structure NontermAssoc = Table(type key =  nonterm
					       val gt = gtNonterm)

		val DEBUG = false

		val prRule = fn (a as symbolToString,nontermToString,print) =>
		   let val printSymbol = print o symbolToString
		       fun printRhs (h::t) = (printSymbol h; print " ";
					      printRhs t)
			 | printRhs nil = ()
		   in fn (RULE {lhs,rhs,num,rulenum,precedence,...}) =>
			((print o nontermToString) lhs; print " : ";
			 printRhs rhs;
			 if DEBUG then (print " num = ";
					print (makestring num);
					print " rulenum = ";
					print (makestring rulenum);
					print " precedence = ";
					case precedence
					    of NONE => print " none"
					     | (SOME i) =>
						 print (makestring i);
					())
			else ())
		   end
			
		val prGrammar =
			 fn (a as (symbolToString,nontermToString,print)) =>
			     fn (GRAMMAR {rules,terms,nonterms,start,...}) =>
		 let val printRule =
			let val prRule = prRule a
			in  fn {lhs,rhs,precedence,rulenum} =>
		   	     (prRule (RULE {lhs=lhs,rhs=rhs,num=0,
				      rulenum=rulenum, precedence=precedence});
			      print "\n")
			end
		 in print "grammar = \n";
		    List.app printRule rules;
		    print "\n";
		    print (" terms = " ^ (makestring terms) ^
			     " nonterms = " ^ (makestring nonterms) ^
			     " start = ");
		    (print o nontermToString) start;
		    ()
		 end
	end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkCore(structure IntGrammar : INTGRAMMAR) : CORE =
	struct
		open IntGrammar
		open  Grammar
		structure IntGrammar = IntGrammar
		structure Grammar = Grammar

		datatype item = ITEM of
				{ rule : rule,
				  dot : int,
				  rhsAfter : symbol list
				}

		val eqItem = fn (ITEM{rule=RULE{num=n,...},dot=d,...},
				 ITEM{rule=RULE{num=m,...},dot=e,...}) =>
					n=m andalso d=e

		val gtItem =  fn (ITEM{rule=RULE{num=n,...},dot=d,...},
				  ITEM{rule=RULE{num=m,...},dot=e,...}) =>
					n>m orelse (n=m andalso d>e)

		structure ItemList = ListOrdSet
			(struct
				type elem = item
				val eq = eqItem
				val gt = gtItem
			end)
		
		open ItemList
		datatype core = CORE of item list * int

		val gtCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_gt(a,b)
		val eqCore = fn (CORE (a,_),CORE (b,_)) => ItemList.set_eq(a,b)

		(* functions for printing and debugging *)

		 val prItem = fn (symbolToString,nontermToString,print) =>
		   let val printInt = print o (makestring : int -> string)
		       val prSymbol = print o symbolToString
		       val prNonterm = print o nontermToString
		       fun showRest nil = ()
			 | showRest (h::t) = (prSymbol h; print " "; showRest t)
		       fun showRhs (l,0) = (print ". "; showRest l)
			 | showRhs (nil,_) = ()
			 | showRhs (h::t,n) = (prSymbol h;
					       print " ";
					       showRhs(t,n-1))
		   in fn (ITEM {rule=RULE {lhs,rhs,rulenum,num,...},
				dot,rhsAfter,...}) =>
			(prNonterm lhs; print " : "; showRhs(rhs,dot);
		 	 case rhsAfter 
			 of nil => (print " (reduce by rule "; 
				    printInt rulenum;
				    print ")")
			  | _ => ();
			  if DEBUG then 
			     (print " (num "; printInt num; print ")")
			  else ())
		   end

	         val prCore = fn a as (_,_,print) =>
		    let val prItem = prItem a
		    in fn (CORE (items,state)) =>
			  (print "state ";
			   print (makestring state);
		   	   print ":\n\n";
		   	   app (fn i => (print "\t";
					 prItem i; print "\n")) items;
			   print "\n")
		    end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkCoreUtils(structure Core : CORE) : CORE_UTILS =
	struct
		val DEBUG = true
		structure Core = Core
		structure IntGrammar = Core.IntGrammar
		structure Grammar = IntGrammar.Grammar

		open Grammar IntGrammar Core

		structure Assoc = SymbolAssoc

		structure NtList = ListOrdSet
			(struct
				type elem = nonterm
				val eq = eqNonterm
				val gt = gtNonterm
			 end)

	val mkFuncs = fn (GRAMMAR {rules,terms,nonterms,...}) =>
	   let val derives=array(nonterms,nil : rule list)

(* sort rules by their lhs nonterminal by placing them in an array indexed
   in their lhs nonterminal *)

	       val _ =
		 let val f = fn {lhs=lhs as (NT n), rhs, precedence,rulenum} =>
			let val rule=RULE{lhs=lhs,rhs=rhs,precedence=precedence,
					  rulenum=rulenum,num=0}
		        in update(derives,n,rule::(derives sub n))
			end
		  in app f rules
		  end

(* renumber rules so that rule numbers increase monotonically with
   the number of their lhs nonterminal, and so that rules are numbered
   sequentially.  **Functions below assume that this number is true**, 
   i.e. productions for nonterm i are numbered from j to k, 
   productions for nonterm i+1 are numbered from k+1 to m, and
   productions for nonterm 0 start at 0 *)

		val _ =
		   let val f =
		         fn (RULE{lhs,rhs,precedence,rulenum,num}, (l,i)) =>
			    (RULE{lhs=lhs,rhs=rhs, precedence=precedence,
				  rulenum=rulenum, num=i}::l,i+1)
			fun g(i,num) =
		          if i<nonterms then
			    let val (l,n) =
				        List.fold f (derives sub i) (nil,num)
			    in update(derives,i,rev l); g(i+1,n)
			    end
			  else ()
		    in g(0,0)
		    end

(* list of rules - sorted by rule number. *)

		 val rules = 
		     let fun g i =
			if i < nonterms then (derives sub i) @ (g (i+1))
			else nil
		     in g 0
		     end

(* produces: set of productions with nonterminal n as the lhs.  The set
   of productions *must* be sorted by rule number, because functions
   below assume that this list is sorted *)

		val produces = fn (NT n) =>
		  if DEBUG andalso (n<0 orelse n>=nonterms) then
		     let exception Produces of int in raise (Produces n) end
		  else derives sub n

		val memoize = fn f =>
		   let fun loop i = if i = nonterms then nil
				  else f (NT i) :: (loop (i+1))
		       val data = arrayoflist(loop 0)
		   in fn (NT i) => data sub i
		   end

 (* compute nonterminals which must be added to a closure when a given
    nonterminal is added, i.e all nonterminals C for each nonterminal A such
    that A =*=> Cx *)

		val nontermClosure =
			let val collectNonterms = fn n =>
			      List.fold (fn (r,l) =>
				  case r
				  of RULE {rhs=NONTERM n :: _,...} =>
					    NtList.insert(n,l)
				   | _ => l) (produces n) NtList.empty
			    val closureNonterm = fn n =>
				   NtList.closure(NtList.singleton n,
						  collectNonterms)
			in memoize closureNonterm
			end

(* ntShifts: Take the items produced by a nonterminal, and sort them
   by their first symbol.  For each first symbol, make sure the item
   list associated with the symbol is sorted also.   ** This function
   assumes that the item list returned by produces is sorted **

   Create a table of item lists keyed by symbols.  Scan the list
   of items produced by a nonterminal, and insert those with a first
   symbol on to the beginning of the item list for that symbol, creating
   a list if necessary.  Since produces returns an item list that is
   already in order, the list for each symbol will also end up in order.
 *)

		fun sortItems nt =
		 let fun add_item (a as RULE{rhs=symbol::rest,...},r) =
		       let val item = ITEM{rule=a,dot=1,rhsAfter=rest}
		       in Assoc.insert((symbol,case Assoc.find (symbol,r)
			  			of SOME l => item::l
			   			 | NONE => [item]),r)
		       end
		       | add_item (_,r) = r
		 in fold add_item (produces nt) Assoc.empty
		 end

		 val ntShifts = memoize sortItems

(* getNonterms: get the nonterminals with a .  before them in a core.
   Returns a list of nonterminals in ascending order *)

		fun getNonterms l =
		  List.fold (fn (ITEM {rhsAfter=NONTERM sym ::_, ...},r) =>
				NtList.insert(sym,r)
			      | (_,r) => r) l nil

(* closureNonterms: compute the nonterminals that would have a . before them
   in the closure of the core.  Returns a list of nonterminals in ascending
   order *)
		fun closureNonterms a =
			let val nonterms = getNonterms a
		 	in List.fold (fn (nt,r) =>
				   NtList.union(nontermClosure nt,r))
			   nonterms nonterms
			end

(* shifts: compute the core sets that result from shift/gotoing on 
   the closure of a kernal set.  The items in core sets are sorted, of
   course.

   (1) compute the core sets that result just from items added
       through the closure operation.
   (2) then add the shift/gotos on kernal items.

   We can do (1) the following way.  Keep a table  which for each shift/goto
symbol gives the list of items that result from shifting or gotoing on the
symbol.  Compute the nonterminals that would have dots before them in the
closure of the kernal set.  For each of these nonterminals, we already have an
item list in sorted order for each possible shift symbol.  Scan the nonterminal
list from back to front.  For each nonterminal, prepend the shift/goto list
for each shift symbol to the list already in the table.

   We end up with the list of items in correct order for each shift/goto
symbol.  We have kept the item lists in order, scanned the nonterminals from
back to front (=> that the items end up in ascending order), and never had any
duplicate items (each item is derived from only one nonterminal). *)

	fun shifts (CORE (itemList,_)) =
	    let

(* mergeShiftItems: add an item list for a shift/goto symbol to the table *)

fun mergeShiftItems (args as ((k,l),r)) =
		  case Assoc.find(k,r)
		  of NONE => Assoc.insert args
		   | SOME old => Assoc.insert ((k,l@old),r)

(* mergeItems: add all items derived from a nonterminal to the table.  We've
   kept these items sorted by their shift/goto symbol (the first symbol on
   their rhs) *)

		fun mergeItems (n,r) =
			Assoc.fold mergeShiftItems (ntShifts n) r

(* nonterms: a list of nonterminals that are in a core after the
   closure operation *)

		val nonterms = closureNonterms itemList

(* now create a table which for each shift/goto symbol gives the sorted list
   of closure items which would result from first taking all the closure items
   and then sorting them by the shift/goto symbols *)

		val newsets = fold mergeItems nonterms Assoc.empty

(* finally prepare to insert the kernal items of a core *)

		fun insertItem ((k,i),r) =
		   case (Assoc.find(k,r))
		     of NONE => Assoc.insert((k,[i]),r)
		      | SOME l => Assoc.insert((k,Core.insert(i,l)),r)
		fun shiftCores(ITEM{rule,dot,rhsAfter=symbol::rest},r) =
		   insertItem((symbol,
			      ITEM{rule=rule,dot=dot+1,rhsAfter=rest}),r)
		  | shiftCores(_,r) = r

(* insert the kernal items of a core *)

		val newsets = fold shiftCores itemList newsets
	   in Assoc.make_list newsets
	   end

(* nontermEpsProds: returns a list of epsilon productions produced by a
   nonterminal sorted by rule number. ** Depends on produces returning
   an ordered list **.  It does not alter the order in which the rules
   were returned by produces; it only removes non-epsilon productions *)

	   val nontermEpsProds =
	      let val f = fn nt =>
		  fold (fn (rule as RULE {rhs=nil,...},results) =>
				rule :: results
			 | (_,results) => results) (produces nt) nil
	       in memoize f
	       end 

(* epsProds: take a core and compute a list of epsilon productions for it
   sorted by rule number.  ** Depends on closureNonterms returning a list
   of nonterminals sorted by nonterminal #, rule numbers increasing
   monotonically with their lhs production #, and nontermEpsProds returning
   an ordered item list for each production 
*)

	fun epsProds (CORE (itemList,state)) =
	   let val prods = map nontermEpsProds (closureNonterms itemList)
	   in fold (op @) prods nil
	   end

     in {produces=produces,shifts=shifts,rules=rules,epsProds=epsProds}
     end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkGraph(structure IntGrammar : INTGRAMMAR
		structure Core : CORE
		structure CoreUtils : CORE_UTILS
		sharing IntGrammar = Core.IntGrammar = CoreUtils.IntGrammar
		sharing CoreUtils.Core = Core
		) : LRGRAPH =
	struct
		structure Core = Core
		structure Grammar = IntGrammar.Grammar
		structure IntGrammar = IntGrammar
		open Core Core.Grammar CoreUtils IntGrammar

		structure NodeSet = RbOrdSet
			(struct
				type elem = core
				val eq = eqCore
				val gt = gtCore
			end)

		open NodeSet
		exception Shift of int * symbol

		type graph = {edges: {edge:symbol,to:core} list array,
			      nodes: core list,nodeArray : core array}
		val edges = fn (CORE (_,i),{edges,...}:graph) => edges sub i
		val nodes = fn ({nodes,...} : graph) => nodes
		val shift = fn ({edges,nodes,...} : graph) => fn a as (i,sym) =>
			let fun find nil = raise (Shift a)
			      | find ({edge,to=CORE (_,state)} :: r) =
					if gtSymbol(sym,edge) then find r
					else if eqSymbol(edge,sym) then state
					else raise (Shift a)
			in find (edges sub i)
			end

		val core = fn ({nodeArray,...} : graph) =>
				 fn i => nodeArray sub i

		val mkGraph = fn (g as (GRAMMAR {start,...})) =>
		   let val {shifts,produces,rules,epsProds} =
				  CoreUtils.mkFuncs g
		       fun add_goto ((symbol,a),(nodes,edges,future,num)) =
				case find(CORE (a,0),nodes)
				  of NONE =>
				     let val core =CORE (a,num)
					 val edge = {edge=symbol,to=core}
				     in (insert(core,nodes),edge::edges,
					 core::future,num+1)
				     end
				   | (SOME c) =>
					let val edge={edge=symbol,to=c}
					in (nodes,edge::edges,future,num)
					end
		       fun f (nodes,node_list,edge_list,nil,nil,num) =
			    let val nodes=rev node_list
			    in {nodes=nodes,
				edges=arrayoflist (rev edge_list),
				nodeArray = arrayoflist nodes
			 	}
			    end
			 | f (nodes,node_list,edge_list,nil,y,num) =
				f (nodes,node_list,edge_list,rev y,nil,num)
			 | f (nodes,node_list,edge_list,h::t,y,num) =
			 	let val (nodes,edges,future,num) =
				   List.fold add_goto (shifts h)
						(nodes,nil,y,num)
				in f (nodes,h::node_list,
				       edges::edge_list,t,future,num)
				end
		in {graph=
		   let val makeItem = fn (r as (RULE {rhs,...})) =>
						ITEM{rule=r,dot=0,rhsAfter=rhs}
			val initialItemList = map makeItem (produces start)
		        val orderedItemList =
			   List.fold Core.insert initialItemList nil
 			val initial = CORE (orderedItemList,0)
		   in f(empty,nil,nil,[initial],nil,1)
		   end,
		   produces=produces,
		   rules=rules,
		   epsProds=epsProds}
		end
	val prGraph = fn a as (nontermToString,termToString,print) => fn g =>
	   let val printCore = prCore a
	       val printSymbol = print o nontermToString
	       val nodes = nodes g
	       val printEdges = fn n => 
		 List.app (fn {edge,to=CORE (_,state)} =>
			(print "\tshift on ";
			 printSymbol edge;
			 print " to ";
			 print (makestring state);
			 print "\n")) (edges (n,g))
	 in List.app (fn c => (printCore c; print "\n"; printEdges c)) nodes
	 end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkGraph(structure IntGrammar : INTGRAMMAR
		structure Core : CORE
		structure CoreUtils : CORE_UTILS
		sharing IntGrammar = Core.IntGrammar = CoreUtils.IntGrammar
		sharing CoreUtils.Core = Core
		) : LRGRAPH =
	struct
		structure Core = Core
		structure Grammar = IntGrammar.Grammar
		structure IntGrammar = IntGrammar
		open Core Core.Grammar CoreUtils IntGrammar

		structure NodeSet = RbOrdSet
			(struct
				type elem = core
				val eq = eqCore
				val gt = gtCore
			end)

		open NodeSet
		exception Shift of int * symbol

		type graph = {edges: {edge:symbol,to:core} list array,
			      nodes: core list,nodeArray : core array}
		val edges = fn (CORE (_,i),{edges,...}:graph) => edges sub i
		val nodes = fn ({nodes,...} : graph) => nodes
		val shift = fn ({edges,nodes,...} : graph) => fn a as (i,sym) =>
			let fun find nil = raise (Shift a)
			      | find ({edge,to=CORE (_,state)} :: r) =
					if gtSymbol(sym,edge) then find r
					else if eqSymbol(edge,sym) then state
					else raise (Shift a)
			in find (edges sub i)
			end

		val core = fn ({nodeArray,...} : graph) =>
				 fn i => nodeArray sub i

		val mkGraph = fn (g as (GRAMMAR {start,...})) =>
		   let val {shifts,produces,rules,epsProds} =
				  CoreUtils.mkFuncs g
		       fun add_goto ((symbol,a),(nodes,edges,future,num)) =
				case find(CORE (a,0),nodes)
				  of NONE =>
				     let val core =CORE (a,num)
					 val edge = {edge=symbol,to=core}
				     in (insert(core,nodes),edge::edges,
					 core::future,num+1)
				     end
				   | (SOME c) =>
					let val edge={edge=symbol,to=c}
					in (nodes,edge::edges,future,num)
					end
		       fun f (nodes,node_list,edge_list,nil,nil,num) =
			    let val nodes=rev node_list
			    in {nodes=nodes,
				edges=arrayoflist (rev edge_list),
				nodeArray = arrayoflist nodes
			 	}
			    end
			 | f (nodes,node_list,edge_list,nil,y,num) =
				f (nodes,node_list,edge_list,rev y,nil,num)
			 | f (nodes,node_list,edge_list,h::t,y,num) =
			 	let val (nodes,edges,future,num) =
				   List.fold add_goto (shifts h)
						(nodes,nil,y,num)
				in f (nodes,h::node_list,
				       edges::edge_list,t,future,num)
				end
		in {graph=
		   let val makeItem = fn (r as (RULE {rhs,...})) =>
						ITEM{rule=r,dot=0,rhsAfter=rhs}
			val initialItemList = map makeItem (produces start)
		        val orderedItemList =
			   List.fold Core.insert initialItemList nil
 			val initial = CORE (orderedItemList,0)
		   in f(empty,nil,nil,[initial],nil,1)
		   end,
		   produces=produces,
		   rules=rules,
		   epsProds=epsProds}
		end
	val prGraph = fn a as (nontermToString,termToString,print) => fn g =>
	   let val printCore = prCore a
	       val printSymbol = print o nontermToString
	       val nodes = nodes g
	       val printEdges = fn n => 
		 List.app (fn {edge,to=CORE (_,state)} =>
			(print "\tshift on ";
			 printSymbol edge;
			 print " to ";
			 print (makestring state);
			 print "\n")) (edges (n,g))
	 in List.app (fn c => (printCore c; print "\n"; printEdges c)) nodes
	 end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkLook (structure IntGrammar : INTGRAMMAR) : LOOK =
    struct
	structure Grammar = IntGrammar.Grammar
	structure IntGrammar = IntGrammar
	open Grammar IntGrammar

	structure TermSet = ListOrdSet
		(struct
			type elem = term
			val eq = eqTerm
			val gt = gtTerm
		end)

	val union = TermSet.union
	val make_set = TermSet.make_set

	val prLook = fn (termToString,print) =>
		let val printTerm = print o termToString
		    fun f nil = print " "
		      | f (a :: b) = (printTerm a; print " "; f b)
		in f
		end

	structure NontermSet = ListOrdSet
		(struct
			type elem = nonterm
			val eq = eqNonterm
			val gt = gtNonterm
		end)
	
	val mkFuncs = fn {rules : rule list, nonterms : int,
			  produces : nonterm -> rule list} =>

	let

	(* nullable: create a function which tells if a nonterminal is nullable
	   or not.

	   Method: Keep an array of booleans.  The nth entry is true if
	   NT i is nullable.  If is false if we don't know whether NT i
	   is nullable.

	   Keep a list of rules whose remaining rhs we must prove to be
	   null.  First, scan the list of rules and remove those rules
	   whose rhs contains a terminal.  These rules are not nullable.

	   Now iterate through the rules that were left:
		   (1) if there is no remaining rhs we have proved that
		    the rule is nullable, mark the nonterminal for the
		    rule as nullable
		   (2) if the first element of the remaining rhs is
		       nullable, place the rule back on the list with
		       the rest of the rhs
		   (3) if we don't know whether the nonterminal is nullable,
		       place it back on the list
		   (4) repeat until the list does not change.

	   We have found all the possible nullable rules. 
      *)

	val nullable =
	  let fun ok_rhs nil = true
		| ok_rhs ((TERM _)::_) = false
		| ok_rhs ((NONTERM i)::r) = ok_rhs r
	      fun add_rule (RULE {lhs,rhs,...},r) =
		 if ok_rhs rhs then (lhs,map (fn (NONTERM (NT i)) => i) rhs)::r
		 else r
	      val items = fold add_rule rules nil
	      val nullable = array(nonterms,false)
	      val f = fn ((NT i,nil),(l,_)) => (update(nullable,i,true);
				 	       (l,true))
		       | (a as (lhs,(h::t)),(l,change)) =>
				case (nullable sub h) 
				  of false => (a::l,change)
				   | true => ((lhs,t)::l,true)
	      fun prove(l,true) = prove(fold f l (nil,false))
		| prove(_,false) = ()
	in (prove(items,true); fn (NT i) => nullable sub i)
	end

     (* scanRhs : look at a list of symbols, scanning past nullable
	nonterminals, applying addSymbol to the symbols scanned *)

    fun scanRhs addSymbol =
	let fun f (nil,result) = result
      	      | f ((sym as NONTERM nt) :: rest,result) =
		if nullable nt then f (rest,addSymbol(sym,result))
		else addSymbol(sym,result)
      	      | f ((sym as TERM _) :: _,result) = addSymbol(sym,result)
	in f 
	end

     (* accumulate: look at the start of the right-hand-sides of rules,
	looking past nullable nonterminals, applying addObj to the visible
	symbols. *)

      fun accumulate(rules, empty, addObj) =
       List.fold (fn (RULE {rhs,...},r) =>(scanRhs addObj) (rhs,r)) rules empty

      val nontermMemo = fn f =>
	let val lookup = array(nonterms,nil)
	    fun g i = if i=nonterms then ()
		      else (update(lookup,i,f (NT i)); g (i+1))
	in (g 0; fn (NT j) => lookup sub j)
	end

     (* first1: the FIRST set of a nonterminal in the grammar. Only looks
	at other terminals, but it is clever enough to move past nullable
	nonterminals at the start of a production. *)

      fun first1 nt = accumulate(produces nt, TermSet.empty,
                                 fn (TERM t, set) => TermSet.insert (t,set)
                                  | (_, set) => set)

      val first1 = nontermMemo(first1)

     (* starters1: given a nonterminal "nt", return the set of nonterminals
	which can start its productions. Looks past nullables, but doesn't
	recurse *)

      fun starters1 nt = accumulate(produces nt, nil,
                                    fn (NONTERM nt, set) =>
					 NontermSet.insert(nt,set)
                                     | (_, set) => set)

     val starters1 = nontermMemo(starters1)

     (* first: maps a nonterminal to its first-set. Get all the starters of
	the nonterminal, get the first1 terminal set of each of these,
	union the whole lot together *)

      fun first nt =
	     fold (fn (a,r) => TermSet.union(r,first1 a))
 	     (NontermSet.closure (NontermSet.singleton nt, starters1))
 	     nil

      val first = nontermMemo(first)

     (* prefix: all possible terminals starting a symbol list *)

      fun prefix symbols =
	  scanRhs (fn (TERM t,r) => TermSet.insert(t,r)
		    | (NONTERM nt,r) => TermSet.union(first nt,r))
	  (symbols,nil)

      fun nullable_string ((TERM t) :: r) = false
	| nullable_string ((NONTERM nt) :: r) =
		(case (nullable nt)
		   of true => nullable_string r
		    | f => f)
	| nullable_string nil = true
	  
    in {nullable = nullable, first = prefix}
    end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkLalr ( structure IntGrammar : INTGRAMMAR
		structure Core : CORE
		structure Graph : LRGRAPH
		structure Look: LOOK
		sharing Graph.Core = Core
		sharing Graph.IntGrammar = Core.IntGrammar =
			Look.IntGrammar = IntGrammar) : LALR_GRAPH =
    struct
	open IntGrammar.Grammar IntGrammar Core Graph Look
	structure Graph = Graph
	structure Core = Core
	structure Grammar = IntGrammar.Grammar
	structure IntGrammar = IntGrammar

	datatype tmpcore = TMPCORE of (item * term list ref) list * int
	datatype lcore = LCORE of (item * term list) list * int
	

	 val prLcore =
	  fn a as (SymbolToString,nontermToString,termToString,print) =>
	    let val printItem = prItem (SymbolToString,nontermToString,print)
		val printLookahead = prLook(termToString,print)
	    in fn (LCORE (items,state)) =>
		(print "\n";
		 print "state ";
		 print (makestring state);
		 print " :\n\n";
		 List.app (fn (item,lookahead) =>
			(print "{";
			 printItem item;
			 print ",";
			 printLookahead lookahead;
			 print "}\n")) items)
	    end

	exception Lalr of int

	structure ItemList = ListOrdSet
		(struct
		   type elem = item * term list ref
		   val eq = fn ((a,_),(b,_)) => eqItem(a,b)
		   val gt = fn ((a,_),(b,_)) => gtItem(a,b)
		 end)

	structure NontermSet = ListOrdSet
		(struct
		   type elem = nonterm
		   val gt = gtNonterm
		   val eq = eqNonterm
		 end)

(* NTL: nonterms with lookahead *)

	structure NTL = RbOrdSet
		(struct
		   type elem = nonterm * term list
		   val gt = fn ((i,_),(j,_)) => gtNonterm(i,j)
		   val eq = fn ((i,_),(j,_)) => eqNonterm(i,j)
		 end)

	val DEBUG = false

	val addLookahead  = fn {graph,nullable,first,eop,
				rules,produces,nonterms,epsProds,
				print,termToString,nontermToString} =>
	  let

		val eop = Look.make_set eop

		val symbolToString = fn (TERM t) => termToString t
				      | (NONTERM t) => nontermToString t

		val print = if DEBUG then print
			    else fn _ => ()

		val prLook = if DEBUG then prLook (termToString,print)
			     else fn _ => ()

		val prNonterm = print o nontermToString

		val prRule = if DEBUG
			      then prRule(symbolToString,nontermToString,print)
			      else fn _ => ()

		val printInt = print o (makestring : int -> string)

		val printItem = prItem(symbolToString,nontermToString,print)

(* look_pos: position in the rhs of a rule at which we should start placing
   lookahead ref cells, i.e. the minimum place at which A -> x .B y, where
   B is a nonterminal and y =*=> epsilon, or A -> x. is true.  Positions are
   given by the number of symbols before the place.  The place before the first
   symbol is 0, etc. *)

	     val look_pos =
		 let val positions = array(length rules,0)

(* rule_pos: calculate place in the rhs of a rule at which we should start
   placing lookahead ref cells *)

		      val rule_pos = fn (RULE {rhs,...}) =>
			case (rev rhs)
			  of nil => 0
			   | (TERM t) :: r => length rhs
			   | (l as (NONTERM n) :: r) =>

			      (* f assumes that everything after n in the
				 rule has proven to be nullable so far.
				 Remember that the rhs has been reversed,
				 implying that this is true initially *)
				
					(* A -> .z t B y, where y is nullable *)

			      let fun f (NONTERM b :: (r as (TERM _ :: _))) =
					(length r)

					(* A -> .z B C y *)

				    | f (NONTERM c :: (r as (NONTERM b :: _))) =
					 if nullable c then f r
					 else (length r)

					(* A -> .B y, where y is nullable *)

				    | f (NONTERM b :: nil) = 0 
			      in  f l
			      end
			     
		 	val check_rule = fn (rule as RULE {num,...}) =>
			    let val pos = rule_pos rule
			    in (print "look_pos: ";
			 	prRule rule;
				print " = ";
				printInt pos;
				print "\n";
				update(positions,num,rule_pos rule))
			    end
		   in app check_rule rules;
		    fn RULE{num,...} => (positions sub num)
		   end

(* rest_is_null: true for items of the form A -> x .B y, where y is nullable *)

	     val rest_is_null =
		 fn (ITEM{rule,dot, rhsAfter=NONTERM _ :: _}) =>
			 dot >= (look_pos rule)
		  | _ => false

(* map core to a new core including only items of the form A -> x. or
   A -> x. B y, where y =*=> epsilon.  It also adds epsilon productions to the
   core. Each item is given a ref cell to hold the lookahead nonterminals for
   it.*)

	      val map_core =
		let val f = fn (item as ITEM {rhsAfter=nil,...},r) =>
				(item,ref nil) :: r
			     | (item,r) =>
				 if (rest_is_null item)
				    then (item,ref nil)::r
				    else r
		in fn (c as CORE (items,state)) =>
		   let val epsItems =
			   map  (fn rule=>(ITEM{rule=rule,dot=0,rhsAfter=nil},
					   ref (nil : term list))
				) (epsProds c)
		   in TMPCORE(ItemList.union(fold f items nil,epsItems),state)
		   end
		end

	      val new_nodes = map map_core (nodes graph)

	      exception Find

(* findRef: state * item -> lookahead ref cell for item *)

	      val findRef = 
		let val states = arrayoflist new_nodes
		    val dummy = ref nil
		in fn (state,item) =>
		    let val TMPCORE (l,_) = states sub state
		    in case ItemList.find((item,dummy),l)
				   of SOME (_,look_ref) => look_ref
				    | NONE => (print "find failed: state ";
					       printInt state;
					       print "\nitem =\n";
					       printItem item;
					       print "\nactual items =\n";
					       app (fn (i,_) => (printItem i;
						    print "\n")) l;
						raise Find)
		    end
		end 
			

(* findRuleRefs: state -> rule -> lookahead refs for rule. *)
		
	       val findRuleRefs =
		 let val shift = shift graph
		 in fn state =>
			(* handle epsilon productions *)
		  fn (rule as RULE {rhs=nil,...}) => 
			 [findRef(state,ITEM{rule=rule,dot=0,rhsAfter=nil})]
		   | (rule as RULE {rhs=sym::rest,...}) =>
		   let	val pos = max(look_pos rule,1)
			fun scan'(state,nil,pos,result) =
				findRef(state,ITEM{rule=rule,
						   dot=pos,
						   rhsAfter=nil}) :: result
			  | scan'(state,rhs as sym::rest,pos,result) =
				scan'(shift(state,sym), rest, pos+1,
				      findRef(state,ITEM{rule=rule,
							 dot=pos,
							 rhsAfter=rhs})::result)
				
(* find first item of the form A -> x .B y, where y =*=> epsilon and
   x is not epsilon, or A -> x.  use scan' to pick up all refs after this
   point *)

			 fun scan(state,nil,_) =
			   [findRef(state,ITEM{rule=rule,dot=pos,rhsAfter=nil})]
			   | scan(state,rhs,0) = scan'(state,rhs,pos,nil)
			   | scan(state,sym::rest,place) =
				    scan(shift(state,sym),rest,place-1)

		  in scan(shift(state,sym),rest,pos-1)
		  end

	     end

(* function to compute for some nonterminal n the set of nonterminals A added
   through the closure of nonterminal n such that n =c*=> .A x, where x is
   nullable *)

	      val nonterms_w_null = fn nt =>
		  let val collect_nonterms = fn n =>
		    fold (fn (rule as RULE {rhs=rhs as NONTERM n :: _,...},r) =>
			   (case
			     (rest_is_null(ITEM {dot=0,rhsAfter=rhs,rule=rule}))
				 of true => n :: r
				  | false => r)
			   | (_,r) => r) (produces n) nil
		       fun dfs(a as (n,r)) =
			 if (NontermSet.exists a) then r 
			 else fold dfs (collect_nonterms n)
				  (NontermSet.insert(n,r))
		  in dfs(nt,NontermSet.empty)
		  end

		val nonterms_w_null =
		   let val data = array(nonterms,NontermSet.empty)
		       fun f n = if n=nonterms then ()
				 else (update(data,n,nonterms_w_null (NT n));
				       f (n+1))
		   in (f 0; fn (NT nt) => data sub nt)
		   end

(* look_info: for some nonterminal n the set of nonterms A added
   through the closure of the nonterminal such that n =c+=> .Ax and the
   lookahead accumlated for each nonterm A *)

		val look_info = fn nt =>
		   let val collect_nonterms = fn n =>
		      fold (fn (RULE {rhs=NONTERM n :: t,...},r) =>
			     (case NTL.find ((n,nil),r)
			      of SOME (key,data) =>
			         NTL.insert((n,Look.union(data,first t)),r)
			       | NONE => NTL.insert ((n,first t),r))
			     | (_,r) => r)
		            (produces n) NTL.empty
			fun dfs(a as ((key1,data1),r)) =
			  case (NTL.find a)
			   of SOME (_,data2) =>
			       NTL.insert((key1,Look.union(data1,data2)),r)
			    | NONE => NTL.fold dfs (collect_nonterms key1)
						   (NTL.insert a)
		    in dfs((nt,nil),NTL.empty)
		    end

		val look_info = 
		  if not DEBUG then look_info
		  else fn nt =>
		       (print "look_info of "; prNonterm nt; print "=\n";
			let val info = look_info nt
			in (NTL.app (fn (nt,lookahead) =>
				    (prNonterm nt; print ": "; prLook lookahead;
				     print "\n\n")) info;
			   info)
			end)

(* prop_look: propagate lookaheads for nonterms added in the closure of a
   nonterm.  Lookaheads must be propagated from each nonterminal m to
   all nonterminals { n | m =c+=> nx, where x=*=>epsilon} *)

		  val prop_look = fn ntl =>
		    let val upd_lookhd = fn new_look => fn (nt,r) =>
			  case NTL.find ((nt,new_look),r)
			  of SOME (_,old_look) =>
			     NTL.insert((nt, Look.union(new_look,old_look)),r)
			   | NONE => raise (Lalr 241)
		         val upd_nonterm = fn ((nt,look),r) =>
			   NontermSet.fold (upd_lookhd look)
					   (nonterms_w_null nt) r
		     in NTL.fold upd_nonterm ntl ntl
		     end

		val prop_look = 
		  if not DEBUG then prop_look
		  else fn ntl =>
		    (print "prop_look =\n";
		     let val info = prop_look ntl
		     in (NTL.app (fn (nt,lookahead) =>
				    (prNonterm nt;
				     print ": ";
				     prLook lookahead;
				     print "\n\n")) info; info)
		     end)

(* now put the information from these functions together.  Create a function
   which takes a nonterminal n and returns a list of triplets of
	 (a nonterm added through closure,
	  the lookahead for the nonterm,
	  whether the nonterm should include the lookahead for the nonterminal
	  whose closure is being taken (i.e. first(y) for an item j of the
	  form A -> x .n y and lookahead(j) if y =*=> epsilon)
*)

		 val closure_nonterms =
		   let val data =
			  array(nonterms,nil: (nonterm * term list * bool) list)
		       val do_nonterm = fn i =>
			let val nonterms_followed_by_null =
				nonterms_w_null i
			    val nonterms_added_through_closure = 
			      NTL.make_list (prop_look (look_info i))
			    val result =
			    map (fn (nt,l) =>
			 (nt,l,NontermSet.exists (nt,nonterms_followed_by_null))
				) nonterms_added_through_closure
			 in if DEBUG then
			       (print "closure_nonterms = ";
				prNonterm i;
				print "\n";
				app (fn (nt,look,nullable) =>
				  (prNonterm nt;
				   print ":";
				   prLook look;
				   case nullable
				     of false => print "(false)\n"
				      | true => print "(true)\n")) result;
				print "\n")
			     else ();
			     result
			 end
		        fun f i =
			  if i=nonterms then ()
			  else (update(data,i,do_nonterm (NT i)); f (i+1))
			val _ = f 0
		    in fn (NT i) => data sub i
		    end

(* add_nonterm_lookahead: Add lookahead to all completion items for rules added
   when the closure of a given nonterm in some state is taken.  It returns
   a list of lookahead refs to which the given nonterm's lookahead should
   be propagated.   For each rule, it must trace the shift/gotos in the LR(0)
   graph to find all items of the form A-> x .B y where y =*=> epsilon or
   A -> x.
*)

		val add_nonterm_lookahead = fn (nt,state) =>
		  let val f = fn ((nt,lookahead,nullable),r) =>
			let val refs = map (findRuleRefs state) (produces nt)
			    val refs = fold (op @) refs nil
			    val _ = app (fn r =>
				     r := (Look.union (!r,lookahead))) refs
			in if nullable then refs @ r else r
			end
		 in fold f (closure_nonterms nt) nil
		 end

(* scan_core: Scan a core for all items of the form A -> x .B y.  Applies
   add_nonterm_lookahead to each such B, and then merges first(y) into
   the list of refs returned by add_nonterm_lookahead.  It returns
   a list of ref * ref list for all the items where y =*=> epsilon *)

		val scan_core = fn (CORE (l,state)) =>
		  let fun f ((item as ITEM{rhsAfter= NONTERM b :: y,
					   dot,rule})::t,r) =
			(case (add_nonterm_lookahead(b,state))
			  of nil => r
			   | l =>
			    let val first_y = first y
			        val newr  = if dot >= (look_pos rule)
					then (findRef(state,item),l)::r
					else r
			    in (app (fn r =>
					 r := Look.union(!r,first_y)) l;
			        f (t,newr))
			    end)
			| f (_ :: t,r) = f (t,r)
			| f (nil,r) = r
		  in f (l,nil)
		  end

(* add end-of-parse symbols to set of items consisting of all items
   immediately derived from the start symbol *)

		val add_eop = fn (c as CORE (l,state),eop) =>
		  let fun f (item as ITEM {rule,dot,...}) =
		    let val refs = findRuleRefs state rule
		    in

(* first take care of kernal items.  Add the end-of-parse symbols to
   the lookahead sets for these items.  Epsilon productions of the
   start symbol do not need to be handled specially because they will
   be in the kernal also *)

		       app (fn r => r := Look.union(!r,eop)) refs;

(* now take care of closure items.  These are all nonterminals C which
   have a derivation S =+=> .C x, where x is nullable *)

		       if dot >= (look_pos rule) then
		       	  case item
			  of ITEM{rhsAfter=NONTERM b :: _,...} =>
			     (case add_nonterm_lookahead(b,state)
			      of nil => ()
			       | l => app (fn r => r := Look.union(!r,eop)) l)
			   | _ => ()
		       else ()
		    end
		  in app f l
		  end

		val iterate = fn l =>
		   let fun f lookahead (nil,done) = done
			 | f lookahead (h::t,done) =
			    let val old = !h
			    in h := Look.union (old,lookahead);
			       if (length (!h)) <> (length old)
					 then f lookahead (t,false)
					 else f lookahead(t,done)
			    end
		       fun g ((from,to)::rest,done) =
			let val new_done = f (!from) (to,done)
			in g (rest,new_done)
			end
			 | g (nil,done) = done
		       fun loop true = ()
			 | loop false = loop (g (l,true))
		   in loop false
		   end

		val lookahead = fold (op @) (map scan_core (nodes graph)) nil

(* used to scan the item list of a TMPCORE and remove the items not
   being reduced *)

		val create_lcore_list =
			fn ((item as ITEM {rhsAfter=nil,...},ref l),r) =>
				(item,l) :: r
			 | (_,r) => r

	in  add_eop(Graph.core graph 0,eop);
	    iterate lookahead;
	    map (fn (TMPCORE (l,state)) =>
		       LCORE (fold create_lcore_list l nil,state)) new_nodes
	end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkMakeStruct(structure LrTable : LR_TABLE) : MAKE_STRUCT =
   struct

      structure LrTable = LrTable
      open LrTable
     
      (* lineLength = approximately the largest number of characters to allow
	 on a line when printing out an encode string *)
	  
      val lineLength = 72

      (* maxLength = length of a table entry.  All table entries are encoded
	 using two 16-bit integers, one for the terminal number and the other
	 for the entry.  Each integer is printed as two characters (low byte,
	 high byte), using the ML ascii escape sequence.  We need 4
	 characters for each escape sequence and 16 characters for each entry
      *)

      val maxLength =  16

      (* number of entries we can fit on a row *)

      val numEntries = lineLength div maxLength

      (* convert integer between 0 and 255 to the three character ascii
	 decimal escape sequence for it *)

      val chr =
	let val lookup = array(256,"\000")
	    val intToString = fn i =>
		if i>=100 then "\\" ^ (makestring i)
		else if i>=10 then "\\0" ^ (makestring i)
		else  "\\00" ^ (makestring i)
	    fun loop n = if n=256 then ()
			 else (update(lookup,n,intToString n); loop (n+1))
	in loop 0; fn i => lookup sub i
	end

      val makeStruct = fn {table,name,print} =>
       let
	 val states = numStates table

         fun printList (prEntry : 'a -> unit) l =
		revfold (fn (a,count) => 
			     let val newcount = count + 1
			     in if newcount >= numEntries then
				  (print "\\\n\\"; prEntry a; 0)
				else (prEntry a; newcount)
			     end)  l 0

	  fun printRow (prEntry,prEnd) =
	       let val printEntries = printList prEntry
	       in fn (l,default) => (printEntries l; prEnd default)
	       end

	  fun printTable (getRow,prEntry,prEnd) =
	      let val printRow = printRow(prEntry,prEnd)
		  val doRow = printRow o getRow
	      in print "\"\\\n\\";
		 let fun f i = if i=states then ()
			       else (doRow (STATE i); f (i+1))
		 in f 0
		 end;
		 print"\"\n"
	      end
	       
	  val printChar = print o chr

	  (* print an integer between 0 and 2^16-1 as a 2-byte character,
	     with the low byte first *)

	  val printInt = fn i => (printChar (i mod 256);
				  printChar (i div 256))

	 (* encode actions as integers:

		ACCEPT => 0
		ERROR => 1
		SHIFT i => 2 + i
		REDUCE rulenum => numstates+2+rulenum

	 *)

	  val printAction =
	      fn (REDUCE rulenum) => printInt (rulenum+states+2)
		 | (SHIFT (STATE i)) => printInt (i+2)
		 | ACCEPT => printInt 0
		 | ERROR => printInt 1
	
	   val printTermAction = fn (T t,action) =>
		(printInt (t+1); printAction action)

	   val printGoto = fn (NT n,STATE s) => (printInt (n+1); printInt s)

	   val endOfRow = "\\000\\000"
	   val endOfLn = "\\\n\\"

	   val defaultAction = fn a => (print endOfRow; printAction a;
					print endOfLn)
	   val defaultGoto = fn _ => (print endOfRow; print endOfRow;
					print endOfLn)

	   val getActions = describeActions table 
	   val getGoto = fn state => (describeGoto table state,())

	in print "val ";
	   print name;
	   print "=";
	   print "let val actionT =\n";
	   printTable(getActions,printTermAction,defaultAction);
	   print "val gotoT =\n";
	   printTable(getGoto,printGoto,defaultGoto);
	   print "val numstates = ";
	   print (makestring states);
	   print "\n\
\val string_to_int = fn(s,index) => (ordof(s,index) + \n\
\			ordof(s,index+1)*256,index+2)\n\
\	val convert_string_to_row = fn (conv_key,conv_entry) =>\n\
\	     fn(s,index) =>\n\
\		let fun f (r,index) =\n\
\			let val (num,index) = string_to_int(s,index)\n\
\			    val (i,index) = string_to_int(s,index)\n\
\			in if num=0 then ((rev r,conv_entry i),index)\n\
\			   else f((conv_key (num-1),conv_entry i)::r,index)\n\
\			end\n\
\		in f(nil,index)\n\
\		end\n\
\	 val convert_string_to_row_list = fn conv_funcs => fn s =>\n\
\		    let val convert_row =convert_string_to_row conv_funcs\n\
\		 	fun f(r,index) =\n\
\			  if index < String.length s then\n\
\			    let val (newlist,index) = convert_row (s,index)\n\
\			    in f(newlist::r,index)\n\
\			    end\n\
\			  else rev r\n\
\		    in f(nil,0)\n\
\		    end\n\
\	 val entry_to_action = fn j =>\n\
\		       if j=0 then ACCEPT\n\
\		       else if j=1 then ERROR\n\
\		       else if j >= (numstates+2) then REDUCE (j-numstates-2)\n\
\		       else SHIFT (STATE (j-2))\n\
\	 val make_goto_table = convert_string_to_row_list(NT,STATE)\n\
\	 val make_action_table=convert_string_to_row_list(T,entry_to_action)\n\
\	 val gotoT = map (fn (a,b) => a) (make_goto_table gotoT)\n\
\	 val actionT = make_action_table actionT\n\
\     in LrTable.mkLrTable {actions=actionT,gotos=gotoT,\n\
\	  numStates=numstates,initialState=STATE ";
print (makestring ((fn (STATE i) => i) (initialState table)));
print "}\n\
\     end\n"
	end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkMakeTable (structure IntGrammar : INTGRAMMAR
		     structure LrTable : LR_TABLE
		     sharing type LrTable.term = IntGrammar.Grammar.term
		     sharing type LrTable.nonterm = IntGrammar.Grammar.nonterm
		    ) : MAKE_LR_TABLE = 
   struct 
	structure Core = mkCore(structure IntGrammar = IntGrammar)
	structure CoreUtils = mkCoreUtils(structure IntGrammar = IntGrammar
				  	  structure Core = Core)
	structure Graph = mkGraph(structure IntGrammar = IntGrammar
			  	  structure Core = Core
			  	  structure CoreUtils = CoreUtils)
	structure Look = mkLook(structure IntGrammar = IntGrammar)
	structure Lalr = mkLalr(structure IntGrammar = IntGrammar
				structure Core = Core
				structure Graph = Graph
				structure Look = Look)
	structure LrTable = LrTable
	structure IntGrammar = IntGrammar
	structure Grammar = IntGrammar.Grammar
	structure GotoList = ListOrdSet
		(struct
		   type elem = Grammar.nonterm * LrTable.state
		   val eq = fn ((Grammar.NT a,_),(Grammar.NT b,_)) => a=b
		   val gt = fn ((Grammar.NT a,_),(Grammar.NT b,_)) => a>b
		end)
	structure Errs : LR_ERRS =
	    struct
		structure LrTable = LrTable
		datatype err = RR of LrTable.term * LrTable.state * int * int
			     | SR of LrTable.term * LrTable.state * int
			     | NONASSOC of LrTable.term * LrTable.state * int
			     | NOT_REDUCED of int
			     | NS of LrTable.term * int
			     | START of int

	        val summary = fn l =>
		   let val numRR = ref 0
		       val numSR = ref 0
		       val numNONASSOC = ref 0
		       val numSTART = ref 0
		       val numNOT_REDUCED = ref 0
		       val numNS = ref 0
		       fun loop (h::t) = 
		       (case h
		         of RR _ => inc numRR
			  | SR _ => inc numSR
			  | NONASSOC _ => inc numNONASSOC
			  | START _ => inc numSTART
			  | NOT_REDUCED _ => inc numNOT_REDUCED
			  | NS _ => inc numNS; loop t)
		      | loop nil = {rr = !numRR, sr = !numSR,
			        nonassoc = !numNONASSOC, start = !numSTART,
			        not_reduced = !numNOT_REDUCED,
			        nonshift = !numNS}
		  in loop l
		  end

		  val printSummary = fn say => fn l =>
		   let val {rr,sr,nonassoc,start,
			    not_reduced,nonshift} = summary l
		       val say_plural = fn (i,s) =>
			  (say (makestring i); say " ";
			   case i
			     of 1 => (say s)
			      | _ => (say s; say "s"))
		       val say_error = fn (args as (i,s)) =>
			  case i
			  of 0 => ()
			   | i => (say_plural args; say "\n")
		   in say_error(rr,"reduce/reduce conflict");
		      say_error(sr,"shift/reduce conflict");
		      say_error(nonassoc," nonassociative error");
		      if nonshift<>0 then
			   (say "non-shiftable terminal used on the rhs of ";
			   say_plural(start,"rule"); say "\n")
		      else ();
		      if start<>0 then (say "start symbol used on the rhs of ";
					say_plural(start,"rule"); say "\n")
		      else ();
		      if not_reduced<>0 then (say_plural(not_reduced,"rule");
					      say " not reduced\n")
		      else ()
		end
	    end


	open IntGrammar Grammar Errs LrTable Core 

(* rules for resolving conflicts:

	shift/reduce:

		  If either the terminal or the rule has no
		  precedence, a shift/reduce conflict is reported.
		  A shift is chosen for the table.

		  If both have precedences, the action with the
		  higher precedence is chosen.

		  If the precedences are equal, a nonassoc 
	  	  conflict is reported.

      reduce/reduce:

		  A reduce/reduce conflict is reported.  The lowest
		  numbered rule is chosen for reduction.
*)


(* method for filling tables - first compute the reductions called for in a
   state, then add the shifts for the state to this information.

How to compute the reductions:

   A reduction initially is given as an item and a lookahead set calling
for reduction by that item.  The first reduction is mapped to a list of
terminal * rule pairs.  Each additional reduction is then merged into this
list and reduce/reduce conflicts are resolved according to the rule
given.

Missed Errors:

   This method misses some reduce/reduce conflicts that exist because
some reductions are removed from the list before conflicting reductions
can be compared against them.  All reduce/reduce conflicts, however,
can be generated given a list of the reduce/reduce conflicts generated
by this method.
	
   This can be done by taking the transitive closure of the relation given
by the list.  If reduce/reduce (a,b) and reduce/reduce (b,c)  are true,
then reduce/reduce (a,c) is true.   The relation is symmetric and transitive.
		  
Adding shifts:

    Finally scan the list merging in shifts and resolving conflicts
according to the rule given.

Missed Shift/Reduce Errors:

    Some errors may be missed by this method because some reductions were
removed as the result of reduce/reduce conflicts.  For a shift/reduce
conflict of term a, reduction by rule n, shift/reduce conficts exist
for all rules y such that reduce/reduce (x,y) or reduce/reduce (y,x)
is true.
*)

    val mergeReduces =
	let val merge = fn state =>
	  let fun f (j as (pair1 as (T t1,action1)) :: r1,
		     k as (pair2 as (T t2,action2)) :: r2,result,errs) =
	  	    if t1 < t2 then f(r1,k,pair1::result,errs)
		    else if t1 > t2 then f(j,r2,pair2::result,errs)
		    else let val REDUCE num1 = action1
			     val REDUCE num2 = action2
			     val errs = RR(T t1,state,num1,num2) :: errs
			     val action = if num1 < num2 then pair1 else pair2
		         in f(r1,r2,action::result,errs)
		          end
		| f (nil,nil,result,errs) = (rev result,errs)
		| f (pair1::r,nil,result,errs) = f(r,nil,pair1::result,errs)
		| f (nil,pair2 :: r,result,errs) = f(nil,r,pair2::result,errs)
	    in f
	    end
	 in fn state => fn ((ITEM {rule=RULE {rulenum,...},...}, lookahead),
		 (reduces,errs)) =>
		let val action = REDUCE rulenum
		    val actions = map (fn a=>(a,action)) lookahead
		in case reduces
		   of nil => (actions,errs)
		    | _ =>  merge state (reduces,actions,nil,errs)
		end
	 end

  val computeActions = fn (rules,precedence,graph,defaultReductions) =>
     
    let val rulePrec =
	  let val precData = array(length rules,NONE : int option)
	  in app (fn RULE {rulenum=r,precedence=p,...} => update(precData,r,p))
	     rules;
	     fn i => precData sub i
	  end

	fun mergeShifts(state,shifts,nil) = (shifts,nil)
	  | mergeShifts(state,nil,reduces) = (reduces,nil)
	  | mergeShifts(state,shifts,reduces) =
	  let fun f(shifts as (pair1 as (T t1,_)) :: r1,
			reduces as (pair2 as (T t2,action)) :: r2,
			result,errs) =
		if t1 < t2 then f(r1,reduces,pair1 :: result,errs)
		else if t1 > t2 then f(shifts,r2,pair2 :: result,errs)
		else let val REDUCE rulenum = action
			 val (term1,_) = pair1
		     in case (precedence term1,rulePrec rulenum)
		      of (SOME i,SOME j) =>
		         if i>j then f(r1,r2,pair1 :: result,errs)
		         else if j>i then f(r1,r2,pair2 :: result,errs)
			 else
		 	   f(r1,r2,pair2 :: result,
				    NONASSOC (term1,state,rulenum)::errs)
		       | (_,_) =>
			   f(r1,r2,pair1 :: result,
			     SR (term1,state,rulenum)::errs)
		     end
	   	| f (nil,nil,result,errs) = (rev result,errs)
	   	| f (nil,h::t,result,errs) =
		   	f (nil,t,h::result,errs)
	   	| f (h::t,nil,result,errs) = 
		   	f (t,nil,h::result,errs)
	  in f(shifts,reduces,nil,nil)
	  end

	fun mapCore ({edge=symbol,to=CORE (_,state)}::r,shifts,gotos) =
	        (case symbol
		 of (TERM t) => mapCore (r,(t,SHIFT(STATE state))::shifts,gotos)
		  | (NONTERM nt) => mapCore(r,shifts,(nt,STATE state)::gotos)
		)
	  | mapCore (nil,shifts,gotos) = (rev shifts,rev gotos)

  in fn (Lalr.LCORE (reduceItems,state),c as CORE (shiftItems,state')) =>
	if DEBUG andalso (state <> state') then
		 let exception MkTable in raise MkTable end
	else
	 let val (shifts,gotos) = mapCore (Graph.edges(c,graph),nil,nil)
	     val tableState = STATE state
	 in case reduceItems
		of nil => ((shifts,ERROR),gotos,nil)
		 | h :: nil =>
		    let val (ITEM {rule=RULE {rulenum,...},...}, l) = h
		        val (reduces,_) = mergeReduces tableState (h,(nil,nil))
			val (actions,errs) = mergeShifts(tableState,
							 shifts,reduces)
		        val (actions,anyReduce) =
			   let fun hasReduce (nil,actions) = (rev actions,true)
				 | hasReduce ((a as (_,SHIFT _)) :: r,actions) =
						hasReduce(r,a::actions)
				 | hasReduce (_ :: r,actions) =
						 hasReduce(r,actions)
			       fun loop (nil,actions) = (rev actions,false)
				 | loop ((a as (_,SHIFT _)) :: r,actions) =
						loop(r,a::actions)
				 | loop ((a as (_,REDUCE _)) :: r,actions) =
						hasReduce(r,actions)
				 | loop (_ :: r,actions) = loop(r,actions)
			  in if defaultReductions then loop(actions,nil)
			     else (actions,false)
			  end
		     in ((actions,if anyReduce then REDUCE rulenum else ERROR),
			 gotos,errs)
		     end
		 | l =>
		  let val (reduces,errs1) =
			 fold (mergeReduces tableState) l (nil,nil)
		      val (actions,errs2) =
			 mergeShifts(tableState,shifts,reduces)
	     	  in ((actions,ERROR),gotos,errs1@errs2)
		  end
	 end
   end			

	val mkTable = fn (grammar as GRAMMAR{rules,terms,nonterms,start,
				  precedence,termToString,noshift,
				  nontermToString,eop},defaultReductions) =>
	     let val symbolToString = fn (TERM t) => termToString t
				       | (NONTERM nt) => nontermToString nt
	         val {rules,graph,produces,epsProds,...} = Graph.mkGraph grammar
		 val {nullable,first} =
		   Look.mkFuncs{rules=rules,produces=produces,nonterms=nonterms}
		 val lcores = Lalr.addLookahead
					    {graph=graph,
					     nullable=nullable,
					     produces=produces,
					     eop=eop,
					     nonterms=nonterms,
					     first=first,
					     rules=rules,
					     epsProds=epsProds,
					     print=(fn s=>output(std_out,s)),
					     termToString = termToString,
					     nontermToString = nontermToString}

		  fun zip (h::t,h'::t') = (h,h') :: zip(t,t')
		    | zip (nil,nil) = nil
		    | zip _ = let exception MkTable in raise MkTable end
		  
		  fun unzip l =
		   let fun f ((a,b,c)::r,j,k,l) = f(r,a::j,b::k,c::l)
			 | f (nil,j,k,l) = (rev j,rev k,rev l)
		   in f(l,nil,nil,nil)
		   end
		
		   val (actions,gotos,errs) =
			let val doState =
		            computeActions(rules,precedence,graph,
					   defaultReductions)
			in unzip (map doState (zip(lcores,Graph.nodes graph)))
			end

		  (* add goto from state 0 to a new state.  The new state
		     has accept actions for all of the end-of-parse symbols *)

		   val (actions,gotos,errs) =
		     case gotos
		     of nil => (actions,gotos,errs)
		      | h :: t =>
			let val newStateActions = 
		           (map (fn t => (t,ACCEPT)) (Look.make_set eop),ERROR)
			   val state0Goto = 
			       GotoList.insert((start,STATE (length actions)),h)
			in (actions @ [newStateActions],
			    state0Goto :: (t @ [nil]),
			    errs @ [nil])
			end 

		val startErrs =
	 	  fold (fn (RULE {rhs,rulenum,...},r) =>
			if (exists (fn NONTERM a => a=start
				     | _ => false) rhs)
			  then START rulenum :: r
			  else r) rules nil

		val nonshiftErrs =
	 	  fold (fn (RULE {rhs,rulenum,...},r) =>
		          (fold (fn (nonshift,r) =>
			   if (exists (fn TERM a => a=nonshift
				     | _ => false) rhs)
			    then NS(nonshift,rulenum) :: r
			    else r) noshift r)
		       ) rules nil

		val notReduced =
		  let val ruleReduced = array(length rules,false)
		      val test = fn REDUCE i => update(ruleReduced,i,true)
				  | _ => ()
		      val _ = app (fn (actions,default) =>
				     (app (fn (_,r) => test r) actions;
				      test default)
				  ) actions;
		      fun scan (i,r) =
			 if i >= 0 then
			      scan(i-1, if ruleReduced sub i then r
					else NOT_REDUCED i :: r)
			else r
		  in scan(Array.length ruleReduced-1,nil)
	    	  end handle Subscript =>
			(if DEBUG then
				print "rules not numbered correctly!"
			 else (); nil)

		val numstates = length actions

	        val allErrs = startErrs @ notReduced @ nonshiftErrs @ 
			      (fold (op @) errs nil)

	      in (mkLrTable {actions=actions, gotos=gotos,
			     numStates=length actions, initialState=STATE 0},
		  let val errArray = arrayoflist errs
		  in fn (STATE state) => errArray sub state
		  end,

		  fn print =>
		    let val printCore =
			  prCore(symbolToString,nontermToString,print)
			val core = Graph.core graph
		    in fn STATE state =>
			 printCore (if state=(numstates-1) then
					 Core.CORE (nil,state)
				        else (core state))
		    end,
		    allErrs)
              end
end;
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkVerbose(structure Errs : LR_ERRS) : VERBOSE =
struct
   structure Errs = Errs
   open Errs Errs.LrTable
   val mkPrintAction = fn print =>
	let val printInt = print o (makestring : int -> string)
	in fn (SHIFT (STATE i)) =>
			(print "\tshift ";
		 	 printInt i;
		 	print "\n")
	     | (REDUCE rulenum) =>
			(print "\treduce by rule ";
		 	 printInt rulenum;
		 	 print "\n")
	     | ACCEPT => print "\taccept\n"
	     | ERROR => print "\terror\n"
	end
   val mkPrintGoto = fn (printNonterm,print) =>
      let val printInt = print o (makestring : int -> string)
      in fn (nonterm,STATE i) =>
		(print "\t";
		 printNonterm nonterm;
		 print "\tgoto ";
		 printInt i;
		 print "\n")
      end

   val mkPrintTermAction = fn (printTerm,print) =>
 	let val printAction = mkPrintAction print
	in fn (term,action) =>
		(print "\t";
		 printTerm term;
		 printAction action)
	end
   val mkPrintGoto = fn (printNonterm,print) =>
	fn (nonterm,STATE i) =>
	    let val printInt = print o (makestring : int -> string)
	    in (print "\t";
		printNonterm nonterm;
		print "\tgoto ";
		printInt i;
		print "\n")
	    end
   val mkPrintError = fn (printTerm,printRule,print) =>
     let val printInt = print o (makestring : int -> string)
	 val printState = fn STATE s => (print " state "; printInt s)
     in fn (RR (term,state,r1,r2)) =>
		(print "error: ";
		 printState state;
		 print ": reduce/reduce conflict between rule ";
		 printInt r1;
		 print " and rule ";
		 printInt r2;
		 print " on ";
		 printTerm term;
		 print "\n")
	 | (SR (term,state,r1)) =>
		(print "error: ";
		 printState state;
		 print ": shift/reduce conflict ";
		 print "(shift ";
		 printTerm term;
		 print ", reduce by rule ";
		 printInt r1;
		 print ")\n")
	 | (NONASSOC (term,state,r1)) =>
		(print "error: ";
		 printState state;
		 print ": associativity conflict ";
		 print "(terminal ";
		 printTerm term;
		 print ", rule ";
		 printInt r1;
		 print ")\n")
	 | NOT_REDUCED i =>
		(print "warning: rule <";
		 printRule i;
		 print "> will never be reduced\n")
	 | START i => 
	        (print "warning: start symbol appears on the rhs of ";
	         print "<";
	         printRule i;
		 print ">\n")
	 | NS (term,i) =>
	        (print "warning: non-shiftable terminal ";
		 printTerm term;
		 print  "appears on the rhs of ";
	         print "<";
	         printRule i;
		 print ">\n")
      end
   val printVerbose =
	fn {termToString,nontermToString,table,stateErrs,
	    print,printRule,errs,printCores} =>
	   let 
		val printTerm = print o termToString
		val printNonterm = print o nontermToString

		val printCore = printCores print
		val printTermAction = mkPrintTermAction(printTerm,print)
		val printAction = mkPrintAction print
		val printGoto = mkPrintGoto(printNonterm,print)
		val printError = mkPrintError(printTerm,printRule print,print)

		val gotos = LrTable.describeGoto table
		val actions = LrTable.describeActions table
		val states = numStates table
		
		val _ = if length errs > 0 
			   then (printSummary print errs;
			         print "\n";
				 app printError errs)
			   else ()  
		fun loop i =
		  if i=states then ()
		  else let val s = STATE i
		       in (app printError (stateErrs s);
			   print "\n";
			   printCore s;
			   let val (actionList,default) = actions s
			   in (app printTermAction actionList;
			       print "\n";
			       app printGoto (gotos s);
			       print "\n";
			       print "\t.";
			       printAction default;
			       print "\n"
			       ) 
			   end;
			   loop (i+1))
			end
	  in loop 0
	  end
end;
structure ParseGen  : sig
		   val parseGen : string -> unit
		 end  =
  struct
    open Grammar
    structure MakeTable = mkMakeTable(structure IntGrammar=IntGrammar
				      structure LrTable = LrTable)
    structure Verbose = mkVerbose(structure Errs = MakeTable.Errs)
    structure PrintStruct = mkMakeStruct(structure LrTable = MakeTable.LrTable)
    open Header

    (* approx. maximum length of a line *)

    val lineLength = 70

    (* record type describing names of structures in the program being
 	generated *)

    datatype names = NAMES 
			of {miscStruct : string,  (* Misc{n} struct name *)
			    tableStruct : string, (* LR table structure *)
			    tokenStruct : string, (* Tokens{n} struct name *)
			    actionsStruct : string, (* Actions structure *)
			    valueStruct: string, (* semantic value structure *)
			    ecStruct : string,  (* error correction structure *)
			    arg: string, (* user argument for parser *)
			    tokenSig : string,  (* TOKENS{n} signature *)
			    miscSig :string, (* Signature for Misc structure *)
			    dataStruct:string, (* name of structure in Misc *)
						(* which holds parser data *)
			    dataSig:string (* signature for this structure *)
					
		 	    }

    (* common functions and values used in printing out program *)

    datatype values = VALS
		      of {say : string -> unit,
			  saydot : string -> unit,
			  sayln : string -> unit,
			  pureActions: bool,
			  pos_type : string,
			  arg_type : string,
			  ntvoid : string,
			  termvoid : string,
			  start : Grammar.nonterm,
			  hasType : Grammar.symbol -> bool,

			  (* actual (user) name of terminal *)

			  termToString : Grammar.term -> string,
			  symbolToString : Grammar.symbol -> string,

			  (* type symbol comes from the HDR structure,
			     and equals  (string * int) right now *)

			  term : (symbol * ty) list,
			  nonterm : (symbol * ty) list,
			  terms : Grammar.term list}
			  
    structure SymbolHash = Hash(type elem = string
	    		        val gt = (op >) : string*string -> bool)

    structure TermTable = Table(type key = Grammar.term
				val gt = fn (T i,T j) => i > j)

    structure SymbolTable = Table(
	type key = Grammar.symbol
	val gt = fn (TERM(T i),TERM(T j)) => i>j
		  | (NONTERM(NT i),NONTERM(NT j)) => i>j
		  | (NONTERM _,TERM _) => true
		  | (TERM _,NONTERM _) => false)

    (* printTypes: function to print the following types in the LrValues
       structure and a structure containing the datatype svalue:

		type svalue -- it holds semantic values on the parse
				   stack
		type pos -- the type of line numbers
		type result -- the type of the value that results
				   from the parse

	The type svalue is set equal to the datatype svalue declared
	in the structure named by valueStruct.  The datatype svalue
	is declared inside the structure named by valueStruct to deal
	with the scope of constructors.
    *)

    val printTypes = fn (VALS {say,sayln,term,nonterm,symbolToString,pos_type,
				 arg_type,
				 termvoid,ntvoid,saydot,hasType,start,
				 pureActions,...},
			   NAMES {valueStruct,...},symbolType) =>
     let val prConstr = fn ((name,_),SOME s) => 
			   say (" | " ^ name ^ " of " ^
			          (if pureActions then "" else "unit -> ") ^
				" (" ^ s ^ ")"
				)
			 | _ => ()
     in sayln "local open Header in";
	sayln ("type pos = " ^ pos_type);
	sayln ("type arg = " ^ arg_type);
	sayln ("structure " ^ valueStruct ^ " = ");
	sayln "struct";
	say ("datatype svalue = " ^ termvoid ^ " | " ^ ntvoid ^ " of" ^
	     (if pureActions then "" else " unit -> ") ^ " unit");
	app prConstr term;
	app prConstr nonterm;
	sayln "\nend";
	sayln ("type svalue = " ^ valueStruct ^ ".svalue");
	say "type result = ";
	case symbolType (NONTERM start)
	of NONE => sayln "unit"
	 | SOME t => (say t; sayln "");
	sayln "end"
    end

     (* function to print Tokens{n} structure *)

    val printTokenStruct =
     fn (VALS {say, sayln, termToString, hasType,termvoid,terms,
	       pureActions,...},
	 NAMES {miscStruct,tableStruct,valueStruct,
		tokenStruct,tokenSig,dataStruct,...}) =>
		(sayln ("structure " ^ tokenStruct ^ " : " ^ tokenSig ^ " =");
		 sayln "struct";
	         sayln ("type svalue = " ^ dataStruct ^ ".svalue");
		 sayln "type ('a,'b) token = ('a,'b) Token.token";
		 let val f = fn term as T i =>
			(say "fun "; say (termToString term);
			 say " (";
		         if (hasType (TERM term)) then say "i," else ();
			 say "p1,p2) = Token.TOKEN (";
			 say (dataStruct ^ "." ^ tableStruct ^ ".T ");
			 say (makestring i);
			 say ",(";
			 say (dataStruct ^ "." ^ valueStruct ^ ".");
			 if (hasType (TERM term)) then 
			    (say (termToString term);
			     if pureActions then say " i"
			     else say " (fn () => i)")
			 else say termvoid;
			 say ",";
			 sayln "p1,p2))")
		in app f terms
		end;
		sayln "end")
			 
    (* function to print signatures out - takes print function which
	does not need to insert line breaks *)

    val printSigs = fn (VALS {term,...},
			NAMES {tokenSig,tokenStruct,miscSig,
				dataStruct, dataSig, ...},
			say) =>
		(say "signature ";
		 say tokenSig;
		 say " =\n";
		 say "sig\n";
		 say	"type ('a,'b) token\n\
			\type svalue\n";
		 app (fn ((s,_),ty) => (say "val "; say s; say ": (";
					case ty
					of NONE => () 
					 | SOME l => (say "(";
						      say l;
						      say ") * ");
					say "'a * 'a) ->";
					say "(svalue,'a) token\n"
				       )) term;
		 say "end\n";
		 say "signature ";
		 say miscSig;
		 say " =\n";
		 say "sig\n";
		 say "structure ";
		 say ("Tokens : " ^ tokenSig ^ "\n");
		 say ("structure " ^ dataStruct ^ ":" ^ dataSig ^ "\n");
		 say ("sharing type " ^ dataStruct ^ ".Token.token = Tokens.token\n");
		 say ("sharing type " ^ dataStruct ^
				 ".svalue = Tokens.svalue\n");
		 say "end\n")
		
    (* function to print structure for error correction *)

    val printEC = fn (keyword : term list,
		      preferred : term list,
		      subst : (term * term) list,
		      noshift : term list,
		      value : (term * string) list,
		      VALS {termToString, say,sayln,terms,saydot,hasType,
			    termvoid,pureActions,...},
		      NAMES {ecStruct,tableStruct,valueStruct,...}) =>
       let

	 (* subst is a list of pairs (sym,sym'), where sym is a preferred
	    substitution for sym'.  Construct a list of (sym,[..syms...]) where
	    the elements of the list are all the preferred substitions for
	    the sym *)
	
	 val subst =
	   let fun f ((sym,sym'),table) =
		case TermTable.find(sym',table)
		  of SOME l => TermTable.insert((sym',sym :: l),table)
		   | NONE => TermTable.insert((sym',[sym]),table)
	   in TermTable.make_list (fold f subst TermTable.empty)
	   end

	 val sayterm = fn (T i) => (say "(T "; say (makestring i); say ")")

	 val printBoolCase = fn ( l : term list) =>
	    (say "fn ";
	     app (fn t => (sayterm t; say " => true"; say " | ")) l;
	     sayln "_ => false")

	 val printTermList = fn (l : term list) =>
	    (app (fn t => (sayterm t; say " :: ")) l; sayln "nil")

	 val printSubst = fn (l : (term * (term list)) list) =>
	    (sayln "val preferred_subst =";
	     say "fn ";
	     app (fn (t,l') =>
		    (sayterm t; say " =>";
		     app (fn t => (sayterm t; say "::")) l';
		     sayln "nil"; say "|"
		    )
		 ) l;
	     sayln " _ => nil")

	 val printErrValues = fn (l : (term * string) list) =>
	    (sayln "val errtermvalue=";
	     sayln "let open Header in";
	     say "fn ";
	     app (fn (t,s) =>
		    (sayterm t; say " => ";
		     saydot valueStruct; say (termToString t);
		     say "(";
		     if pureActions then () else say "fn () => ";
		     say "("; say s; say "))";
		     sayln " | "
		    )
		 ) l;
	    say "_ => ";
	    say (valueStruct ^ ".");
	    sayln termvoid; sayln "end")
	      

	  val printNames = fn () =>
		let val f = fn term =>
			 (sayterm term; say " => "; say "\"";
			  say (termToString term); sayln "\""; say "  | ")
		in (sayln "val showTerminal =";
		    say "fn ";
		    app f terms;
		    sayln "_ => \"bogus-term\"")
		end

	   val ecTerms = 
		List.fold (fn (t,r) =>
		  if hasType (TERM t) orelse exists (fn (a,_)=>a=t) value
		    then r
		    else t::r)
		terms nil
				  
	in  say "structure ";
	    say ecStruct;
	    sayln "=";
	    sayln "struct";
	    say "open ";
	    sayln tableStruct;
	    sayln "val is_keyword =";
	    printBoolCase keyword;
	    sayln "val preferred_insert =";
	    printBoolCase preferred;
	    printSubst subst;
	    sayln "val noShift = ";
	    printBoolCase noshift;
	    printNames ();
	    printErrValues value;
	    say "val terms = ";
	    printTermList ecTerms;
	    sayln "end"
	end

    val printAction = fn (rules,
			  VALS {hasType,say,sayln,termvoid,ntvoid,
			        symbolToString,saydot,start,pureActions,...},
			  NAMES {actionsStruct,valueStruct,tableStruct,arg,...}) =>
	 let val is_nonterm = fn (NONTERM i) => true | _ => false
	     val numberRhs = fn r =>
		List.revfold (fn (e,(r,table)) =>
			let val num = case SymbolTable.find(e,table)
				       of SOME i => i
					| NONE => 1
			 in ((e,num,hasType e orelse is_nonterm e)::r,
			     SymbolTable.insert((e,num+1),table))
			 end) r (nil,SymbolTable.empty)

	     val saySym = say o symbolToString

	     val printCase = fn (i:int, r as {lhs=lhs as (NT lhsNum),prec,
				        rhs,code,rulenum}) =>
		(say "("; say(makestring i); say ",";

		   (* prToken: print an argument *)

		 let val prToken = fn (sym,num : int,typed) =>
		 	let val symString = symbolToString sym
			    val symNum = symString ^ (makestring num)
			in say "(_,(";
			   if not (hasType sym) then
			      (if is_nonterm sym then
				   (saydot valueStruct;
				    say ntvoid; say  " "; say symNum)
			      else say "_")
			   else	
			       (saydot valueStruct; say symString;
				say " ("; say symNum;
			         if num=1 andalso pureActions
				     then say (" as " ^ symString)
				 else ();
				 say ")"
				);
			   say ",";
			   if num=1 then (say (symString ^ "left as "))
				    else ();
			   say (symNum ^ "left");
			   say ",";
			   if num=1 then (say (symString ^ "right as "))
				    else ();
			   say (symNum ^ "right");
			   say ")) :: "
		        end
		    val (numberedRhs,_) = numberRhs rhs

		    (* print arguments *)

		    val _ = (app prToken numberedRhs; sayln "rest671) =>")

		    (* remove terminals in argument list w/o types *)

		    val argsWithTypes =
			  fold (fn ((_,_,false),r) => r
				 | (s as (_,_,true),r) => s::r) numberedRhs nil

		    val prBody =
			(sayln "let val result = ";
			 saydot valueStruct;
			 if hasType (NONTERM lhs)
			    then saySym (NONTERM lhs)
			    else say ntvoid;
			 say " (";
			 if pureActions then 
				(say "(("; say code; say ")")
			 else if argsWithTypes = nil then
				(say " fn () => (("; say code; say ")")
			 else
 				 (sayln "fn () => (let ";
				  revapp (fn (sym,num : int,_) =>
				 let val symString = symbolToString sym
				     val symNum = symString ^ (makestring num)
				 in (say "val ";
				     if num=1 then (say symString; say " as ")
				     else ();
				     say symNum;
				     say " = ";
				     say symNum;
				     sayln "()")
				 end) argsWithTypes;
				 say " in ("; say code; say ") end "
				);
			  if hasType (NONTERM lhs) 
			      then ()
			      else say "; ()";
			 sayln "))";
			 say "in ("; say (tableStruct ^ ".NT ");
			 say (makestring lhsNum);
			 say ",(result,";
			 case rhs
			  of nil => say "defaultPos,defaultPos"
			   | r => let val (rsym,rnum,_) = hd(numberedRhs)
				      val (lsym,lnum,_) = hd(rev numberedRhs)
				  in say (symbolToString lsym ^
					  (makestring lnum) ^ "left,");
				     say (symbolToString rsym ^
					  (makestring rnum) ^ "right")
 				  end;
			 sayln "),rest671)";
			 sayln "end")
		in ()
		end)
	  val prRules = fn () =>
	     (sayln "fn (i392,defaultPos,stack,";
	      say   "    ("; say arg; sayln "):arg) =>";
	      sayln "case (i392,stack)";
	      say "of ";
	      app (fn (rule as {rulenum,...}) =>
		   (printCase(rulenum,rule); say "| ")) rules;
	     sayln "_ => raise (mlyAction i392)")

   	in say "structure ";
	   say actionsStruct;
	   sayln " =";
	   sayln "struct ";
	   sayln "exception mlyAction of int";
	   sayln "val actions = ";
	   sayln "let open Header";
	   sayln "in";
	   prRules();
	   sayln "end";
	   say "val void = ";
	   saydot valueStruct;
	   sayln termvoid;
	   say "val extract = ";
	   say "fn a => (fn ";
	   saydot valueStruct;
	   if hasType (NONTERM start)
	      then say (symbolToString (NONTERM start))
	      else say "ntVOID";
	   sayln " x => x";
	   sayln "| _ => let exception ParseInternal";
	   say "\tin raise ParseInternal end) a ";
	   sayln (if pureActions then "" else "()");
	   sayln "end"
	end

    val make_parser = fn ((header,
	 {eop,prefer,keyword,nonterm,prec, subst,
	  term, control,value} : declData,
	  rules : rule list),spec) =>
     let
	val verbose = List.exists (fn a=>a=VERBOSE) control
	val defaultReductions = not (List.exists (fn a=>a=NODEFAULT) control)
	val pos_type =
	   let fun f nil = NONE
		 | f ((POS s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val start =
	   let fun f nil = NONE
		 | f ((START_SYM s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val name =
	   let fun f nil = NONE
		 | f ((PARSER_NAME s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val header_decl =
	   let fun f nil = NONE
		 | f ((FUNCTOR s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val arg_decl =
	   let fun f nil = ("()","unit")
		 | f ((PARSE_ARG s)::r) = s 
		 | f (_::r) = f r
	   in f control
	   end

	val noshift =
	   let fun f nil = nil
		 | f ((NSHIFT s)::r) = s 
		 | f (_::r) = f r
	   in f control
	   end

	val pureActions =
	   let fun f nil = false
		 | f ((PURE)::r) = true 
		 | f (_::r) = f r
	   in f control
	   end

	val term =
	 case term
	   of NONE => (cerror "missing %term definition"; nil)
	    | SOME l => l

	val nonterm =
	 case nonterm
	  of NONE => (cerror "missing %nonterm definition"; nil)
	   | SOME l => l

	val pos_type =
	 case pos_type
	  of NONE => (cerror "missing %pos definition"; "")
	   | SOME l => l


	val termHash = 
	  fold (fn (((name,line),_),table) =>
		if SymbolHash.exists(name,table) then
		   (error ("duplicate definition of " ^ name ^ " in %term") line;
		    table)
		else SymbolHash.add(name,table)) term SymbolHash.empty

	val isTerm = fn name => SymbolHash.exists(name,termHash)

	val symbolHash = 
	  fold (fn (((name,line),_),table) =>
	     if SymbolHash.exists(name,table) then
	       (if (isTerm name) then
		  (error (name ^ " is defined as a terminal and a nonterminal")
		   line; table)
		else 
		  (error ("duplicate definition of " ^ name ^ " in %nonterm")
		   line; table)
		)
	     else SymbolHash.add(name,table)) nonterm termHash

	fun makeUniqueId s =
		if SymbolHash.exists(s,symbolHash) then makeUniqueId (s ^ "'")
		else s

	val _ = if (!errflag) then raise SemanticError else ()

	val numTerms = SymbolHash.size termHash
	val numNonterms = SymbolHash.size symbolHash - numTerms

	val symError = fn sym => fn err => fn (name,line) =>
	    error (name ^ " in " ^ err ^ " is not defined as a " ^ sym) line

	val termNum : string -> symbol -> term =
	  let val termError = symError "terminal" 
	  in fn stmt =>
	     let val stmtError = termError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; T ~1)
	         | SOME i => T (if i<numTerms then i
			        else (stmtError symbol; ~1))
	     end
	  end
			
	val nontermNum : string -> symbol -> nonterm =
	  let val nontermError = symError "nonterminal" 
	  in fn stmt =>
	     let val stmtError = nontermError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; NT ~1)
	         | SOME i => if i>=numTerms then NT (i-numTerms)
			     else (stmtError symbol;NT ~1)
	     end
	  end

	val symbolNum : string -> symbol -> Grammar.symbol =
	  let val symbolError = symError "symbol" 
	  in fn stmt =>
	     let val stmtError = symbolError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; NONTERM (NT ~1))
	         | SOME i => if i>=numTerms then NONTERM(NT (i-numTerms))
			     else TERM(T i)
	     end
	  end

(* map all symbols in the following values to terminals and check that
   the symbols are defined as terminals:

	eop : symbol list
	keyword: symbol list
	prefer: symbol list
	prec: (lexvalue * (symbol list)) list
	subst: (symbol * symbol) list
*)

	val eop = map (termNum "%eop") eop
	val keyword = map (termNum "%keyword") keyword
	val prefer = map (termNum "%prefer") prefer
	val prec = map (fn (a,l) => 
			(a,case a
			   of LEFT => map (termNum "%left") l
			    | RIGHT => map (termNum "%right") l
			    | NONASSOC => map (termNum "%nonassoc") l
			)) prec
	val subst =
	 let val mapTerm = termNum "%subst"
	 in map (fn (a,b) => (mapTerm a,mapTerm b)) subst
	 end
	val noshift = map (termNum "%noshift") noshift
	val value =
	  let val mapTerm = termNum "%value"
	  in map (fn (a,b) => (mapTerm a,b)) value
	  end
	val (rules,_) =
	   let val symbolNum = symbolNum "rule"
	       val nontermNum = nontermNum "rule"
	       val termNum = termNum "%prec tag"
           in List.fold
	   (fn ({lhs,rhs,code,prec},(l,n)) =>
	     ( {lhs=nontermNum lhs,rhs=map symbolNum rhs,
	        code=code,prec=case prec
				of NONE => NONE
				 | SOME t => SOME (termNum t),
		 rulenum=n}::l,n-1))
		 rules (nil,length rules-1)
	end

	val _ = if (!errflag) then raise SemanticError else ()

	(* termToString: map terminals back to strings *)

	val termToString =
	   let val data = array(numTerms,"")
	       val unmap = fn ((name,_),_) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i,name)
	       val _ = app unmap term
	   in fn T i =>
		if DEBUG andalso (i<0 orelse i>=numTerms)
		  then "bogus-num" ^ (makestring i)
		  else data sub i
	   end

	val nontermToString = 
	   let val data = array(numNonterms,"")
	       val unmap = fn ((name,_),_) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i-numTerms,name)
	       val _ = app unmap nonterm
	   in fn NT i =>
		if DEBUG andalso (i<0 orelse i>=numNonterms)
		  then "bogus-num" ^ (makestring i)
		  else data sub i
	   end

(* create functions mapping terminals to precedence numbers and rules to
   precedence numbers.

  Precedence statements are listed in order of ascending (tighter binding)
  precedence in the specification.   We receive a list composed of pairs
  containing the kind of precedence (left,right, or assoc) and a list of
  terminals associated with that precedence.  The list has the same order as
  the corresponding declarations did in the specification.

  Internally, a tighter binding has a higher precedence number.  We give
  precedences using multiples of 3:

		p+2 = right associative (force shift of symbol)
		p+1 = precedence for rule
		p = left associative (force reduction of rule)

  Nonassociative terminals are given also given a precedence of p+1.  The
table generator detects when the associativity of a nonassociative terminal
is being used to resolve a shift/reduce conflict by checking if the
precedences of the rule and the terminal are equal.

  A rule is given the precedence of its rightmost terminal *)

	val termPrec =
	    let val precData = array(numTerms, NONE : int option)
	        val addPrec = fn termPrec => fn term as (T i) =>
		   case precData sub i
		   of SOME _ =>
		     cerror("multiple precedences specified for terminal " ^
			    (termToString term))
		    | NONE => update(precData,i,termPrec)
		val termPrec = fn ((LEFT,_) ,i) => i
			      | ((RIGHT,_),i) => i+2
			      | ((NONASSOC,l),i) => i+1
		val _ = revfold (fn (args as ((_,l),i)) =>
			        (app (addPrec (SOME (termPrec args))) l; i+3))
			  prec 0
	   in fn (T i) =>
		if  DEBUG andalso (i < 0 orelse i >= numTerms) then
			NONE
		else precData sub i
	   end

	val rulePrec = 
	   let fun findRightTerm (nil,r) = r
	         | findRightTerm (TERM t :: tail,r) =
				 findRightTerm(tail,SOME t)
		 | findRightTerm (_ :: tail,r) = findRightTerm(tail,r)
	   in fn rhs =>
		 case findRightTerm(rhs,NONE)
		 of NONE => NONE
		  | SOME term => 
		       case termPrec term
		       of SOME i => SOME (i - (i mod 3) + 1)
		        | a => a
	   end

	val grammarRules =
	  let val conv = fn {lhs,rhs,code,prec,rulenum} =>
		{lhs=lhs,rhs =rhs,precedence=
			case prec
			  of SOME t => termPrec t
			   | _ => rulePrec rhs,
	         rulenum=rulenum}
	  in map conv rules
	  end

    (* get start symbol *)

	val start =
	 case start
	   of NONE => #lhs (hd grammarRules)
	    | SOME name => 
		nontermNum "%start" name

	val symbolType = 
	   let val data = array(numTerms+numNonterms,NONE : ty)
	       val unmap = fn ((name,_),ty) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i,ty)
	       val _ = (app unmap term; app unmap nonterm)
	   in fn NONTERM(NT i) =>
		if DEBUG andalso (i<0 orelse i>=numNonterms)
		  then NONE
		  else data sub (i+numTerms)
	       | TERM (T i) =>
		if DEBUG andalso (i<0 orelse i>=numTerms)
		  then NONE
		  else data sub i
	   end

	val symbolToString = 
	     fn NONTERM i => nontermToString i
	      | TERM i => termToString i

	val grammar  = GRAMMAR {rules=grammarRules,
				 terms=numTerms,nonterms=numNonterms,
				 eop = eop, start=start,noshift=noshift,
				 termToString = termToString,
				 nontermToString = nontermToString,
				 precedence = termPrec}

	val name' = case name
			 of NONE => ""
		          | SOME (s,_) => s
		    
	val names = NAMES {miscStruct=name' ^ "LrValsFun",
			   valueStruct="MlyValue",
			   tableStruct="LrTable",
			   tokenStruct="Tokens",
			   actionsStruct="Actions",
			   ecStruct="EC",
			   arg= #1 arg_decl,
			   tokenSig = name' ^ "_TOKENS",
			   miscSig = name' ^ "_LRVALS",
			   dataStruct = "ParserData",
			   dataSig = "PARSER_DATA"}
		       


	val (table,stateErrs,corePrint,errs) =
		 MakeTable.mkTable(grammar,defaultReductions)
	
    in if verbose then
	 let val f = open_out (spec ^ ".desc")
	     val say = fn s=> output(f,s)
	     val printRule =
	        let val rules = arrayoflist grammarRules
	        in fn say => 
		   let val prRule = fn {lhs,rhs,precedence,rulenum} =>
		     ((say o nontermToString) lhs; say " : ";
		      app (fn s => (say (symbolToString s); say " ")) rhs)
	           in fn i => prRule (rules sub i)
	           end
	        end
	 in Verbose.printVerbose
	    {termToString=termToString,nontermToString=nontermToString,
	     table=table, stateErrs=stateErrs,errs = errs,
	     print=say, printCores=corePrint,printRule=printRule};
	    close_out f
	 end
        else ();

        let val result = open_out (spec ^ ".sml")
 	    val sigs = open_out (spec ^ ".sig")
	    val pos = ref 0
	    val pr = fn s => output(result,s)
	    val say = fn s => let val l = String.length s
			           val newPos = (!pos) + l
			      in if newPos > lineLength 
				    then (pr "\n"; pos := l)
				    else (pos := newPos);
				   pr s
			      end
	    val saydot = fn s => (say (s ^ "."))
	    val sayln = fn t => (pr t; pr "\n"; pos := 0)
	    val termvoid = makeUniqueId "VOID"
	    val ntvoid = makeUniqueId "ntVOID"
	    val hasType = fn s => case symbolType s
				  of NONE => false
				   | _ => true
	    val terms = let fun f n = if n=numTerms then nil
				      else (T n) :: f(n+1)
		        in f 0
		        end
            val values = VALS {say=say,sayln=sayln,saydot=saydot,
		 	       termvoid=termvoid, ntvoid = ntvoid,
			       hasType=hasType, pos_type = pos_type,
			       arg_type = #2 arg_decl,
			       start=start,pureActions=pureActions,
			       termToString=termToString,
			       symbolToString=symbolToString,term=term,
			       nonterm=nonterm,terms=terms}

	    val (NAMES {miscStruct,tableStruct,dataStruct,...}) = names

         in case header_decl
	    of NONE => (say "functor "; say miscStruct; 
			sayln "(structure Token : TOKEN)")
	     | SOME s => say s;
	    sayln " = ";
	    sayln "struct";
	    sayln ("structure " ^ dataStruct ^ "=");
	    sayln "struct";
	    sayln "structure Header = ";
	    sayln "struct";
	    sayln header;
	    sayln "end";
	    sayln "structure LrTable = Token.LrTable";
	    sayln "structure Token = Token";
	    sayln "local open LrTable in ";
	    PrintStruct.makeStruct{table=table,print=pr, name = "table"};
	    sayln "end";
	    printTypes(values,names,symbolType);
	    printEC (keyword,prefer,subst,noshift,value,values,names);
	    printAction(rules,values,names);
	    sayln "end";
	    printTokenStruct(values,names);
	    sayln "end";
	    printSigs(values,names,fn s => output(sigs,s));    
	    close_out sigs;
	    close_out result;
	    MakeTable.Errs.printSummary (fn s => output(std_out,s)) errs
	end		
    end

    val parseGen = fn spec =>
		(Header.errflag := false;
		 Header.infile := spec;
		 let val (result,_) = ParseGenParser.parse spec
		 in make_parser(result,spec)
		 end)
end
