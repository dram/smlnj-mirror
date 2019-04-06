signature LEX = sig
    structure Token: TOKEN
    structure Symbol: SYMBOL
    val toplevel : bool ref
    val nextToken: Token.token ref
    val advance: unit -> unit
    val at: Token.token -> bool
    val checkToken: Token.token -> unit
    val getSymbol : unit -> Symbol.symbol
    val flush : unit -> unit
    val pushSource : instream * string -> unit
    val popSource : unit -> unit
end

structure Lex : LEX =
struct
    structure Token : TOKEN = Token
    structure Symbol : SYMBOL = Symbol
    open Mlex.UserDeclarations

    val toplevel = ref true
    val interactive = System.interactive

    val nextToken = ref Token.SEMICOLON
    val getToken = ref(fn () => Token.SEMICOLON)
    val stream = ref std_in

    fun prompt() = (output std_out (if !toplevel
				    then !System.Control.primaryPrompt
				    else !System.Control.secondaryPrompt);
		    flush_out std_out)
    
    fun advance() = nextToken := !getToken()

    val doprompt = ref true

    val makeLexer = fn strm =>
	let val read = input strm
	    fun yyinput n = 
		if !interactive
		then (if !doprompt then (prompt(); doprompt := false) else ();
		      let val s = input_line strm
		      in doprompt := (ordof(s,length(s)-1)=ord("\n")
				     handle Ord => false);
		         s
		      end)
		else read n
	 in Mlex.makeLexer yyinput
	end

fun getSymbol () = case !nextToken of
		     Token.ID s => (advance(); s)
                   | Token.ASTERISK => (advance(); Symbols.ASTERISKsym)
		   | Token.EQUAL => (advance(); Symbols.EQUALsym)
		   | Token.TYVAR s => (advance(); s)
		   | Token.IDDOT s => (advance(); s)
		   | tok => ErrorMsg.impossible("getSymbol: " ^ Token.tokenName tok)

fun at tok = if !nextToken = tok then (advance(); true) else false

fun checkToken tok =
    if at(tok)
    then ()
    else complain("expected "^Token.tokenName tok^
			    ", found "^Token.tokenName(!nextToken))

type source = {gettok : unit -> Token.token, stream : instream,
	       nexttok : Token.token, filename : string, comlev : int,
	       linenum : int, doprompt : bool, interactive : bool}


val sourceStack = ref([]:source list)

fun save() = {gettok = !getToken, stream = !stream, doprompt = !doprompt,
	      nexttok = !nextToken, filename = !fileName, linenum = !lineNum,
	      comlev = !comLevel, interactive = !interactive}

fun restore
 ({gettok,stream=s,doprompt=d,nexttok,linenum,comlev,filename,interactive=i}) =
    (getToken := gettok; stream := s; doprompt := d;
     nextToken := nexttok; lineNum := linenum; comLevel := comlev;
     fileName := filename; interactive := i)

fun pushSource(strm,fname) =
    (sourceStack := save() :: !sourceStack; getToken := makeLexer(strm);
     stream := strm; fileName := fname; interactive := is_term_in strm;
     nextToken := Token.SEMICOLON; lineNum := 1; comLevel := 0;
     doprompt := true)

val _ = pushSource (std_in, "std_in")

fun popSource() = 
    case !sourceStack
      of [ _ ] => impossible "exhausted sources"
       | source::rest => (restore source; sourceStack := rest)

fun flush() = (input (!stream) (can_input (!stream)); 
	       nextToken := Token.SEMICOLON; comLevel := 0; doprompt := true;
	       getToken := makeLexer (!stream))

end
    
