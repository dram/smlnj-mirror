structure LexGen =
   struct
   val sub = Array.sub
   infix 9 sub

   datatype token = CHARS of bool array | QMARK | STAR | PLUS | BAR
	  | LP | RP | CARAT | DOLLAR | SLASH | STATE of string list
	  | REPS of int * int | ID of string | ACTION of string
	  | BOF | EOF | ASSIGN | SEMI | ARROW | LEXMARK | LEXSTATES
	  | COUNT | REJECT | FULLCHARSET | STRUCT | HEADER | ARG | POSARG

   datatype exp = EPS | CLASS of bool array * int | CLOSURE of exp
		| ALT of exp * exp | CAT of exp * exp | TRAIL of int
		| END of int

   (* flags describing input Lex spec. - unnecessary code is omitted *)
   (* if possible *)

   val CharFormat = ref false;
   val UsesTrailingContext = ref false;
   val UsesPrevNewLine = ref false;

   (* flags for various bells & whistles that Lex has.  These slow the
      lexer down and should be omitted from production lexers (if you
      really want speed) *)

   val CountNewLines = ref false;
   val PosArg = ref false;
   val HaveReject = ref false;

   (* Can increase size of character set *)

   val CharSetSize = ref 129;

   (* Can name structure or declare header code *)

   val StrName = ref "Mlex"
   val HeaderCode = ref ""
   val HeaderDecl = ref false
   val ArgCode = ref (NONE: string option)
   val StrDecl = ref false

   val ResetFlags = fn () => (CountNewLines := false; HaveReject := false;
			      PosArg := false;
			      UsesTrailingContext := false;
			       CharSetSize := 129; StrName := "Mlex";
				HeaderCode := ""; HeaderDecl:= false;
				ArgCode := NONE;
				StrDecl := false)

   val LexOut = ref(TextIO.stdOut)
   fun say x = TextIO.output(!LexOut, x)

(* Union: merge two sorted lists of integers *)

fun union(a,b) = let val rec merge = fn
	  (nil,nil,z) => z
	| (nil,el::more,z) => merge(nil,more,el::z)
	| (el::more,nil,z) => merge(more,nil,el::z)
	| (x::morex,y::morey,z) => if (x:int)=(y:int)
		then merge(morex,morey,x::z)
		else if x>y then merge(morex,y::morey,x::z)
		else merge(x::morex,morey,y::z)
	in merge(rev a,rev b,nil)
end

(* Nullable: compute if a important expression parse tree node is nullable *)

val rec nullable = fn
	  EPS => true
	| CLASS(_) => false
	| CLOSURE(_) => true
	| ALT(n1,n2) => nullable(n1) orelse nullable(n2)
	| CAT(n1,n2) => nullable(n1) andalso nullable(n2)
	| TRAIL(_) => true
	| END(_) => false

(* FIRSTPOS: firstpos function for parse tree expressions *)

and firstpos = fn
	  EPS => nil
	| CLASS(_,i) => [i]
	| CLOSURE(n) => firstpos(n)
	| ALT(n1,n2) => union(firstpos(n1),firstpos(n2))
	| CAT(n1,n2) => if nullable(n1) then union(firstpos(n1),firstpos(n2))
		else firstpos(n1)
	| TRAIL(i) => [i]
	| END(i) => [i]

(* LASTPOS: Lastpos function for parse tree expressions *)

and lastpos = fn
	  EPS => nil
	| CLASS(_,i) => [i]
	| CLOSURE(n) => lastpos(n)
	| ALT(n1,n2) => union(lastpos(n1),lastpos(n2))
	| CAT(n1,n2) => if nullable(n2) then union(lastpos(n1),lastpos(n2))
		else lastpos(n2)
	| TRAIL(i) => [i]
	| END(i) => [i]
	;

(* ++: Increment an integer reference *)

fun ++(x) : int = (x := !x + 1; !x);

structure dict =
    struct
	type 'a relation = 'a * 'a -> bool
        abstype ('b,'a) dictionary = DATA of { Table : ('b * 'a) list,
				          Leq : 'b * 'b -> bool }
	with
    	    exception LOOKUP
	    fun create Leqfunc = DATA { Table = nil, Leq = Leqfunc }
	    fun lookup (DATA { Table = entrylist, Leq = leq }) key =
		let fun search [] = raise LOOKUP
		      | search((k,item)::entries) =
			if leq(key,k)
			then if leq(k,key) then item else raise LOOKUP
			else search entries
		in search entrylist
	        end
	     fun enter (DATA { Table = entrylist, Leq = leq })
		(newentry as (key : 'b,item :'a)) : ('b,'a) dictionary =
		   let val gt = fn a => fn b => not (leq(a,b))
		       val eq = fn k => fn k' => (leq(k,k')) andalso (leq(k',k))
		       fun update nil = [ newentry ]
			 | update ((entry as (k,_))::entries) =
			      if (eq  key k) then newentry::entries
			      else if gt k key then newentry::(entry::entries)
			      else entry::(update entries)
		   in DATA { Table = update entrylist, Leq = leq }
	           end
	     fun listofdict (DATA { Table = entrylist,Leq = leq}) =
		let fun f (nil,r) = rev r
		      | f (a::b,r) = f (b,a::r)
	   	in f(entrylist,nil)
		end
      end
end

open dict;

(* INPUT.ML : Input w/ one character push back capability *)

val LineNum = ref 1;

abstype ibuf =
	BUF of TextIO.instream * {b : string ref, p : int ref}
with
	fun make_ibuf(s) = BUF (s, {b=ref"", p = ref 0})
	fun close_ibuf (BUF (s,_)) = TextIO.closeIn(s)
	exception eof
	fun getch (a as (BUF(s,{b,p}))) =
		 if (!p = (size (!b)))
		   then (b := TextIO.inputN(s, 1024);
			 p := 0;
			 if (size (!b))=0
			    then raise eof
			    else getch a)
		   else (let val ch = String.sub(!b,!p)
			 in (if ch = #"\n"
				 then LineNum := !LineNum + 1
				 else ();
			     p := !p + 1;
			     ch)
			 end)
	fun ungetch(BUF(s,{b,p})) = (
	   p := !p - 1;
	   if String.sub(!b,!p) = #"\n"
	      then LineNum := !LineNum - 1
	      else ())
end;

exception Error

fun prErr x = (
      TextIO.output (TextIO.stdErr, String.concat [
	  "ml-lex: error, line ", (Int.toString (!LineNum)), ": ", x, "\n"
	]);
      raise Error)
fun prSynErr x = (
      TextIO.output (TextIO.stdErr, String.concat [
	  "ml-lex: syntax error, line ", (Int.toString (!LineNum)), ": ", x, "\n"
	]);
      raise Error)

exception SyntaxError; (* error in user's input file *)

exception LexError; (* unexpected error in lexer *)

val LexBuf = ref(make_ibuf(TextIO.stdIn));
val LexState = ref 0;
val NextTok = ref BOF;
val inquote = ref false;

fun AdvanceTok () : unit = let
      fun isLetter c =
	    ((c >= #"a") andalso (c <= #"z")) orelse
	    ((c >= #"A") andalso (c <= #"Z"))
      fun isDigit c = (c >= #"0") andalso (c <= #"9")
    (* check for valid (non-leading) identifier character (added by JHR) *)
      fun isIdentChr c =
	    ((isLetter c) orelse (isDigit c) orelse (c = #"_") orelse (c = #"'"))
      fun atoi s = let
	    fun num (c::r, n) = if isDigit c
		  then num (r, 10*n + (Char.ord c - Char.ord #"0"))
		  else n
	      | num ([], n) = n
	    in
	      num (explode s, 0)
	    end

      fun skipws () = let val ch = nextch()
	    in
	      if Char.isSpace ch
		then skipws()
		else ch
	    end

      and nextch () = getch(!LexBuf)

      and escaped () = (case nextch()
	     of #"b" => #"\008"
	      | #"n" => #"\n"
	      | #"r" => #"\r"
	      | #"t" => #"\t"
	      | #"h" => #"\128"
	      | x => let
		  fun err t = prErr("illegal ascii escape '"^(implode(rev t))^"'")
		  fun cvt c = (Char.ord c - Char.ord #"0")
		  fun f (n, c, t) = if c=3
			then if n >= (!CharSetSize)
			  then err t
			  else Char.chr n
		        else let val ch=nextch()
			  in
			    if isDigit ch
			      then f(n*10+(cvt ch), c+1, ch::t)
		  	      else err t
			  end
		  in
		    if isDigit x then f(cvt x, 1, [x]) else x
		  end
	    (* end case *))

      and onechar x = let val c = Array.array(!CharSetSize, false)
	      in
		Array.update(c, Char.ord(x), true); CHARS(c)
	      end

      in case !LexState of 0 => let val makeTok = fn () =>
		case skipws()
			(* Lex % operators *)
		 of #"%" => (case nextch() of
		  	  #"%" => LEXMARK
			| a => let fun f s =
				    let val a = nextch()
				    in if isLetter a then f(a::s)
					else (ungetch(!LexBuf);
					      implode(rev s))
				    end
			        in case f [a]
				 of "reject" => REJECT
				  | "count"  => COUNT
				  | "full"   => FULLCHARSET
				  | "s"      => LEXSTATES
				  | "S"      => LEXSTATES
				  | "structure" => STRUCT
				  | "header" => HEADER
				  | "arg"    => ARG
				  | "posarg" => POSARG
			          | _ => prErr "unknown % operator "
			       end
			     )
			(* semicolon (for end of LEXSTATES) *)
		| #";" => SEMI
			(* anything else *)
		| ch => if isLetter(ch) then
			 let fun getID matched =
			     let val x = nextch()
(**** fix by JHR
			     in if isLetter(x) orelse isDigit(x) orelse
                                   x = "_" orelse x = "'"
****)
			     in if (isIdentChr x)
				 then getID (x::matched)
				 else (ungetch(!LexBuf); implode(rev matched))
			     end
			in ID(getID [ch])
			end
		      else prSynErr (String.concat[
			  "bad character: \"", Char.toString ch, "\""
			])
	in NextTok := makeTok()
	end
	| 1 => let val rec makeTok = fn () =>
		if !inquote then case nextch() of
			(* inside quoted string *)
		  #"\\" => onechar(escaped())
		| #"\"" => (inquote := false; makeTok())
		| #"\n" => (prSynErr "end-of-line inside quoted string";
			    inquote := false; makeTok())
		| x => onechar(x)
		else case skipws() of
			(* single character operators *)
		  #"?" => QMARK
		| #"*" => STAR
		| #"+" => PLUS
		| #"|" => BAR
		| #"(" => LP
		| #")" => RP
		| #"^" => CARAT
		| #"$" => DOLLAR
		| #"/" => SLASH
		| #";" => SEMI
		| #"." => let val c = Array.array(!CharSetSize,true) in
				Array.update(c,10,false); CHARS(c)
			end
			(* assign and arrow *)
		| #"=" => let val c = nextch() in
			if c = #">" then ARROW else (ungetch(!LexBuf); ASSIGN)
		end
			(* character set *)
		| #"[" => let val rec classch = fn () => let val x = skipws()
				in if x = #"\\" then escaped() else x
				end;
			val first = classch();
			val flag = (first <> #"^");
			val c = Array.array(!CharSetSize,not flag);
			fun add NONE = ()
			  | add (SOME x) = Array.update(c, Char.ord(x), flag)
			and range (x, y) = if x>y
			      then (prErr "bad char. range")
			      else let
				val i = ref(Char.ord(x)) and j = Char.ord(y)
				in while !i<=j do (
				  add (SOME(Char.chr(!i)));
				  i := !i + 1)
				end
			and getClass last = (case classch()
			     of #"]" => (add(last); c)
			      | #"-" => (case last
				   of NONE => getClass(SOME #"-")
				    | (SOME last') => let val x = classch()
					in
					  if x = #"]"
					    then (add(last); add(SOME #"-"); c)
					    else (range(last',x); getClass(NONE))
					end
				  (* end case *))
			      | x => (add(last); getClass(SOME x))
			    (* end case *))
		in CHARS(getClass(if first = #"^" then NONE else SOME first))
		end
			(* Start States specification *)
		| #"<" => let val rec get_state = fn (prev,matched) =>
			case nextch() of
			  #">" => matched::prev
			| #"," => get_state(matched::prev,"")
			| x => if isIdentChr(x)
				then get_state(prev,matched ^ String.str x)
				else (prSynErr "bad start state list")
		in STATE(get_state(nil,""))
		end
			(* {id} or repititions *)
		| #"{" => let val ch = nextch() in if isLetter(ch) then
			let fun getID matched = (case nextch()
			  of #"}" => matched
			   | x => if (isIdentChr x) then
				getID(matched ^ String.str x)
				else (prErr "invalid char. class name")
			 (* end case *))
			in ID(getID(String.str ch))
			end
			else if isDigit(ch) then
			 let fun get_r (matched, r1) = (case nextch()
				 of #"}" => let val n = atoi(matched) in
					if r1 = ~1 then (n,n) else (r1,n)
					end
				  | #"," => if r1 = ~1 then get_r("",atoi(matched))
				       else (prErr "invalid repetitions spec.")
				  | x => if isDigit(x)
				    then get_r(matched ^ String.str x,r1)
			            else (prErr "invalid char in repetitions spec")
				(* end case *))
			 in REPS(get_r(String.str ch,~1))
			 end
			else (prErr "bad repetitions spec")
		end
			(* Lex % operators *)
		| #"\\" => onechar(escaped())
			(* start quoted string *)
		| #"\"" => (inquote := true; makeTok())
			(* anything else *)
		| ch => onechar(ch)
	in NextTok := makeTok()
	end
        | 2 => NextTok :=
               (case skipws() of
                  #"(" =>
                  let
                    fun loop_to_end (backslash, x) =
                      let
                        val c    = getch (! LexBuf)
                        val notb = not backslash
                        val nstr = c :: x
                      in
                        case c of
                          #"\"" => if notb then nstr
                                   else loop_to_end (false, nstr)
                        | _ => loop_to_end (c = #"\\" andalso notb, nstr)
                      end
                    fun GetAct (lpct, x) =
                      let
                        val c    = getch (! LexBuf)
                        val nstr = c :: x
                      in
                        case c of
                          #"\"" => GetAct (lpct, loop_to_end (false, nstr))
                        | #"(" => GetAct (lpct + 1, nstr)
                        | #")" => if lpct = 0 then implode (rev x)
                                  else GetAct(lpct - 1, nstr)
                        | _ => GetAct(lpct, nstr)
                      end
                  in
                    ACTION (GetAct (0,nil))
                  end
                | #";" => SEMI
                | c => (prSynErr ("invalid character " ^ String.str c)))
        | _ => raise LexError
end
handle eof => NextTok := EOF ;

fun GetTok (_:unit) : token =
	let val t = !NextTok in AdvanceTok(); t
	end;
val SymTab = ref (create String.<=) : (string,exp) dictionary ref

fun GetExp () : exp =

	let val rec optional = fn e => ALT(EPS,e)

	    and lookup' = fn name =>
		lookup(!SymTab) name
		handle LOOKUP => prErr ("bad regular expression name: "^
					    name)

	and newline = fn () => let val c = Array.array(!CharSetSize,false) in
		Array.update(c,10,true); c
		end

	and endline = fn e => trail(e,CLASS(newline(),0))

	and trail = fn (e1,e2) => CAT(CAT(e1,TRAIL(0)),e2)

	and closure1 = fn e => CAT(e,CLOSURE(e))

	and repeat = fn (min,max,e) => let val rec rep = fn
		  (0,0) => EPS
		| (0,1) => ALT(e,EPS)
		| (0,i) => CAT(rep(0,1),rep(0,i-1))
		| (i,j) => CAT(e,rep(i-1,j-1))
	in rep(min,max)
	end

	and exp0 = fn () => case GetTok()
	       of CHARS(c) => exp1(CLASS(c,0))
		| LP => let
		    val e = exp0()
		    in
		      case !NextTok
		       of RP => (AdvanceTok(); exp1(e))
			| _ => (prSynErr "missing ')'")
		    end
		| ID(name) => exp1(lookup' name)
		| _ => raise SyntaxError

	and exp1 = fn (e) => case !NextTok of
		  SEMI => e
		| ARROW => e
		| EOF => e
		| LP => exp2(e,exp0())
		| RP => e
		| t => (AdvanceTok(); case t of
			  QMARK => exp1(optional(e))
			| STAR => exp1(CLOSURE(e))
			| PLUS => exp1(closure1(e))
			| CHARS(c) => exp2(e,CLASS(c,0))
			| BAR => ALT(e,exp0())
			| DOLLAR => (UsesTrailingContext := true; endline(e))
			| SLASH => (UsesTrailingContext := true;
				    trail(e,exp0()))
			| REPS(i,j) => exp1(repeat(i,j,e))
			| ID(name) => exp2(e,lookup' name)
			| _ => raise SyntaxError)

	and exp2 = fn (e1,e2) => case !NextTok of
		  SEMI => CAT(e1,e2)
		| ARROW => CAT(e1,e2)
		| EOF => CAT(e1,e2)
		| LP => exp2(CAT(e1,e2),exp0())
		| RP => CAT(e1,e2)
		| t => (AdvanceTok(); case t of
		  	  QMARK => exp1(CAT(e1,optional(e2)))
			| STAR => exp1(CAT(e1,CLOSURE(e2)))
			| PLUS => exp1(CAT(e1,closure1(e2)))
			| CHARS(c) => exp2(CAT(e1,e2),CLASS(c,0))
			| BAR => ALT(CAT(e1,e2),exp0())
			| DOLLAR => (UsesTrailingContext := true;
				     endline(CAT(e1,e2)))
			| SLASH => (UsesTrailingContext := true;
				    trail(CAT(e1,e2),exp0()))
			| REPS(i,j) => exp1(CAT(e1,repeat(i,j,e2)))
			| ID(name) => exp2(CAT(e1,e2),lookup' name)
			| _ => raise SyntaxError)
in exp0()
end;
val StateTab = ref(create(String.<=)) : (string,int) dictionary ref

val StateNum = ref 0;

fun GetStates () : int list =

   let fun add nil sl = sl
  	  | add (x::y) sl = add y (union ([lookup (!StateTab)(x)
					   handle LOOKUP =>
					      prErr ("bad state name: "^x)
					  ],sl))

	fun addall i sl =
	    if i <= !StateNum then addall (i+2) (union ([i],sl))
	    else sl

	fun incall (x::y) = (x+1)::incall y
	  | incall nil = nil

	fun addincs nil = nil
  	  | addincs (x::y) = x::(x+1)::addincs y

	val state_list =
	   case !NextTok of
	     STATE s => (AdvanceTok(); LexState := 1; add s nil)
	     | _ => addall 1 nil

      in case !NextTok
	   of CARAT => (LexState := 1; AdvanceTok(); UsesPrevNewLine := true;
			incall state_list)
	    | _ => addincs state_list
      end

val LeafNum = ref ~1;

fun renum(e : exp) : exp =
	let val rec label = fn
	  EPS => EPS
	| CLASS(x,_) => CLASS(x,++LeafNum)
	| CLOSURE(e) => CLOSURE(label(e))
	| ALT(e1,e2) => ALT(label(e1),label(e2))
	| CAT(e1,e2) => CAT(label(e1),label(e2))
	| TRAIL(i) => TRAIL(++LeafNum)
	| END(i) => END(++LeafNum)
in label(e)
end;

exception ParseError;

fun parse() : (string * (int list * exp) list * ((string,string) dictionary)) = let
	fun isSEMI SEMI = true | isSEMI _ = false
	val Accept = ref (create String.<=) : (string,string) dictionary ref
	val rec ParseRtns = fn l => case getch(!LexBuf) of
		  #"%" => let val c = getch(!LexBuf) in
		    	   if c = #"%" then (implode (rev l))
			   else ParseRtns(c :: #"%" :: l)
			end
		| c => ParseRtns(c::l)
	and ParseDefs = fn () =>
		(LexState:=0; AdvanceTok(); case !NextTok of
		  LEXMARK => ()
		| LEXSTATES =>
		   let fun f () = (case !NextTok of (ID i) =>
				    (StateTab := enter(!StateTab)(i,++StateNum);
				     ++StateNum; AdvanceTok(); f())
					| _ => ())
		   in AdvanceTok(); f ();
		      if isSEMI (!NextTok) then ParseDefs() else
			(prSynErr "expected ';'")
		   end
		| ID x => (
		    LexState:=1; AdvanceTok();
		    case GetTok()
		     of ASSIGN => (
			  SymTab := enter(!SymTab)(x,GetExp());
			  if isSEMI (!NextTok) then ParseDefs()
			  else (prSynErr "expected ';'"))
		      | _ => raise SyntaxError)
		| REJECT => (HaveReject := true; ParseDefs())
		| COUNT => (CountNewLines := true; ParseDefs())
		| FULLCHARSET => (CharSetSize := 256; ParseDefs())
		| HEADER => (LexState := 2; AdvanceTok();
			     case GetTok()
			     of ACTION s =>
				if (!StrDecl) then
				   (prErr "cannot have both %structure and %header \
				    \declarations")
				else if (!HeaderDecl) then
				   (prErr "duplicate %header declarations")
				else
				    (HeaderCode := s; LexState := 0;
				     HeaderDecl := true; ParseDefs())
				| _ => raise SyntaxError)
	        | POSARG => (PosArg := true; ParseDefs())
                | ARG => (LexState := 2; AdvanceTok();
			     case GetTok()
			     of ACTION s =>
				(case !ArgCode
				   of SOME _ => prErr "duplicate %arg declarations"
				    | NONE => ArgCode := SOME s;
				 LexState := 0;
				 ParseDefs())
				| _ => raise SyntaxError)
		| STRUCT => (AdvanceTok();
			    case !NextTok of
			       (ID i) =>
			        if (!HeaderDecl) then
				   (prErr "cannot have both %structure and %header \
				    \declarations")
				else if (!StrDecl) then
				   (prErr "duplicate %structure declarations")
				else (StrName := i; StrDecl := true)
			         | _  => (prErr "expected ID");
				ParseDefs())
		| _ => raise SyntaxError)
	and ParseRules =
		fn rules => (LexState:=1; AdvanceTok(); case !NextTok of
		  EOF => rules
		| _ =>
		 let val s = GetStates()
		     val e = renum(CAT(GetExp(),END(0)))
		 in
		   case !NextTok
		    of ARROW => (LexState:=2; AdvanceTok();
			 case GetTok() of ACTION(act) =>
			   if isSEMI (!NextTok) then
			     (Accept:=enter(!Accept) (Int.toString (!LeafNum),act);
			      ParseRules((s,e)::rules))
			   else (prSynErr "expected ';'")
			 | _ => raise SyntaxError)
		     | _ => (prSynErr "expected '=>'")
		end)
in let val usercode = ParseRtns nil
   in (ParseDefs(); (usercode,ParseRules(nil),!Accept))
   end
end handle SyntaxError => (prSynErr "")

fun makebegin () : unit =
   let fun make nil = ()
	 | make ((x,n:int)::y)=(say "val "; say x; say " = " ;
				say "STARTSTATE ";
				say (Int.toString n); say ";\n"; make y)
   in say "\n(* start state definitions *)\n\n"; make(listofdict(!StateTab))
   end

structure RB : sig
    type tree
    type key
    val empty : tree
    val insert : key * tree -> tree
    val lookup : key * tree -> key
    exception notfound of key
  end =  struct
    structure Map = RedBlackMapFn (
	struct
	  type ord_key = int list
	  val compare = List.collate Int.compare
	end)
    type key = (int list * string)
    type tree = string Map.map
    val empty = Map.empty
    val insert = Map.insert'
    exception notfound of key
    fun lookup (arg as (key, _), t) = (case Map.find(t, key)
	   of SOME item => (key, item)
	    | NONE => raise notfound arg
	  (* end case *))
  end

fun maketable (fins:(int * (int list)) list,
	     tcs :(int * (int list)) list,
	     tcpairs: (int * int) list,
	     trans : (int*(int list)) list) : unit =

(* Fins = (state #, list of final leaves for the state) list
   tcs = (state #, list of trailing context leaves which begin in this state)
	 list
   tcpairs = (trailing context leaf, end leaf) list
   trans = (state #,list of transitions for state) list *)

   let datatype elem = N of int | T of int | D of int
       val count = ref 0
       val _ = (if length(trans)<256 then CharFormat := true
		 else CharFormat := false;
		 if !UsesTrailingContext then
    		     (say "\ndatatype yyfinstate = N of int | \
			   \ T of int | D of int\n")
		 else say "\ndatatype yyfinstate = N of int";
		 say "\ntype statedata = {fin : yyfinstate list, trans: ";
		 case !CharFormat of
		       true => say "string}"
		     | false => say "int Vector.vector}";
	         say "\n(* transition & final state table *)\nval tab = let\n";
		 case !CharFormat of
		       true => ()
		     | false =>
		       (say "fun decode s k =\n";
			say "  let val k' = k + k\n";
			say "      val hi = Char.ord(String.sub(s, k'))\n";
			say "      val lo = Char.ord(String.sub(s, k' + 1))\n";
			say "  in hi * 256 + lo end\n"))

      val newfins =
	let fun IsEndLeaf t =
	     let fun f ((l,e)::r) = if (e=t) then true else f r
		   | f nil = false in f tcpairs end

	 fun GetEndLeaf t =
	   let fun f ((tl,el)::r) = if (tl=t) then el else f r
		 | f _ = raise Match
	   in f tcpairs
	   end
	 fun GetTrConLeaves s =
	   let fun f ((s',l)::r) = if (s = s') then l else f r
	         | f nil = nil
	   in f tcs
	   end
	 fun sort_leaves s =
	   let fun insert (x:int) (a::b) =
		 if (x <= a) then x::(a::b)
		 else a::(insert x b)
		 | insert x nil = [x]
	   in List.foldr (fn (x,r) => insert x r) [] s
	   end
	 fun conv a = if (IsEndLeaf a) then (D a) else (N a)
	 fun merge (a::a',b::b') =
	   if (a <= b) then (conv a)::merge(a',b::b')
	   else (T b)::(merge(a::a',b'))
	   | merge (a::a',nil) = (conv a)::(merge (a',nil))
	   | merge (nil,b::b') = (T b)::(merge (b',nil))
	   | merge (nil,nil) = nil

	in map (fn (x,l) =>
	  rev (merge (l,
		sort_leaves (map (fn x => GetEndLeaf x) (GetTrConLeaves x)))))
		    fins
	end

	val rs =
	 let open RB
	     fun makeItems x =
	       let fun emit8(x, pos) =
		     let val s = StringCvt.padLeft #"0" 3 (Int.toString x)
		     in
		       case pos
			 of 16	=> (say "\\\n\\\\"; say s; 1)
			  | _	=> (say "\\"; say s; pos+1)
		     end
		   fun emit16(x, pos) =
		     let val hi8 = x div 256
			 val lo8 = x - hi8 * 256	(* x rem 256 *)
		     in
		       emit8(lo8, emit8(hi8, pos))
		     end
		   fun MakeString([], _, _) = ()
		     | MakeString(x::xs, emitter, pos) =
			MakeString(xs, emitter, emitter(x, pos))
	        in case !CharFormat of
		    true => (say " \n\""; MakeString(x,emit8,0); say "\"\n")
		  | false => (say (Int.toString(length x));
		     say ", \n\""; MakeString(x,emit16,0); say "\"\n")
	        end

	    fun makeEntry(nil,rs,t) = rev rs
	      | makeEntry(((l:int,x)::y),rs,t) =
	          let val name = (Int.toString l)
		  in let val (r,n) = lookup ((x,name),t)
		      in makeEntry(y,(n::rs),t)
		      end handle notfound _ =>
                        (count := !count+1;
                          say " ("; say name; say ",";
		          makeItems x; say "),\n";
		         makeEntry(y,(name::rs),(insert ((x,name),t))))
	   	  end

            val _ = say "val s = [ \n"
            val res =  makeEntry(trans,nil,empty)
            val _ =
              case !CharFormat
               of true => (say "(0, \"\")]\n"; say "fun f x = x \n")
                | false => (say "(0, 0, \"\")]\n";
                    say "fun f(n, i, x) = (n, Vector.tabulate(i, decode x)) \n")

            val _ = say "val s = List.map f (List.rev (tl (List.rev s))) \n"
            val _ = say "exception LexHackingError \n"
            val _ = say "fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) \n"
            val _ = say "  | look ([], i) = raise LexHackingError\n"

 	 in res
	end

	fun makeTable args =
	    let fun makeOne (a, b) =
		    let fun makeItems [] = ()
			  | makeItems [x] = ()
			  | makeItems (hd :: tl) = makeItems tl
		    in makeItems b
		    end
		fun mt ([], []) = ()
		  | mt ([a], [b]) = makeOne (a, b)
		  | mt (a :: a', b :: b') = mt (a', b')
		  | mt _ = ()
	    in mt args
	    end

  in ()
end


end (* structure LEXGEN *)
