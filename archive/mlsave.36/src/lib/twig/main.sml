(* May 1989, Jussi Rintanen, Helsinki University of Technology *)

functor MAKEmain (structure Parser : PARSER
		  and Automata : AUTOMATA
		    sharing Automata.Parser = Parser)
		    : sig val main : string -> unit end =
  struct

    open Parser Automata

    val int2str : int -> string = makestring

    val accum = revfold

    fun count (a,nil) = 0
      | count (a,h::t) = (if a = h then 1 else 0) + count (a,t)

    fun member p = (count p) <> 0

(* Output *)

    fun labellist (Leaf (Label l)) = [l]
      | labellist (Tree (_,cs)) = accum (fn (c,ac) => ac@(labellist c)) cs []
      | labellist _ = []

    fun index (passed,nil) = []
      | index (passed,h::t) =
	(if member (h,passed) orelse member (h,t)
	   then h^(int2str (count (h,passed) + 1))
	 else h):: (index (h::passed,t))

    fun emitlist' (e,nil) = ()
      | emitlist' (e,[a]) = e a
      | emitlist' (e,h::t) = (e h;e",";emitlist' (e,t))

    fun emitlist (e,l) = (e"[";emitlist'(e,l);e"]")

    fun emitcost (emit, rules, defaultcost) =
      (emit "fun execute_cost (n:rule, ir, children) =\nlet open User\n";
       case defaultcost of
	 NoCost => ()
       | Cost code => (emit "val DC = ( "; app emit code; emit ") (map cost children)\n");
       emit "val ABORT = (fn () => raise MatchAbort)\n in\ncase n of\n  ";
       app (fn (Rule(n,_,_,p,Cost ss,_)) =>
	    (emit (int2str n);
	     emit " => (case map cost children of ";
	     emitlist (emit, index ([],labellist p));
	     emit " => (";
	     app emit ss;
	     emit ") | _ => raise InternalError \"S4\")\n  | ")
              | (Rule(n,_,_,_,NoCost,_)) => (emit (int2str n);emit" => DC\n  | "))
		   rules;
       emit "_ => raise InternalError \"S4.3.\"\nend\n\n")

    fun tokens (passed,h::t) =
      let val suffix =
	if member (h,passed) orelse member (h,t)
	  then (int2str (count (h,passed)+1))
	else ""
      in
	("_"^h^" "^h^suffix,
	 "execute (nth(children,"^(int2str (length passed))^"))"
	 )::(tokens (h::passed,t))
      end
    | tokens _ = []

    fun reftokens ls = tokens ([],ls)

    fun DOtokens (passed,h::t) =
      let val suffix =
	if member (h,passed) orelse member (h,t)
	  then (int2str (count (h,passed)+1))
	else ""
      in
	("DO"^h^suffix,
(*	 "let val C = nth(children,"^(int2str (length passed))^")in fn () => let val _"^h^" V = execute C in V end end" *)
	 "let val C = nth(children,"^(int2str (length passed))^")in fn () => case execute C of _"^h^" V => V | _ => raise InternalError \"S4.3\" end"
	 )::(DOtokens (h::passed,t))
      end
      | DOtokens _ = []

    fun DOreftokens ls = DOtokens ([],ls)

    fun emittuple'(e,nil) = ()
      | emittuple'(e,[a]) = e a
      | emittuple'(e,h::t) = (e h;e",";emittuple'(e,t))
	
    fun emittuple (e,t) = (e"(";emittuple'(e,t);e")")
      
    fun emitval (e,l) =
      (emittuple (e,map (fn (_,a) => a) l);
       e" of ";
       emittuple (e,map (fn (a,_) => a) l))

    fun emitaction (emit, rules) =
      (emit "fun execute (Skeleton (n,_, ir, children)) =\n\
       \ let open User\n";
       emit "in\ncase n of\n";
       app (fn (Rule(n,t,Label l,p,_,Action ss)) =>
	    let val labels = labellist p
	      in
		emit (int2str n);
		emit " => ";
		  (case t of
		     Ordinary => emit ("_"^l)
		   | Topdown => emit ("_"^l)
		   | Rewrite => emit "__rewrite");
		     if labels <> nil
		       then (emit " ( case ";
			     (case t of
				Ordinary =>
				  emitval (emit,reftokens (labellist p))
			      | Topdown =>
				  emitval (emit,DOreftokens (labellist p))
			      | Rewrite => emitval (emit,[("_","()")]));
				emit " => ")
		     else ();
		     app emit ss;
		     if labels <> nil
		       then
			 case t of
			   Ordinary => emit " | _ => raise InternalError \"S5\" )"
			 | Topdown => emit" )"
			 | Rewrite => emit" )"
		     else ();
		     emit "\n  | "
	    end | _ => ())
	      rules;
	      emit "_ => raise Match\n";
		emit "end\n\n")

    fun emitrewrite (emit, rules) =
      (emit "fun rewriterule (r:rule) =\n\
       \ case r of\n";
       app (fn (Rule(n,Rewrite,_,_,_,_)) =>
	    (emit (int2str n); emit " => true |") | _ => ())
	       rules;
       emit "_ => false\n")

(* Symbol datatype declaration *)

    fun symbol2str (Label s) = "__"^s
      | symbol2str (Node (s,_)) = s

    fun emitsymbols (emit,symbols) =
      let val maxarity =
	accum
	(fn (Node (_,a),max) => if a > max then a else max | (_,a) => a )
	   symbols 0
      in
	emit "ARC of int";
	map
	(fn s => emit (" | "^(symbol2str s)))
	   symbols;
	emit "\n"
      end

(* Unit match trees *)
	    
    fun leafs (Leaf _) = 1
      | leafs (Tree(_,cs)) = fold (op +) (map leafs cs) 0

    fun emitmatches (_,nil) = ()
      | emitmatches (emit, (Rule(n0,_,_,p,_,_)::rules)) =
      (emit "val matchcounts = [\n";
      let val n =
	(emit ("(" ^ (int2str n0) ^ "," ^ (int2str (leafs p)) ^ ")");
	 accum
	 (fn (Rule(n,_,_,p,_,_),m) =>
	    (emit (",\n(" ^ (int2str n) ^ "," ^ (int2str (leafs p)) ^ ")");
	    if n > m then n else m))
	      rules n0)
      in
	 emit "]\nval matchtable = let val a = array(";
	 emit (int2str (n+1));
	 emit ",0) in ((app (fn(r,m)=>update (a,r,m)) matchcounts); a) end\n\n\
	   \fun matches r = matchtable sub r\n\n"
      end)

(* Unit rules *)

    datatype matchtree = Chain of int * symbol * matchtree list

    fun closurize unitrules =
      let val rec member =
	(fn (a,nil) => false | (a,h::t) => if a=h then true else member (a,t))
	  val initials =
	    accum
	    (fn ((_,_,i),a) => if member (i,a) then a else i::a)
	       unitrules nil
	  fun build_unittree (nt,visited) =
	    accum (fn ((r,n,p),ac) =>
		   if p = nt andalso not (member (n,visited))
		     then Chain(r,n,build_unittree(n,n::visited))::ac
		   else ac) unitrules nil
      in
	map (fn i => (i,build_unittree (i,[i]))) initials
      end
    
    fun emitmatchtreelist (emit,nil) = ()
      | emitmatchtreelist (emit,[m]) = emitmatchtree (emit, m)
      | emitmatchtreelist (emit,(h::t)) = (emitmatchtree (emit,h); emit ","; emitmatchtreelist (emit,t))
    and emitmatchtree (emit, Chain (i, j, ml)) =
      (emit "Chain (";
       emit (int2str i);
       emit ",";
       emit (symbol2str j);
       emit ",[";
       emitmatchtreelist (emit,ml);
       emit "])")

    fun emitunitrules (emit, matchtrees,symbols) =
      (emit "datatype matchtree = Chain of int * symbol * matchtree list\n";
       emit "fun unitmatches nt = (case nt of\n";
       app (fn (s,ms) => (emit (symbol2str s);
			  emit " => [";
			  emitmatchtreelist (emit,ms);
			  emit "]\n  | "))
	    matchtrees;
       emit "_ => [])\n\n")

    fun partition' (nil,u,n) = (u,n)
      | partition' (Rule(r,_,l,Leaf s,_,_)::t,u,n) = partition' (t,(r,l,s)::u,n)
      | partition' (r::t,u,n) = partition' (t,u,r::n)

    fun partition l = partition' (l,nil,nil)

(* Main *)

    fun fatal s = output std_out ("Fatal error: "^s^"\n")

    fun main inputfilename =
      let
	val outputfilename = inputfilename ^ ".sml"
	val (inputf, outputf) = (open_in inputfilename,open_out outputfilename)
	val emit = output outputf
	val (inserts, rules, dcost, structuren, label_type, symbols) = specification inputf
	val (unitrules, otherrules) = partition rules
      in
	emit "structure ";
	emit structuren;
	emit " =\n\
	\struct\n\
	\  structure User =\n\
	\  struct\n\
	\datatype symbol =\n";
	emitsymbols (emit,symbols);
	app (fn Prologue ss => (app emit ss) | Insert ss => (app emit ss)) inserts;
	emit "\n\ndatatype result = __rewrite of tree | ";
	emit label_type;
	emit "\n end\n\n\
	\structure Specification =\n\
	\  struct\n\
	\structure User = User\n\nopen User\n\
	\type rule = int\n\
	\datatype skeletal = Skeleton of (rule * cost * tree * skeletal list)\n\
	\exception MatchAbort\n\
	\fun cost (Skeleton(_,c,_,_)) = c\n\
	\exception InternalError of string\n\n\
	\fun get_subtree (n,t) = nth (get_subtrees t,n-1)\n\n";
	emitcost (emit,rules,dcost);
	emitaction (emit,rules);
	emitmatches (emit, rules);
	build_automaton (outputf,symbols,otherrules);
	emitunitrules(emit, closurize unitrules,symbols);
	emitrewrite (emit,rules);
	emit "fun getreplacement (__rewrite t) = t | getreplacement _ = raise InternalError \"problem with rewrite 996\"\n\
	\  end\n\
        \structure Internal = MAKEtreeprocessor(Specification)\n\
	\exception NoCover = Internal.NoCover\n\
	\exception InternalError = Internal.InternalError\n\
	\val translate = Internal.translate\n\
        \end;\n";
	close_in inputf;
	close_out outputf
      end
    handle ParserError s => fatal s
	 | AutomatonError s => fatal s
	   
  end;

structure Parser = MAKEparser(structure Symboltable = Symboltable
			      and Lexer = Lexer);

structure Main = MAKEmain(structure Parser = Parser
			  and Automata = MAKEautomata(structure Parser = Parser));
