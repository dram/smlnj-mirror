(* ML-Yacc parser generator version 1.0
Copyright (c) 1989 by Andrew W. Appel, David R. Tarditi

This software comes with ABSOLUTELY NO WARRANTY.
This software is subject only to the GNU GENERAL PUBLIC LICENSE
(in the file "LICENSE", distributed with this software, and available
from the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139).
You may copy and distribute this software; see the LICENSE
for details and restrictions.
*)
signature BASESET =
   sig
      type elem
      type base_set
      exception Select_arb
      val empty: base_set
          and insert: elem ->  base_set -> base_set
	  and exists: elem -> base_set -> bool
	  and find: elem -> base_set -> elem option
	  and setfold: ((elem * 'b) -> 'b) -> base_set -> 'b -> 'b
	  and revsetfold: ((elem * 'b) -> 'b) -> base_set -> 'b -> 'b
	  and elem_gt: (elem * elem -> bool)
	  and elem_eq: (elem * elem -> bool)
          and select_arb: base_set -> elem
	  and set_eq: (base_set * base_set) -> bool
	  and set_gt: (base_set * base_set) -> bool
	  and app : (elem -> 'a) -> base_set -> unit
   end;

signature FULLSET =
   sig
      type set
      type elem
      exception Select_arb
      val card: set -> int
	  and app: (elem -> 'b) -> set -> unit
	  and set_eq: (set * set) -> bool
	  and set_gt: (set * set) -> bool
	  and find : elem -> set ->  elem option
	  and exists: elem -> set -> bool
          and contained: elem -> (set -> bool)
          and difference: set * set -> set
          and elem_eq: (elem * elem -> bool)
	  and elem_gt : (elem * elem -> bool)
          and empty: set
          and insert: elem -> set -> set
          and is_empty: set -> bool
          and make_list: set -> elem list
          and make_set: (elem list -> set)
          and remove: (elem * set) -> set
          and same_set: set * set -> bool
          and partition: (elem -> bool) -> (set -> set * set)
	  and revsetfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
	  and setfold: ((elem * 'b) -> 'b) -> set -> 'b -> 'b
          and select_arb: set -> elem
          and singleton: (elem -> set)
          and union: set * set -> set
          and closure: set * (elem -> set) -> set
   end;

signature GRAPH =
   sig

      type node
      type edge
      type graph

      val null_graph: graph
      val nodes_of: graph -> node list
      val num_nodes: graph -> int
      val join: graph * node * edge * node -> graph

      (* drt(9/12/88) - new functions added because join is inefficient,
         if one or both of the nodes already exist *)

      val add_edge : graph * node * edge * node -> graph
      val add_node : graph * node -> graph

      (* set of graph edges: records of {From:node, Edge:edge, To:node} *)


      structure EdgeSet : FULLSET
    	 
      val edges: graph * node -> EdgeSet.set  (* all edges from a node *)
      val all_edges : graph -> EdgeSet.set

      val find_node : graph * node -> node option

end;

signature BUSY =
   sig
      val dot: unit -> unit
      val star: unit -> unit
      val print: string -> unit
      val println: string -> unit

      val withSpace: ('a -> unit) -> ('a -> unit)
      val withNewline: ('a -> unit) -> ('a -> unit)

      val withDot: ('a -> 'b) -> 'a -> 'b

      val sendto_list : unit -> unit
      val sendto_file : outstream -> unit

      val get_list : unit -> string list
   end;

signature MEMO =
   sig
      type Arg

      val memo_fn: ((Arg * Arg) -> bool) -> ((Arg -> '2a) -> (Arg -> '2a))

      exception Enum_memo_fn

      val enum_memo_fn: ((Arg -> int) * int) -> ((Arg -> '2a) -> (Arg -> '2a))

      exception Catalog

      val catalog: {tag: Arg -> 'tag,
                    ordOfTag: 'tag -> int,
                    items: Arg list
                   } -> ('tag -> Arg list)
   end;
signature V2_LR_GRAMMAR =
   sig
      datatype Terminal = T of int
      and Nonterminal = NT of int

      datatype Symbol = TERM of Terminal
                      | NONTERM of Nonterminal

      datatype Attribute = ATTRIB of {lhs: Nonterminal,
                                      rhsLength: int,
                                      num : int
                                     }

      datatype Rule = RULE of {lhs: Nonterminal,
                               rhs: Symbol list,
                               attribute: Attribute,
                               precedence: int option
                              }

      val termHash: Terminal -> int
      val nontermHash: Nonterminal -> int

      val eqTerminal: Terminal * Terminal -> bool
      val eqNonterminal: Nonterminal * Nonterminal -> bool
	
      val gtTerminal: Terminal * Terminal -> bool
      val gtNonterminal: Nonterminal * Nonterminal -> bool
   end;
signature V2_LSET =
   sig
      structure G : V2_LR_GRAMMAR
      type Lookahead 
      val emptylookahead : Lookahead

      (* Sets the bool ref to true if lookahead arg 1
	 set - lookahead arg 2 set <> null, i.e. if merging the 2 lookahead
	 sets adds something to lookahead arg 1 set 
      *)

      val mergelookahead : ((Lookahead * Lookahead) * bool ref) -> Lookahead

       (* returns true if any terminal in the terminal list is not in
	  was not already in the lookahead set
	*)

      val addterms: Lookahead * G.Terminal list -> Lookahead
      val makelookaheadlist : Lookahead -> G.Terminal list
   end

signature V2_LR_UTILS =
   sig
      structure G: V2_LR_GRAMMAR
      structure CoreSet : FULLSET
      structure Lset : V2_LSET

      datatype Core = CORE of { I : {coreLHS: G.Nonterminal,
                  		     corePrecedence: int option,
				     coreAttribute: G.Attribute},
                               coreRHSbefore: G.Symbol list,
                               coreRHSafter: G.Symbol list,
			       prop: bool ref,
			       lookaheads : Lset. Lookahead ref
                              }

      sharing type CoreSet.elem = Core

      val eqSymbol: G.Symbol * G.Symbol -> bool
      val gtSymbol: G.Symbol * G.Symbol -> bool

      val eqCore: Core * Core -> bool
      val gtCore: Core * Core -> bool

      val mkshowSymbol: {showTerminalClass : G.Terminal -> string,
			 showNonterminal : G.Nonterminal -> string } ->
			 G.Symbol -> string

      val printCore: {showSymbol : G.Symbol -> string,
		      showNonterminal : G.Nonterminal -> string,
		      showTerminalClass : G.Terminal -> string} -> Core -> unit
      val printCoreSet: {printCore : Core -> unit} -> CoreSet.set -> unit

      val mkselectCores: {rules : G.Rule list,
			  numNonterminals : int }
				  -> G.Nonterminal -> CoreSet.set
      val mkcoreClosure: {rules : G.Rule list,
			  numNonterminals : int,
			  selectCores : G.Nonterminal -> CoreSet.set
			 } -> CoreSet.set -> CoreSet.set

      val immediateSymbols: CoreSet.set -> G.Symbol list
      val copyCoreSet : CoreSet.set -> CoreSet.set

      type true
      val prop_f_to_c_a :
	 {first_string : G.Symbol list -> G.Terminal list,
	  selectCores : G.Nonterminal -> CoreSet.set}  ->
			  (CoreSet.set -> unit)
      val propagate_l_to_c_a :
	{selectCores : G.Nonterminal -> CoreSet.set } ->
		(CoreSet.set * bool ref) -> unit
      val propagate_l_to_g_i : (Core -> Core) ->
			        (((CoreSet.set * G.Symbol * CoreSet.set) *
				   bool ref) -> unit)
      val set_prop : (G.Symbol list -> bool) -> (CoreSet.set -> unit)
      val hardwire_eof : (CoreSet.set * G.Terminal * G.Nonterminal) -> unit
   end;

signature V2_LR_GRAPH =
   sig
      structure G: V2_LR_GRAMMAR
      structure Lr_Graph : GRAPH
      val mkGraph : {rules : G.Rule list, verbose : bool,
		     eof : G.Terminal, start : G.Nonterminal,
		     termPrecedence : G.Terminal -> int option,
		     showTerminalClass : G.Terminal -> string,
		     showTerminalValue : G.Terminal -> string,
		     showNonterminal : G.Nonterminal -> string,
		     showAttribute : G.Attribute -> string,
		     numTerminals : int,
		     numNonterminals : int
		    } -> Lr_Graph.graph
   end;

signature V2_LOOKAHEAD =
   sig
      structure G: V2_LR_GRAMMAR
      structure Utils : V2_LR_UTILS
      val mkfunctions : {numNonterminals:int,
			 selectCores : G.Nonterminal -> Utils.CoreSet.set,
			 showNonterminal : G.Nonterminal -> string,
			 showTerminalClass : G.Terminal -> string
			 } -> {nullable_string : G.Symbol list -> bool,
			       first_string : G.Symbol list -> G.Terminal list}
   end;
signature V2_LR_TABLE =
   sig
      structure G: V2_LR_GRAMMAR
      val mktable :
	 outstream ->
		    {rules : G.Rule list, verbose : bool,
		     eof : G.Terminal, start : G.Nonterminal,
		     termPrecedence : G.Terminal -> int option,
		     showTerminalClass : G.Terminal -> string,
		     showTerminalValue : G.Terminal -> string,
		     showNonterminal : G.Nonterminal -> string,
		     showAttribute : G.Attribute -> string,
		     numTerminals : int,
		     numNonterminals : int
		     } -> unit
   end;
structure Busy: BUSY =
   struct
      val len = ref 0
      val file = ref std_out		(* file to send output *)
      val list = ref (nil : string list)  (* or list to save output on *)
      val to_file = ref true		(* true = to file, false = to_list *)
      
      val sendto_list = fn () => (to_file := false; list := nil)
      val sendto_file = fn f => (file := f; to_file := true)
      val get_list = fn () => (!list)

      val dots = ref false

      val P = fn x => if (!to_file) then output (!file) x
		      else list := x :: (!list)

      fun sym s = (if !len = 75 then (P "\n"; len := 0) else ();
                   flush_out (!file);
                   P s;
                   len := !len + 1;
                   dots := true
                  )

      fun dot() = sym "."
      fun star() = sym "*"

      fun print s = (if !dots then (P "\n"; len := 0) else ();
                     dots := false;
                     P s;
                     len := !len + size s
                    );

      fun println s = (print s; P "\n"; len := 0)

      fun withSpace pr = fn x => (pr x; print " ")
      fun withNewline pr = fn x => (pr x; println " ")

      fun withDot f a = f a before dot()
   end;

functor Memo(type Arg): MEMO =
   struct
      type Arg = Arg
      type 'a relation = 'a * 'a -> bool

      fun memo_fn (Eq: Arg relation) (F: Arg -> '2a) =
         let val MemoSet = (ref nil): (Arg * '2a) list ref
             fun MemoCall(Arg, (X, Y) :: XYRest, Eq) =
                    if Eq(Arg, X) then Y else MemoCall(Arg, XYRest, Eq)   |
                 MemoCall(Arg, nil, _) =
                    let val Y = F(Arg)
                    in  (MemoSet := (Arg, Y) :: !MemoSet; Y)
                    end

         in  fn X => MemoCall(X, !MemoSet, Eq)
         end

      exception Enum_memo_fn
      fun enum_memo_fn (Enum: Arg -> int, Max: int) (F: Arg -> '2a) =
         let val MemoArray = array(Max, NONE): '2a option array
         in  fn x =>
               let val n = Enum(x)
               in  case MemoArray sub n of
                      NONE => let val y = F(x)
                              in  update(MemoArray, n, SOME(y)); y
                              end   |
                      SOME(y) => y
               end
               handle Subscript => raise Enum_memo_fn
         end

	(* catalog: given a function Tag for getting the selector tag
	   from any object, bundle the objects to give an efficient selector
	   function *)

      exception Catalog
      fun catalog{tag, ordOfTag, items}: ('tag -> Arg list) =
        let val OrdOfThing = ordOfTag o tag

             fun MaxOrdOfTag(Item :: IRest, Result): int =
                    (Busy.dot();
                     let val ThisOrd = OrdOfThing(Item)
                     in  if ThisOrd > Result then MaxOrdOfTag(IRest, ThisOrd)
                                             else MaxOrdOfTag(IRest, Result)
                     end
                    )   |
                 MaxOrdOfTag(nil, Result) = Result

             val MaxOrdOfTags =
                (Busy.print "MaxOrdOfTags"; MaxOrdOfTag(items, 0))

             val TheCatalog =
                array(MaxOrdOfTags + 1, NONE): Arg list option array

             fun InsertItem(Item) =
                let val ord = OrdOfThing(Item)
                in  (case TheCatalog sub ord of
                        SOME(L) =>
                           update(TheCatalog, ord, SOME(Item :: L))   |

                        NONE =>
                           update(TheCatalog, ord, SOME [Item])
                    ) before Busy.dot()
                end

             val _ = (Busy.print "catalog"; map InsertItem items)

         in  fn Tag => (case TheCatalog sub ordOfTag(Tag) of
                           SOME(L) => L   |
                           NONE => raise Catalog
                       ) handle Subscript => raise Catalog
         end
   end;

functor BaseSet(B : sig type elem
		  	val gt : elem * elem -> bool
			val eq : elem * elem -> bool
		    end ) : BASESET =

struct

 type elem = B.elem
 val elem_gt = B.gt
 val elem_eq = B.eq 

 datatype Color = RED | BLACK

 datatype base_set = EMPTY | TREE of (B.elem * Color * base_set * base_set)
 exception Select_arb
 val empty = EMPTY

 fun insert key t =
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
   
 fun exists key t =
  let fun look EMPTY = false
	| look (TREE(k,_,l,r)) =
		if elem_gt(k,key) then look l
		else if elem_gt(key,k) then look r
		else true
   in look t
   end

 fun find key t =
  let fun look EMPTY = NONE
	| look (TREE(k,_,l,r)) =
		if elem_gt(k,key) then look l
		else if elem_gt(key,k) then look r
		else SOME k
   in look t
  end

  fun revsetfold f t start =
     let fun scan (EMPTY,value) = value
	   | scan (TREE(k,_,l,r),value) = scan(r,f(k,scan(l,value)))
     in scan(t,start)
     end

   fun setfold f t start =
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
end

functor FullSet (B : sig type elem
			 val eq : (elem*elem) -> bool
		 	 val gt : (elem*elem) -> bool
		     end
		) : FULLSET =
   struct
      structure C = BaseSet(B)
      open C
      type set = base_set

      fun is_empty(S) = (let val (_) = select_arb(S) in false end
                         handle Select_arb => true)

      fun make_list(S) = setfold (fn (a,r) => a::r) S nil

      val contained = exists

      fun make_set l =
	  List.fold (fn (a,NewSet) => insert a NewSet) l empty

      fun partition F S = setfold (fn (a,(Yes,No)) =>
				if F(a) then (insert a Yes,No)
				else (Yes,insert a No)) 
			     S (empty,empty)

      fun remove(X, XSet) =
             let val (YSet, _) =
                        partition (fn a => not (elem_eq (X, a))) XSet
             in  YSet
             end

      fun difference(Xs, Ys) =
	   setfold (fn (a,Xs') => if exists a Ys then Xs' else insert a Xs') Xs 
		empty

      fun singleton X = insert X empty

      fun card(S) = setfold (fn (a,count) => count+1) S 0

      val same_set = set_eq

      fun union(Xs,Ys)= setfold (fn (a,Xs) => insert a Xs) Ys Xs

      local
	    fun closure'(from, f, result) =
	      if is_empty from then result
	      else
		let val (more,result) =
			setfold (fn (a,(more',result')) =>
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
   end;

functor Graph (B :
		sig
		  type node
		  type edge
		  val eq_node : node * node -> bool
		  val gt_node : node * node -> bool
		  val eq_edge : edge * edge -> bool
		  val gt_edge : edge * edge -> bool
		end
		) : GRAPH =
   struct

 	open B

	type graph_edge = {From : node, Edge : edge, To :node }

	fun EqEdge ({From=F1,Edge=E1, To = T1},{From=F2,Edge=E2,To=T2}) =
	  eq_node (F1,F2) andalso eq_node (T1,T2) andalso eq_edge(E1,E2)

	fun GtEdge ({From=F1,Edge=E1, To = T1},{From=F2,Edge=E2,To=T2}) =
	  gt_node(F1,F2) orelse (eq_node(F1,F2) andalso
		(gt_node(T1,T2) orelse (eq_node(T1,T2) andalso gt_edge(E1,E2))))

	structure N = FullSet (struct
				  type elem = node
				  val eq = eq_node
				  val gt = gt_node
				end 
				)

	structure EdgeSet =  FullSet (struct
				    type elem = graph_edge
				    val eq = EqEdge
				    val gt= GtEdge
				end
				)


	datatype graph = GRAPH of {TheNodes : N.set, TheEdges : EdgeSet.set} 

	fun find_node (GRAPH {TheNodes, ...},n) = N.find n TheNodes

        val null_graph  = GRAPH {TheNodes = N.empty, TheEdges = EdgeSet.empty}

        fun nodes_of(GRAPH{TheNodes, ...}) = N.make_list TheNodes

        fun num_nodes(GRAPH{TheNodes, ...}) = N.card(TheNodes)

        fun add_node (GRAPH {TheNodes, TheEdges} , n1) =
                   GRAPH{TheNodes=N.insert n1 TheNodes, TheEdges=TheEdges}

        fun add_edge (GRAPH {TheNodes, TheEdges}, n1, Edge, n2) =
                   let val NewEdges =
                            EdgeSet.insert {From=n1, Edge=Edge, To=n2} TheEdges
                   in GRAPH {TheNodes=TheNodes, TheEdges=NewEdges}
                   end

        fun join (GRAPH{TheNodes, TheEdges},n1, Edge, n2) =
                   GRAPH {TheNodes=N.insert n1 (N.insert n2 TheNodes),
		          TheEdges= EdgeSet.insert {From=n1,Edge=Edge,
					            To=n2} TheEdges }

        fun edges ( GRAPH {TheNodes, TheEdges }, n) = 
               let fun match {From, Edge,To} = eq_node (From, n)
	       in EdgeSet.setfold (fn (e,r) =>
		   if match e then EdgeSet.insert e r else r)
		 TheEdges EdgeSet.empty
	       end

        fun all_edges(GRAPH{TheEdges, ...}) =  TheEdges

   end;
functor V2_Lset (G : V2_LR_GRAMMAR) : V2_LSET = 
     struct
	open G
	structure G = G
	abstype Lookahead = LS of (G.Terminal*int) list 
	with val emptylookahead = LS nil

	     (* mergelookahead - return true as the second element of the
	 	pair if lookahead set 2 contains some elements lookahead
		set 1 does not
	     *)

	     fun mergelookahead (p,flag : bool ref) : Lookahead =
	        let fun f(nil,nil,r) = rev r
                      | f(a::a',nil,r) = f(a',nil,a::r)
                      | f(nil,b::b',r) = f(b',nil,b::r)
                      | f(x as ((a as (_,hash_a))::a'),
                          y as ((b as (_,hash_b))::b'),r) =
                             if ((hash_a : int ) <  hash_b) then
                                 f(a',y,a::r)
                             else if (hash_a>hash_b) then
                                 f(x,b',b::r)
                             else f(a',b',a::r)
		in case p of
                  (LS a,LS b) =>
                     let val r = f(a,b,nil)
                     in (if (length a) < (length r) then
			     flag := true
			 else (); LS r)
                     end
                end

	    (* addterms - adds terminals to a lookahead set *)

	    fun addterms(LS a,l) =
                let fun g(l,e) =
                      let val hash_e = termHash e
                           fun insert(nil,e) = [(e,hash_e)]
                           | insert(t as ((a' as (_,hash_a))::a),e) =
                              if (hash_a<hash_e) then a'::(insert(a,e))
                              else if (hash_a=hash_e) then t
                              else (e,hash_e)::a'::a
                      in insert(l,e)
                      end
                    val r=List.fold (fn (e,r) => g(r,e)) l a
                in LS r
                end

	    fun makelookaheadlist (LS a) = List.map (fn (t,_) => t) a
	end
    end;
		    
functor V2_LrUtils(structure G : V2_LR_GRAMMAR): V2_LR_UTILS =
   struct
  
      structure G = G
      structure Lset = V2_Lset(G)
      structure Memo = Memo(type Arg = G.Nonterminal)

      open G 

      val DEBUG = false
      val print = if DEBUG then output std_out
		  else Busy.print
      val println = if DEBUG then fn x => (print x; print "\n")
		    else Busy.println
      val withNewline = if DEBUG then (fn pr => fn x => (pr x; print "\n"))
		        else Busy.withNewline

     (* diagnostics *)
      val mkshowSymbol =
	fn {showTerminalClass = showTerminalClass : G.Terminal -> string,
	    showNonterminal = showNonterminal : G.Nonterminal -> string
	   } => fn (TERM t) => showTerminalClass t
		 | (NONTERM nt) => showNonterminal nt

      fun showSpaced f lst = fold (fn (x, y) => x ^ " " ^ y) (map f lst) ""

      datatype Core = CORE of { I : {coreLHS: G.Nonterminal,
                  		     corePrecedence: int option,
				     coreAttribute: G.Attribute},
                               coreRHSbefore: G.Symbol list,
                               coreRHSafter: G.Symbol list,
			       prop : bool ref,
			       lookaheads : Lset.Lookahead ref
                              } 

	
      val printCore =
	fn {showSymbol=showSymbol: G.Symbol -> string,
	    showNonterminal=showNonterminal : G.Nonterminal -> string,
	    showTerminalClass=showTerminalClass : G.Terminal -> string } =>
        fn (CORE c) =>
         let val {I = {coreLHS=coreLHS, ...}, coreRHSbefore, coreRHSafter,
		  prop, lookaheads} = c
         in (
            print(showNonterminal coreLHS ^ " : "
                       ^ showSpaced showSymbol (rev coreRHSbefore)
                       ^ "_"
                       ^ showSpaced showSymbol coreRHSafter
                      );
	  if DEBUG then
	    print(" lookaheads: " ^ showSpaced showTerminalClass
			(Lset.makelookaheadlist (!lookaheads))
		 )
	  else ()
	    )
         end

      fun gtSymbol(TERM t1, TERM t2) = gtTerminal(t1,t2)
        | gtSymbol(NONTERM nt1, NONTERM nt2) = gtNonterminal(nt1,nt2)
        | gtSymbol (TERM _,NONTERM _) = true
	| gtSymbol _ = false

      fun eqSymbol(TERM t1, TERM t2) = eqTerminal(t1,t2)
        | eqSymbol(NONTERM nt1, NONTERM nt2) =eqNonterminal(nt1,nt2)
        | eqSymbol _ = false

      fun eqCore(CORE {I = {coreAttribute = G.ATTRIB {num=n1,...}, ...},
			    coreRHSbefore=b1, ...},
		 CORE {I = {coreAttribute = G.ATTRIB {num=n2,...}, ...},
			    coreRHSbefore = b2, ...}
		) =
	 (n1 = n2) andalso (length b1 = length b2)

      fun gtCore(CORE {I = {coreAttribute = G.ATTRIB {num=n1,...}, ...},
			    coreRHSbefore=b1, ...},
		 CORE {I = {coreAttribute = G.ATTRIB {num=n2,...}, ...},
			    coreRHSbefore = b2, ...}
		) =
	  (n1>n2) orelse (n1=n2 andalso (length b1) > length (b2))

      structure CoreSet = FullSet (struct
					type elem = Core
					val gt = gtCore
					val eq = eqCore
				    end)

      fun filter f (x :: xs) = if f x then x :: filter f xs else filter f xs
        | filter f nil = nil

      fun buildCore(RULE{lhs, rhs, attribute, precedence}) =
         CORE({I = {coreLHS=lhs,
                    corePrecedence=precedence,
                    coreAttribute=attribute},
               coreRHSbefore=nil,
               coreRHSafter=rhs,
	       prop=ref false,
	       lookaheads=ref Lset.emptylookahead
              }
             )

      fun copyCore(a as (CORE c)) =
	 case c
	   of {I,coreRHSbefore, coreRHSafter, ...} =>
	     CORE({I=I,
		   coreRHSbefore=coreRHSbefore,
		   coreRHSafter=coreRHSafter,
		   prop=ref false,
		   lookaheads = ref Lset.emptylookahead
		   })

	val copyCoreSet  = fn s =>
	   CoreSet.setfold (fn (a,r)=> CoreSet.insert (copyCore a) r) s
	   CoreSet.empty

      val printCoreSet = fn {printCore = printCore : Core -> unit} =>
	   let val printCoreIndented = fn core =>
		(print "   "; withNewline printCore core)
	   in fn cores => CoreSet.app printCoreIndented cores
	   end

      val buildCores = fn rules => fn nt =>
         let val matchingProds =
                filter (fn RULE{lhs, ...} => eqNonterminal(lhs, nt))
                       rules

             val cores = CoreSet.make_set (map buildCore matchingProds)
         in
(*
            if DEBUG then
               (println("buildCores " ^ showNonterminal nt);
                printCoreSet cores
               )
            else
*) 		 ();

            cores
         end

      val mkselectCores =
	 fn {rules=rules : G.Rule list,numNonterminals=n : int} =>
	   (Memo.enum_memo_fn (G.nontermHash, n))
	    (buildCores rules)

      fun immediateSymbols cores =
	CoreSet.setfold
	   (fn (CORE {coreRHSafter,...},result) =>
               case coreRHSafter
                 of sym :: _ =>
                    if (exists (fn a => eqSymbol(a,sym)) result)
                     then result
                     else sym::result
                  | nil => result
           ) cores nil


        datatype true = T of Nonterminal | F

	(* Transitive closure : assumes array is an array of row arrays,
	   where entry a(i,j) is true if the ith item is connected to the
	   jth item, i.e. if the nonterminal with hash # i will cause all
	   the productions of the nonterminal with hash # j to be added to a
	   core set when
	   a production of the form 'a . B 'b, where hash B = i is
	   encountered *)

	val transitive_closure = fn  (a : true array array) =>
	    let val size = Array.length a
	        val i = ref 0
		val j = ref 0
		val k = ref 0
	    in while (!i < size) do
		(j := 0; while (!j < size) do

		     (* check if jth nonterm derives ith nonterm *)

		     (case ((a sub !j) sub !i) of
			(T _) =>

		     (* if so then connect jth nonterm w/ everything the
			ith derives
		     *)
			   (k := 0;
			    while (!k < size) do
				(case ((a sub !i) sub !k)
				   of (d as (T  x)) => update(a sub !j,!k,d)
				    | _ => ();
		if DEBUG then (
			print ("doing " ^ (makestring (!i)) ^ " " ^
				(makestring (!j)) ^ " " ^ (makestring (!k)) ^
				"\n") 
			      )
		else ();
			         k := !k +1
			        )
			    )
		      | F => (); j := !j+1
		     );
		 i := !i + 1
		 )
	    end 

	(* ComputeClosureAdditions - compute the nonterminals which must
	   be added when one nonterminal with a dot before it is added to
	   the core set.  Do this by constructing an array indexed by
	   nonterminal hash #.  Each a[i,j] entry is set to true when 
	   nonterminal i derives nonterminal j.  Then take the closure
	   of the array.  (Clearly, if A -> .B, and B -> .C, then A -> .C,
	   also. *)
	 	
	fun ComputeClosureAdditions(rules,max) =
	    let val a = array(max,array(max,F))	(* re: same subarray, always *)
		val _ = let val i = ref 0	(* re: make them different *)
			in while (!i<max) do	(* arrays *)
			   (update(a,!i,array(max,F)); i := !i + 1)
			end
	        val g = fn (RULE {lhs,rhs=(NONTERM n)::_,...}) =>
			   update(a sub (nontermHash lhs),nontermHash n,T n)
	      		 | _ => ()
	    in (app g rules; transitive_closure(a); a)
	    end

	val ComputeClosureAdditions =
(*
	  if DEBUG then
         	fn (rules,max) =>
		   let val a = ComputeClosureAdditions(rules,max)
		       val i = ref 0
		   in (while (!i < max) do
			(let val j = ref 0
			 in while (!j < max) do
			      (case ((a sub !i) sub !j) of
				 T i => print ((showNonterminal i) ^ " ")
				 | F => ();
			       j := !j+1)
			 end; println ""; i := !i+1
			); a)
		    end
	  else
*)  ComputeClosureAdditions

	(* ComputeCoreSetClosure:
	
	   Takes the closure of a core set.  This consists of adding for
	 each item with a nonterminal immediately after the dot all
	 productions for which the nonterminal is the lhs.  This process
	 is then repeated for the new production.

	   We can compute the set of all nonterminals whose productions
	must be added when a nonterminal is added using ComputeClosure-
	Additions.

	   We keep a boolean array indexed by nonterminal hash #'s.  We look
	at all the items in the core set.  When we find an item with a non-
	terminal immediately after the ., we set the corresponding element
	in the bool array to true,s ince we must add all the productions for
	that terminal.  We also set the entries for all the nonterminals
	which must be added when that nonterminal is added.  This uses the
	information from ComputeClosureAdditions.

	   Note that we can check the entry for the nonterminal with the dot
	before it before doing any of this.  If it is true, since the
	relation defined by ComputeClosureAdditions is transitive, all non-
	terminals which would need to be added for this nonterminal have
	already been added when it was added

	   We then take the boolean array, and add all productions for
	all nonterminals whose have been set to true.
	 *)

	fun ComputeCoreSetClosure(cores,a,max,selectCores) =
	  (* cores = core set, a = array from ComputeClosureAdditions,
	     max = # of nonterminals *)

	let val b = array(max,F)
	    fun g (CORE c) =
		  let val {coreRHSafter,...} = c
		  in case coreRHSafter of
		     ((NONTERM n) :: _) =>
			let val num = nontermHash n
		 	in case (b sub num) of
			   F => (update(b,num,T n);
				 let val i = ref 0
				 in while (!i < max) do
				  (case ((a sub num) sub !i)
				     of F => ()
				      | (m as (T _)) =>
					    update(b,!i,m);
				    i := !i + 1
				   )
				  end
				 )
			 | (T _ ) => ()
			end
		       | _ => ()
		   end
	     fun add_cores(cores) =
		 let fun g (i,cores) =
			case (b sub i) of
			  (T k) => CoreSet.union(cores,
					(selectCores k))
			 | F => cores
		     fun f(i,r) =
			 if (i < max) then f(i+1,g(i,r))
			 else r
		 in f(0,cores)
		 end
	in ((CoreSet.app g cores); add_cores cores)
	end

	val mkcoreClosure=
		fn {rules=rules:G.Rule list,
		    numNonterminals=numNonterminals:int,
		    selectCores=selectCores:G.Nonterminal->CoreSet.set
	 	   } => 
	let val a = ComputeClosureAdditions (rules,numNonterminals)
  	in fn cores =>
		 ComputeCoreSetClosure(cores,a,numNonterminals,selectCores)
	end

(* undefined now : printCoreSet, coreClosure

	 val coreClosure =
	    if DEBUG then
 	      fn cores =>
               (println "coreClosure of";
                printCoreSet cores;
                println "yields";
                let val cores' = coreClosure cores
                in
                   printCoreSet cores';
                   cores'
                end
               )
         else coreClosure
*)

	(* prop_f_to_c_a: Propagate firsts to closure additions.
	   
	   For each item in a core of the form A -> 'a . B 'b, where
	   B is a nonterminal, all B productions will contain first_string
	   'b in their lookahead set
	*)

	val prop_f_to_c_a : {first_string : G.Symbol list -> G.Terminal list,
			     selectCores : G.Nonterminal -> CoreSet.set} ->
				(CoreSet.set -> unit) =
	   fn {first_string = first_string : G.Symbol list -> G.Terminal list,
	       selectCores = selectCores : G.Nonterminal -> CoreSet.set}
	          => fn (cores : CoreSet.set) =>

		(* f :	check if a core has a . before a nonterminal.
			If so, propagate the first set of the string
			following the nonterminal to all the productions
			derived from the nonterminal
		*)

		let val f = fn (CORE {coreRHSafter=(NONTERM b)::r, ...}) =>
			      let val firsts = first_string r
				  exception ClosureError
				    val g = fn a =>
					case (CoreSet.find a cores) of
					  NONE => raise ClosureError
					| SOME (CORE {lookaheads=l1, ...}) =>
					   l1 := Lset.addterms(!l1,firsts)
				in CoreSet.app g (selectCores b)
				end
			      | _ => ()

		in (CoreSet.app f cores)
		end

	(* set_prop:  Prop is a boolean variable which is set to true
	   for items which propagate their lookaheads to items derived
	   from them.  These items have the form 'a .B 'c, where
	   'c derives epsilon.  The lookaheads propagate to those items
	   derived from B through the closure operation, and to the
	   item GOTO('a .B 'c, B).  This function sets prop for all items
	   in a core set.
	   
	*)

	val set_prop = 
	   fn (nullable_string : G.Symbol list -> bool) =>
	       let val g =  fn (CORE c) =>
			let val c' = c
			in case c'
			    of {coreRHSafter=(NONTERM _):: t,prop,...} =>	
			       if (nullable_string t) then prop := true
			       else ()
			     | _ => ()
			end
	       in CoreSet.app g  (* must be applied to a core set now *)
	       end

	val hardwire_eof = fn (cores,eof,start) =>
	   let val f = fn (CORE c) =>
		let val {I={coreLHS=lhs,...},lookaheads,...} = c
		in if (eqNonterminal(lhs,start)) then
		     lookaheads := Lset.addterms(!lookaheads,[eof])
		   else ()
	        end
	   in CoreSet.app f cores
	   end

	(* propagate_l_to_c_a: Propagate lookaheads to closure additions.
	  If an item in a core has the form A-> 'a .B 'b where B is a
	  nonterminal, and 'b derives epsilon, then all productions of
	  B get A's lookahead.  Returns true if any change to a
	  B productions lookahead occurs.
	*)

   val propagate_l_to_c_a = 
      fn {selectCores=selectCores : G.Nonterminal -> CoreSet.set} =>
        fn (cores: CoreSet.set, result : bool ref) =>
	    let val f = fn (CORE c) =>
		   case c

		     (* Check that a core item has the correct form *)

		     of {prop=ref true,coreRHSafter=NONTERM n::_,
			    lookaheads,...}=>
			let  exception PropClosureError

		      (* g: Takes an item whose lookahead depends on the
			    above item, and merges the above item's
			    lookahead into the item's lookahead
		       *)
			    exception PropClosureError

			    val g = fn c =>
			       case (CoreSet.find c cores) of
			          (SOME (CORE {lookaheads=l, ...})) =>
					l := Lset.mergelookahead
						((!l,!lookaheads),result)
				 | NONE => raise PropClosureError

			(* apply g to the items directly derived from 
			   NONTERM n.
			*)

			in CoreSet.app g (selectCores n)
			end
		      | _ => ()
	   in CoreSet.app f cores
	   end

	(* propagate_l_to_g_i: Propagate lookaheads to goto items *)

	val propagate_l_to_g_i =


	   fn (shiftCore : Core -> Core) =>

		(* takes an edge from n1 to n2 *)

		fn ((n1: CoreSet.set, e: G.Symbol, n2 : CoreSet.set),result) =>

	   let 

		(* match: merge lookaheads of an item into the items
		   which result from a shift
	        *)

	       val match=
		  fn (a as (CORE {coreRHSafter=b::_,lookaheads=l1,...})) =>
		     if eqSymbol(b,e) then
		        case (CoreSet.find (shiftCore a) n2) 
		            of SOME (CORE {lookaheads=l2,...}) =>
		                 l2 := Lset.mergelookahead((!l2,!l1),result)
		             | NONE => ()
		     else ()
		   | _ => ()
	  in (CoreSet.app match n1)
	  end
   end;

functor V2_Lookahead(structure G : V2_LR_GRAMMAR
                  structure Utils: V2_LR_UTILS
                  sharing Utils.G = G
                 ): V2_LOOKAHEAD =
   struct
      structure NontermMemo = Memo(type Arg = G.Nonterminal)

      structure G = G
      structure Utils = Utils
      open G Utils

     structure TermSet = FullSet(type elem = Terminal
				   val gt = gtTerminal
				   val eq = eqTerminal)

     structure NontermSet = FullSet(type elem = Nonterminal
				    val gt = gtNonterminal
				      val eq = eqNonterminal)

      val DEBUG = false

     (* build an enumerated memo-fn over nonterminals *)

    val mkfunctions = 
	 fn {numNonterminals,selectCores,showNonterminal,showTerminalClass} =>
let 
      val nontermMemo = NontermMemo.enum_memo_fn(nontermHash, numNonterminals)

     (* actual print functions (rather than string generators) *)

      val printTerminalClass = Busy.print o showTerminalClass
      val printNonterminal = Busy.print o showNonterminal

     (* The FIRST set construction functions *)

     (* nullable: true if nonterminal "nt" has some null rhs production *)
      fun nullable(nt, cores) =
	 CoreSet.setfold (fn (CORE c,rest) => 
	   let val {coreRHSafter, ...} = c
	   in rest orelse (null coreRHSafter)
	   end) cores false


     (* memo-ise "nullable", add diags *)
      fun prNullable(nt, b) =
         (Busy.println("Nullable " ^ showNonterminal nt ^ "? "
                       ^ case b of true => "YES" | false => "NO"
                      );
          b
         )

      val nullable =
         nontermMemo(if DEBUG then
                        fn nt => prNullable(nt, nullable(nt, selectCores nt))
                     else
                        fn nt => nullable(nt, selectCores nt)
                    )

(* nullable_string: check if a string of terminals and variables is nullable *)

      fun nullable_string (TERM t :: _ ) = false
        | nullable_string (NONTERM t :: r ) =
                        (if nullable t then (nullable_string r) else false)
        | nullable_string nil = true

     (* accumulate: look at the start of core right-hand-sides, looking past
	nullable nts, applying addObj to the visible symbols. *)
      fun accumulate(cores, empty, addObj) =
         let
            fun accumAlongSymbols(symbols, result) =
               case symbols
                 of (sym as NONTERM nt) :: rest =>
                       if nullable nt then
                          accumAlongSymbols(rest, addObj(sym, result))
                       else
                          addObj(sym, result)

                  | (sym as TERM _) :: _ => addObj(sym, result)

                  | nil => result

            fun accumAlongCores(cores, result) =
	      CoreSet.setfold (fn (CORE c,result) =>
		 let val {coreRHSafter, ...} = c
		 in accumAlongSymbols(coreRHSafter, result)
	 	 end) cores result
         in
            accumAlongCores(cores, empty)
         end


     (* first1: the FIRST set of a nonterminal in the grammar. Only looks
	at other terminals, but it *is* clever enough to move past nullable
	nonterminals at the start of a production. *)
      fun first1 nt = accumulate(selectCores nt,
				 TermSet.empty,
                                 fn (TERM t, set) => TermSet.insert t set
                                  | (_, set) => set
                                )

     (* memo-ise first1, add diags *)
      fun prFirst1(nt, termSet) =
         (Busy.print("First1 set of " ^ showNonterminal nt ^ " = { ");
          TermSet.app (Busy.withSpace printTerminalClass) termSet;
          Busy.println "}";
          termSet)
      val first1 = nontermMemo(if DEBUG then fn nt => prFirst1(nt, first1 nt)
                               else first1
                              )

     (* starters1: given a nonterminal "nt", return the set of nonterminals
	which can start its productions. Looks past nullables, but doesn't
	recurse *)
      fun starters1 nt = accumulate(selectCores nt,
                                    NontermSet.empty,
                                    fn (NONTERM nt, set) =>
					 NontermSet.insert nt set
                                     | (_, set) => set
                                   )

     (* memo-ise starters1, add diags *)
      fun prStarters1(nt, nontermSet) =
         (Busy.print("Starters1 set of " ^ showNonterminal nt  ^ " = { ");
          NontermSet.app (Busy.withSpace printNonterminal) nontermSet;
          Busy.println "}";
          nontermSet
         )

      val starters1 =
         nontermMemo(if DEBUG then fn nt => prStarters1(nt, starters1 nt)
                     else starters1
                    )

     (* starters: a closure over starters1 *)
      fun starters nt = NontermSet.closure(NontermSet.singleton nt, starters1)

     (* memo-ise starters, add diags *)
      fun prStarters(nt, nontermSet) =
         (Busy.print("Starters set of " ^ showNonterminal nt  ^ " = { ");
          NontermSet.app (Busy.withSpace printNonterminal) nontermSet;
          Busy.println "}";
          nontermSet
         )

      val starters =
         nontermMemo(if DEBUG then fn nt => prStarters(nt, starters nt)
                     else starters
                    )

     (* first: maps a nonterminal to its first-set. Get all the starters of
	the nonterminal, get the first1 terminal set of each of these,
	union the whole lot together *)
      fun first nt =
         let val startersSet = starters nt
             val startersList = startersSet
	  in 
	     NontermSet.setfold (fn (a,r) => TermSet.union(r,first1 a))
		 startersList TermSet.empty
         end

     (* memo-ise first, add diags *)
      fun prFirst(nt, termSet) =
         (Busy.print("First set of " ^ showNonterminal nt  ^ " = { ");
          TermSet.app (Busy.withSpace printTerminalClass) termSet;
          Busy.println "}";
          termSet
         )

      val first =
         nontermMemo(if DEBUG then fn nt => prFirst(nt, first nt)
                     else first
                    )



     (* prefix: all possible terminals starting a symbol list *)
      fun prefix symbols =
         case symbols
           of TERM t :: _ => TermSet.singleton t
            | NONTERM nt :: rest =>
                 if nullable nt then
                    TermSet.union(first nt, prefix rest)
                 else
                    first nt
            | nil => TermSet.empty

	in {first_string = fn n => TermSet.make_list (prefix n),
	    nullable_string = nullable_string}
	end
   end

functor V2_LrGraph(structure G: V2_LR_GRAMMAR
                structure Utils: V2_LR_UTILS
                structure Lookahead: V2_LOOKAHEAD
		sharing Lookahead.Utils = Utils
                sharing Lookahead.G = Utils.G = G
               ) : V2_LR_GRAPH =
   struct

      open G Lookahead Utils

      val DEBUG = false

     (* type abbrevs for the type of graph we're generating *)
      type State = {state: int, cores: CoreSet.set}

      structure Lr_Graph = Graph
	 (struct
	    type node = {state : int, cores : CoreSet.set }
	    type edge = Symbol
      	    val gt_node = fn ({cores,...}:State,{cores=cores',...}:State) =>
			 CoreSet.set_gt(cores,cores')
            val eq_node = fn ({cores,...}:State,{cores=cores',...}:State) =>
			  CoreSet.set_eq(cores,cores')
	    val eq_edge = eqSymbol
	    val gt_edge = gtSymbol
	   end)
				    
      type Graph = Lr_Graph.graph

     (* the goto function from State * Symbol to State list *)
      fun coreMatch(CORE c, symbol) =
         let val {coreRHSafter, ...} = c
         in
            case coreRHSafter
              of (symbol' :: _) => eqSymbol(symbol, symbol')
               | _ => false
         end

      exception ShiftCore
      fun shiftCore (a as (CORE c)) =
         case c
           of {I, coreRHSbefore, coreRHSafter=hd :: tl, prop,lookaheads} =>
                 CORE({I=I,
                       coreRHSbefore=hd :: coreRHSbefore,
                       coreRHSafter=tl,
		       prop=prop,
                       lookaheads = lookaheads
                      }
                     )
            | _ => raise ShiftCore

val mkGraph = fn {rules = rules : G.Rule list, verbose = verbose : bool,
		     eof = eof: G.Terminal,start = start: G.Nonterminal,
		     termPrecedence = termPrecedence: G.Terminal -> int option,
		     showTerminalClass=showTerminalClass: G.Terminal -> string,
		     showTerminalValue=showTerminalValue : G.Terminal -> string,
		     showNonterminal=showNonterminal : G.Nonterminal -> string,
		     showAttribute=showAttribute : G.Attribute -> string,
		     numTerminals=numTerminals : int,
		     numNonterminals=numNonterminals : int
		     } =>
let val showSymbol = mkshowSymbol {showTerminalClass=showTerminalClass,
				   showNonterminal=showNonterminal}
	val printCore = printCore {showSymbol = showSymbol,
				   showNonterminal = showNonterminal,
				   showTerminalClass = showTerminalClass}
	val printCoreSet = printCoreSet {printCore=printCore}
	val selectCores = mkselectCores
		{rules=rules,numNonterminals=numNonterminals}
	val coreClosure = mkcoreClosure {rules=rules,
					 numNonterminals = numNonterminals,
					 selectCores = selectCores}

	val {first_string,nullable_string} = 
		mkfunctions {numNonterminals = numNonterminals,
			     selectCores = selectCores,
			     showNonterminal = showNonterminal,
			     showTerminalClass = showTerminalClass
			     }
     (* the traversal of a set of cores which can shift on "symbol" *)
      fun traversal'(cores, symbol, result) =
	coreClosure(
	 CoreSet.setfold (fn (a,result) =>
	    if coreMatch(a,symbol) then CoreSet.insert (shiftCore a) result
            else result) cores result
		)

     (* include the symbol itself as well, for convenience in graph
	generation *)
      fun traversal cores symbol =
         (symbol, traversal'(cores, symbol, CoreSet.empty))

      val NextStateNum = ref 0

      val traversal =
         if DEBUG then
            fn cores =>
               fn symbol =>
                  (Busy.println("Traversal by " ^ showSymbol symbol
                                ^ " gives"
                               );
                   let val (_, cores') = traversal cores symbol
                   in
                      printCoreSet cores';
                      (symbol, cores')
                   end
                  )
         else
            traversal

     (* incorporate1: incorporate a traversal from->symbol->cores into
	"graph". Return new graph, and optionally the `to' node, if
	it wasn't already present.
      *)
      fun incorporate1(graph, from: Lr_Graph.node,
		      (symbol, cores : CoreSet.set)) =
         case (Lr_Graph.find_node(graph,{state=0,cores=cores})) of
	        SOME existingNode =>
                    (if DEBUG then
                        (Busy.println "*Existing node found:";
                         printCoreSet(#cores(existingNode))
                        )
                     else ();
                     (Lr_Graph.add_edge (graph, from, symbol,existingNode),
			  NONE)
                    )
               | NONE =>
                    let val _ =
                           if DEBUG then
                              (Busy.println("*New state "
                                            ^ makestring (!NextStateNum));
                               printCoreSet cores
                              )
                           else
			Busy.dot()

                        (* make sure every graph node has * UNIQUE * reference
                           variables
                         *)

                        val newNode = {cores=copyCoreSet cores,
                                       state=(!NextStateNum)}
			val _ = NextStateNum := (!NextStateNum)+1
                    in  (Lr_Graph.add_edge (Lr_Graph.add_node (graph,newNode) , from, symbol,
			           newNode), SOME newNode)
                    end

     (* incorporate': takes a graph, a from-node, and a list of traversals
	((symbol*Core set) list). adds any new nodes to the front of "newNodes"
      *)
      fun incorporate'(graph, from: Lr_Graph.node , traversals, newNodes) =

         case traversals
           of this :: rest =>
                 let val (graph', to) = incorporate1(graph, from, this)
                 in  case to
                       of SOME node =>
                             incorporate'(graph', from, rest, node :: newNodes)
                        | NONE => incorporate'(graph', from, rest, newNodes)
                 end
            | nil => (graph, newNodes)

      fun generateGraph(events, graph) =
         case events
           of (thisEvent :: restEvents) =>
                 let
                    val {state, cores}: State = thisEvent

                    val closuredCores = cores
                    val _ =
                       if DEBUG then
                          (Busy.println("*Collecting on event: "
                                        ^ makestring state
                                       );
                           printCoreSet closuredCores
                          )
                       else ()

                    val immedSymbols = immediateSymbols closuredCores

                    val prSymbol = (Busy.withSpace Busy.print) o showSymbol
                    val _ =
                       if DEBUG then
                          (Busy.print "*Immediate symbols: { ";
                           map prSymbol immedSymbols;
                           Busy.println "}"
                          )
                       else ()

                    val traversals =
                        List.fold (fn (immed_symbol,r) =>
                          (traversal closuredCores immed_symbol) :: r)
                           immedSymbols nil

                    val (newGraph, newEvents) =
                       incorporate'(graph, thisEvent, traversals,restEvents)
                 in
                    generateGraph(newEvents, newGraph)
                 end

            | _ => graph

     (* propagate lookaheads : propagate lookaheads throughout the 
	graph 
 	*)

     val propagatelookaheads = fn (g : Lr_Graph.graph) =>
	let val cores = Lr_Graph.nodes_of g
	    val cores = List.map (fn c => #cores c) cores
	    val edges = List.map (fn {From,Edge,To} =>
				(#cores(From),Edge,#cores(To)))
			    (Lr_Graph.EdgeSet.make_list (Lr_Graph.all_edges g))
	    val gp = ref true;
	    val cp = ref true;
	    val _ = List.app (set_prop nullable_string) cores
	    val _ = List.app (prop_f_to_c_a {first_string=first_string,
					     selectCores=selectCores}) cores
	    val closure_prop = fn c =>
		 propagate_l_to_c_a {selectCores=selectCores} (c,cp)
	    val goto_prop = 
		let val goto_prop' = propagate_l_to_g_i shiftCore
	 	in fn e => goto_prop'(e,gp)
	        end
	in while (!gp = true orelse !cp = true) do
	   (gp := false; cp := false;
	    output std_out "Propagating lookaheads ...\n";
	    List.app closure_prop cores;
	    List.app goto_prop edges
	   )
	end
	    
     (* generate the graph *)
      val startCores = copyCoreSet(coreClosure(selectCores start))
      val startEvent = {cores=startCores, state=0}

      val _ = if DEBUG then () else Busy.print "Generating State Graph"

      val _ = NextStateNum := 1
      val graph = generateGraph([startEvent],
		   Lr_Graph.add_node(Lr_Graph.null_graph,startEvent))
     
      val _ = hardwire_eof(startCores,eof,start)
      val _ = propagatelookaheads graph
  in graph
  end
end;
functor V2_TableGen(structure G: V2_LR_GRAMMAR): V2_LR_TABLE =
   struct

      structure G = G

      structure Utils =
       V2_LrUtils(structure G = G)

     structure Lookahead =
        V2_Lookahead(structure G = G
        	  structure Utils = Utils
       		 )

      structure Graph =
        V2_LrGraph (
           structure G = G
           structure Utils = Utils
           structure Lookahead = Lookahead
          )

      type 'entry Table = 'entry array array

      fun PrTable ((pr : string->unit),
		   (T : 'entry Table),
		   (Str: (int * 'entry) -> string),
		   (EncaseRow : ('entry array -> unit) * (string->unit) ->
					'entry array -> unit)) =
	let
	  val PrintRow = fn (R : 'entry array) =>
	     let fun f(i,cnt) =
		if i<Array.length(R) then
	 	   let val s = Str (i,(R sub i))
		       val cnt = 
			     let val newcount = cnt + size s
			     in if newcount > 73 then (pr "\\\n\\"; size s)
			         else newcount
			     end
		   in (pr s; f (i+1,cnt))
		   end
		else ()
	     in f(0,0)
	     end
	  fun PrintRows i =
	       if i < (Array.length T) then
		      (EncaseRow (PrintRow,pr) (T sub i); PrintRows(i+1))
	       else ()
       in PrintRows 0
       end

      fun access(tab, row, col) = (tab sub row) sub col

     (* Generate a table from an initialisation value. Won't work if the
	actual value contains references *)
      fun genTable(value, r, c) =
             let
                fun rows 0 = nil
                  | rows m = array(c, value) :: rows(m - 1)
             in
                arrayoflist(rows r)
             end

     (* assign a table entry, using "resolve" to check it against the existing
        entry *)
      fun assign(resolve: ('entry * 'entry) -> 'entry,
                 T: 'entry Table,
                 Row: int, Col: int,
                 newEntry: 'entry) =
             let val R = T sub Row
                 val oldEntry = R sub Col

             in  update(R, Col, resolve(oldEntry, newEntry))
             end

      type State = int
      fun showState(state: State) = makestring state

     (* Specific table stuff *)
      datatype Action = SHIFT of State
                      | REDUCE of G.Attribute
                      | ERROR

      type Prec = int option
      type ActionEntry = {action: Action, prec: Prec}
      datatype Goto = GOTO of int option

      exception AssignAction (* of ActionEntry * ActionEntry *)
      exception AssignGoto of Goto	(* Returns the current entry *)

  (* convert integer between 0 and 65535 to a a 2 byte character
     string representing the integer, with low byte first *)

val convert_int = fn (i:int) => 
  if i>(256*256-1) then 
	let exception Convert_int  in raise Convert_int end
  else
	let val make_char = fn (i:int) =>
		let val s = makestring i
		in (substring("\\000",0,1+3 + (~(size s)))) ^ s
		end
	in (make_char (i mod 256)) ^ (make_char (i div 256))
	end

local
	  val make_entry = fn (i,s) => (convert_int (i+1)) ^ s
	  val showAction = fn states => fn (n : int,{action, prec}) =>
         	  case action
	   	     of (REDUCE (G.ATTRIB {num,...})) =>
			 make_entry(n,convert_int(states+num))
		      | (SHIFT i) =>
			  make_entry(n,convert_int(i))
		      | ERROR => ""
           val showGoto =
		fn (n:int,(GOTO(SOME j))) =>
		   make_entry(n,convert_int(j))
 	         | _ => ""
in
val printLr_Table = fn (file : outstream, gotos, actions,
			DisplayAttributes,states) =>
let val pr = output file
    val prln = fn s => (pr s; pr "\n");
    val EncaseRow = fn (PrintRow,pr) => fn r =>
		(PrintRow r; pr (convert_int 0); pr "\\\n\\")
    val PrintTable = fn (table,entry_print) =>
		  (pr "\"\\\n\\";
		   PrTable (pr,table,entry_print,EncaseRow);
		   prln "\"")
in (pr "\
\structure Lr_Table : MLY_LR_TABLE = \n\
\    struct\n\
\     structure G = G\n\
\     open G\n\
\\n\
\     type State = int \n\
\     val initialState = 0 \n\
\     fun showState(state: State) = makestring state \n\
\\n\
\    (* Specific table stuff *) \n\
\     datatype Action = SHIFT of State \n\
\                     | REDUCE of Attribute \n\
\                     | ERROR	\n\
\      datatype Goto = GOTO of State\n";
	pr "local\n\
\		val string_to_int = fn(s,index) => (ordof(s,index) + \n\
\			ordof(s,index+1)*256,index+2)\n";
	DisplayAttributes(file);
	pr "val numStates =";
	prln (makestring states);
	pr "\
\		val convert_string_to_list = fn conv_func => fn(s,index) =>\n\
\		   let fun f (r,index) =\n\
\			 let val (num,index) = string_to_int(s,index)\n\
\			 in if num=0 then (rev r,index)\n\
\			    else let val (i,index) = string_to_int(s,index)\n\
\				 in f((num-1,(conv_func i))::r,index)\n\
\				 end\n\
\			 end\n\
\		    in f(nil,index)\n\
\		    end\n\
\		 val convert_string_to_array = fn conv_func => fn s =>\n\
\		    let val convert_row =convert_string_to_list conv_func\n\
\		 	fun f(r,index) =\n\
\			  if (index < size s) then\n\
\			   let val (newlist,index) = convert_row (s,index)\n\
\			   in f(newlist::r,index)\n\
\			   end\n\
\			  else arrayoflist(rev r)\n\
\		    in f(nil,0)\n\
\		    end\n\
\		 val int_to_goto = fn i => GOTO i\n\
\\n\
\		 val int_to_action = fn i =>\n\
\			if i >= numStates then\n\
\				get_attribute(i-numStates)\n\
\			   else SHIFT i\n\
\		 val make_goto_table = convert_string_to_array int_to_goto\n\
\		 val make_action_table=convert_string_to_array int_to_action\n";
	prln "in";
	prln "val actionT = make_action_table";
	PrintTable(actions,showAction states);
	prln "val gotoT = make_goto_table";
	PrintTable(gotos,showGoto);
	pr "\
\	exception NotThere\n\
\	fun find(((key:int),data)::b,i) =\n\
\		if (i>key) then find(b,i)\n\
\		else if (i<key) then raise NotThere\n\
\		else data\n\
\	  | find (nil,i) = raise NotThere\n\
\	 fun action(state,T t) =\n\
\		find(actionT sub state,t) handle NotThere => ERROR\n\
\	 exception Goto\n\
\	 fun goto(state,NT t) =\n\
\		(case find(gotoT sub state,t)\n\
\		   of (GOTO i) => i)\n\
\	         handle NotThere => raise Goto\n\
\     end\nend\n")
end
end

val mktable = fn dest => fn (ALL as
		    {rules = rules : G.Rule list, verbose=verbose : bool,
		     eof = eof : G.Terminal, start = start: G.Nonterminal,
		     termPrecedence = termPrecedence: G.Terminal -> int option,
		     showTerminalClass= showTerminalClass: G.Terminal -> string,
		     showTerminalValue=showTerminalValue : G.Terminal -> string,
		     showNonterminal=showNonterminal : G.Nonterminal -> string,
		     showAttribute=showAttribute : G.Attribute -> string,
		     numTerminals=numTerminals : int,
		     numNonterminals=numNonterminals : int
		     }) =>
let
      val rr_error = ref false

      (* DisplayAttributes: print values for the attributes of the rules,
	 of the form a{rule #} = ATTRIB ...   These values are used in
 	 the printed out version of the Action table. *)

	fun DisplayAttributes (f) =
	   let val pr = (output f)
	   in (pr
"val get_attribute= \n\
\	let val convert_back = fn (s,i) =>\n\
\		let val (lhs,ni) = string_to_int(s,i)\n\
\		    val (rhsLength,ni) = string_to_int(s,ni)\n\
\		    val (num,ni) = string_to_int(s,ni)\n\
\		in (REDUCE(G.ATTRIB{lhs=G.NT lhs,rhsLength=rhsLength,\n\
\		  		    num=num}),ni,num)\n\
\		end\n\
\	     val numRules =";
	       pr (makestring (List.length rules));
	       pr "\nval attrib_data =\n\"";
	      app(fn (G.RULE {attribute=G.ATTRIB {lhs,rhsLength,num},...}) =>
		    (
		     (* nonterminal number *)

		     pr (convert_int (G.nontermHash lhs));

		     (* rhs length *) 

		     pr (convert_int rhsLength);

		     (* num *)

		     pr (convert_int num);
		     pr "\\\n\\"
		    )) rules;

		pr "\"\n\
\	     val attrib_array = array(numRules,ERROR)\n\
\	     fun convert_string(s,index) =\n\
\		if (index < (size s)) then\n\
\		   let val (result,newindex,num)=convert_back(s,index)\n\
\		   in (update(attrib_array,num,result);\n\
\		       convert_string(s,newindex))\n\
\		   end\n\
\		else ()\n\
\	in (convert_string(attrib_data,0); fn i => attrib_array sub i)\n\
\	end\n";
	  ())
 end

      val printTerminalClass = Busy.print o showTerminalClass
      val printNonterminal = Busy.print o showNonterminal

      fun srConflict(state, attrib, terminal) =
         (Busy.println(showState state ^ ": " ^ "shift/reduce conflict " ^
		       "(shift " ^ (showTerminalClass terminal) ^ ",reduce " ^
			showAttribute attrib ^ ")")
         )

      fun precConflict(state, attrib, terminal) =
	(Busy.println ((showState state) ^
		": terminal and rule have the same precedence for the");
	 Busy.println ("shift/reduce conflict " ^
		       "(shift " ^ (showTerminalClass terminal) ^ ",reduce " ^
			showAttribute attrib ^ ")")
	)

      fun rrConflict(a1, a2) =
         Busy.println("reduce/reduce conflict between "
                      ^ showAttribute a1 ^ " and " ^ showAttribute a2
                     )
     (* assign an action table entry for state on encountering terminal *)

      (* resolve: precedence handling was modified for nonassociative
	 terminals.  Before, if a rule's precedence was greater than a
	 terminal's a precedence in a s/r conflict, a reduce was planted.
         Otherwise a shift occurred.  If a rule's precedence was equal to
	 a terminal's precedence, a shift was planted, but no warning message
         was printed.  This does not allow for proper handling of
	 associativities and precedence in a yacc-like parser generator.

 	 In yacc, each terminal may have a precedence and an associativity.
 	 A terminal may be left associative, right associative,
	 or nonassociative.  A rule is usually given the precedence of its
	 rightmost terminal.  We want to reduce if the precedence of the
	 rule is > the terminal, or if the precedences are = and the terminal
	 is left associative.  We want to shift the terminal if the
	 precedence of the terminal is higher than the rule's precedence, 
	 or if the precedences are equal and the terminal is right associative.
	 If the precedences are equal and the terminal is nonassociative
	 this is an error condition.
	
	 We need to print a diagnostic indicating to the user when the
	 precedences of the rule and the terminal are equal.  Then we
	 can give the rule precedence x, right associative terminals
	 precedence x+1, left associative terminals precedence x-1, and
	 nonassociative terminals precedence. x.
	*)


      fun assignAction(actions, state, terminal, entry) =
	let fun resolve ({action=ERROR, ...}, x) = x
            | resolve (s as {action=SHIFT _, prec=shiftPrec},
                 r as {action=REDUCE a, prec=redPrec}) =
		(case (shiftPrec,redPrec)
		 of (NONE,_) => (srConflict(state, a, terminal); s)
		  | (_,NONE) => (srConflict(state, a, terminal); s)
		  | (SOME (i:int),SOME j) =>
			if i = j then (precConflict(state, a, terminal); s)
			else if i > j then s
			else r
		 )
            | resolve (r as {action=REDUCE _, ...},s as {action=SHIFT _, ...})=
			resolve(s,r)
            | resolve (r1 as {action=REDUCE a1, ...},
		       r2 as {action=REDUCE a2, ...}) =
		 (rrConflict(a1,a2); rr_error := true; r1)
            | resolve _ = let exception Resolve in raise Resolve end
	in assign(resolve, actions, state, G.termHash terminal, entry)
	end

     (* assign a goto entry for a state * nonterminal *)
      fun assignGoto(gotos, state, Nonterminal, Entry) =
             let fun resolve(GOTO NONE, x) = x   |
                     resolve(Old, _) = raise AssignGoto Old
             in
                assign(resolve, gotos, state, G.nontermHash Nonterminal, Entry)
             end

     (* make table entries for all the graph's edges *)
      fun plantEdgeMoves(actions, gotos, allEdges) =
	let val f  = 
		 fn ({From={state=from,...},Edge,To={state=to,...}}
				: Graph.Lr_Graph.EdgeSet.elem) =>
			case Edge
			     of G.TERM t =>
                    		assignAction(actions, from, t,
                                 {action=SHIFT to, prec=termPrecedence t})
			 | G.NONTERM nt =>
				  assignGoto(gotos, from, nt, GOTO(SOME to))
	in Graph.Lr_Graph.EdgeSet.app f allEdges
	end

     (* plant all reductions from a particular state *)
      fun plantReductions actions (state,r) =
         let val {cores, state} = state

	     (* plant a reduction for a core item if the dot is at the end of
		the item *)

	     val plantReduction =
         	 fn  Utils.CORE {coreRHSafter=nil,lookaheads,
				 I={corePrecedence, coreAttribute, ...},
				 ...} =>

			(* plant a reduction for each terminal in the
			   lookahead set *)

			List.app (fn lookahead =>
				assignAction(actions,state,lookahead,
				     {action=REDUCE coreAttribute,
				      prec=corePrecedence})
			  ) (Utils.Lset.makelookaheadlist (!lookaheads))

                    | _ => ()
	     val closuredCores = (* coreClosure *) cores

         in (Busy.sendto_list();
             Utils.CoreSet.app plantReduction closuredCores;
	     case Busy.get_list()
		of nil => r
		 | l => (state,l)::r)
         end

     (* plant all reductions for all states *)

      fun plantStates(actions: ActionEntry Table,
		      states: Graph.Lr_Graph.node list) =
         List.fold (fn (state,r) => (plantReductions actions) (state,r))
			 states nil;

     (*  Generate graph *)

	val graph = Graph.mkGraph ALL;
	val theNodes = Graph.Lr_Graph.nodes_of graph
	val theEdges = Graph.Lr_Graph.all_edges graph
	val numNodes = Graph.Lr_Graph.num_nodes graph

        val actions=genTable({action=ERROR,prec=NONE},numNodes, numTerminals)
        val gotos = genTable(GOTO NONE, numNodes, numNonterminals)

      in (Busy.println "Filling Tables";
	  if verbose then
	   (let val outfile = open_out "y.output"
		val allerrs =  (plantEdgeMoves(actions,gotos,theEdges);
				plantStates(actions,theNodes))
		val errmsgs = fn state =>
		    fold (fn ((s,l),r) => if s=state then l::r else r) allerrs
			nil
	    	val _ = Busy.sendto_file outfile
	  	val printCore = Utils.printCore
			 {showSymbol = (Utils.mkshowSymbol
					({showTerminalClass=showTerminalClass,
					  showNonterminal=showNonterminal})
					),
			   showTerminalClass = showTerminalClass,
			   showNonterminal = showNonterminal
			 }
		val printCores = fn (state : int,cores) =>
			Utils.CoreSet.app (fn a =>
			    let val (Utils.CORE c) = a
			 	val {coreRHSafter,
				     I = {coreLHS,
					  coreAttribute=Utils.G.ATTRIB {num, ...},
					  ...},
				     ...
				    } = c
			    in (Busy.print "\t";
			        printCore a;
				case coreRHSafter
				 of nil => Busy.println (" (reduce by rule "^
							(makestring num) ^ ")")
				 | _ => Busy.println ""
				 )
			    end) cores

		val printActions = fn (state : int) =>
		   let fun f i = 
			if i < numTerminals then
			  (case (#action(access(actions,state, i)))
			     of SHIFT s =>
				Busy.println ("\t" ^
					      (showTerminalClass (G.T i)) ^
					      "\tshift " ^ (showState s))
	      	  	  | REDUCE (G.ATTRIB {lhs,num,...}) =>
		      	     Busy.println ("\t" ^ (showTerminalClass (G.T i))^
					   "\treduce by rule " ^
				 (makestring num))
		  	  | ERROR => ();
			   f (i+1))
			  else ()
		        fun g i =
			  if i < numNonterminals then
			     (case access(gotos, state, i)
				 of GOTO(SOME s) =>
				Busy.println("\t" ^(showNonterminal (G.NT i))^
					 "\tgoto " ^ (showState s))
				  | _  => ();
			      g (i+1))
			  else ()
		    in (f 0; g 0)
		    end
		  fun print_state a =
		      let val {state,cores} = a
			  val cores = (* coreClosure *) cores
		      in
			(Busy.println "";
			 revapp (revapp Busy.print) (errmsgs state);
			 Busy.println "";
			 Busy.println(" state " ^ (makestring state) ^
				     ":");
		         Busy.println "";
			 printCores(state,cores);
			 Busy.println "";
			 printActions(state)
			)
		       end

	 	 fun find_state n =
		   let fun f(a::b) = 
			let val {state,cores} = a
			in if state=n then a else f b
		        end
		   in f theNodes
		   end
		 fun all_states () =
		   let fun f i = if i<numNodes then
				   (print_state (find_state i); f (i+1))
				 else ()
		   in f 0
		   end
	    in all_states();
	       Busy.sendto_file std_out;
	       close_out outfile
	    end)
	else(plantEdgeMoves(actions, gotos,theEdges);
	     let val errs = (plantStates(actions,theNodes))
	     in (Busy.sendto_file std_out;
       		 revapp (fn (_,s) => map Busy.print s) errs)
	     end
	    );

	 if (!rr_error) then raise AssignAction
	 else printLr_Table(dest,gotos,actions,DisplayAttributes,numNodes)
	)
   end
end;
structure MLY_MAKE_PARSER = 
  struct
val print_parser = fn pr => pr
"\
\\n\
\\n\
\signature MLY_GRAMMAR =\n\
\   sig\n\
\      type Terminal\n\
\      type Nonterminal\n\
\      datatype Symbol = TERM of Terminal | NONTERM of Nonterminal\n\
\\n\
\      datatype Attribute = ATTRIB of {lhs:Nonterminal,rhsLength:int,num: int }\n\
\\n\
\      val showTerminalClass : Terminal -> string\n\
\      val showTerminalValue: Terminal -> string\n\
\      val showNonterminal : Nonterminal -> string\n\
\      val eqNonterminal : Nonterminal * Nonterminal -> bool\n\
\      val eqTerminal : Terminal * Terminal -> bool\n\
\\n\
\      type Lineno\n\
\      val lineno : Lineno ref\n\
\      val error: string -> Lineno -> unit\n\
\\n\
\     val ErrTermList : Terminal list\n\
\     val preferred_subst : Terminal -> Terminal list\n\
\     val is_keyword : Terminal -> bool\n\
\     val preferred_insert : Terminal -> bool\n\
\     val eof : Terminal\n\
\     val start : Nonterminal\n\
\   end\n\
\\n\
\signature MLY_LR_TABLE =\n\
\   sig\n\
\      structure G : MLY_GRAMMAR\n\
\      \n\
\      type State\n\
\      val initialState: State\n\
\      val showState: State -> string\n\
\\n\
\      datatype Action =   SHIFT of State | REDUCE of G.Attribute | ERROR\n\
\      val action: State * G.Terminal -> Action\n\
\      val actionT : (int * Action) list array\n\
\      val goto: State * G.Nonterminal -> State\n\
\   end\n\
\\n\
\signature MLY_ACTIONS = \n\
\    sig \n\
\	type Value\n\
\	val ErrValList : Value list\n\
\	val VOID : Value\n\
\	val rule : (int * Value list) -> (Value * Value list)\n\
\    end\n\
\\n\
\\n\
\signature MLY_PARSER =\n\
\   sig\n\
\      structure A: MLY_GRAMMAR\n\
\      structure B: MLY_ACTIONS\n\
\      val parse: (unit -> A.Terminal * B.Value) -> (int*int) -> B.Value\n\
\   end;\n\
\\n\
\functor ParserGen(structure Lr_Table : MLY_LR_TABLE\n\
\		      structure RuleAction: MLY_ACTIONS\n\
\                     ) : MLY_PARSER =\n\
\   struct\n\
\     structure A = Lr_Table.G\n\
\     structure B = RuleAction\n\
\     open Lr_Table RuleAction Lr_Table.G\n\
\     exception Error\n\
\     val DEBUG = false\n\
\\n\
\     \n\
\\n\
\     exception Joinlists\n\
\\n\
\     val TestLexVList = \n\
\	let fun f(a::a',b::b',r) = f(a',b',(a,b)::r)\n\
\	      | f(nil,nil,r) = rev r\n\
\	      | f _ = raise Joinlists\n\
\	in f(ErrTermList,ErrValList,nil)\n\
\	end\n\
\\n\
\     type Element = { term : G.Terminal, value : Value, stack : State list,\n\
\			 def_reduces : G.Attribute list, lineno : G.Lineno}\n\
\\n\
\      local \n\
\	 val print = output std_out\n\
\	 val println = fn s => (print s; print \"\\n\")\n\
\      in\n\
\        fun printStack(stack: State list, n: int) =\n\
\         case stack\n\
\           of (state) :: rest =>\n\
\                 (print(\"          \" ^ makestring n ^ \": \");\n\
\                  println(showState state);\n\
\                  printStack(rest, n+1)\n\
\                 )\n\
\            | nil => ()\n\
\                \n\
\        fun prAction(stack as (state) :: _, next, action) =\n\
\             (println \"Parse: state stack:\";\n\
\              printStack(stack, 0);\n\
\              print(\"       state=\"\n\
\                         ^ showState state	\n\
\                         ^ \" next=\"\n\
\                         ^ showTerminalClass next\n\
\                         ^ \" action=\"\n\
\                        );\n\
\              case action\n\
\                of SHIFT state' =>\n\
\                      println(\"SHIFT \" ^ showState state')\n\
\                 | REDUCE(ATTRIB{lhs, ...}) =>\n\
\                      println(\"REDUCE \" ^ showNonterminal lhs)\n\
\                 | ERROR =>\n\
\                      println \"ERROR\";\n\
\              action\n\
\             )\n\
\        | prAction (_,_,action) = action\n\
\     end\n\
\\n\
\    \n\
\\n\
\     val pr_errln = error\n\
\     val pr_err = error\n\
\      \n\
\\n\
\     \n\
\\n\
\     fun parse lexer (MaxLookAhead,Size) = \n\
\      let exception Remove\n\
\	  val MaxLookAhead = max(0,MaxLookAhead) \n\
\          val Size = max(0,Size)\n\
\\n\
\	val ls = (ref nil) : ((Terminal * Value) * Lineno) list ref\n\
\\n\
\	val lexer = fn () =>\n\
\	  case (!ls) of\n\
\	    nil => (lexer(),!lineno)\n\
\	  | a::b => (ls := b; a)\n\
\\n\
\	\n\
\\n\
\	fun print_lookahead () = \n\
\	 (app (fn ((a:Terminal,_),_) => (print (showTerminalClass a); print \" \")) (!ls);\n\
\	  print \"\\n\")\n\
\\n\
\	fun remove (0,s) = s\n\
\	  | remove (n,a::b) = remove(n-1,b)\n\
\	  | remove _ = raise Remove\n\
\\n\
\	exception ParseStep\n\
\	val FixError = ParseStep\n\
\	exception psRemoveBind\n\
\\n\
\	exception Reduce\n\
\\n\
\	fun reduce(l,vs) =\n\
\	     fold (fn (ATTRIB{rhsLength,num,...},vs) =>\n\
\		let val (nv,vs) = rule (num,vs)\n\
\		in nv::vs\n\
\		end) l vs\n\
\\n\
\\n\
\	\n\
\\n\
\	fun fix_error(ss as (topstate :: _ ) : State list ,\n\
\		(vs,oss) : (Value list * State list),\n\
\		queue as (x,y) : (Element list * Element list),\n\
\		reductions :  G.Attribute list,\n\
\		lexv as ((term,value),lineno),\n\
\		c : int,\n\
\		min_advance : int,\n\
\	        max_advance : int) =\n\
\	   let\n\
\\n\
\		val _ = pr_errln(\"syntax error found at \" ^\n\
\				  (showTerminalClass term)) lineno\n\
\\n\
\		val min_delta = 3\n\
\\n\
\	 \n\
\\n\
\	 	val toklist = x@(rev ({term=term,value=value,lineno=lineno,\n\
\				       stack=oss,def_reduces=nil}::y))\n\
\\n\
\	\n\
\\n\
\	 	datatype Oper = INSERT | DELETE  | SUBST of Terminal\n\
\		datatype Change = CHANGE of {pos : int, distance : int,\n\
\					     term : Terminal, value : Value,\n\
\					     oper : Oper,lineno : Lineno}\n\
\\n\
\	 val print_change = fn (CHANGE {pos,distance,term,value,oper,lineno}) =>\n\
\	    (print (\"{ pos= \" ^ (makestring pos));\n\
\	     print (\" dis= \" ^ (makestring distance));\n\
\	     print (\" term = \" ^ (showTerminalClass term));\n\
\	     print (\"oper= \" ^ (case oper\n\
\			         of INSERT => \"INSERT \"\n\
\				  | SUBST _ => \"SUBST \"\n\
\				  | DELETE => \"DELETE \"));\n\
\\n\
\	     print \"}\\n\")\n\
\\n\
\	val print_cl = map print_change\n\
\\n\
\	\n\
\\n\
\	        val ExtraTokens =\n\
\		  let fun f (t,0) = rev t\n\
\			| f (t,n) =\n\
\			   	let val (lexval as ((term,_),_)) = lexer()\n\
\			   	in f(lexval::t,\n\
\				     if eqTerminal(term,eof) then 0 else n-1)\n\
\				end\n\
\		  in f(nil,max_advance)\n\
\		  end\n\
\\n\
\	\n\
\				\n\
\\n\
\	   val LexValueList =\n\
\		(map (fn ({term, value,lineno, ...} : Element) =>\n\
\			    ((term,value),lineno))\n\
\	         toklist) @ (ExtraTokens)\n\
\\n\
\	  val TermList = map (fn ((a,_),l) => (a,l)) LexValueList\n\
\\n\
\	 \n\
\\n\
\	  exception parseTest\n\
\	  exception parseRemoveBind\n\
\\n\
\	  fun parse (ss as (s :: _),tokenlist) =\n\
\	    (case tokenlist\n\
\		 of nil => 0\n\
\		  | (e as (a, _ ))::b => \n\
\	 	(case Lr_Table.action(s,a)\n\
\		  of ERROR => length tokenlist\n\
\		   | (SHIFT s) => parse(s::ss,b)\n\
\		   | (REDUCE (ATTRIB {lhs,rhsLength, ...})) =>\n\
\\n\
\			\n\
\\n\
\			if eqNonterminal(lhs,start) then ~1\n\
\		        else case remove(rhsLength,ss)\n\
\			 	of (ns as (ts :: _ )) =>\n\
\			  	   parse(goto(ts,lhs)::ns,e::b)\n\
\			  	 |  _ => raise parseRemoveBind\n\
\		  )\n\
\	      )\n\
\	      | parse _ = raise parseTest\n\
\\n\
\        exception Rev_queue_fold\n\
\\n\
\	\n\
\\n\
\	fun rev_queue_fold (queue : 'b list,toklist : 'c list)\n\
\			   (g : (int * 'a * 'b list * 'c list) -> 'a)\n\
\			   (start : 'a) =\n\
\\n\
\	let fun f(count,results,nil,_) = results\n\
\	      | f(count,results,queue as (q :: q'),toklist as (t :: t')) = \n\
\		  f(count+1,g(count,results,queue,toklist),q',t')\n\
\	      | f _ = raise Rev_queue_fold\n\
\	in f(0,start,queue,toklist)\n\
\	end\n\
\\n\
\\n\
\	\n\
\\n\
\	fun test (new_token_list :\n\
\			 (Terminal * Lineno) * ((Terminal * Lineno) list) ->\n\
\				 ((Terminal * Lineno) list),\n\
\		       oper : Oper) =\n\
\	 let fun test' (count,results, ({stack, ...} : Element) :: _,\n\
\		tl as 	((_,lineno) :: _)) =\n\
\	    List.fold (fn ((a,v),r) =>\n\
\		let val tokens_left = parse(stack,new_token_list((a,lineno),tl))\n\
\		in if tokens_left > (max_advance - min_advance) then r\n\
\		   else (CHANGE {pos=count,term=a,value=v,distance=tokens_left,\n\
\			     oper = oper,lineno=lineno})::r\n\
\		end) TestLexVList results\n\
\	 in rev_queue_fold (toklist,TermList) test' nil\n\
\	 end\n\
\		\n\
\	val SubstChanges =\n\
\	  let fun test (count,results,({stack, term, ...} : Element) :: _,\n\
\			 (_,lineno) :: rest) =\n\
\	    let val max_left = max_advance - min_advance\n\
\	    in List.fold (fn ((a,v),r) =>\n\
\		  let val tokens_left = parse(stack,(a,lineno)::rest)\n\
\		  in if tokens_left > max_left then r\n\
\		    else (CHANGE {pos=count,term=a,value=v,distance=tokens_left,\n\
\			        oper=SUBST term,lineno=lineno})::r\n\
\		  end) TestLexVList results\n\
\	    end\n\
\	 in rev_queue_fold (toklist,TermList) test nil\n\
\	 end\n\
\\n\
\	val DeleteChanges = \n\
\	  let fun test(count,results,({term,value,stack, ...} : Element) :: _,\n\
\		       termlist as ((_,lineno) :: rest)) =\n\
\		     let val tokens_left = parse(stack,rest)\n\
\		     in if tokens_left > (max_advance - min_advance) then\n\
\			      results\n\
\			else (CHANGE {pos=count,distance=tokens_left,term=term,\n\
\			       value=value,oper=DELETE,lineno=lineno}) :: results\n\
\		     end\n\
\	   in rev_queue_fold (toklist,TermList) test nil\n\
\	   end\n\
\\n\
\	val InsertChanges =\n\
\	   test ((fn (a,rest) => a::rest),INSERT)\n\
\\n\
\\n\
\\n\
\	local\n\
\\n\
\	 fun sieve(a as (CHANGE {distance, ...}),b as (min,results)) =\n\
\	        if min>distance then (distance,[a])\n\
\	        else if min=distance then (min,a::results)\n\
\	        else b\n\
\\n\
\	 fun sieve_list l = List.fold sieve l (max_advance,nil)\n\
\\n\
\	in\n\
\\n\
\	  val (min1,DeleteChanges) = sieve_list DeleteChanges\n\
\	  val (min2,SubstChanges) = sieve_list SubstChanges\n\
\	  val (min3,InsertChanges) = sieve_list InsertChanges\n\
\\n\
\	  val min0 = min(min(min1,min2),min3)\n\
\\n\
\	 val DeleteChanges = if min1>min0 then nil else DeleteChanges\n\
\	 val SubstChanges = if min2>min0 then nil else SubstChanges\n\
\	 val InsertChanges = if min3>min0 then nil else InsertChanges\n\
\\n\
\	end\n\
\\n\
\ 	val _ = if DEBUG then\n\
\			 (print_cl InsertChanges; print_cl DeleteChanges;\n\
\		 	  print_cl SubstChanges; ())\n\
\		else ()\n\
\\n\
\\n\
\\n\
\     val (InsertChanges,t) =\n\
\	 List.fold (fn (a as (CHANGE {term, ...}),(r,t)) => \n\
\		if preferred_insert term then \n\
\			if t then (a::r,t) else ([a],true)\n\
\		else if t then (r,t) else (a::r,t)\n\
\	      ) InsertChanges (nil,false)\n\
\\n\
\     val (SubstChanges,t') =\n\
\	List.fold (fn (a as (CHANGE {term=term,oper=SUBST t', ...}),(r,t)) =>\n\
\		if List.exists (fn a=>eqTerminal(a,term)) (preferred_subst t') then\n\
\		  if t then (a::r,t) else ([a],true)\n\
\	 	else if t then (r,t) else (a::r,t)\n\
\	        | (a,(r,t)) => (a::r,t) \n\
\	      ) SubstChanges (nil,false)\n\
\\n\
\\n\
\\n\
\    local val max_tokens = max_advance - (min_advance+min_delta)\n\
\\n\
\          val remove_keywords = fn l =>\n\
\		List.fold (fn (a as (CHANGE {term,distance,...}),r) =>\n\
\		    if (is_keyword term) andalso (distance > max_tokens) then\n\
\		       r\n\
\		    else a::r) l nil\n\
\\n\
\    in\n\
\\n\
\     val InsertChanges =\n\
\	 if t then InsertChanges else remove_keywords InsertChanges\n\
\			\n\
\     val DeleteChanges = remove_keywords DeleteChanges\n\
\\n\
\     val SubstChanges =\n\
\	 if t' then SubstChanges else remove_keywords SubstChanges\n\
\\n\
\    end\n\
\    val MinChanges =\n\
\	let val ic = length InsertChanges\n\
\	    and dc = length DeleteChanges\n\
\	    and sc = length SubstChanges\n\
\	in if ic=1 then SOME InsertChanges\n\
\	   else if dc=1 then SOME DeleteChanges\n\
\	   else if sc=1 then SOME SubstChanges\n\
\	   else if (min0 > (max_advance-(min_advance+min_delta))\n\
\		    orelse (ic+dc+sc)=0)\n\
\		then NONE\n\
\	 	else SOME (InsertChanges @ DeleteChanges @ SubstChanges)\n\
\	end\n\
\\n\
\\n\
\in case MinChanges \n\
\	   of (SOME l) =>\n\
\	        let fun print_msg (CHANGE {term, oper, lineno, ...}) =\n\
\\n\
\		     let val s = \n\
\		       case oper\n\
\			 of DELETE => \"deleting \"\n\
\			  | INSERT => \"inserting \"\n\
\		          | SUBST t => \"replacing \" ^ (showTerminalClass t) ^\n\
\				   \" with \"\n\
\		     in pr_errln (s ^ (showTerminalClass term)) lineno\n\
\		     end\n\
\		   \n\
\		   val a = \n\
\		      if (length l > 1)\n\
\			 then (\n\
\	if DEBUG then\n\
\		(pr_errln \"multiple fixes possible: could fix it by\" lineno;\n\
\		 map print_msg l;\n\
\		 pr_errln \"fixing it with\" lineno\n\
\		)\n\
\	else ();\n\
\			       print_msg (hd l);\n\
\			       (hd l))\n\
\			  else (print_msg (hd l); (hd l))\n\
\\n\
\		    val pos = (fn (CHANGE {pos, ...}) => pos) a\n\
\			 \n\
\		  fun f(0,q,termlist,rq',CHANGE {oper,term,value, lineno,\n\
\			 ...}) =\n\
\		   let val ({stack, ...} : Element) = hd(q)\n\
\		    in\n\
\		     (case oper\n\
\		      of DELETE =>\n\
\		      if eqTerminal(term,eof) then\n\
\			   (pr_errln \"EOF encountered: goodbye!\" lineno;\n\
\			    raise FixError)\n\
\		      else ls := (tl termlist) @ (!ls)\n\
\		       | (SUBST _) =>\n\
\			 ls := (((term,value),lineno)::(tl termlist)) @ (!ls)\n\
\		       | INSERT =>\n\
\			 ls := (((term,value),lineno)::termlist) @ (!ls));\n\
\		      parse_step(stack,(vs,stack),(rev rq',nil),nil,lexer(),\n\
\				 Size-pos)\n\
\		     end\n\
\		     | f(n,e  :: r, _ :: termlist, rq',change) =\n\
\			f(n-1,r,termlist,e::rq',change)\n\
\		in f(pos,toklist,LexValueList,nil,a)\n\
\		end\n\
\	  | NONE => if eqTerminal(term,eof) then\n\
\			   (pr_errln \"EOF encountered: goodbye!\" lineno;\n\
\			    raise FixError)\n\
\		      else\n\
\		 (raise FixError\n\
\\n\
\)\n\
\	end\n\
\\n\
\      | fix_error _ = raise FixError\n\
\	\n\
\	and parse_step(ss as (topstate :: _ ),\n\
\		       v as (vs,oss),queue as (x,y),reductions,\n\
\		       lexv as ((term,value),lineno),c) =\n\
\	   (case (if DEBUG then prAction(ss, term,\n\
\					Lr_Table.action (topstate,term))\n\
\			   else Lr_Table.action (topstate,term))\n\
\	     of SHIFT s =>\n\
\	       let val ss = s::ss\n\
\	 	   val ny = {value=value,def_reduces=reductions,\n\
\			    term=term,stack=oss,lineno=lineno}::y\n\
\\n\
\	       in if c > 0 then\n\
\	             parse_step(ss,(vs,ss),(x,ny),nil,lexer(),c-1)\n\
\	          else (case x of nil =>\n\
\		    let val ({value,def_reduces, ...}::nx) = rev ny\n\
\		    in parse_step(ss,(value::(reduce(def_reduces,vs)),ss),\n\
\			          (nx,nil),nil, lexer(),c)\n\
\		    end\n\
\		   | ({value,def_reduces, ...}::b) =>\n\
\			parse_step(ss,(value::(reduce(def_reduces,vs)),ss),\n\
\				  (b,ny),nil,lexer(),c))\n\
\	        end\n\
\		| REDUCE (r as (ATTRIB {lhs,rhsLength , ...})) =>\n\
\		   if eqNonterminal(lhs,start) then\n\
\		     hd(reduce(r::reductions,\n\
\			fold (fn ({value,def_reduces, ...} : Element,vs) =>\n\
\			    value::(reduce(def_reduces,vs))) (y@(rev x)) vs))\n\
\		   else (\n\
\		    case (remove(rhsLength,ss)) \n\
\		      of (ss as (ts :: _)) =>\n\
\			(\n\
\\n\
\		   	 parse_step(goto(ts,lhs)::ss,v,queue,\n\
\				       r::reductions,lexv,c)\n\
\			)\n\
\			| _ => raise psRemoveBind\n\
\		    )\n\
\		 \n\
\		 | ERROR => fix_error(ss,v,queue,reductions,lexv,c,1,MaxLookAhead))\n\
\	      | parse_step _ = raise ParseStep\n\
\ 	    in parse_step([initialState],(nil,[initialState]),\n\
\			  (nil,nil),nil,lexer(),Size) \n\
\	    end\n\
\end\n\
\"
  end
structure Misc =
struct
    structure G : V2_LR_GRAMMAR =
	struct
		datatype Terminal = T of int
		and Nonterminal = NT of int

		datatype Symbol = TERM of Terminal
				| NONTERM of Nonterminal

		datatype Attribute = ATTRIB of { lhs : Nonterminal,
						 rhsLength : int,
						 num : int
					       }
		datatype Rule = RULE of {lhs : Nonterminal,
					 rhs : Symbol list,
					 attribute : Attribute,
					 precedence : int option
					}

		val termHash = fn (T i) => i
		val nontermHash = fn (NT i) => i
		val eqTerminal = fn ((T i),(T i')) => i = i'
		val eqNonterminal = fn ((NT i),(NT i')) => i = i'

		val gtTerminal = fn ((T i),(T i')) => i > i'
		val gtNonterminal = fn ((NT i),(NT i')) => i > i'
	end

     structure MakeTable = V2_TableGen(structure G = G)

type Lineno = int
val lineno = ref 1
val infile = ref "";

val error = fn t => fn (l : Lineno) =>
		 (output std_out ((!infile) ^ ", line " ^
				 (makestring l) ^ ": " ^ t ^ "\n"))

datatype LexValue = LEFT | RIGHT | NONASSOC

type symbol = string
type constr = string
type ty = string list option
type constr_data = {ty: ty,num : int}

structure PrecSet = FullSet(struct 
			      type elem = (symbol * (int*LexValue))
			      val gt = fn ((a:string,_),(a',_)) => a > a'
			      val eq = fn ((a:string,_),(a',_)) => a = a'
			     end
			    )

structure ConstrSet = FullSet(struct
				type elem = (constr * {ty : ty, num : int})
			   	val gt =  fn ((a:string,_),(a',_)) => a > a'
			  	val eq = fn ((a:string,_),(a',_)) => a = a'
			      end)

type decl_data = {start : symbol option,
		  prec :  { d : PrecSet.set,
			    h : int} option,
		  nonterm : ConstrSet.set option,
		  term : ConstrSet.set option,
		  eof : symbol option,
		  prefer : (symbol*symbol) list,
		  iprefer : symbol list,
	          keyword : symbol list,
		  structure' : symbol option,
		  verbose : bool}

type rhs_data = {rhs:symbol list, code:string, prec: symbol option} list
type rule = { lhs : symbol, rhs : symbol list,
		      code : string, prec : symbol option }

val out = ref std_out;

val len = ref 0
val indent = ref 0
val tw = 4
val inc_margin = fn () => indent := (!indent)+tw
val dec_margin = fn () => indent := (!indent)-tw
val reset_margin = fn () => indent := tw
val err_flag = ref false
(* The next line is bogus, it gets the wrong value of lineno *)
val errmsg = fn x => (err_flag := true; error x (!lineno))
val errln = errmsg
	
val say = fn x => output (!out) x

fun  newln () =
   let fun f i =  if i > 0 then (say " "; f (i-1)) else ()
   in  (say "\n"; len := (!indent); f(!indent))
   end

val sayln = fn (x : string) =>
	let val wl = size x
	    val new_count = !len + wl
	in if (!len = 0 orelse new_count < 78)
	      then (say x; newln())
	      else (newln(); say  x; newln())
	end

val saywd = fn x : string =>
 	 let val wl = size x
	     val new_count = !len + wl
	 in if (!len = 0 orelse new_count < 78) 
	       then (len := new_count; say x)
	       else (newln(); len := (!len) + wl; say x)
	 end

local
    fun add_nums(nil,i) = nil
      | add_nums ((c,{ty=t, ...} : constr_data)::r,i) =
		(c,{ty=t,num=i}) :: add_nums(r,i+1)
in
  fun make_tok_dict (l as (_ :: _)) = SOME (ConstrSet.make_set (add_nums(l,0)))
    | make_tok_dict nil = NONE
end

fun save_prec (l as (_::_),parity) =
    SOME {d= PrecSet.make_set (map (fn a => (a,(1,parity))) l), h=3}
  | save_prec _ = NONE

fun join_decls {start=a, prec=b, nonterm=c, term=d,eof=e,
		iprefer=f,prefer=g,keyword=h,structure'=k,verbose=verbose}
               {start=a',prec=b',nonterm=c',term=d',eof=e',
	 	iprefer=f',prefer=g',keyword=h',structure'=k',
		verbose=verbose'} =

    let fun j (f,NONE,NONE) = NONE
	  | j (f,a,NONE) = a
	  | j (f,NONE,a) = a
	  | j (f,SOME i,SOME j) = f(i,j)

	 fun join e = fn(i,j) =>
	  (errln ("ignoring duplicate "^e^" declaration"); SOME i)

	 fun join_prec({d=t,h=h},{d=t',h=h'}) =
	   let fun f ((e as  (a,(_,p))),t) =
		if (PrecSet.exists e t) then 
	   	    (errln ("ignoring duplicate %prec definition of" ^ a); t)
		else (PrecSet.insert (a,(h+1,p)) t)
	  in SOME {d = PrecSet.setfold f t' t,h=h+3} 
	  end

      in {start= j (join "start",a,a'),
	   prec=j (join_prec,b,b'),
	   term = j (join "%term",d,d'),
	   nonterm = j (join "%nonterm",c,c'),
	   eof = j (join "%eof",e,e'),
	   iprefer=f'@f,
	   prefer=g'@g,
	   keyword=h'@h,
	   structure' = j (join "%structure",k,k'),
	   verbose = verbose orelse verbose'}
      end

local fun print_bool_case (l : string list) =
	(sayln "fn t => ";
	 sayln "case t";
         saywd "of ";
	 List.app (fn s => (saywd s; sayln " => true"; saywd " | ")) l;
	 sayln "_ => false"
	)

in fun print_is_keyword_func (l : string list) =
	(sayln "val is_keyword =";
	 print_bool_case l)

   and print_preferred_insert_func (l : string list) =
	(sayln "val preferred_insert =";
	 print_bool_case l)

   and print_preferred_subst_func (l : (string*(string list)) list) =
	(sayln "val preferred_subst = fn t =>";
	 sayln "case t";
	 saywd "of ";
	 List.app (fn (a,l') =>
	    (saywd a; saywd " => ";
	     List.app (fn s => (saywd s; saywd "::")) l';
	     sayln "nil"; saywd "| "
	    )
	 ) l;
	 sayln " _ => nil"
	)
end

fun printConstrSet(constr_set,name,only_with_values) =
  let val _ = (inc_margin(); inc_margin(); inc_margin();
		saywd ("datatype " ^ name ^ " = "))
      val printed_something = 
	    ConstrSet.revsetfold (fn ((s,{ty=d,...}),result) =>
		  (if only_with_values then
			(case d
			 of NONE => result
			  | SOME t =>
				(if result then saywd " | " else ();
				 saywd s;
				 saywd " of ";
				 List.app saywd t;
				 true
				)
			)
		     else
			(if result then saywd " | " else ();
		         saywd s;
		         case d
				of NONE => ()
				 | SOME t => (saywd " of "; List.app saywd t);
		   	 true
			)
		   )
		) constr_set false;
         in (dec_margin(); dec_margin(); dec_margin(); sayln "";
	     printed_something)
	 end

fun PrConstructors(n,t) = 
     (saywd ("datatype "^n^ " = ");
      ConstrSet.revsetfold( fn ((s,_),r) =>
		(if r then saywd " | " else (); saywd s; true)) t false;
      newln())

val make_parser = fn (HEADER : string, MPC_DECLS : decl_data,
		      TRULELIST : rule list) =>
(let exception SemanticError
 in let val ({start=start,prec=p,nonterm=nt,term=t,eof=eof,
	   prefer=prefer,iprefer=iprefer,keyword=keyword,
	   structure'=structure',verbose=verbose}) = MPC_DECLS

	val p = case p of NONE => PrecSet.empty | SOME {d,h} => d

	val _ = (let val f = fn d => errln("missing "^d^" definition")
	         in (case t of NONE => f "%term" | _ => ();
		     case nt of NONE => f "%nonterm" | _ => ())
		 end);

	val nonterms = 
		case nt of SOME i => i
		 	 | NONE => raise SemanticError

	val terms = case t of SOME i => i
			    | NONE => raise SemanticError

	val dummy_data = {ty = NONE,num = 0}
	val is_term = fn a => ConstrSet.exists (a,dummy_data) terms
	val is_nonterm = fn a => ConstrSet.exists (a,dummy_data) nonterms

	exception Get_type
 	val get_type = fn (a,terms,nonterms) =>
	   case (ConstrSet.find (a,dummy_data) terms)
	    of NONE =>
		    (case (ConstrSet.find (a,dummy_data) nonterms)
			 of SOME (_,{ty=t,...}) => t
			  | _ => raise Get_type)
	     | SOME (_,{ty=t,...}) => t

	val get_prec = fn a =>
	   case PrecSet.find (a,(0,LEFT)) p 
		of NONE => NONE
		 | SOME (_,(a,_)) => SOME a

        val _ = PrecSet.app
		  (fn (s,_) =>
		  	if is_term s then ()
	                else errln (s^" in %prec is not defined as a token")
		  ) p

	val start =
	    case start
		  of NONE => start
		   | SOME i =>
		if is_nonterm i then start
		else (errln
		  (i ^ " in %start is not defined as a nonterminal"); NONE)

	val eof = case eof
		     of NONE =>(errln ("missing %eof definition"); "")
		      | SOME i => 
		if is_term i then i
		else (errln (i ^ " in %eof is not defined as a nonterminal"); "")

	fun make_unique_id s = 
	   if (is_term s) orelse (is_nonterm s) then make_unique_id (s ^ "'")
		else s
	val void = make_unique_id "mlyVOID"

	local
	   val dummy_start = make_unique_id "mlySTART"
	   val nontermlist = rev (map (fn {lhs=lhs : symbol,...} => lhs) TRULELIST)
	   val start = case start of NONE => (hd nontermlist) | SOME a => a
	   val dummy_type = get_type(start,terms,nonterms)
	   val code = case dummy_type of NONE => "" | _ => start
	in 
	      val nonterms = ConstrSet.insert (dummy_start,
		  {ty=dummy_type,num=ConstrSet.card nonterms}) nonterms
	      val is_nonterm = fn a => ConstrSet.exists (a,dummy_data) nonterms
	      val TRULELIST={lhs=dummy_start,rhs=[start],code=code,prec=NONE}::
			     TRULELIST
	      val start=dummy_start
	end

      val (keyword, iprefer) =
	 let val f = fn x =>
	     fn (a,r) =>
	        if (is_term a) then a::r
		else (errln (a^" in "^x^" is not defined as a terminal"); r)
	 in (List.fold (f "%keyword") keyword nil,
	     List.fold (f "%insert_prefer") iprefer nil)
	 end

      val prefer =
	let val print_err =
		fn s => errln (s^" in %prefer is not defined as a terminal")
	    val f = fn (pair as (a,a'),r) =>
		let val flag =
		      if (is_term a) then false else (print_err a; true)
		    val flag' =
		      if (is_term a') then false else (print_err a'; true)
	        in if (flag orelse flag') then r else (pair::r)
		end
	 in List.fold f prefer nil
	 end

      (* prefer_map : take list of (sym,sym') where sym is a preferred
	 substitution for sym', make list of (sym,[ ... syms]) where
	 elements in the list are all the preferred substitutions for some
	 sym *)

      val prefer_map = 
	 let 
	  (* take prefer list, second elem of pair, return list of elems
	     in prefer list w/ same second elem, prefer list - list of
	     elems *)

	    fun g (prefer_list,second_elem) =
	      List.fold (fn (e as (f,s),(same,differ)) =>
		 if s=second_elem then (f::same,differ) else (same,e::differ))
		prefer_list (nil,nil)

	    fun f(nil,prefer_map) = prefer_map
	      | f(l as (e as (_,s):: _),prefer_map) = 
		   let val (same,differ) = g(l,s)
		   in f(differ,(s,same)::prefer_map)
	 	   end

	in f(prefer,nil)
	end

	val _ = 
	  ConstrSet.app (fn (s,_) =>
	    if is_term s andalso is_nonterm s
		then errln (s ^ " is defined as a nonterminal and a terminal")
		else  ()) terms

	val both = ConstrSet.union(terms,nonterms)

	val _ =
	  let fun undef s =
	       if (is_term s orelse is_nonterm s) then ()
	       else (errln (s ^" is not defined as a terminal or nonterminal"))

	      fun undef' s =
	         if is_nonterm s then ()
	 	 else (errln (s ^ " is not defined as a nonterminal"))

	      fun check_rule {lhs,rhs,code,prec} = (map undef rhs; undef' lhs)

	  in map check_rule TRULELIST
	  end

	in if (!err_flag = false) then
		     (MLY_MAKE_PARSER.print_parser say;
		      reset_margin();
		      sayln ("structure " ^ 
			 	 (case structure'
				  of NONE => "C"
				   | SOME i => i) ^
			      " = ");
		      inc_margin();
		      sayln "struct";
		      inc_margin();

		(* print HDR structure *)

		      sayln "structure HDR =";
		      inc_margin();
		      sayln "struct";
		      say HEADER; 
		      dec_margin();
		      sayln "";
		      dec_margin();
		      sayln "end";

		(* print V structure *)

		      inc_margin();
		      sayln "structure V =";
		      inc_margin();
		      sayln "struct";
		      sayln "open HDR;";

		 (* printConstrSet returns true if it printed at least
		    one constructor *)

		      if (printConstrSet(both,"Value",true)=true)
			then say " | "
			else ();
		      sayln void;
		      dec_margin();
		      sayln "";
		      dec_margin(); 
		      sayln "end";

		(* print LexValue structure *)

		      inc_margin();
		      sayln "structure LexValue =";
		      inc_margin();
		      sayln "struct";
		      sayln "open HDR;";
		      printConstrSet(terms,"V",false);
		      dec_margin();
		      sayln "";
		      dec_margin();
		      sayln "end";

		 (* print G (the grammar) structure *)

		      inc_margin();
		      sayln "structure G =";
		      inc_margin();
		      sayln "struct";
sayln
"\tdatatype Terminal = T of int\n\
\\tand	    Nonterminal = NT of int\n\
\\n\
\\tval eqTerminal = fn ((T i),(T i')) => i = i'\n\
\\tval eqNonterminal = fn ((NT i),(NT i')) => i = i'";

		      saywd "datatype Symbol = TERM of Terminal | ";
		      sayln "NONTERM of Nonterminal";
		      newln();

		     let fun showSymbol(s,t,constr) =
		     	(sayln ("fun "^s^" t = ");
		    	 sayln "case t"; saywd "of ";
		    	 ConstrSet.app
		 	 (fn (nt,{num,...}) =>
				 ( say constr;
				  say (makestring num);
				  say " => ";
				  sayln ("\""^nt^"\"");
				  say " | "
				  )
			  ) t; 
			sayln "_ => \"bogus\"";
			newln)
		     in (showSymbol ("showTerminalClass",terms,"T ");
			 showSymbol ("showNonterminal",nonterms,"NT "))
		     end;

		     sayln "fun showTerminalValue t = showTerminalClass t";
		     newln();

		     sayln "type Lineno = HDR.Lineno";
		     sayln "val lineno = HDR.lineno";
		     sayln "val error = HDR.error";

		     saywd "datatype Attribute = ATTRIB of {lhs: Nonterminal,";
		     sayln "rhsLength: int, num: int }";
		     newln();
		     saywd "fun showAttribute(ATTRIB{lhs,...}) =" = 
		     sayln "showNonterminal lhs";
		     newln();
		     saywd "datatype Rule = RULE of {lhs:Nonterminal,";
		     saywd "rhs: Symbol list,";
		     saywd "attribute : Attribute,";
		     sayln "precedence : int option }";
		     newln() ;
		    saywd "val ErrTermList=";
		    ConstrSet.app
			 (fn (x,{ty=NONE,num}) =>
				if x=eof then ()
				else saywd ("(T " ^ (makestring num) ^ ")::")
			   | _ => ()) terms;
		    sayln "nil";

	      (* invoke Table generator *)

		let
		   exception TermNum
		   exception NontermNum
		   exception SymbolNum

		   val get_term_num = fn t => 
			case ConstrSet.find (t,dummy_data) terms
			of NONE => raise TermNum
			 | SOME (_,{num=num,...}) => num

		   val get_nonterm_num = fn nt => 
			case ConstrSet.find (nt,dummy_data) nonterms
			of NONE => raise NontermNum
			 | SOME (_,{num=num,...}) => num

		    val get_symbol = fn s =>
		        (G.TERM (G.T (get_term_num s)) handle TermNum =>
			    (G.NONTERM (G.NT (get_nonterm_num s)))
				 handle NontermNum => raise SymbolNum
			)

		    val numTerminals = ConstrSet.card terms
		    val numNonterminals = ConstrSet.card nonterms

		    val showTerminalClass =
			let val b = array(numTerminals,"bogus")
			    val f = fn (s,{num=num,...}:constr_data) =>
				update(b,num,s)
			    val _ = ConstrSet.app f terms
			in fn (G.T i) => ((b sub i) handle _ => "bogus")
			end

		    val showNonterminal =
			let val b = array(numNonterminals,"bogus")
			    val f = fn (s,{num=num,...}:constr_data) =>
				update(b,num,s)
			    val _ = ConstrSet.app f nonterms
			in fn (G.NT i) => ((b sub i) handle _ => "bogus")
			end

		   val _ =
		  (let val get_term_string = fn a =>
			"(T " ^ (makestring (get_term_num a)) ^ ")"
		   in print_is_keyword_func (map get_term_string keyword);
		      print_preferred_insert_func
				 (map get_term_string iprefer);
	 	      print_preferred_subst_func 
				(map (fn (a,b) => ((get_term_string a),
					           map get_term_string b))
				 prefer_map);
		      sayln ("val eof = " ^ (get_term_string eof))
		   end;
		   sayln ("val start = NT "^(makestring (get_nonterm_num start)));
		   dec_margin();
		   sayln "end"
		   )

		    val showTerminalValue = showTerminalClass

		    val showAttribute = fn (G.ATTRIB {lhs,rhsLength,num}) =>
			showNonterminal lhs

		    val eof = G.T (get_term_num eof)
		    val start = G.NT (get_nonterm_num start)

		   val termPrecedence =
			let val b = array(numTerminals,NONE)
			    val f = fn (tk,(j:int,p)) =>
			              let val prec =
					 SOME (case p of NONASSOC => j
						       | RIGHT => j+1
						       | LEFT => j-1)
				       in update(b,get_term_num tk,prec)
				       end
			     val _  = PrecSet.app f p
			in fn (G.T i) => b sub i
			end

		    val (_,rules) = 
		      List.fold (fn ({lhs=lhs,rhs=rhs,code=_,prec},(n,r)) =>
		      (let val newlhs = G.NT (get_nonterm_num lhs)
			   val newrhs = map get_symbol rhs
			   val newattrib =
				 G.ATTRIB {lhs=newlhs,
				           rhsLength=List.length rhs,
					    num = n
					  }
			    val newprec =
			     let fun f (a::b) =
 			           if (is_nonterm a) then f b else get_prec a
			           | f nil = NONE
			     in case prec
			         of NONE => f (rev rhs)
			          | SOME i => get_prec i
			     end
			in (n+1,(G.RULE {lhs=newlhs,rhs=newrhs,
					attribute=newattrib,
					precedence=newprec})::r)
			end
			)) TRULELIST (0,nil)

		in MakeTable.mktable (!out)
			{rules=rules,verbose=verbose,
			 eof = eof, start = start,
			termPrecedence = termPrecedence,
			showTerminalClass = showTerminalClass,
			showTerminalValue = showTerminalValue,
			showNonterminal = showNonterminal,
			showAttribute = showAttribute,
			numTerminals = numTerminals,
			numNonterminals = numNonterminals}
		end;

	      (* Print R structure - actions for rules *)

		    sayln "structure R = ";
		    inc_margin();
		    sayln "struct";
		    saywd "val ErrValList = ";
		    ConstrSet.app (fn (x,{ty=NONE,...}) =>
			if x=eof then ()
			else saywd ("(V." ^ void ^ ")::") | _ => ()) terms;
		    sayln "nil";

		    sayln "exception mlyRULE of int";
		    sayln "fun rule(i,vl) =";
		    sayln "let open HDR";
		    sayln "    val rule_array = arrayoflist(";

		    let fun getconstr s = 
			 case (get_type(s,terms,nonterms))
			    of NONE => NONE
			     | SOME _ => SOME ("V."^s)

(* prlist : print list of arguments for a function for a rule *)

		  fun prlist rhs =


(* f: list of symbols for rhs, list of pairs of (symbol,last # used)
   Takes a rhs symbol, finds the correct variable name (eg. the number
   to attach to it, and whether or not to have a variable w/o a number
   if the rhs symbol is used only once)
*)

		     let fun f(nil,l) = ()
		           | f(t::rhs,l) =

			let

			   fun g((e as (a,i))::b,r) =
			     if a=t then ((a,i+1)::(b@r),t^(makestring (i+1)))
			     else g(b,e::r)
			     | g(nil,r) =
				  if (List.exists (fn a=>a=t) rhs) then
				      ((t,1)::r,"("^t^"1)")
				  else ((t,1)::r,"("^t^" as "^t^"1)")

			   val (newlist,var) = g(l,nil)

		    	 in 

			   (f(rhs,newlist);
		 	    saywd ("(" ^
				   (case (getconstr t)
				     of NONE => "_"
				      | SOME i => ("(" ^ i ^ " " ^ var ^ ")")
				   ) ^
				  ")");
			    saywd "::"
			    )
			  end

		    in 
			(f(rhs,nil); saywd "r672")
		    end

		     in
		        List.fold (fn ({lhs,rhs,code,prec},n) =>
			          ( saywd "(fn (";
				   prlist rhs;
				   saywd ") => ((";

				   let val constr = getconstr lhs
				   in case constr
				       of SOME i  => saywd i
					| NONE => ();
				      saywd "(";
				      saywd code;
				      saywd ")";
				      case constr
					of SOME i => ()
				         | NONE => (saywd "; ";
						    saywd ("V."^void)
						   );
				      sayln "),r672)";
				      if (List.length rhs > 0) then
				        (saywd "         | _ => raise mlyRULE ";
				         saywd (makestring (n:int))
					)
				      else ();
				      sayln ")"
				    end; saywd "::"; n+1)) TRULELIST 0
		     end;
		     sayln "nil)";
		     dec_margin();
		     sayln "in (rule_array sub i) vl end";
		     dec_margin();
		     sayln "type Value = V.Value";
		     say "val VOID = V.";
		     sayln void;
		     sayln "end";
		     dec_margin();

	(* print P structure - contains parsing function *)

		     sayln "structure P = ParserGen(structure Lr_Table = Lr_Table";
		     dec_margin();
		     sayln "			    structure RuleAction = R)";
		     sayln "fun parse (lex : unit -> (LexValue.V)) i= ";
		     inc_margin();
		     saywd "(fn (";
		     case (ConstrSet.find (start,dummy_data) nonterms) of
			 SOME (_,{ty=SOME _,...})
			      => (saywd "V."; saywd start; sayln " a) => a)")
			  | _ => sayln "_) => ())";
		     sayln "(P.parse (fn () => ";
		     inc_margin();
		     sayln "(case lex() of ";
		     ConstrSet.setfold (fn ((x,{ty=constr,num}),result) =>
			(if result then () else saywd " | ";
			 say "(LexValue."; say x;
			 case constr
			   of NONE => (say ") => (G.T ";
				       say (makestring num);
				       sayln ("," ^ "V." ^ void ^")"))
			    | _ => (say " a) => (G.T ";
				    say (makestring num);
				    sayln ("," ^ "V." ^ x ^ " a)"));
			 false))
		     terms true;
		     dec_margin(); dec_margin();
		     sayln ")) i)";
		     sayln "end";
		     ()
		 )
	else ()
end handle SemanticError => ()
end)
end
