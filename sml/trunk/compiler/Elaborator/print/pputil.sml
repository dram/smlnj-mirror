(* Copyright 2003 by The SML/NJ Fellowship *)
(* basics/pputil.sml *)

structure PPUtil : PPUTIL =
struct

  structure S : SYMBOL = Symbol
  structure SS = SpecialSymbols
  structure PP = PrettyPrint
  structure IP = InvPath
  structure SP = SymPath

  val pps = PP.string

  datatype break_style = CONSISTENT | INCONSISTENT

  fun openStyleBox style = 
      case style
        of CONSISTENT => PP.openHVBox
         | INCONSISTENT => PP.openHOVBox

  fun ppSequence0 ppstream (sep:PP.stream->unit,pr,elems) =
      let fun prElems [el] = pr ppstream el
	    | prElems (el::rest) =
	        (pr ppstream el;
		 sep ppstream;
                 prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun ppSequence ppstream {sep:PP.stream->unit, pr:PP.stream->'a->unit, 
                           style:break_style} (elems: 'a list) =
      (openStyleBox style ppstream (PP.Rel 0);
       ppSequence0 ppstream (sep,pr,elems);
       PP.closeBox ppstream)

  fun ppClosedSequence ppstream{front:PP.stream->unit,sep:PP.stream->unit,
                                back:PP.stream->unit,pr:PP.stream->'a->unit,
                                style:break_style} (elems:'a list) =
      (PP.openHVBox ppstream (PP.Rel 1);
       front ppstream;
       openStyleBox style ppstream (PP.Rel 0);
       ppSequence0 ppstream (sep,pr,elems); 
       PP.closeBox ppstream;
       back ppstream;
       PP.closeBox ppstream)

  fun ppSym ppstream (s:S.symbol) = PP.string ppstream (S.name s)

  fun ppString ppstream = PP.string ppstream o PrintUtil.formatString

  fun ppvseq ppstream ind (sep:string) pr_elem elems =
      let fun prElems [el] = pr_elem ppstream el
	    | prElems (el::rest) = (pr_elem ppstream el; 
                                    PP.string ppstream sep; 
				    PP.cut ppstream;
                                    prElems rest)
	    | prElems [] = ()
       in PP.openVBox ppstream (PP.Abs ind);
	   PP.cut ppstream;
           prElems elems;
          PP.closeBox ppstream
      end

  fun ppvlist ppstrm (header,separator,pr_elem,elems) =
      case elems
	of nil => ()
	 | first::rest =>
	     (PP.string ppstrm header;
	      pr_elem ppstrm first;
	      app (fn x => (PP.cut ppstrm;
			    PP.string ppstrm separator;
			    pr_elem ppstrm x))
		   rest)

  fun ppvlist' ppstrm (header,separator,pr_elem,elems) =
      case elems
	of nil => ()
	 | first::rest =>
	     (pr_elem ppstrm header first;
	      app (fn x => (PP.newline ppstrm;
			    pr_elem ppstrm separator x))
		   rest)

  (* debug print functions *)
  fun ppIntPath ppstream =
      ppClosedSequence ppstream 
	{front=(fn pps => PP.string pps "["),
	 sep=(fn pps => (PP.string pps ","; PP.break pps {nsp=0,offset=0})),
	 back=(fn pps => PP.string pps "]"),
	 style=INCONSISTENT,
	 pr=(fn pps => PP.string pps o Int.toString)}

  fun ppSymPath ppstream (path: SymPath.path) = 
      PP.string ppstream (SymPath.toString path)

  fun ppInvPath ppstream (rpath: InvPath.path) =
      PP.string ppstream (InvPath.toString rpath)

  (* findPath:  convert inverse symbolic path names to a printable string in the
    context of an environment.

    Its arguments are the inverse symbolic path, a check predicate on static
    semantic values, and a lookup function mapping paths to their bindings
    (if any) in an environment and raising Env.Unbound on paths with no
    binding.

    It looks up each suffix of the path name, going from shortest to longest
    suffix, in the current environment until it finds one whose lookup value
    satisfies the check predicate.  It then converts that suffix to a string.
    If it doesn't find any suffix, the full path (reversed, i.e. in the 
    normal order) and the boolean value false are returned, otherwise the
    suffix and true are returned.

    Example:
	   Given A.B.t as a path, and a lookup function for an
	   environment, this function tries:
		     t
		     B.t
		     A.B.t
	   If none of these work, it returns ?.A.B.t

    Note: the symbolic path is passed in reverse order because that is
    the way all symbolic path names are stored within static semantic objects.
   *)

  fun findPath (IP.IPATH p: IP.path, check, look): (S.symbol list * bool) =
      let fun try(name::untried,tried) =
	        (if (S.eq(name,SS.resultId)) orelse (S.eq(name,SS.returnId)) 
		 then try(untried,tried)
		 else
		   let val elem = look(SP.SPATH(name :: tried))
		    in if check elem
		       then (name::tried,true)
		       else try(untried,name::tried)
		   end handle StaticEnv.Unbound => try(untried,name::tried))
	    | try([],tried) = (tried, false)
       in try(p,[])
      end

  fun ppi ppstrm (i:int) = pps ppstrm (Int.toString i)

  fun ppcomma ppstrm = pps ppstrm ","

  fun ppcomma_nl ppstrm  = (ppcomma ppstrm; PP.newline ppstrm)

  fun nl_indent ppstrm i =
      PP.break ppstrm {nsp=1000,offset=i}

  fun nl_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.newline ppstrm; g rst)
       in g
      end

  fun br_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.break ppstrm {nsp=1,offset=0}; g rst)
       in g
      end

  fun en_pp ppstrm =
      {openVBox = (fn indent => PP.openVBox ppstrm (PP.Abs indent)),  (* vertical *)
       openHVBox = (fn indent => PP.openHVBox ppstrm (PP.Abs indent)),  (* CONSISTENT *)
       openHOVBox = (fn indent => PP.openHOVBox ppstrm (PP.Abs indent)),  (* INCONSISTENT *)
       closeBox = fn () => PP.closeBox ppstrm,
       pps = pps ppstrm,
       ppi = ppi ppstrm,
       break = fn nsp_offset => PP.break ppstrm nsp_offset,
       newline = fn () => PP.newline ppstrm};

  fun ppArray ppstrm (f:PP.stream -> 'a -> unit, a:'a array) =
      let val {openHVBox,openHOVBox,pps,break,closeBox,...} = en_pp ppstrm
	  fun loop i = 
	      let val elem = Array.sub(a,i)
	       in pps (Int.toString i);
		  pps ": "; 
		  f ppstrm elem;
		  break {nsp=1,offset=0};
		  loop (i+1)
	      end
       in openHOVBox 0;
	  loop 0 handle General.Subscript => ();
	  closeBox()
      end

  fun C f x y = f y x;

  fun ppTuple ppstrm f =
      ppClosedSequence ppstrm 
	{front=C pps "(",
	 sep=fn ppstrm => (pps ppstrm ","; PP.break ppstrm {nsp=0,offset=0}),
	 back=C pps ")",
	 pr=f, style=INCONSISTENT}


end (* structure PPUtil *)
