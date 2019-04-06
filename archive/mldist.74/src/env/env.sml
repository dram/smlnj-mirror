(* Copyright 1989 by AT&T Bell Laboratories *)
(* env.sml *)

structure Env : ENV =
struct

  structure Symbol = 
  struct
    datatype symbol = SYMBOL of int * string
    datatype namespace =
       VARspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace
    fun eq(SYMBOL(a1,b1),SYMBOL(a2,b2)) = a1=a2 andalso b1=b2
    fun symbolGt(SYMBOL(_,s1), SYMBOL(_,s2)) = s1 > s2
    fun varSymbol (name: string) =
	SYMBOL(StrgHash.hashString name,name)
    fun tycSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 1, name)
    fun sigSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 2, name)
    fun strSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 3, name)
    fun fctSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 4, name)
    fun fixSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 5, name)
    fun labSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 6, name)
    fun tyvSymbol (name: string) =
	SYMBOL(StrgHash.hashString name + 7, name)
    fun var'n'fix name =
        let val h = StrgHash.hashString name
	 in (SYMBOL(h,name),SYMBOL(h+5,name))
	end
    fun name (SYMBOL(_,name)) = name
    fun number (SYMBOL(number,_)) = number
    fun nameSpace (SYMBOL(number,name)) : namespace =
	case number - StrgHash.hashString name
	  of 0 => VARspace
           | 1 => TYCspace
           | 2 => SIGspace
           | 3 => STRspace
           | 4 => FCTspace
           | 5 => FIXspace
           | 6 => LABspace
           | 7 => TYVspace
	   | _ => ErrorMsg.impossible "Symbol.nameSpace"
    fun nameSpaceToString (n : namespace) : string =
         case n
         of VARspace => "variable or constructor"
          | TYCspace => "type constructor"
          | SIGspace => "signature"
          | STRspace => "structure"
          | FCTspace => "functor"
          | FIXspace => "fixity"
          | LABspace => "label"
	  | TYVspace => "type variable"
  end

  (* representation of environments *)
  (* 'b will always be instantiated to Basics.binding *)
  datatype 'b env
    = EMPTY
    | BIND of int * string * 'b * 'b env
    | TABLE of 'b IntStrMap.intstrmap * 'b env
    | OPEN of 'b env * ('b->'b) * 'b env
    | SPECIAL of (Symbol.symbol -> 'b) * 'b env  (* for debugger *)

  exception Unbound = System.Unsafe.Assembly.UnboundTable

  exception SpecialEnv 
    (* raised by app when it encounters a SPECIAL env *)

  val empty = EMPTY

  fun look (env,sym as Symbol.SYMBOL(is as (i,s))) = 
    let fun f EMPTY = raise Unbound
          | f (BIND(i',s',b,n)) =
	       if i = i' andalso s = s' then b else f n
          | f (TABLE(t,n)) = (IntStrMap.map t is handle Unbound => f n)
          | f (OPEN(e1,g,n)) = (g(look(e1,sym)) handle Unbound => f n)
          | f (SPECIAL(g,n)) = (g sym handle Unbound => f n)
    in f env
    end

  fun bind (Symbol.SYMBOL(i,s),binding,env) = BIND (i,s,binding,env)
  val open' = OPEN
  val special = SPECIAL

  infix atop
  fun EMPTY atop e = e
    | (BIND(i,s,b,n)) atop e = BIND(i,s,b,n atop e)
    | (TABLE(t,n)) atop e = TABLE(t,n atop e)
    | (OPEN(e1,g,n)) atop e = OPEN(e1,g, n atop e)
    | (SPECIAL(g,n)) atop e = SPECIAL(g, n atop e)
 
  fun app f =
    let fun g (BIND(i,s,b,n)) = (g n; f (Symbol.SYMBOL(i,s),b))
          | g (TABLE(t,n)) =
	      (g n; IntStrMap.app (fn (i,s,b) => f(Symbol.SYMBOL(i,s),b)) t)
          | g (OPEN(e1,h,n)) = (g n; app (fn (s,b) => f(s, h b)) e1)
	  | g (SPECIAL _) = raise SpecialEnv
	  | g (EMPTY) = ()
     in g
    end

  fun map f (TABLE(t,EMPTY)) =  (* optimized case *)
        TABLE(IntStrMap.transform f t, EMPTY)
    | map f e =
	let val t = IntStrMap.new(10,Unbound)
	 in app (fn (Symbol.SYMBOL(i,s),b) => IntStrMap.add t (i,s,f b)) e;
	    TABLE(t,EMPTY)
	end

  fun consolidate e =
      let open Symbol
	  fun f (BIND(i,s,b,n),c) =
	      let val t = f(n,c+1)
	       in IntStrMap.add t (i,s,b);
		  t
	      end
	    | f (TABLE(t,n),c) =
	      let val t' = f(n, c + IntStrMap.elems t)
	       in IntStrMap.app (IntStrMap.add t') t;
		  t'
	      end
	    | f (OPEN(e as TABLE(t,EMPTY),g,n),c) = 
	      let val t' = f(n,c+IntStrMap.elems t)
	       in IntStrMap.app (fn (i,s,b) => IntStrMap.add t' (i,s,g b)) t;
		  t'
	      end
	    | f (OPEN(e,g,n),c) = 
	      let val t = f(n,c+10 (*bogus, but just a hint*))
	       in app (fn (Symbol.SYMBOL(i,s), b) =>
			  IntStrMap.add t (i, s, g b))
		      e;
		  t
	      end
	    | f (SPECIAL(g,n),c) = raise SpecialEnv
	    | f (EMPTY,c) = IntStrMap.new (c,Unbound)
       in TABLE(f(e,0),EMPTY) handle SpecialEnv => e
      end

end (* structure Env *)

structure Symbol = Env.Symbol
