(* printutil.sml *)
signature PRINTUTIL =
sig
  structure Symbol : SYMBOL
  val newline : unit -> unit
  val prstr : string -> unit
  val tab : int -> unit
  val printSequence : string -> ('a -> unit) -> 'a list -> unit
  val printClosedSequence : (string*string*string) -> ('a -> unit) ->
					 'a list -> unit
  val printSym : Symbol.symbol -> unit
  val mlstr : string -> string
  val pr_mlstr : string -> unit
  val nlindent : int -> unit
  val printvseq : int -> string -> ('a -> unit) -> 'a list -> unit
end

structure PrintUtil : PRINTUTIL = struct

  structure Symbol : SYMBOL = Symbol

  fun prstr (s:string) : unit = (print s; ())

  fun newline () : unit = (print "\n"; ())

  fun tab (n: int): unit =
      if n=0
	then ()
	else (prstr " "; tab(n-1))

  fun printSequence (separator: string) (pr:'a->unit) (elems: 'a list) : unit =
      let fun prElems [el] = pr el
	    | prElems (el::rest) =
		(pr el; prstr separator; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printClosedSequence (front: string, sep: string, back:string) (pr:'a -> unit)
			  (elems : 'a list) : unit =
      (prstr front; printSequence sep pr elems; prstr back)

  fun printSym(s: Symbol.symbol) = (print(Symbol.name s); ())
      (* fix -- maybe this belongs in Symbol *)

  val stringDepth = System.Control.Print.stringDepth
  local
  fun decimal i = let val m = Integer.makestring
		  in  m(i div 100)^m((i div 10)mod 10)^m(i mod 10) end
  val ctrl_a = 1
  val ctrl_z = 26
  val offset = ord "A" - ctrl_a
  val smallestprintable = ord " "
  val biggestprintable = ord "~"
  fun ml_char "\n" = "\\n"
    | ml_char "\t" = "\\t"
    | ml_char "\\" = "\\\\"
    | ml_char "\"" = "\\\""
    | ml_char c =
	  let val char = ord c
	  in  if char >= ctrl_a andalso char <= ctrl_z
	      then "\\^" ^ chr(char+offset)
	      else if char >= smallestprintable andalso char <= biggestprintable
		   then c
	      else "\\" ^ decimal char
	  end
  in
      fun mlstr s = "\"" ^ implode(map ml_char (explode s)) ^ "\""
      fun pr_mlstr s =
	  let val depth = !stringDepth
	      fun pr i =
		  if i=depth then (print "#"; ())
		  else (let val ch = substring(s,i,1)
			in  print(ml_char ch);
			    pr (i+1)
			end handle Substring => ())
	  in  print "\"";
	      pr 0;
	      print "\"";
	      ()
	  end

    fun nlindent n = (newline(); tab n)

    fun printvseq ind sep pr elems =
	let fun prElems [el] = pr el
	      | prElems (el::rest) =
		  (pr el; nlindent ind; prstr sep; prElems rest)
	      | prElems [] = ()
	 in prElems elems
	end

  end (* local *)

end (* structure PrintUtil *)
