(* printutil.sml *)

structure PrintUtil = struct

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

fun printSym(s: Symbol.symbol) = (print(Symbol.Name s); ())
    (* fix -- maybe this belongs in Symbol *)

end
