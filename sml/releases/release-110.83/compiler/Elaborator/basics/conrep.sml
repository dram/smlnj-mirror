(* Copyright 1996 by AT&T Bell Laboratories *)
(* conrep.sml *)

signature CONREP = 
sig

  val infer : bool -> (Symbol.symbol * bool * Types.ty) list
                   -> (Access.conrep list * Access.consig)

end (* signature CONREP *)


structure ConRep : CONREP =
struct

local open Access Types
in 

fun notconst(_,b,_) = not b

fun count l = foldl (fn (c,acc) => if notconst c then acc+1 else acc) 0 l

(* the first argument indicates whether this is a recursive datatype *)
fun infer false ([(_, false, CONty(_,[_,_]))]) = 
    ([UNTAGGED], CSIG(1,0)) (* [TRANSPARENT] *)
      (* The TRANSPARENT conrep is temporarily turned off;
         it should be working very soon. Ask zsh. *)

  | infer _ cons =
      let val multiple = (count cons) > 1

	  fun decide (ctag,vtag, (_,true,_)::rest, reps) = 
                if multiple andalso !ElabControl.boxedconstconreps
                then decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
                else decide(ctag+1, vtag, rest, (CONSTANT ctag) :: reps)
	    | decide (ctag,vtag, (_,false,CONty(_,[_,_]))::rest, reps) =
		if multiple
		then decide(ctag, vtag+1, rest, (TAGGED vtag) :: reps)
		else decide(ctag, vtag+1, rest, (UNTAGGED :: reps))
            | decide (_, _, _::_, _) =
	        ErrorMsg.impossible "Conrep: unexpected conrep-decide"
            | decide (ctag, vtag, [], reps) = (rev reps, CSIG(vtag,ctag))

       in decide(0, 0, cons, [])
      end

end (* local *)
end (* structure ConRep *)
