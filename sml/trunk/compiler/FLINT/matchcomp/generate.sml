(* FLINT/matchcomp/generate.sml *)
				    
(* generate match "code" from decision tree and andor tree *)

(* The "code" for the match is generated from the decision tree and information from
 * the original andor (about record deconstruction and variable bindings (and types).
 * This code performs pattern dispatching and deconstruction and then invokes the appropriate
 * RHS expressions with the necessary variable bindings.
 *
 * The "code" (at least initially) will be a neutral minimal abstract code (perhaps similar
 * to the Lambda IR in the early compiler (FN, APP, SELECT, SWITCH, etc.).  Types and type
 * variables could be taken into account.  Goal is to see if the "code" language can be
 * replaced by (or translated into) Absyn (appropirately modified or augmented *).
 *)

structure MCGenerate =
struct

(* pseudocode *)
datatype code
  = FN          (* function abstraction (typed arg?) *)
  | APP         (* function application *)
  | SEL         (* record selection (positional) *)
  | SWITCHD     (* switch on datatype data constructor (representation) *)	     
  | SWITCHN     (* switch on a number (int, word, char) *)
  | SWITCHS     (* switch on a string *)
(*  | TFN       (* type abstraction *)
    | TAPP      (* type application *)
*)

(* How can this code representation be translated into Absyn? What new Absyn
 * forms are needed? *)

(* Alternatively, how can this code representation be translated into FLINT lambda? *)


end (* structure MCGenerate *)
