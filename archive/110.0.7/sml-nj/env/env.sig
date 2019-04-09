(* Copyright 1996 by AT&T Bell Laboratories *)
(* env.sig *)

signature ENV =
sig
  structure Symbol : SYMBOL

  structure FastSymbol :
    sig
      type raw_symbol
      type symbol
      val rawSymbol: int * string -> raw_symbol
      val sameSpaceSymbol : symbol -> raw_symbol -> symbol
      val varSymbol: raw_symbol -> symbol
      val tycSymbol: raw_symbol -> symbol
      val sigSymbol: raw_symbol -> symbol
      val strSymbol: raw_symbol -> symbol
      val fctSymbol: raw_symbol -> symbol
      val fixSymbol: raw_symbol -> symbol
      val labSymbol: raw_symbol -> symbol
      val tyvSymbol: raw_symbol -> symbol
      val fsigSymbol: raw_symbol -> symbol
      val var'n'fix : raw_symbol -> symbol * symbol
    end

  type 'b env

  exception Unbound  
  exception SpecialEnv

  val empty: 'b env
  val look: 'b env * Symbol.symbol -> 'b
  val bind: Symbol.symbol * 'b * 'b env -> 'b env

  val special: (Symbol.symbol -> '_b) * (unit -> Symbol.symbol list) -> '_b env
      (* Note: special(f,NONE) means Don't Memoize! *)

  val atop: 'b env * 'b env -> 'b env
      (* atop(e1,e2): place e1 on top of e2 *)

  val consolidate: 'b env -> 'b env
  val consolidateLazy: 'b env -> 'b env
  val app: (Symbol.symbol * 'b -> unit) -> 'b env -> unit
  val map: ('1b -> '1b) -> '1b env -> '1b env
  val fold: ((Symbol.symbol * 'b) * 'a -> 'a) -> 'a -> 'b env -> 'a

  val symbols : 'b env -> Symbol.symbol list 
                                (* may contain duplicate symbols *)

end (* signature ENV *)

(*
 * $Log: env.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:45  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/08/15 20:38:33  dbm
 *   Added new consolidateLazy function.  Used in Environ.concatEnv and
 *   intended to cut down on overhead from consolidate in interactive
 *   top-level loop.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
