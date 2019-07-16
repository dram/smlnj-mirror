(* Copyright 1989 by AT&T Bell Laboratories *)
signature SYMBOL = sig
    type symbol
    datatype namespace =
       VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace | FSIGspace
    val eq: symbol * symbol -> bool
    and symbolGt : symbol * symbol -> bool
    and symbolCMLt : symbol * symbol -> bool
    and varSymbol: string -> symbol
    and tycSymbol: string -> symbol
    and sigSymbol: string -> symbol
    and strSymbol: string -> symbol
    and fctSymbol: string -> symbol
    and fsigSymbol: string -> symbol
    and fixSymbol: string -> symbol
    and labSymbol: string -> symbol
    and tyvSymbol: string -> symbol
    and var'n'fix : string -> symbol * symbol
    and name: symbol -> string
    and number: symbol -> int
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val symbolToString : symbol -> string

(* Probably should merge STRspace and FCTspace into one namespace.
   Similarly for SIGspace and FSIGspace. *)

end

(*
 * $Log: symbol.sig,v $
 * Revision 1.1.1.1  1999/12/03 19:59:36  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:11  george
 *   Version 109.24
 *
 *)
