(* Copyright 1989 by AT&T Bell Laboratories *)
signature SYMBOL = sig
    type symbol
    datatype namespace =
       VARspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace
    val eq: symbol * symbol -> bool
    and symbolGt : symbol * symbol -> bool
    and varSymbol: string -> symbol
    and tycSymbol: string -> symbol
    and sigSymbol: string -> symbol
    and strSymbol: string -> symbol
    and fctSymbol: string -> symbol
    and fixSymbol: string -> symbol
    and labSymbol: string -> symbol
    and tyvSymbol: string -> symbol
    and var'n'fix : string -> symbol * symbol
    and name: symbol -> string
    and number: symbol -> int
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
end
