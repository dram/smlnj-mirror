(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log$
 * Revision 1.3  2000/06/06 02:14:55  blume
 * merging changes from devel branch; new boot files
 *
 * Revision 1.1.1.10.2.1  2000/06/02 08:11:06  blume
 * added several appendices to CM manual;
 * merged recent changes to main trunk into devel branch
 *
 * Revision 1.2  2000/06/01 18:33:44  monnier
 * bring revisions from the vendor branch to the trunk
 *
 * Revision 1.1.1.10  1999/04/17 18:56:11  monnier
 * version 110.16
 *
 * Revision 1.1.1.1  1997/01/14 01:38:05  george
 *   Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:44  george
 * Version 109
 * 
 *)

signature ABSYN =
    sig
       datatype exp = EVAR of string
                    | EAPP of exp * exp
                    | ETUPLE of exp list
                    | EINT of int
                    | FN of pat * exp
                    | LET of decl list * exp
                    | UNIT
                    | SEQ of exp * exp
                    | CODE of string
       and      pat = PVAR of string
                    | PAPP of string * pat
                    | PTUPLE of pat list
                    | PLIST of pat list
                    | PINT of int
                    | WILD
                    | AS of pat * pat
       and     decl = VB of pat * exp
       and     rule = RULE of pat * exp
       val printRule : ((string -> unit) * (string -> unit)) -> rule -> unit
    end
