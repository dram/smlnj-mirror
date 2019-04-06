DUMMY  =
struct
   val dummy = int
   val dummy2 = int
   val dummy3 = int
   val dummy4 = int
end

ERRORMSG =				(* forgot signature *)
 sig
    exception Syntax
    val anyErrors : bool ref
    val lineNum : int ref
    val fileName : string ref
    val debugging : bool ref
    val say  : string -> unit
    val warn  string -> unit		(* forgot : *)
    val complain : string -> unit
    val condemn : string -> 'a
    val impossible : string -> 'a
    val debugmsg : string -> bool
    flaggedmsg : bool ref -> string -> bool	(* forgot val *)
 end
