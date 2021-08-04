(* FLINT/match/paths.sml *)

structure Paths =
struct

local
  structure T = Types
  structure TU = TypesUtil
in

(* --------------------------------------------------------------------------- *)
(* con: case discriminators *)

(* con translates direclty to PLambda.con, except for VLENcon (n,t), which is translated
 *  to PLambda.INTcon n (in Generate.generate). *)
datatype con
  = DATAcon of T.datacon * T.tyvar list
  | INTcon of int IntConst.t
  | WORDcon of int IntConst.t
  | STRINGcon of string
  | VLENcon of int * T.ty
     (* element type ty is used in VLENcon case of Generate..genSwitch, then
      * eliminated by translation to INTcon in transVecCase *)

datatype link
  = PI of int  (* indexed record/tuple selection *)
  | VI of int * Types.ty (* indexed vector selection, with vector element type *)
  | DC of con  (* datacon/constant/vector-length discriminant *)

type path = link list   (* from root down to node *)
type rpath = link list  (* from node back to root *)

(* eqCon : con * con -> bool *)
fun eqCon (DATAcon (d1, _), DATAcon (d2, _)) = TU.eqDatacon (d1, d2)
  | eqCon (INTcon n, INTcon n') = (#ival n = #ival n')   (* types assumed compatible *)
  | eqCon (WORDcon n, WORDcon n') = (#ival n = #ival n') (* types assumed compatible *)
  | eqCon (STRINGcon s, STRINGcon s') = (s = s')
  | eqCon (VLENcon (n, _), VLENcon (n', _)) = (n = n')   (* types assumed compatible *)
  | eqCon _ = false

(* eqLink : link * link -> bool *)
fun eqLink (PI n1, PI n2) = (n1 = n2)
  | eqLink (VI (n1,_), VI (n2,_)) = (n1 = n2)  (* types assumed to agree *)
  | eqLink (DC con1, DC con2) = eqCon (con1, con2)
  | eqLink _ = false

val rootpath: path = nil
val rootrpath: rpath = nil

(* pathToList : path -> link list *)
fun pathToList (p: path) = p

(* pathToRpath : path -> rpath *)
fun pathToRpath p = rev p

(* rpathToPath : rpath -> path *)
fun rpathToPath rp = rev rp

(* pathLength : path -> int *)
fun pathLength (p: path) = length p

(* addLink : link * path -> path *)
fun addLink (link: link, path: path) = 
    path @ [link]

(* addLinkR : link * path -> path *)
fun addLinkR (link, rpath) = 
    link::rpath

fun eqPath (nil, nil) = true
  | eqPath (link1::rest1, link2::rest2) = 
      eqLink (link1, link2) andalso eqPath (rest1, rest2)
  | eqPath _ = false

(* prefix : path * path -> bool *)
fun prefix (nil, _) = true
  | prefix (_, nil) = false
  | prefix (link1::rest1, link2::rest2) = 
    eqLink (link1, link2) andalso prefix (rest1, rest2)

(* suffix : path * path -> link list option *)
fun suffix (path1: path, path2: path) =
    let fun strip (nil, path) = SOME (pathToList path)
	  | strip (_, nil) = NONE
	  | strip (link1::rest1, link2::rest2) = 
	      if eqLink (link1, link2)
	      then strip (rest1, rest2)
	      else NONE
    in strip (path1, path2)
    end				 

(* utility functions for printing (conversion to strings) *)

(* conToSTring : con -> string *)
fun conToString (DATAcon (dcon,_)) = Symbol.name(TU.dataconName dcon)
  | conToString (VLENcon (n, _)) = "L" ^ (Int.toString n)
  | conToString (INTcon {ival,ty}) = "I" ^ (IntInf.toString ival)
  | conToString (WORDcon {ival,ty}) = "W" ^ (IntInf.toString ival)
  | conToString (STRINGcon s) = "S:" ^ s

(* linkToString : link -> string *)
fun linkToString (PI n) = "PI:" ^ Int.toString n
  | linkToString (VI (n,_)) = "VI:" ^ Int.toString n
  | linkToString (DC con) = "DC[" ^ conToString con ^ "]"

(* pathToString : path -> string *)
fun pathToString (path: path) =
    PrintUtil.listToString ("[", ",", "]") linkToString path


end (* top local *)
end (* structure Paths *)
