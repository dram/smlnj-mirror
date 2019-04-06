(* Copyright 1989 by AT&T Bell Laboratories *)
signature CPS = sig

type lvar
exception Ctable

datatype value = VAR of lvar
	       | LABEL of lvar
	       | INT of int 
	       | REAL of string 
	       | STRING of string
	       | OBJECT of System.Unsafe.object

datatype accesspath = OFFp of int | SELp of int * accesspath

datatype cexp = RECORD of (value * accesspath) list * lvar * cexp
	      | SELECT of int * value * lvar * cexp
	      | OFFSET of int * value * lvar * cexp
	      | APP of value * value list
	      | FIX of (lvar * lvar list * cexp) list * cexp
	      | SWITCH of value * cexp list
	      | PRIMOP of Access.primop * value list * lvar list * cexp list
type function
val combinepaths : accesspath * accesspath -> accesspath
val lenp : accesspath -> int

end

structure CPS : CPS = struct

type lvar = int
exception Ctable

datatype value = VAR of lvar
	       | LABEL of lvar
	       | INT of int 
	       | REAL of string 
	       | STRING of string
	       | OBJECT of System.Unsafe.object

datatype accesspath = OFFp of int | SELp of int * accesspath

datatype cexp = RECORD of (value * accesspath) list * lvar * cexp
	      | SELECT of int * value * lvar * cexp
	      | OFFSET of int * value * lvar * cexp
	      | APP of value * value list
	      | FIX of (lvar * lvar list * cexp) list * cexp
	      | SWITCH of value * cexp list
	      | PRIMOP of Access.primop * value list * lvar list * cexp list
withtype function = lvar * lvar list * cexp

fun combinepaths(p,OFFp 0) = p
  | combinepaths(p,q) = 
    let val rec comb =
	fn (OFFp 0) => q
	 | (OFFp i) => (case q of
		          (OFFp j) => OFFp(i+j)
		        | (SELp(j,p)) => SELp(i+j,p))
	 | (SELp(i,p)) => SELp(i,comb p)
    in comb p
    end

fun lenp(OFFp _) = 0
  | lenp(SELp(_,p)) = 1 + lenp p

end
