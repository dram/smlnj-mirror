signature CPS = sig

type lvar
exception Ctable

datatype const = INTconst of int | REALconst of string | STRINGconst of string

datatype accesspath = OFFp of int | SELp of int * accesspath

datatype cexp = RECORD of (lvar * accesspath) list * lvar * cexp
	      | SELECT of int * lvar * lvar * cexp
	      | OFFSET of int * lvar * lvar * cexp
	      | APP of lvar * lvar list
	      | FIX of (lvar * lvar list * cexp) list * cexp
	      | SWITCH of lvar * cexp list
	      | PRIMOP of Access.primop * lvar list * lvar list * cexp list
type function
val recordpath : lvar list -> (lvar * accesspath) list
val combinepaths : accesspath * accesspath -> accesspath
val lenp : accesspath -> int

end

structure CPS : CPS = struct

type lvar = int
exception Ctable

datatype const = INTconst of int | REALconst of string | STRINGconst of string

datatype accesspath = OFFp of int | SELp of int * accesspath

datatype cexp
  = RECORD of (lvar * accesspath) list * lvar * cexp
  | SELECT of int * lvar * lvar * cexp
  | OFFSET of int * lvar * lvar * cexp
  | APP of lvar * lvar list
  | FIX of function list * cexp
  | SWITCH of lvar * cexp list
  | PRIMOP of Access.primop * lvar list * lvar list * cexp list
withtype function = lvar * lvar list * cexp

fun recordpath l = map (fn v => (v,OFFp 0)) l
val rec combinepaths =
  fn (OFFp i,OFFp j) => OFFp(i+j)
   | (OFFp i,SELp(j,p)) => SELp(i+j,p)
   | (SELp(i,p),path) => SELp(i,combinepaths(p,path))
fun lenp(OFFp _) = 0
  | lenp(SELp(_,p)) = 1 + lenp p

end
