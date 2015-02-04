functor VaxPeephole(M : VAXCODER) : VAXCODER =

struct
   open M


val movl =
 fn args => case args
	     of (direct x, direct y) => if x=y then () else movl args
	    | (displace x, displace y) => if x=y then () else movl args
	    | (deferred x, deferred y) => if x=y then () else movl args
	    | _ => movl args

local
	datatype StackArg = Addr of EA | Val of EA | NoArg
	val stack : StackArg ref = ref NoArg
	val cuts = ref 0
in

fun stackref (direct(reg 14)) = true
  | stackref (autoinc(reg 14)) = true
  | stackref (autodec(reg 14)) = true
  | stackref (displace(_, reg 14)) = true
  | stackref (deferred(_, reg 14)) = true
  | stackref (index(ea, reg 14)) = true
  | stackref (index(ea, _)) = stackref ea
  | stackref _ = false 

fun adjust () = case !cuts of 0 => () 
			| n => (addl2(immed(n),direct sp); cuts := 0)

fun stackopt () =
	(adjust();
	 case stack of
	  ref NoArg => ()
	| ref (Addr source) => (moval(source,autodec sp); stack := NoArg)
	| ref (Val source) => (movl(source,autodec sp); stack := NoArg))

fun push ea = case stack of
		  ref NoArg => (if stackref(ea) then adjust() else ();
				stack := Val ea)
		| ref (Addr source) => (adjust();
				        pushal source;
					stack := Val ea)
		| ref (Val source) => (adjust();
				       movl(source,autodec sp);
				       stack := Val ea)
fun pusha ea =  case stack of
		  ref NoArg =>  (if stackref(ea) then stackopt() else ();
				stack := Addr ea)
		| ref (Addr source) => (adjust();
					pushal source;
					stack := Addr ea)
		| ref (Val source) => (adjust();
				       movl(source,autodec sp);
				       stack := Addr ea)
fun pop ea = case stack of
		  ref NoArg => (adjust(); movl(autoinc sp,ea))
		| ref (Addr source) => 
			(if stackref(ea) then (stackopt(); pop ea)
			   else (moval(source,ea); stack := NoArg))
		| ref (Val source) =>
		        (if stackref(ea) then (stackopt(); pop ea)
		         else (movl(source,ea); stack := NoArg))

val addl2 = fn (immed(n), direct(reg 14)) => cuts := !cuts + n
	     | args => (stackopt(); addl2 args)


end (* local *)


val align = fn arg => (stackopt(); align arg)
val mark = fn arg => (stackopt(); mark arg)
val define = fn arg => (stackopt(); define arg)
val emitstring = fn arg => (stackopt(); emitstring arg)
val emitlong = fn arg => (stackopt(); emitlong arg)

val jne = fn args => (stackopt(); jne args)
val jbr = fn args => (stackopt(); jbr args)
val bbc = fn args => (stackopt(); bbc args)
val bbs = fn args => (stackopt(); bbs args)

val movb = fn args => (stackopt(); movb args)
val movzbl = fn args => (stackopt(); movzbl args)
val movl = fn args => (stackopt(); movl args)
val moval = fn args => (stackopt(); moval args)
val pushal = fn args => (stackopt(); pushal args)
val rsb = fn args => (stackopt(); rsb args)
val cmpl = fn args => (stackopt(); cmpl args)
val addl3 = fn args => (stackopt(); addl3 args)
val subl2 = fn args => (stackopt(); subl2 args)
val subl3 = fn args => (stackopt(); subl3 args)
val ashl = fn args => (stackopt(); ashl args)
val mull2 = fn args => (stackopt(); mull2 args)
val divl3 = fn args => (stackopt(); divl3 args)
val divl2 = fn args => (stackopt(); divl2 args)
val jmp = fn args => (stackopt(); jmp args)
val brb = fn args => (stackopt(); brb args)
val brw = fn args => (stackopt(); brw args)
val beql = fn args => (stackopt(); beql args)
val bneq = fn args => (stackopt(); bneq args)
val bgeq = fn args => (stackopt(); bgeq args)
val bgtr = fn args => (stackopt(); bgtr args)
val blss = fn args => (stackopt(); blss args)
val bleq = fn args => (stackopt(); bleq args)

val movg = fn args => (stackopt(); movg args)
val mnegg = fn args => (stackopt(); mnegg args)
val addg3 = fn args => (stackopt(); addg3 args)
val subg3 = fn args => (stackopt(); subg3 args)
val mulg3 = fn args => (stackopt(); mulg3 args)
val divg3 = fn args => (stackopt(); divg3 args)
val cmpg = fn args => (stackopt(); cmpg args)


end
