
functor M68Peephole(M : M68CODER) : M68CODER = 
struct 
    open M

(* push/pop opts:  comment out the rest of the file to disable *)
local
	datatype StackArg = Addr of EA | Val of EA | NoArg
	val stack : StackArg ref = ref NoArg
	val cuts = ref 0
in

val stackref = fn Direct (AddrReg 7) => true
		| PostInc (AddrReg 7) => true
	        | PreDec (AddrReg 7) => true
		| Displace (AddrReg 7, _) => true
		| Index (AddrReg 7, _, _, _) => true
		| Index (_, _, AddrReg 7, _) => true
		| _ => false

fun adjust () = case !cuts of 0 => () 
			| n => (addl(Immed(n),Direct sp); cuts := 0)

fun stackopt () =
        (adjust(); 
	 case stack of
          ref NoArg => ()
        | ref (Val source) => (movl(source,PreDec sp); stack := NoArg)
        | ref (Addr source) => (pea source; stack := NoArg))

fun push ea = case stack of
                  ref NoArg => (if stackref(ea) then adjust() else ();
				stack := Val ea)
                | ref (Val source) => (adjust(); movl(source,PreDec sp);
                                    stack := Val ea)
                | ref (Addr source) => (adjust(); pea source;
                                    stack := Val ea)

fun pop ea = case (!stack,ea) of
    (NoArg,_) => (adjust(); movl(PostInc sp,ea))
  | (Val(source as Direct x),Direct y) => 
		       if stackref(ea) then (stackopt(); pop ea)
			   else if x=y then stack := NoArg
			   else (movl(source,ea); stack := NoArg)
  | (Addr source,Direct(AddrReg _)) => 
		       if stackref(ea) then (stackopt(); pop ea)
			   else (lea (source,ea); stack := NoArg)
  | (Addr source,_) => if stackref(ea) then (stackopt(); pop ea)
			   else (pea source; movl(PostInc sp,ea);
			         stack := NoArg)
  | (Val source,_) => if stackref(ea) then (stackopt(); pop ea)
		       else (movl(source,ea); stack := NoArg)

fun pusha ea = case stack of
		  ref NoArg => (if stackref(ea) then stackopt() else ();
				stack := Addr ea)
		| ref (Addr source) => (adjust(); pea source;
					stack := Addr ea)
		| ref (Val source) => (adjust(); movl(source,PreDec sp);
				       stack := Addr ea)

val addl = fn (Immed(n), Direct(AddrReg 7)) => cuts := !cuts + n
	     | args => (stackopt(); addl args)

end (* local *)


val align = fn arg => (stackopt(); align arg)
val setmark = fn arg => (stackopt(); setmark arg)
val define = fn arg => (stackopt(); define arg)
val emitstring = fn arg => (stackopt(); emitstring arg)

val rts = fn arg => (stackopt(); rts arg)
val exg = fn arg => (stackopt(); exg arg)
val movl = fn arg => (stackopt(); movl arg)
val subl = fn arg => (stackopt(); subl arg)
val cmpl = fn arg => (stackopt(); cmpl arg)
val btst = fn arg => (stackopt(); btst arg)
val pea = fn arg => (stackopt(); pea arg)
val lea = fn arg => (stackopt(); lea arg)
val jne = fn arg => (stackopt(); jne arg)
val jeq = fn arg => (stackopt(); jeq arg)
val jra = fn arg => (stackopt(); jra arg)
val jbsr = fn arg => (stackopt(); jbsr arg)
val jgt = fn arg => (stackopt(); jgt arg)
val jge = fn arg => (stackopt(); jge arg)
val jlt = fn arg => (stackopt(); jlt arg)
val jle = fn arg => (stackopt(); jle arg)
val asrl = fn arg => (stackopt(); asrl arg)
val asll = fn arg => (stackopt(); asll arg)
val divl = fn arg => (stackopt(); divl arg)
val mull = fn arg => (stackopt(); mull arg)
val movb = fn arg => (stackopt(); movb arg)

val fjne = fn arg => (stackopt(); fjne arg)
val fjeq = fn arg => (stackopt(); fjeq arg)
val fjgt = fn arg => (stackopt(); fjgt arg)
val fjge = fn arg => (stackopt(); fjge arg)
val fjlt = fn arg => (stackopt(); fjlt arg)
val fjle = fn arg => (stackopt(); fjle arg)
val fcmpd = fn arg => (stackopt(); fcmpd arg)
val faddd = fn arg => (stackopt(); faddd arg)
val fsubd = fn arg => (stackopt(); fsubd arg)
val fmuld = fn arg => (stackopt(); fmuld arg)
val fdivd = fn arg => (stackopt(); fdivd arg)
val fnegd = fn arg => (stackopt(); fnegd arg)
val fmoved = fn arg => (stackopt(); fmoved arg)

val trapv = fn arg => (stackopt(); trapv arg)

end
