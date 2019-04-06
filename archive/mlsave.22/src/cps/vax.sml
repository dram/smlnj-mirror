functor VaxCM(V : VAXCODER) : CMACHINE = struct

structure V' : sig datatype Register = reg of int

		   eqtype Label sharing type Label = V.Label
		   datatype EA = direct of Register
			    | autoinc of Register
			    | autodec of Register
			    | displace of int * Register
			    | deferred of int * Register
			    | immed of int
			    | immedlab of Label
			    | address of Label
			    | index of EA * Register
		end = V
open V'

fun defer(direct r) = displace(0,r)
  | defer(displace z) = deferred z
  | defer(immedlab lab) = address lab
  | defer _ = ErrorMsg.impossible "defer in cpsvax"

val sp' = reg 14
val exnptr = direct(reg 13)
val dataptr as direct dataptr' = direct(reg 12)
val arithtemp as direct arithtemp' = direct(reg 9)
val arithtemp2 = direct(reg 10)
val storeptr = direct(reg 11)
val standardclosure = direct(reg 2)
val standardarg = direct(reg 0)
val standardcont = direct(reg 1)
val miscregs = map (direct o reg) [3,4,5,6,7,8]

fun newlabel() = immedlab(V.newlabel())
fun emitlab(i,immedlab lab) = V.emitlab(i,lab)
fun define (immedlab lab) = V.define lab
val align = V.align
val mark = V.mark
val move = V.movl
val emitlong = V.emitlong
val realconst = V.realconst
val emitstring = V.emitstring

fun jmpindexb lab = V.jmp(index(defer lab, arithtemp'))

fun storefield(i, v) = V.movl(v, displace(i*4,dataptr'))

fun addtodataptr i = V.addl2(immed (4*i), dataptr);

fun select(i, direct r, s) = V.movl(displace(i*4,r),s)
fun offset(i, direct r, s) = V.moval(displace(i*4,r),s)

fun sceql(x',y',immed k, immedlab lab) =
    let val loop = V.newlabel()
     in V.movl(immed(k div 2), arithtemp);
	V.define loop;
	V.cmpl(index(defer x',arithtemp'),index(defer y',arithtemp'));
	V.bneq(address lab);
	V.sobgeq(arithtemp, address loop)
    end


val addl3 = V.addl3
val subl3 = V.subl3
val ashl = V.ashl
val mull2 = V.mull2
val divl2 = V.divl2
val bisl3 = V.bisl3
fun store(v,w) = V.movl(v, defer w)
fun fetchindexl(v,w) = V.movl(index(defer v, arithtemp'),w)
fun storeindexl(v,w) = V.movl(v,index(defer w, arithtemp'))
fun fetchindexb(v,w) = V.movzbl(index(defer v, arithtemp'),w)
fun storeindexb(v,w) = V.movb(v,index(defer w, arithtemp'))
fun mnegg(x,y) = V.mnegg(defer x, defer y)
fun defer3 f = fn (a,b,c) => f(defer a, defer b, defer c)
val mulg3 = defer3 V.mulg3
val divg3 = defer3 (fn (a,b,c) => V.divg3 (b,a,c))
val addg3 = defer3 V.addg3
val subg3 = defer3 (fn (a,b,c) => V.subg3 (b,a,c))
val cmpl = V.cmpl

fun cmpg(a,b) = V.cmpg(defer a, defer b)

fun defer' j = fn x => j(defer x)
val jmp = defer' V.jmp
val bneq = defer' V.bneq
val beql = defer' V.beql
val bleq = defer' V.bleq
val bgeq = defer' V.bgeq
val blss = defer' V.blss
val bgtr = defer' V.bgtr
val bbs = fn(x,y,l) => V.bbs(x,y, defer l)

fun isimmed(immed i) = SOME i
  | isimmed _ = NONE

fun isreg(direct(reg i)) = SOME i | isreg _ = NONE
fun eqreg (a: EA) b = a=b

fun profile(i,incr) = if i >= Profile.PROFSIZE
			then ErrorMsg.impossible ("Bad profiling in vax: trying "
						  ^makestring i^" with size "
						  ^makestring Profile.PROFSIZE)
			else V.addl2(immed incr, displace(4*i,sp'))

val comment = V.comment
end
