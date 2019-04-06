signature REAL =
  sig
    infix 7 * /
    infix 6 + -
    infix 4 > < >= <=
    type real
    exception Floor and Sqrt and Exp and Ln
    exception Real of string  (* Overflow *)
    val ~ : real -> real 
    val + : (real * real) -> real 
    val - : (real * real) -> real 
    val * : (real * real) -> real 
    val / : (real * real) -> real 
    val > : (real * real) -> bool
    val < : (real * real) -> bool
    val >= : (real * real) -> bool
    val <= : (real * real) -> bool
    val abs : real ->  real
    val real : int -> real
    val floor : real -> int
    val truncate : real -> int
    val ceiling : real -> int
    val sqrt : real -> real
    val sin : real -> real
    val cos : real -> real
    val arctan : real -> real
    val exp : real -> real
    val ln : real -> real
    val print : real -> unit
    val makestring : real -> string
  end

signature FIELD =
sig
   type elem
   val zero: elem
   val one: elem
   val eq : elem * elem -> bool
   val + : elem * elem -> elem
   val * : elem * elem -> elem
   val ~ : elem -> elem
   exception Div
   val inv: elem -> elem
end

structure RealField : FIELD =
struct
  open Real
  type elem = real
  exception Div = Div  (* *)
  fun eq(a,b) = a=b
  val zero = 0.0 and one = 1.0
  fun inv x = 1.0 / x
end

signature COMPLEX =
sig
   include FIELD
   val complex: real*real -> elem
   val - : elem * elem -> elem
   val / : elem * elem -> elem
   val re: elem -> real
   val im: elem -> real
   val abs : elem -> real
   val conj : elem -> elem
   val cis: real -> elem
end

structure Cartesian : COMPLEX =
struct
  open Real
  type elem = real * real
  fun complex ri = ri
  exception Div = (* Real. *) Div
  val eq = (op = : elem * elem -> bool)
  val zero = (0.0,0.0) and one = (1.0,0.0)
  val op + = fn ((a,b),(c,d)) => (a+c,b+d)
  and op - = fn ((a,b),(c,d)) => (a-c,b-d)
  and op * = fn ((a,b),(c,d)) => (a*c-b*d, a*d+b*c)
  and op / = fn ((a,b),(c,d)) => 
		let val z = c*c+d*d
		 in ((a*c+b*d)/z, (b*c-a*d)/z)
		end
  and inv = fn (a,b) => let val z = a*a+b*b 
			 in (a/z,b/z) 
			end
  and ~ = fn (a,b) => (~a,~b)
  and re = fn(a,b) => a
  and im = fn(a,b) => b
  and abs = fn (a,b) => sqrt(a*a+b*b)
  and conj = fn (a,b) => (a,~b)
  and cis = fn t => (cos t, sin t)
end

functor CSQRT(structure C : COMPLEX) = 
struct

  val sqrt = fn z =>
   let val zr = C.re z and zi = C.im z
    in if zr=0.0 andalso zi = 0.0 then z
       else let val x = abs zr and y = abs zi
		val cn = C.complex
		fun finish(r,w) = 
			if zr >= 0.0 then cn(w,zi/(w+w))
			   else let val ci=if zi>=0.0 
					then w else ~w
				 in cn(zi/(ci+ci), ci)
				end
             in if x >= y 
		 then let val r=y/x 
		       in finish(r,sqrt x * 
				    sqrt(0.5*(1.0+
					sqrt(1.0+r*r))))
		      end
		 else let val r=x/y
		       in finish(r,sqrt y * 
				    sqrt(0.5*(r+
					sqrt(1.0+r*r))))
		      end
	    end
    end
end

signature POLYNOMIAL =
sig
    structure F : FIELD
    type elem
    val degree: elem -> int
    val zero : elem
    val one : elem
    val x : elem
    val const : F.elem -> elem
    val poly : F.elem list -> elem  (* low-order 1st *)
    val coeff: elem -> F.elem list  (* low-order 1st *)
    val * : elem * elem -> elem
    val + : elem * elem -> elem
    val ~ : elem -> elem
    val eval: elem -> F.elem -> F.elem
    val deriv: elem -> elem
    exception Div of elem * elem
    val divmod : elem * elem -> elem * elem
end


functor Polynomial(structure F : FIELD) : POLYNOMIAL =
struct
  structure F=F
  type elem = F.elem list
  val zero = nil
  val one = [F.one]
  fun poly nil = nil 
    | poly (a::r) = case poly r
		     of nil => if F.eq(a,F.zero) 
				then nil else [a]
		      | pr => a::pr
  fun coeff nil = [F.zero] | coeff p = p
  fun degree nil = 0
    | degree p = length(p)-1
  val x = [F.zero,F.one]
  fun const c = if F.eq(c,F.zero) then nil else [c]
  fun []+a = a
    | a+[] = a
    | (a::b) + (c::d) = 
	let val ac = F.+(a,c)
         in case b+d
	     of nil => if F.eq(ac,F.zero) 
			then nil else [ac]
	      | bd => ac::bd
        end

  fun scalarmult(a,[]) = []
    | scalarmult(a,b::c) = 
		let val ab = F.*(a,b)
	         in case scalarmult(a,c)
		     of nil => if F.eq(ab,F.zero) 
				then nil else [ab]
		      | ac => ab::ac
		end
  fun []*a = []
    | a*[] = []
    | (a::b)*c = scalarmult(a,c) + (F.zero::(b*c))

  fun ~ [] = []
    | ~ (a::b) = F.~(a) :: ~(b)

  fun eval p x =
    let fun f([],z,sum) = sum
          | f(a::b,z,sum) = f(b,F.*(x,z),
				F.+(sum,F.*(a,z)))
     in f(p,F.one,F.zero)
    end

  fun deriv [] = []
    | deriv (a::r) =
    let fun f(z,a::b) = F.*(z,a)::f(F.+(z,F.one),b)
	  | f(z,[]) = []
     in f(F.one,r)
    end

  exception Div of elem * elem
  fun last [x] = x | last(a::r) = last r

  fun divmod(a,b) =
   let val bh = last b handle Match => raise Div(a,b)
       fun f(q,rh,rl) =
	    (* Invariant:  let k = length rl
	        in b * q * x^k + rh*x^k + rev rl = a *)
               let val lastr = last rh
		   val z = F.*(lastr,F.inv bh)
		   val bs = scalarmult(z,b)
	           val rh = rh + ~bs and q = z::q
	        in case rl 
		    of rl0::rl' => f(q,rl0::rh,rl')
		     | [] => (q,rh)
	       end
       fun split(0,rh,rl) = (rh,rl)
	 | split(i,z::rh,rl) = split(i-1,rh,z::rl)
       val (rh,rl) = split(length a - length b, a,nil)
    in f(nil,rh,rl)
   end       

end

signature ROOT = 
  sig structure P : POLYNOMIAL
      exception Root (* root of degree-0 polynomial *)
      val root : P.elem -> P.F.elem -> P.F.elem
  end

functor Root(structure P : POLYNOMIAL 
	     structure C: COMPLEX
	     val sqrt: C.elem -> C.elem
	     sharing P.F=C) : ROOT =
struct
 structure P = P
 val EPS = 1E~14

 fun eval_err p x =
    let val absx = C.abs(x)
	fun f(a::r) = let val (b,e) = f r
		          val bb = C.+(C.*(x,b),a)
		       in (bb, absx*e+C.abs(bb))
		      end
          | f nil = (C.zero,0.0)
	val (b,e) = f (P.coeff p)
     in (b,e*EPS)
    end

 fun laguerre p =
    let open C
	val p' = P.deriv p
        val p'' = P.deriv p'
	val n = complex(real(P.degree p), 0.0)
     in fn x => let val px = P.eval p x
		    val G = P.eval p' x / px
                    val HmGG = P.eval p'' x / px
		    val s = sqrt(n*(n-one)*HmGG)
		    val d1 = G+s and d2 = G-s
		    val d = if abs d1 > abs d2 
				then d1 else d2
		 in x-n/d
	        end
    end

 fun newton p = 
  let open C
      val p' = P.deriv p
   in fn x => x - (P.eval p x / P.eval p' x) 
		handle Div => x | Overflow => x
  end

 exception Root
 fun root p x =
  case P.coeff p
   of [b,a] => C.~(C./(b,a))
    | [a] => raise Root
    | _ =>
  let val eval_err = eval_err p 
      and laguerre = laguerre p 
      and newton = newton p
      fun f(x,px,err) = 
	if C.abs(px)<err then x
	else let val nx = newton x
		 val (pnx,err') = eval_err nx
	      in if C.abs(pnx) < C.abs(px) 
		 then f(nx,pnx,err')
		 else let val lx = laguerre x
			  val (plx,err') = eval_err lx
			in if C.abs(plx)>=C.abs(px) 
			   then x
			   else f(lx,plx,err')
		      end
	     end
      val (px,err) = eval_err x
   in f(x,px,err)
  end

end


functor Roots(structure R : ROOT) =
struct
  fun roots0 p = if R.P.degree p < 1 then nil
	else let val r1 = R.root p R.P.F.zero
	         val b = R.P.+(R.P.x, 
			       R.P.const(R.P.F.~ r1))
		 val (quotient,remainder) = R.P.divmod(p,b)
	      in r1 :: roots0 quotient
	     end

  fun roots p = map (R.root p) (roots0 p)
end

structure CSQRT = CSQRT(structure C = Cartesian)
structure PC : POLYNOMIAL = 
		Polynomial(structure F = Cartesian)
structure RC : ROOT = Root(structure P = PC 
			         and C = Cartesian
			   val sqrt = CSQRT.sqrt)
structure RsC = Roots(structure R = RC)

val p =
let open PC
 in x*x+x+x+const(Cartesian.complex(0.75,0.0))
end;

(* val rp = RsC.roots p; *)

