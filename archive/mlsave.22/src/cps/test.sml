structure test =
  struct
local
val F = (1,1) val g = (1,1) val h = (1,1) val i = (1,1)
val j = (1,1) val k = (1,1) val l = (1,1)
in
fun tupknown1() =
  let fun f(a,b,c,d,e) = (a,b,c,d,e,F,g,h,i,j,k,l)
  in  (f(1,2,3,4,5),f(5,4,3,2,1))
  end
end
(*
fun tupknown() =
  let fun f(a,b,c,d,e,f,g,h,i,j,k) = (k,j,i,h,g,f,e,d,c,b,a)
  in  (f(1,2,3,4,5,6,7,8,9,10,11),f(11,10,9,8,7,6,5,4,3,2,1))
  end

fun fcps i =
  let fun f 0 = 1
	| f 1 = 1
	| f i = 1 + f(i-1)+f(i-2)
    in f i
   end

exception Foo
val a = (raise Foo; 5) handle Foo => 3



fun f x =
 let val a = (0,1)
     val b = (2,3)
     val c = (2,3)
     val d = (2,3)
     val e = (2,3)
     val f = (2,3)
     val g = (2,3)
     val h = (2,3)
     val i = (2,3)
     val j = (2,3)
     val k = (2,3)
     val l = (2,3)
     val m = (2,3)
     val n = (2,3)
 in (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
 end

fun len nil = 0 | len (a::b) = 1 + len b

fun len l = 
   let fun j(nil,k) = k | j(a::x,k) = j(x,k+1) in j(l,0) end


fun f x =
  let fun g y = (y,x)
  in  (g,x+1)
  end

val g =
let val f = fn x => fn y => (x,y)
 in f 1 2; f 2 3
end



val g =
     fn 3 => 3 | 4 => 4 | 6 => 6 | 8 => 8 | 9 => 9
      | 20 => 20 | 21 => 21 | 22 => 22 | 40 => 40

val g = 
 let fun f (a,b) c = (a,a,b)
  in (8, f (1,4) 2)
 end


type lvar = int
datatype cexp
  = RECORD of (lvar * int list) list * lvar * cexp
  | SELECT of int * lvar * lvar * cexp
  | OFFSET of int * lvar * lvar * cexp
  | APP of lvar * lvar list
  | FIX of (lvar * lvar list * cexp) list * cexp
  | SWITCH of lvar * cexp list
  | PRIMOP of Access.primop * lvar list * lvar list * cexp list
fun root(RECORD(_,_,e)) = root e
  | root(SELECT(_,_,_,e)) = root e
  | root(OFFSET(_,_,_,e)) = root e
  | root(SWITCH(_,e::_)) = root e
  | root(PRIMOP(_,_,_,e::_)) = root e
  | root(e as APP _) = e



val g =
let fun iter(0,s) = s | iter (i,s) = iter(i-1,s+i)
 in iter(1000,0)
end

fun f x = let fun g z = (z,z)
	   in (g x; g x; ())
         end

fun f x = let fun g() = (1,2) in (g(),g()) end

fun f x = let fun q(a,b) = (a,b,b)
	   in (q(x,x),q(x,x))
	  end
 fun f x = 
	let fun q(((a,b),(c,d)),((e,f),(g,h))) = (a,c,d,d)
	 in (q(((1,2),(3,4)),((5,6),(7,8))), q(x))
	end
val g = 
   fn 1 => 1 | 2 => 2 | 3 => 3 | 5 => 5 | 7 => 7 
      | 20 => 20 | 21 => 21
      | 40 => 40 | 41 => 41

fun g 0 = 1
     | g j = j * g(j-1) 
*)

end
