(* lazystream.sml *)

(* LazyStream: implements a lazy stream type, analagous to lazy lists in Haskell *)

structure LazyStream =
struct

datatype 'a susp
  = SUSP of unit -> 'a
  | VALUE of 'a

datatype 'a stream
  = Sthunk of 'a stream susp ref
  | SCons of 'a * 'a stream
  | SNil

(* this should never be raised *)
exception StreamError

(* strict cons, for use when we already have a stream *)
fun scons (x, s) = SCons (x,s)

(* lcons : (unit -> 'a) -> 'a stream
 * lazy cons, for when we are defining a stream whose tail has not
 * been computed yet *)
fun lcons f = Sthunk(ref(SUSP f))

(* sforce : 'a stream -> 'a stream
 * force suspended stream, producing concrete (SNil,SCons) form.
 * INVARIANT: the result of sforce is a "concrete" stream, formed
 * using the SNil or SCons constructors. *)
fun sforce (Sthunk r) =
    (case !r
       of VALUE(s) => s
        | SUSP f => let val s = f() in r := VALUE s; s end)   
  | sforce s = s

(* mkStream : (unit -> 'a stream) -> 'a stream *)
fun mkStream f = Sthunk(ref(SUSP f))

(* snull : 'a stream -> bool
 * test for empty stream *)
fun snull s =
    case sforce s
      of SNil => true
       | SCons _ => false
       | Sthunk _ => raise StreamError

(* shd : 'a stream -> 'a
 * head of a stream *)
fun shd s =
    case sforce s
      of SNil => raise Empty
       | SCons(x,_) => x
       | Sthunk _ => raise StreamError

(* stl: 'a stream -> 'a stream
 * tail of a stream *)
fun stl s = 
    case sforce s
      of SNil => raise Empty
       | SCons(_,xs) => xs
       | Sthunk _ => raise StreamError

(* sdest : 'a stream -> 'a * 'a stream
 * destruct a stream into its head and tail elements *)
fun sdest s =
    case sforce s
      of SNil => raise Empty
       | SCons(x,xs) => (x,xs)
       | Sthunk _ => raise StreamError

(* stake : 'a stream * int -> 'a stream *)
fun stake (s,0) = []
  | stake (s,n) = 
      if snull s then raise Subscript
      else shd s :: stake (stl s, n-1)

(* sdrop : 'a stream * int -> 'a stream *)
fun sdrop (s,0) = s
  | sdrop (s,n) = 
    if snull s then raise Subscript
    else sdrop (stl s, n-1)

(* streamRdr : 'a stream -> ('a, 'a stream) option *)
fun streamRdr s =
    case sforce s
      of SNil => NONE
       | SCons(c,s') => SOME(c,s')
       | _ => raise StreamError

end (* structure Stream *)


(*
structure StreamExamples =
struct

open LazyStream

(* generate the infinite stream of successive numbers starting with n *)
fun nums n = lcons(fn() => SCons(n, nums(n+1)));

(* stream of all positive ints:  1,2,3,... *)
val posints = nums 1;

(* Eratosthenes Sieve *)

fun sfilter f s =
    let val (x,s') = sdest s
     in if f x then lcons(fn () => SCons(x, sfilter f s'))
        else sfilter f s'
    end;

fun notdivisibleby n m = m mod n <> 0;

fun sieve s = 
    let val (x,s') = sdest s 
     in lcons(fn () => SCons(x, sieve(sfilter (notdivisibleby x) s')))
    end;

(* stream of prime numbers *)
val primes = sieve (nums 2);

fun first_n_primes n = stake(primes,n);

end (* structure Primes *)
*)
