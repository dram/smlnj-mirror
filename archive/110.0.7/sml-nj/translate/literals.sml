(* COPYRIGHT (c) 1995 AT&T Bell Laboratories *)
(* literals.sml *)

signature LITERALS =
  sig
    val liftlits : (PLambda.lexp -> PLambda.lexp) * PLambda.lexp -> PLambda.lexp
  end

structure Literals : LITERALS = 
struct

local open Access PLambda 
      structure LT = LambdaType
in

fun error msg = ErrorMsg.impossible ("Literals: "^msg) 

(**************************************************************************
 * liftlits: (lexp -> lexp) * lexp -> lexp                                *
 *   The first argument is the "stringcopy" function; the second argument *
 *   is the source lexp, which may contain free lvars because the whole   *
 *   source lambda expression has not been closed yet in translate.sml.   *
 *   The "liftlits" function lifts all the constant literals to the top   *
 *   level, inserting proper stringcopy operatings.                       *
 **************************************************************************)
fun liftlits(dupstring,lexp) = if not(!Control.CG.liftLiterals) 
                               then lexp  else 
let val constLvar = LambdaVar.mkLvar()
    val constLexp = VAR constLvar
    val constTable : lexp list ref = ref []
    val counter : int ref = ref 0
    fun index() = let val x = !counter in counter := x+1; x end

    fun dup le = 
      case le
       of INT _ => le
        | INT32 _ => le
        | WORD _ => le
        | WORD32 _ => le
        | REAL _ => le (* no dup, but the cps/contract.sml will not
                          pull any real constant literals out of an
                          record if the CG.liftLiterals flag is on *)
        | STRING _ => dupstring(le)
        | RECORD el => RECORD (map dup el)
        | SRECORD el => SRECORD (map dup el)
        | SELECT(i,e) => SELECT(i, dup e)
        | VECTOR(el,t) => VECTOR (map dup el,t)
        | CON(c,ts,e) => CON(c, ts, dup e)
        | DECON(c,ts,e) => DECON(c, ts, dup e)
        | WRAP(t,b,e) => WRAP(t, b, dup e)
        | UNWRAP(t,b,e) => UNWRAP(t, b, dup e)
        | _ => error "impossible constants in translate/literals.sml"

    fun checkIn(e as INT _, _) = e
      | checkIn(e as WORD _, _) = e
      | checkIn(e as INT32 _, _) = e
      | checkIn(e as WORD32 _, _) = e
      | checkIn(e as RECORD [], _) = e
      | checkIn(e as SRECORD [], _) = e
      | checkIn(e as VECTOR ([],_), _) = e
      | checkIn(e, c) = 
          if c then (constTable := (dup e::(!constTable)); 
                     SELECT(index(), constLexp))
          else e

    fun g' le = checkIn(g le)

    and g le = 
      case le 
       of VAR v => (le, false)
        | FN(v,t,b) => (FN(v, t, g' b), false)
        | FIX(fl,t,el,b) => 
             let fun h([],z) = rev z
                   | h(a::r,z) = let val (na,_) = g(a) in h(r, na::z) end
                 val nel = h(el,[])
              in (FIX(fl, t, nel, g' b), false)
             end
        | APP(a,b) => (APP(g' a, g' b), false)
        | SWITCH(e,cl,el,d) => 
            let fun h([],z) = rev z
                  | h((c,a)::r,z) = let val na = g'(a) in h(r,(c,na)::z) end

                val ne = g' e
                val nel = h(el,[])
                val nd = case d of SOME d' => SOME(g' d')
                                 | NONE => NONE
             in (SWITCH(ne, cl, nel, nd), false)
            end
        | CON (c as (_, EXNCONST _, _), ts, e) => 
                         (CON(c, ts, g' e), false)
        | CON (c as (_, EXNFUN _, _), ts, e) => 
                         (CON(c, ts, g' e), false)
        | DECON (c as (_, EXNCONST _, _), ts, e) => 
                         (DECON(c, ts, g' e), false)  
        | DECON (c as (_, EXNFUN _, _), ts, e) => 
                         (DECON(c, ts, g' e), false)  
        | CON (c,ts,e) => let val (ne, z) = g e in (CON(c,ts,ne), z) end
        | DECON (c,ts,e) => let val (ne, z) = g e in (DECON(c,ts,ne), z) end
        | RECORD el => 
             let val nel = map g el
                 val c = foldr (fn ((_,x),b) => x andalso b) true nel
              in if c then (RECORD (map #1 nel), true)
                 else (RECORD (map checkIn nel), false)
             end
        | SRECORD el => 
             let val nel = map g el
                 val c = foldr (fn ((_,x),b) => x andalso b) true nel
              in if c then (SRECORD (map #1 nel), true)
                 else (SRECORD (map checkIn nel), false)
             end
        | VECTOR (el, t) => 
             let val nel = map g el
                 val c = foldr (fn ((_,x),b) => x andalso b) true nel
              in if c then (VECTOR (map #1 nel, t), true)
                 else (VECTOR (map checkIn nel, t), false)
             end
        | SELECT(i,e) => let val (ne,c) = g e in (SELECT(i, ne), c) end
        | RAISE(e,t) => (RAISE(g' e, t), false)
        | HANDLE(a,b) => (HANDLE(g' a, g' b), false)
        | EXNF(e,t) => (EXNF(g' e, t), false)
        | EXNC e => (EXNC (g' e), false)
        | WRAP(t,b,e) => let val (ne,c) = g e in (WRAP(t,b,ne), c) end
        | UNWRAP(t,b,e) => let val (ne,c) = g e in (UNWRAP(t,b,ne), c) end
        | PRIM _ => (le, false)
        | REAL _ => (le, true)
        | STRING _ => (le, true)
        | INT _ => (le, true)
        | WORD _ => (le, true)
        | INT32 _ => (le, true)
        | WORD32 _ => (le, true)
        | _ => error "unexpected lambda expressions in liftlits."

   in LET(constLvar, RECORD(rev (!constTable)), g' lexp) 
  end

end (* toplevel local *)
end (* Literals *)

(*
 * $Log: literals.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/05/05 20:00:11  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
