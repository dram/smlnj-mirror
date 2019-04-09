(* Copyright 1996 by Bell Laboratories *)
(* mcprint.sml *)

signature MC_PRINT =
sig

  val printCon : Lambda.con -> unit
  val printLexp : Lambda.lexp -> unit
  val printSval : Lambda.value -> unit
  val printMatch : StaticEnv.staticEnv ->  
                       (Absyn.pat * Lambda.lexp) list -> unit
  val printFun : Lambda.lexp -> LambdaVar.lvar -> unit

end (* signature MC_PRINT *)


structure MCprint : MC_PRINT = 
struct

local structure A = Absyn
      structure DA = Access
      structure S = Symbol
      structure PP = PrettyPrint
      structure PU = PrintUtil
      structure LT = LambdaType
      structure LE = LtyEnv
      open Lambda PrintUtil 
in 

val say = Control.Print.say
fun sayrep rep = say (DA.prRep rep)
val lvarName = LambdaVar.lvarName

fun bug s = ErrorMsg.impossible ("MCprint: "^s)

fun app2(f, [], []) = ()
  | app2(f, a::r, b::z) = (f(a, b); app2(f, r, z))
  | app2(f, _, _) = bug "unexpected list arguments in function app2"
  
val margin = ref 0
fun indent i = margin := !margin + i

exception Undent
  
fun undent i = 
  (margin := !margin - i; if !margin < 0 then raise Undent else ())

fun dent () = tab(!margin)

fun whitespace() =
  let fun ws(n) =
        if n < 0 then raise Undent
        else if n >= 8 then "\t" :: ws(n-8)
             else let val str = case n of 0 => "" | 1 => " " | 2 => "  "
                                        | 3 => "   " | 4 => "    " 
                                        | 5 => "     " | 6 => "      " 
                                        | _ => "       "
                   in [str]
                  end
   in concat(ws(!margin))
  end

fun prCon (DATAcon(sym, _, _)) = S.name sym
  | prCon (INTcon i) = Int.toString i
  | prCon (INT32con i) = "(I32)" ^ (Int32.toString i)
  | prCon (WORDcon i) = "(W)" ^ (Word.toString i)
  | prCon (WORD32con i) = "(W32)" ^ (Word32.toString i)
  | prCon (REALcon r) = r
  | prCon (STRINGcon s) = PU.mlstr s (* was PU.pr_mlstr s *)
  | prCon (VLENcon n) = Int.toString n

fun printCon x = say (prCon x)

(** use of complex in printLexp may lead to stupid n^2 behavior. *)
fun complex le = 
  let fun h [] = false
        | h (a::r) = g a orelse h r

      and g (FN(_, _, b)) = g b
        | g (FIX(vl, _, ll, b)) = true
        | g (LET _) = true
        | g (SWITCH _) = true
        | g (TAPP(l, _)) = true
        | g (CON(_, _, l)) = true
        | g (DECON(_, _, l)) = true
        | g (HANDLE _) = true 
        | g _ = false

   in g le
  end

fun printSval sv = 
  let fun prTyc t = say (LE.tcPrint t)
      fun prLty t = say (LE.ltPrint t)
      fun plist (p, [], sep) = ()
        | plist (p, a::r, sep) = 
           (p a; app (fn x => (say sep; p x)) r)

      fun g (VAR v) = say(lvarName v)
        | g (INT i) = say(Int.toString i)
        | g (WORD i) = (say "(W)"; say(Word.toString i))
        | g (INT32 i) = (say "(I32)"; say(Int32.toString i))
        | g (WORD32 i) = (say "(W32)"; say(Word32.toString i))
        | g (REAL s) = say s
        | g (STRING s) = say (mlstr s)
        | g (PRIM(p,t,ts)) = 
              (say ("PRIM (" ^ (PrimOp.prPrimop p) ^ ", "); prLty t; 
               say ", ["; plist(prTyc, ts, ","); say "])")
        | g (GENOP(dict, p, t, ts)) = 
              (say ("GENOP (" ^ (PrimOp.prPrimop p) ^ ", "); prLty t; 
               say ", ["; plist(prTyc, ts, ","); say "])")
   in g sv
  end

fun printLexp l = 
  let fun prLty t = say (LE.ltPrint t)
      fun prTyc t = say (LE.tcPrint t)
      fun prKnd k = say (LT.tk_print k)
      fun ipr (i:int) = say(Int.toString i)
      val psv = printSval

      fun plist (p, [], sep) = ()
        | plist (p, a::r, sep) = 
           (p a; app (fn x => (say sep; p x)) r)

      fun g (SVAL v) = psv v
        | g (EXNF (v,_)) = (say "EXNF("; psv v; say ")")
        | g (EXNC v) = (say "EXNC("; psv v; say ")")

        | g (RECORD vs) =
            (say "RECORD"; PU.printClosedSequence ("(", ",", ")") psv vs)

        | g (SRECORD vs) =
            (say "SRECORD"; PU.printClosedSequence ("(", ",", ")") psv vs)

        | g (VECTOR (vs, _)) =
            (say "VECTOR"; PU.printClosedSequence ("(", ",", ")") psv vs)

        | g (l as SELECT(i, v)) =
            (psv v; PU.printClosedSequence ("[",",","]") ipr [i])

        | g (FN(v,t,l)) = 
            (say "FN("; say(lvarName v); say " : "; prLty t; say ", ";
             if complex l then (newline(); indent 3; dent();
                                g l; say ")"; undent 3)
             else (g l; say ")"))

        | g (CON((s, c, lt), ts, sv)) = 
            (say "CON(("; say(S.name s); say ","; sayrep c; say ",";
             prLty lt; say "), ["; plist(prTyc, ts, ","); say "], ";
             psv sv; say ")")

        | g (DECON((s, c, lt), ts, sv)) = 
            (say "DECON(("; say(S.name s); say ","; sayrep c; say ",";
             prLty lt; say "), ["; plist(prTyc, ts, ","); say "], ";
             psv sv; say ")")

        | g (LET(v, r, l)) = 
            let val lv = lvarName v
                val len = size lv + 3
             in say lv; say " = ";
                if complex r
                then (indent 2; newline(); dent(); g r; undent 2)
                else (indent len ; g r; undent len);
                newline(); dent(); g l
            end

        | g (APP(sv1, sv2)) = 
            (say "APP("; psv sv1; say ","; psv sv2; say ")")

        | g (TFN(ks, b)) = 
            (say "TFN("; app (fn k => (prKnd k; say ",")) ks; 
             if complex b 
             then (newline(); indent 3; dent(); g b; say ")"; undent 3)
             else (g b; say ")"))
                  
        | g (TAPP(sv, ts)) = 
            (say "TAPP("; 
             psv sv; say ", ["; plist(prTyc, ts, ","); say "])")

        | g (PACK(lt, ts, nts, sv)) = 
            (say "PACK("; 
             app2 (fn (tc,ntc) => (say "<"; prTyc tc; say ","; prTyc ntc;
                                 say ">,"), ts, nts);
             say " "; prLty lt; say ", "; psv sv; say ")")

        | g (SWITCH (sv,_,llist,default)) =
            let fun switch [(c,l)] =
                      (printCon c; say " => "; indent 8; g l; undent 8)
                  | switch ((c,l)::more) = 
                      (printCon c; say " => ";
                       indent 8; g l; undent 8; newline(); dent(); switch more)
                  | switch [] = bug "unexpected case in switch" 

             in say "SWITCH ";
                indent 7; psv sv; undent 6; newline(); dent();
                say "of "; indent 3; switch llist;

                case (default,llist)
                 of (NONE,_) => ()
                  | (SOME l,nil) => (say "_ => "; indent 5; g l; undent 5)
                  | (SOME l,_) => (newline(); dent(); say "_ => ";
                                   indent 5; g l; undent 5);

                undent 4
            end

        | g (FIX(varlist,ltylist,lexplist,lexp)) =
            let fun flist([v],[t],[l]) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in say lv; say " : ";prLty t;say " :: ";
                          indent len ; g l; undent len
                      end
                  | flist(v::vs,t::ts,l::ls) =
                      let val lv = lvarName v
                          val len = size lv + 2
                       in say lv; say " : "; prLty t; say " :: ";
                          indent len ; g l; undent len;
                          newline(); dent(); flist(vs,ts,ls)
                      end
                  | flist(nil,nil,nil) = ()
                  | flist _ = bug "unexpected cases in flist"

             in say "FIX("; indent 4; flist(varlist,ltylist,lexplist); 
                undent 4; newline(); dent(); say "IN  ";
                indent 4; g lexp; say ")"; undent 4
            end

        | g (RAISE(sv,t)) = 
            (say "RAISE("; prLty t; say ", "; indent 6; 
             psv sv; say ")"; undent 6)

        | g (HANDLE (l, sv)) =
            (say "HANDLE "; indent 7; g l; undent 5; newline(); dent();
             say "WITH "; indent 5; psv sv; undent 7)

        | g (WRAP(t, _, sv)) = 
            (say "WRAP("; prTyc t; say ","; psv sv; say ")")

        | g (UNWRAP(t, _, sv)) = 
            (say "UNWRAP("; prTyc t; say ","; psv sv; say ")")

   in g l; newline(); newline()
  end

fun printMatch env ((p,r)::more) =
      (PP.with_pp (ErrorMsg.defaultConsumer())
       (fn ppstrm =>
        (PPAbsyn.ppPat env ppstrm (p,!Control.Print.printDepth);
         PP.add_newline ppstrm));
       say " => "; printLexp r; printMatch env more)
  | printMatch _ [] = ()

fun printFun l v =
  let fun last (DA.LVAR x) = x 
        | last (DA.PATH(r,_)) = last r
        | last _ = bug "unexpected access in last"

      fun findsv (VAR w) = 
            if (v=w) then 
              (say("VAR " ^ lvarName v ^ " is free in <lexp>\n");())
            else ()
        | findsv (GENOP({default=w,...}, _, _, _)) = 
            if (v=w) then 
              (say("VAR " ^ lvarName v ^ " is free in <lexp>\n");())
            else ()
        | findsv _ = ()

      val rec find =
        fn l as FN(w,_,b) => if v=w then printLexp l else find b
         | l as FIX(vl,_,ll,b) => 
             if List.exists (fn w => v=w) vl then printLexp l
             else (app find ll; find b)
         | APP(l,r) => (findsv l; findsv r)
         | LET(w,l,r) => (if v=w then printLexp l else find l; find r)
         | PACK(_,_,_,r) => findsv r
         | TFN(_, r) => find r
         | TAPP(l, _) => findsv l
         | SWITCH (l,_,ls,d) =>
             (findsv l; app (fn(_,l) => find l) ls;
              case d of NONE => () | SOME l => find l)
         | RECORD l => app findsv l 
         | SRECORD l => app findsv l 
         | VECTOR (l, t) => app findsv l 
         | SELECT(_,l) => findsv l
         | CON((_, DA.EXNFUN p, _), _, sv) => 
             (findsv (VAR(last p)); findsv sv)
         | CON((_, DA.EXNCONST p, _), _, sv) => 
             (findsv (VAR(last p)); findsv sv)
         | DECON((_, DA.EXNFUN p, _), _, sv) => 
             (findsv (VAR(last p)); findsv sv)
         | DECON((_, DA.EXNCONST p, _), _, sv) => 
             (findsv (VAR(last p)); findsv sv)
         | CON(_,_,sv) => findsv sv
         | DECON(_,_,sv) => findsv sv
         | HANDLE(e,h) => (find e; findsv h) 
         | RAISE(l,_) => findsv l
         | EXNF (sv,_) => findsv sv
         | EXNC sv => findsv sv
         | WRAP(_, _, sv) => findsv sv
         | UNWRAP(_, _, sv) => findsv sv
         | _  => bug "unexpected cases in printFun"

   in find l
  end

end (* toplevel local *)
end (* structure MCprint *)

