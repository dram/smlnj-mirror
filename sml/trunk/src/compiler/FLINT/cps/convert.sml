(* Copyright 1996 by Bell Laboratories *)
(* convert.sml *)

(***************************************************************************
 *                         IMPORTANT NOTES                                 *
 *                                                                         *
 *          OFFSET and RECORD accesspath SELp should not be                *
 *                  generated by this module.                              *
 ***************************************************************************)

signature CONVERT = sig 
  val convert : Lambda.lexp -> CPS.function * LtyDef.lty Intmap.intmap 
end (* signature CONVERT *)

functor Convert(MachSpec : MACH_SPEC) : CONVERT = struct

local open CPS 
      structure DA = Access
      structure LT = LtyExtern
      structure LV = LambdaVar
      structure AP = PrimOp
in

fun bug s = ErrorMsg.impossible ("Convert: " ^ s)
val say = Control.Print.say

val ident = fn le => le
fun split(Lambda.SVAL v) = (v, ident)
  | split x = let val v = LV.mkLvar()
               in (Lambda.VAR v, fn z => Lambda.LET(v, x, z))
              end

fun APPg(e1, e2) = 
  let val (v1, h1) = split e1
      val (v2, h2) = split e2
   in h1(h2(Lambda.APP(v1, v2)))
  end

val rep_flag = MachSpec.representations
fun which (a,b) = if rep_flag then a else fn x => b

val arrowLty = which(LT.lt_arrow, (LT.ltc_void, LT.ltc_void))
val selectLty = which(LT.lt_select, LT.ltc_void)

val ltc_cont = LT.ltc_cont
val lt_vcont = ltc_cont [LT.ltc_void]
val lt_scont = LT.ltc_arw (LT.ltc_void, LT.ltc_void)


(***************************************************************************
 *              CONSTANTS AND UTILITY FUNCTIONS                            *
 ***************************************************************************)
val OFFp0 = OFFp 0
val id = fn x => x

val IntOpTy = LT.ltc_arw(LT.ltc_tuple[LT.ltc_int,LT.ltc_int],LT.ltc_int)
val seqTy = LT.ltc_arw(LT.ltc_tuple[LT.ltc_void,LT.ltc_void],LT.ltc_bool)

fun numkind (AP.INT bits) = P.INT bits
  | numkind (AP.UINT bits) = P.UINT bits
  | numkind (AP.FLOAT bits) = P.FLOAT bits

fun cmpop(stuff,argt) = 
  (case stuff
    of {oper=AP.EQL,kind=AP.INT 31} => 
	 if LT.lt_eqv(argt,LT.ltc_tuple[LT.ltc_void,LT.ltc_void])
	 then (say "int-equality used for ptr-equality\n"; P.peql)
         else P.ieql
     | {oper=AP.NEQ,kind=AP.INT 31} => 
         if LT.lt_eqv(argt,LT.ltc_tuple[LT.ltc_void,LT.ltc_void])
	 then (say "int-equality used for ptr-equality\n"; P.pneq)
         else P.ineq
     | {oper,kind=AP.FLOAT size} => 
         let fun c AP.>    = P.fGT
	       | c AP.>=   = P.fGE
	       | c AP.<    = P.fLT
	       | c AP.<=   = P.fLE
 	       | c AP.EQL  = P.fEQ
 	       | c AP.NEQ  = P.fLG
 	       | c _ = bug "cmpop:kind=AP.FLOAT"
          in P.fcmp{oper= c oper, size=size}
         end
     | {oper, kind} => 
         let fun check (_, AP.UINT _) = ()
 	       | check (oper, _) = bug ("check" ^ oper)
 	     fun c AP.>   = P.>  
 	       | c AP.>=  = P.>= 
 	       | c AP.<   = P.< 
 	       | c AP.<=  = P.<=
 	       | c AP.LEU = (check ("leu", kind); P.<= )
 	       | c AP.LTU = (check ("ltu", kind); P.< )
 	       | c AP.GEU = (check ("geu", kind); P.>= )
 	       | c AP.GTU = (check ("gtu", kind); P.> )
 	       | c AP.EQL = P.eql
 	       | c AP.NEQ = P.neq
  	  in P.cmp{oper=c oper, kind=numkind kind} 
         end)

fun arity AP.~ = 1
  | arity AP.ABS = 1
  | arity AP.NOTB = 1
  | arity AP.+ = 2
  | arity AP.- = 2
  | arity AP.* = 2
  | arity AP./ = 2
  | arity AP.LSHIFT = 2
  | arity AP.RSHIFT = 2
  | arity AP.RSHIFTL = 2
  | arity AP.ANDB = 2
  | arity AP.ORB = 2
  | arity AP.XORB = 2
    
fun arithop AP.~ = P.~
  | arithop AP.ABS = P.abs
  | arithop AP.NOTB = P.notb
  | arithop AP.+ = P.+
  | arithop AP.- = P.-
  | arithop AP.* = P.*
  | arithop AP./ = P./
  | arithop AP.LSHIFT = P.lshift
  | arithop AP.RSHIFT = P.rshift
  | arithop AP.RSHIFTL = P.rshiftl
  | arithop AP.ANDB = P.andb
  | arithop AP.ORB = P.orb
  | arithop AP.XORB = P.xorb

(***************************************************************************
 *                        THE MAIN FUNCTION                                *
 *     convert : Lambda.lexp -> CPS.cexp * CPS.lty Intmap.intmap           *
 ***************************************************************************)
fun convert lexp = 
let 
 
(**** We are not supporting unrolled lists right now *********************
   val cvtrfty = if (MachSpec.newListRep) then TransList.cvtrfty 
                 else (fn x => x)
   val selectLty = if (MachSpec.newListRep) then TransList.selectLty
                   else selectLty
****)
fun cvtrfty x = x

(* the following should be reconfigured in the future *)
(**  (* replaced with below to avoid infinite loop in spill when #fpregs=7 *)
val maxrepregs1 = if not rep_flag then 0
  else (let val k = MachSpec.numRegs - MachSpec.numCalleeSaves
         in Int.min(k-2,MachSpec.numArgRegs)
        end)

val maxrepregs2 = if not rep_flag then 0
  else (let val k = MachSpec.numRegs - MachSpec.numCalleeSaves
         in Int.min(k-2,MachSpec.maxRepRegs)
        end)
**)

val maxrepregs1 = 
  if not rep_flag then 0
  else (let val k = MachSpec.numRegs - MachSpec.numCalleeSaves
         in Int.min(Int.min(k-2,MachSpec.numFloatRegs-1),MachSpec.numArgRegs)
        end)
 
val maxrepregs2 = 
   if not rep_flag then 0
   else (let val k = MachSpec.numRegs - MachSpec.numCalleeSaves
          in Int.min(Int.min(k-2,MachSpec.numFloatRegs-1),MachSpec.maxRepRegs)
         end)

local open Intmap
      exception Rename
      val m : value intmap = new(32, Rename)
      val rename = map m

   in fun ren v = rename v handle Rename => VAR v
      val newname = add m
  end

local open Intmap
in 

exception TypeInfo
val typtable : LT.lty intmap = new(32, TypeInfo)
val mapty = 
     if rep_flag then 
       (fn v => (map typtable v) 
             handle TypeInfo => 
 	              (List.app say
		       ["The lvar ", LV.lvarName v,
                  " is not in the current hashtable!\n"];
                  bug "TypeInfo hash table in convert.sml"))
     else (fn v => LT.ltc_void)
val addty = if rep_flag then (add typtable) else (fn v => ())
val rmvty = if rep_flag then (rmv typtable) else (fn v => ())
val nthty = if rep_flag then List.nth else (fn _ => LT.ltc_void)
fun grabty(VAR v) = mapty v
  | grabty(LABEL v) = mapty v
  | grabty(INT _) = LT.ltc_int
	| grabty(INT32 _) = LT.ltc_int32
  | grabty(REAL _) = LT.ltc_real
  | grabty _ = LT.ltc_void

end (* end of local open Intmap *)

val mkLvar = LV.mkLvar

fun mkfn(f,t) = 
  let val v = mkLvar()
   in addty(v,t); f v
  end

fun mkv(t) = 
  let val v = mkLvar()
   in addty(v,t); v
  end

val bogus_cont = mkv(lt_vcont)

val unboxedfloat = MachSpec.unboxedFloats
val untaggedint = MachSpec.untaggedInt
val flatfblock = (!Control.CG.flatfblock) andalso unboxedfloat

fun unwrapfloat(u,x,ce) = PURE(P.funwrap,[u],x,FLTt,ce)
fun wrapfloat(u,x,ce) = PURE(P.fwrap,[u],x,BOGt,ce)
fun unwrapint(u,x,ce) = PURE(P.iunwrap,[u],x,INTt,ce)
fun wrapint(u,x,ce) = PURE(P.iwrap,[u],x,BOGt,ce)
fun unwrapi32(u,x,ce) = PURE(P.i32unwrap,[u],x,INT32t,ce)
fun wrapi32(u,x,ce) = PURE(P.i32wrap,[u],x,BOGt,ce)

fun primwrap(INTt) = P.iwrap
  | primwrap(INT32t) = P.i32wrap
  | primwrap(FLTt) = P.fwrap
  | primwrap _ = P.wrap

fun primunwrap(INTt) = P.iunwrap
  | primunwrap(INT32t) = P.i32unwrap
  | primunwrap(FLTt) = P.funwrap
  | primunwrap _ = P.unwrap

(* check if a record contains only reals *)
fun isFloatRec lt = 
  if (LT.ltp_tyc lt) then
    (let val tc = LT.ltd_tyc lt
      in if (LT.tcp_tuple tc) then
           (let val l = LT.tcd_tuple tc
                fun h [] = flatfblock
                  | h (x::r) = 
                      if LT.tc_eqv(x, LT.tcc_real) then h r else false
             in case l of [] => false | _ => h l
            end)
         else false
     end)
  else false

fun selectFL(i,u,x,ct,ce) = SELECT(i,u,x,ct,ce)
fun selectNM(i,u,x,ct,ce) =
  (case (ct,unboxedfloat,untaggedint)
    of (FLTt,true,_) => let val v = mkLvar()
                         in SELECT(i,u,v,BOGt,unwrapfloat(VAR v,x,ce))
                        end
     | (INTt,_,true) => let val v = mkLvar()
                         in SELECT(i,u,v,BOGt,unwrapint(VAR v,x,ce))
                        end
     | (INT32t,_,_) => let val v = mkLvar()
                        in SELECT(i,u,v,BOGt,unwrapi32(VAR v,x,ce))
                       end
     | _ => SELECT(i,u,x,ct,ce))

fun recordFL(ul,_,w,ce) = 
  let val nul = map (fn u => (u,OFFp 0)) ul
   in RECORD(RK_FBLOCK,nul,w,ce)
  end

fun recordNM(ul,tyl,w,ce) =
  let fun g(FLTt::r,u::z,l,h) = 
             if unboxedfloat then 
               (let val v = mkLvar()
                 in g(r, z, (VAR v,OFFp 0)::l, fn ce => h(wrapfloat(u,v,ce)))
                end)
             else g(r, z, (u,OFFp 0)::l, h)
        | g(INTt::r,u::z,l,h) = 
             if untaggedint then 
               (let val v = mkLvar()
                 in g(r, z, (VAR v,OFFp 0)::l, fn ce => h(wrapint(u,v,ce)))
                end)
             else g(r, z, (u,OFFp 0)::l, h)
        | g(INT32t::r,u::z,l,h) = 
             let val v = mkLvar()
              in g(r, z, (VAR v,OFFp 0)::l, fn ce => h(wrapi32(u,v,ce)))
             end
        | g(_::r,u::z,l,h) = g(r, z, (u,OFFp0)::l, h)
        | g([],[],l,h) = (rev l, h)
        | g _ = bug "unexpected in recordNM in convert"

      val (nul,header) = 
        if rep_flag then g(map ctype tyl,ul,[],fn x => x)
        else (map (fn u => (u,OFFp 0)) ul, fn x => x)
   in header(RECORD(RK_RECORD,nul,w,ce))
  end

fun convpath(DA.LVAR v, k) = k(ren v)
  | convpath(DA.PATH(p, i), k) =
       let fun kont(v) =
            let val t = selectLty(grabty(v),i)
                val w = mkv(t)
             in SELECT(i, v, w, ctype t, k(VAR w))
            end
        in convpath(p,kont)
       end
  | convpath _ = bug "unexpected path in convpath"

(* BUG: The defintion of E_word is clearly incorrect since it can raise
 *	an overflow at code generation time. A clean solution would be 
 *	to add a WORD constructor into the CPS language -- daunting! The
 *	revolting hack solution would be to put the right int constant 
 *	that gets converted to the right set of bits for the word constant.
 *)
val do_switch = Switch.switch {
   E_int = fn i => if i < ~0x20000000 orelse i >= 0x20000000
                 then raise Switch.TooBig else INT i, 
   E_word = fn w => if w >= 0wx20000000 
                 then raise Switch.TooBig else INT (Word.toIntX w),
   E_real = fn s => REAL s,
   E_switchlimit = 4,
   E_neq = P.ineq,
   E_w32neq = P.cmp{oper=P.neq,kind=P.UINT 32},
   E_i32neq = P.cmp{oper=P.neq,kind=P.INT 32},
   E_word32 = INT32,
   E_int32 = INT32, 
   E_wneq = P.cmp{oper=P.neq, kind=P.UINT 31},
   E_pneq = P.pneq,
   E_fneq = P.fneq,
   E_less = P.ilt,
   E_branch= fn(cmp,x,y,a,b) => BRANCH(cmp,[x,y],mkv(LT.ltc_int),a,b),
   E_strneq= fn(w,str,a,b) => BRANCH(P.strneq, [INT(size str),w,STRING str],
				     mkv(LT.ltc_int), a, b),
   E_switch= fn(v,list) => SWITCH(v, mkv(LT.ltc_int), list),
   E_add= fn(x,y,c) => let val v = mkv(LT.ltc_int) in ARITH(P.iadd,[x,y],v,INTt,c(VAR v))
		     end,
   E_gettag= fn(x,c) => let val v = mkv(LT.ltc_int)
                     in PURE(P.getcon,[x],v,INTt,c(VAR v))
		    end,
   E_unwrap= fn(x,c) => let val v = mkv(LT.ltc_int)
                     in PURE(P.unwrap,[x],v,INTt,c(VAR v))
		    end,
   E_getexn= fn(x,c) => let val v = mkv(LT.ltc_void)
                     in PURE(P.getexn,[x],v,BOGt,c(VAR v))
		    end,
   E_length= fn(x,c) => let val v = mkv(LT.ltc_int)
                     in PURE(P.length,[x],v,INTt,c(VAR v))
		    end,
   E_boxed= fn(x,a,b) => BRANCH(P.boxed,[x],mkv(LT.ltc_int),a,b),
   E_path= convpath}

		     
(***************************************************************************
 *        mkArgIn : lty * lvar -> lvar list * cty list * (cexp -> cexp)    *
 *       mkArgOut : lty * value -> value list * (cexp -> cexp)             *
 *                                                                         *
 * When the type of the argument x of a function f(x) is an "small"        *
 * unboxed record, f will be transformed to a multi-argument function      *
 * with #1(mkArgIn(...,x)) as its list of arguments.                       *
 *                                                                         *
 * When a function f is applied to a argument x, and x is of a "small"     *
 * unboxed record type, x will be flattened. #1(mkArgOut(...,x)) will      *
 * become the actual arguments of the function call f.                     *
 *                                                                         *
 * When the Control.CG.representations flag is turned off, all             *
 * these effects are gone.  (l >> 0)                                       *
 ***************************************************************************)

fun tc_size t = 
  LT.tcw_tuple(t, 
               fn ts => case ts of [] => 1
                                 | _ =>foldr (op +) 0 (map tc_size ts),
               fn _ => 1)

fun lt_size t = LT.ltw_tyc (t, fn tc => tc_size tc, fn _ => 1)

fun tc_length t = 
  LT.tcw_tuple(t, 
               fn ts => case ts of [] => 1
                                 | _ => length ts,
               fn _ => 1)

fun lt_length t = LT.ltw_tyc (t, fn tc => tc_length tc, fn _ => 1)

fun mkArgIn0(t,v) = 
  let val l = lt_size(t)
      fun megl((vl1,cl1,f1),(vl2,cl2,f2)) = (vl1 @ vl2, cl1 @ cl2, f1 o f2)

      (* recFlat: recursive flatten *)
      fun recFlat(tt,p) = 
        LT.ltw_tuple (tt, 
           fn args => (case args
                        of [] => ([p],[INTt],id)
                         | _ => 
                            let val args = map LT.ltc_tyc args
                                val ul = map (fn t => mkv(t)) args
                                val recordCE = 
                                 if isFloatRec tt then recordFL else recordNM  
                                val header = 
                                 fn ce => recordCE(map VAR ul,args,p,ce)
                             in foldr megl ([], [], header) 
                                 (ListPair.map recFlat (args,ul))
                            end),
           fn tt => ([p],[ctype tt],id))

      (* oneFlat: flatten only one level *)
      fun oneFlat (tt,p) =
        LT.ltw_tuple (tt, 
           fn args => let val args = map LT.ltc_tyc args 
                          val wl = map (fn t => mkv(t)) args
                          val cl = map ctype args
                          val recordCE = 
                           if isFloatRec tt then recordFL else recordNM  
                          val header = fn ce => recordCE(map VAR wl,args,p,ce)
                       in (wl,cl,header) 
                      end,
           fn tt => ([p],[ctype(tt)],id))

   in if l < maxrepregs1 then recFlat(t,v)
      else (let val s = lt_length(t)
             in if s < maxrepregs2 then oneFlat(t,v)
                else ([v],[ctype(t)],id)
            end)
  end

fun mkArgIn(t,v) = mkArgIn0(cvtrfty t,v)

fun mkArgOut0(t,z as VAR v) = 
  let val l = lt_size(t)
      fun megr((vl1,f1),(vl2,f2)) = ((vl1 @ vl2), f2 o f1)
  
      fun recFlat (tt,p) = 
        LT.ltw_tuple (tt, 
           fn args => 
             (case args
               of [] => ([VAR p],id)
                | _ => 
                    let val args = map LT.ltc_tyc args
                        val wl = map (fn t => (t, mkv(t))) args
                        val selectCE = 
                          if isFloatRec tt then selectFL else selectNM

                        fun sel((t,x)::tl,i) = 
                              let val header = sel(tl,i+1)
                               in fn ce => selectCE(i, VAR p, x, ctype(t), 
                                                    header(ce))
                              end
                          | sel(nil,i) = id
  
                        val header = sel(wl,0)
                     in foldr megr ([], header) (map recFlat wl)
                    end),
           fn _ => ([VAR p],id))

      fun oneFlat (tt,p) = 
        LT.ltw_tuple (tt, 
           fn args => 
             let val args = map LT.ltc_tyc args
                 val wl = map (fn t => (mkv(t), ctype(t))) args
                 val selectCE = 
                   if isFloatRec tt then selectFL else selectNM
                 fun sel((x,ct)::tl,i) = 
                       let val header = sel(tl,i+1)
                        in fn ce => selectCE(i, VAR p, x, ct, 
                                             header(ce))
                       end
                   | sel(nil,i) = id
                 val header = sel(wl,0)
              in (map (VAR o #1)  wl,header) 
             end,
           fn _ => ([VAR p],id))
                            
   in if l < maxrepregs1 then recFlat(t,v)
      else (let val s = lt_length(t)
             in if s < maxrepregs2 then oneFlat(t,v)
                else ([z],id)
            end)
  end
  | mkArgOut0(t,z) = ([z],id) 

fun mkArgOut(t,v) = mkArgOut0(cvtrfty t,v)


(***************************************************************************
 *           preventEta : cexp * lty -> cexp * value                       *
 ***************************************************************************)
fun preventEta(c,argt) =
  let val f = mkv(ltc_cont [argt]) and v = mkv(argt)
      val (vl,cl,header) = mkArgIn(argt,v)
      val b = header(c(VAR v))
   in case b
       of APP(w as VAR w', [VAR v']) => 
            if v=v' andalso v<>w'
		(* The case v=w' never turns up in practice,
		   but v<>v' does turn up. *)
	    then (id,w)
	    else (fn x => FIX([(CONT,f,vl,cl,b)],x),VAR f)
	| _ => (fn x => FIX([(CONT,f,vl,cl,b)],x),VAR f)
  end

(***************************************************************************
 *   convlist : Lambda.lexp list * (value list -> cexp) -> cexp            *
 ***************************************************************************)
fun convlist (el,c) =
  let fun f(le::r, vl) = convle(le, fn v => f(r,v::vl))
	| f(nil, vl) = c (rev vl)
   in f (el,nil)
  end

(***************************************************************************
 *   getargs : int * Lambda.lexp * (value list -> cexp) -> cexp            *
 ***************************************************************************)
and getargs(1,a,g) = convle(a, fn z => g[z])
  | getargs(n,Lambda.RECORD l,g) = g (map convsv l)
  | getargs(n,Lambda.VECTOR(l, _), g) = g(map convsv l)
  | getargs(0,a,g) = g(nil)
  | getargs(n,a,g) =
     let fun kont(v) = 
           let val lt = grabty(v)
               val selectCE = if (isFloatRec lt) then selectFL else selectNM
               fun f(j,wl) = 
                 if j = n then g(rev wl)
                 else (let val tt = selectLty(lt,j)
                           fun h(w) = 
                             selectCE(j,v,w,ctype(tt),f(j+1,VAR w :: wl))
                        in mkfn(h,tt)
                       end)
            in f(0,nil)
           end
      in convle(a,kont)
     end

(***************************************************************************
 *   convsv : Lambda.value -> value                                        *
 *   convle : Lambda.lexp * (value list -> cexp) -> cexp                   *
 ***************************************************************************)
and convsv sv = 
 (case sv
   of Lambda.VAR v => ren v
    | Lambda.INT i => INT i
(*
        ((i+i+2; c(INT i)) handle Overflow =>
	 let open Lambda
 	  in convle(APPg(SVAL(PRIM(AP.IADD,IntOpTy,[])),
                      RECORD([INT(i div 2),INT(i - i div 2)])),c)
	 end)
*)
    | Lambda.INT32 i32 => 
        let val int32ToWord32 = Word32.fromLargeInt o Int32.toLarge
         in INT32 (int32ToWord32 i32)
        end
    | Lambda.WORD w => INT(Word.toIntX w)
(*
        let val maxWord = 0wx20000000
         in if Word.<(w, maxWord) then c(INT(Word.toIntX w))
            else let open Lambda
                     val addu = 
                       AP.ARITH{oper=AP.+, overflow=false, kind=AP.UINT 31}
                     val x1 = Word.div(w, 0w2)
                     val x2 = Word.-(w, x1)
                  in convle(APPg(SVAL(PRIM(addu, IntOpTy,[])), 
                                RECORD([WORD x1, WORD x2])), c)
                 end
        end
*)
    | Lambda.WORD32 w => INT32 w
    | Lambda.REAL i => REAL i
    | Lambda.STRING s =>  STRING s
    | Lambda.PRIM(i,lt,_) => bug "unexpected primop in convsv"
(*
        let (* val _ = print ("prim chkarrow "^(AP.prPrimop i)^"\n") *)
            val (t,_) = arrowLty(lt)
            val v = mkLvar()
            val e = Lambda.FN(v,t,Lambda.APP(sv, Lambda.VAR v))
         in convle(e,c)
        end
*)
    | _ => bug "unexpected case in convsv")

and convle (le, c : value -> cexp) = 
 case le 
  of Lambda.SVAL sv => c(convsv(sv))
   | Lambda.APP(Lambda.PRIM(AP.CALLCC,_,_), f) =>
       let val vf = convsv f
           val (t1,t2) = arrowLty(grabty(vf))
           val h = mkv(lt_scont)
           (* t1 must be SRCONTty here *)
           val k' = mkv(t1) and x' = mkv(t2) 
           val (header,F) = preventEta(c,t2)
           val (vl,cl,_) = mkArgIn(t2,x')
           val z = mkv(lt_vcont) (* bogus cont *)
        in header(LOOKER(P.gethdlr, [], h, FUNt,
                  FIX([(ESCAPE, k', z::vl, CNTt::cl, 
                          SETTER(P.sethdlr, [VAR h],
                                      APP(F, map VAR vl)))],
                         APP(vf,[F, VAR k']))))
       end
   | Lambda.APP(Lambda.PRIM(AP.CAPTURE,_,_), f) =>
       let val vf = convsv f
           val (t1,t2) = arrowLty(grabty(vf))
           val k' = mkv(t1) and x' = mkv(t2)
           val (header,F) = preventEta(c,t2)
           val (vl,cl,_) = mkArgIn(t2,x')     
           val z = mkv(lt_vcont) (* bogus cont *)
                 (* this k' is one kind of eta redexes that optimizer
                  * should not reduce! The type of k' and F is different.
                  *)
        in header(FIX([(ESCAPE, k', z::vl, CNTt::cl, 
                              APP(F, map VAR vl))],
                            APP(vf,[F, VAR k'])))
       end

   | Lambda.APP(Lambda.PRIM(AP.ISOLATE,_,_), f) =>
       let val vf = convsv f
           val k = mkv(lt_scont)
           val z = mkv(lt_vcont)
           val x = mkv(LT.ltc_void)
           val h = mkv(lt_scont)
           val z' = mkv(lt_vcont)
           val x' = mkv(LT.ltc_void)
        in FIX([(ESCAPE, h, [z', x'], [CNTt, BOGt],
                  APP(VAR bogus_cont, [VAR x']))],
               FIX([(ESCAPE, k, [z, x], [CNTt, BOGt],
                   SETTER(P.sethdlr, [VAR h],
                          APP(vf, [VAR bogus_cont, VAR x])))],
                   c(VAR k)))
       end

(* We can't do this because the of representation type problems:
   | Lambda.APP(Lambda.PRIM(AP.THROW,_,_), v) => convle(v,c)
*)
   | Lambda.APP(Lambda.PRIM(AP.THROW,_,_), v) => 
        let val kv = convsv v
            val t = LT.ltc_arw(LT.ltc_void,LT.ltc_void)
            val f = mkv(t)
         in PURE(P.cast,[kv],f,ctype(t),c(VAR f))
        end
   | Lambda.APP(Lambda.PRIM(AP.CAST,lt,_), x) => 
        let val vx = convsv x
            val (_,t) = arrowLty(lt)
         in mkfn(fn u => PURE(P.cast,[vx],u,ctype(t),c(VAR u)), t)
        end
   | Lambda.APP(Lambda.PRIM(i,lt,_), a) => 
       let val (argt,t) = arrowLty(lt)
           val ct = ctype t

           fun arith(n,i) = 
             let fun kont(vl) = mkfn(fn w => ARITH(i,vl,w,ct,c(VAR w)),t)
              in getargs(n, Lambda.SVAL a,kont)
             end

           fun setter(n,i) = 
             let fun kont(vl) = SETTER(i,vl,c(INT 0))
              in getargs(n, Lambda.SVAL a,kont)
             end

           fun looker(n,i) =
             let fun kont(vl) = mkfn(fn w => LOOKER(i,vl,w,ct,c(VAR w)),t)
              in getargs(n, Lambda.SVAL a,kont)
             end

           fun pure(n,i) =
             let fun kont(vl) = mkfn(fn w => PURE(i,vl,w,ct,c(VAR w)),t)
              in getargs(n, Lambda.SVAL a,kont)
             end

  	   fun branch(n,i)= 
             let val (header,F) = preventEta(c,t) 
                 fun kont(vl) = header(BRANCH(i,vl,mkv(LT.ltc_int),
                                              APP(F,[INT 1]),APP(F,[INT 0])))
              in getargs(n, Lambda.SVAL a,kont)
             end

        in case i
	    of AP.BOXED => branch(1,P.boxed)
	     | AP.UNBOXED => branch(1,P.unboxed)
	     | AP.CMP stuff => branch(2,cmpop(stuff,argt))
	     | AP.PTREQL => branch(2,P.peql)
	     | AP.PTRNEQ => branch(2,P.pneq)

	     | AP.TEST(from,to) => arith(1, P.test(from, to))
	     | AP.TESTU(from,to) => arith(1, P.testu(from, to))
	     | AP.COPY(from,to) => pure(1, P.copy(from,to))
	     | AP.EXTEND(from,to) => pure(1, P.extend(from, to))
	     | AP.TRUNC(from,to) => pure(1, P.trunc(from, to))
	     | AP.ARITH{oper,kind,overflow=true} =>
		arith(arity oper,
		      P.arith{oper=arithop oper,kind=numkind kind})
	     | AP.ARITH{oper,kind,overflow=false} =>
		pure(arity oper,
		     P.pure_arith{oper=arithop oper,kind=numkind kind})

	     | AP.ROUND{floor,fromkind,tokind} =>
		arith(1,P.round{floor=floor,
				fromkind=numkind fromkind,
				tokind=numkind tokind})

             | AP.REAL{fromkind,tokind} =>
		pure(1,P.real{tokind=numkind tokind,
			      fromkind=numkind fromkind})

	     | AP.SUBSCRIPTV => pure(2,P.subscriptv)
	     | AP.MAKEREF => pure(1,P.makeref)
	     | AP.LENGTH => pure(1,P.length)
	     | AP.OBJLENGTH => pure(1,P.objlength)
	     | AP.GETTAG => pure(1, P.gettag)
	     | AP.MKSPECIAL => pure(2, P.mkspecial)
		
	     | AP.SUBSCRIPT => looker(2,P.subscript)
	     | AP.NUMSUBSCRIPT{kind,immutable=false,checked=false} => 
		   looker(2,P.numsubscript{kind=numkind kind})
	     | AP.NUMSUBSCRIPT{kind,immutable=true,checked=false} => 
		   pure(2,P.pure_numsubscript{kind=numkind kind})
	     | AP.DEREF => looker(1,P.!)
	     | AP.GETRUNVEC => looker(0, P.getrunvec)
	     | AP.GETHDLR => looker(0,P.gethdlr)
	     | AP.GETVAR  => looker(0,P.getvar)
             | AP.GETPSEUDO => looker(1,P.getpseudo)
	     | AP.GETSPECIAL => looker(1, P.getspecial)
	     | AP.DEFLVAR  => looker(0,P.deflvar)
		
	     | AP.SETHDLR => setter(1,P.sethdlr)
	     | AP.NUMUPDATE{kind,checked=false} =>
		   setter(3,P.numupdate{kind=numkind kind})
	     | AP.UNBOXEDUPDATE => setter(3,P.unboxedupdate)
	     | AP.BOXEDUPDATE => setter(3,P.boxedupdate)
	     | AP.UPDATE => setter(3,P.update)
	     | AP.SETVAR => setter(1,P.setvar)
             | AP.SETPSEUDO => setter(2,P.setpseudo)
             | AP.SETMARK => setter(1,P.setmark)
             | AP.DISPOSE => setter(1,P.free)
	     | AP.SETSPECIAL => setter(2, P.setspecial)
	     | AP.USELVAR => setter(1,P.uselvar)
	     | AP.MARKEXN => getargs(2, Lambda.SVAL a,fn[x,m']=>
	  	  let val bty = LT.ltc_void
                      val ety = LT.ltc_tuple[bty,bty,bty]

                      val xx = mkv ety
		      val x0 = mkv bty
		      val x1 = mkv bty
		      val x2 = mkv bty

                      val y = mkv ety
                      val y' = mkv bty

		      val z = mkv(LT.ltc_tuple[bty,bty])
                      val z' = mkv bty

                   in PURE(P.unwrap,[x],xx,ctype(ety),
                        SELECT(0,VAR xx,x0,BOGt,
   		        SELECT(1,VAR xx,x1,BOGt,
		        SELECT(2,VAR xx,x2,BOGt,
		          RECORD(RK_RECORD,[(m',OFFp0),(VAR x2,OFFp0)],z,
                          PURE(P.wrap,[VAR z],z',BOGt,
  		          RECORD(RK_RECORD,[(VAR x0,OFFp0),
				            (VAR x1,OFFp0),
					    (VAR z', OFFp0)], y,
                          PURE(P.wrap,[VAR y], y', BOGt,c(VAR y')))))))))
                  end)

	     | _ => bug ("calling with bad primop \"" 
                                         ^ (AP.prPrimop i) ^ "\"")
       end
   | Lambda.ETAG(v,_) =>
       let val u = convsv v
           val x = mkv(LT.ltc_void) 
        in PURE(P.makeref,[u],x,BOGt,c(VAR x))
       end
   | Lambda.FN(v,t,e) =>   (* using "save" the reference cell is 
                              dirty, but i can't find better way *)
       let val _ = addty(v,t)
           val save = ref LT.ltc_void and k = mkLvar()
           fun kont(vb) =
             let val t = grabty(vb)
                 val _ = (save := t)
                 val (ul,header) = mkArgOut(t,vb)
              in header(APP(VAR k,ul))
             end
           val ce = convle(e,kont)
           val t1 = !save
           val f = mkv(LT.ltc_fun(t,t1)) 
           val _ = (addty(k, ltc_cont [t1]))
           val (vl,cl,header) = mkArgIn(t,v)
        in FIX([(ESCAPE,f,k::vl,CNTt::cl,header(ce))],c(VAR f))
       end
   | Lambda.APP(f,a) =>   (* different from the old version in 
                             that header is now put in the middle
                             of evaluations between f and a, a bit odd *)
       let val vf = convsv f
           val (t1,t2) = arrowLty(grabty(vf))
           val (header,F) = preventEta(c,t2)
           val va = convsv a
           val (ul,header') = mkArgOut(t1,va)
        in header(header'(APP(vf,F::ul)))
       end
   | Lambda.FIX(fl, tl,  el, body) =>
       let fun g(f::fl, t::tl, Lambda.FN(v,_,b)::el) =
                let val (t1,t2) = arrowLty(t)
                    val _ = addty(v,t1) 
                    val k = mkv(ltc_cont [t2])
                    val (vl,cl,header) = mkArgIn(t1,v)
                    fun kont(vb) = 
                       let val (ul,header') = mkArgOut(t2,vb)
                        in header'(APP(VAR k,ul))
                       end
                    val be = convle(b,kont)
                 in (ESCAPE,f,k::vl,CNTt::cl,header(be))::g(fl,tl,el)
                end
             | g(nil, nil, nil) = nil
             | g _ = bug "convert.conv.FIX1"

           fun h(f::fl,t::tl) = (addty(f,t);h(fl,tl))
             | h(nil,nil) = ()
             | h _ = bug "convert.conv.FIX2"

           val _ = h(fl,tl)
        in FIX(g(fl,tl,el),convle(body,c))
       end
   | Lambda.RECORD [] => c(INT 0) 
                         (* bug "zero length records in convert" *)
   | Lambda.SRECORD [] => c(INT 0) 
                         (* bug "zero length records in convert" *)
   | Lambda.VECTOR ([], _) => bug "zero length vectors in convert"
   | Lambda.RECORD l => 
       let val vl = map convsv l
           val tyl = map grabty vl
           val lt = LT.ltc_tuple tyl
           val recordCE = 
             if (isFloatRec lt) then recordFL else recordNM
           val w = mkv(lt)
        in recordCE(vl,tyl,w,c(VAR w))
       end                
   | Lambda.SRECORD l => 
       let val vl = map convsv l
           val ts = map grabty vl
           val w = mkv(LT.ltc_str ts)
        in recordNM(vl,ts,w,c(VAR w))
       end                
   | Lambda.VECTOR (l, _) => 
       let val vl = map convsv l
           val w = mkv(LT.ltc_void)
        in RECORD(RK_VECTOR, map (fn v => (v, OFFp0)) vl, w, c(VAR w))
       end
   | Lambda.SELECT(i, v) => 
       let val v = convsv v
           val lt = grabty(v)
           val t = selectLty(lt,i)
           val w = mkv(t)
           val selectCE = if (isFloatRec lt) then selectFL else selectNM
        in selectCE(i, v, w, ctype t, c(VAR w))
       end
   | Lambda.SWITCH(e,l,[a as (Lambda.DATAcon(_,DA.CONSTANT 0,_),_),
		        b as (Lambda.DATAcon(_,DA.CONSTANT 1,_),_)], 
                   NONE) =>
       convle(Lambda.SWITCH(e,l,[b,a],NONE),c)
(*
   | Lambda.LET(v, x as Lambda.APP(oper, args), 
                Lambda.SWITCH(VAR z, _,
                   [(Lambda.DATAcon(_,DA.CONSTANT 1,_),e1),
	           (Lambda.DATAcon(_,DA.CONSTANT 0,_),e2)],NONE)) =>
       let fun g i' =
	     let val k = mkLvar() and save = ref LT.ltc_void
                 fun kont(w) = 
                   let val t = grabty(w) 
                       val _ = (save := t)
                       val (ul,header1) = mkArgOut(t,w)
                    in header1(APP(VAR k,ul))
                   end
                 val ce1 = convle(e1,kont) and ce2 = convle(e2,kont)
                 val t = !save
                 val _ = addty(k, ltc_cont [t]) and v = mkv(t)
                 val (vl,cl,header) = mkArgIn(t,v)
	      in FIX([(CONT,k,vl,cl,header(c(VAR v)))],
                  getargs(2,args,
                          fn vl => BRANCH(i',vl,mkv(LT.ltc_int),ce1,ce2)))
	     end
        in case oper
	    of Lambda.PRIM(AP.CMP stuff,lt,_) => 
                g(cmpop(stuff,#1(arrowLty lt)))
	     | Lambda.PRIM(AP.PTREQL,_,_) => g(P.peql)
	     | Lambda.PRIM(AP.PTRNEQ,_,_) => g(P.pneq)
	     | _ => genswitch(x,c)
       end
*)
   | Lambda.SWITCH x => genswitch(x,c)
   | Lambda.LET(v,a,e) =>
       let fun kont(w) = 
             let val _ = newname(v,w)
                 val _ = addty(v,grabty(w))
                 val _ = case w of VAR w' => LV.sameName(v,w')
                                 | _ => ()
              in convle(e,c)
             end
        in convle(a,kont)
       end
   | Lambda.RAISE(v,t) =>
       let val w = convsv v
           val h = mkv(lt_scont)
           val _ = mkfn(fn u => c(VAR u), t)
        in LOOKER(P.gethdlr,[],h,FUNt,APP(VAR h,[VAR bogus_cont,w]))
       end
   | Lambda.HANDLE(a,b) =>
       let val vb = convsv b
           val (_,t) = arrowLty(grabty(vb))
           val h = mkv(lt_scont) 
           val v = mkv(LT.ltc_void)
           val k = mkv(lt_scont)
           val (header,F) = preventEta(c,t)
           fun kont1(va) = 
                  let val (ul,header1) = mkArgOut(t,va)
                    in SETTER(P.sethdlr,[VAR h],
                              header1(APP(F,ul)))
                   end
        in LOOKER(P.gethdlr,[],h,FUNt,
                header(FIX([(ESCAPE,k,[mkv(lt_vcont),v],
                             [CNTt,BOGt],
                               SETTER(P.sethdlr,[VAR h],APP(vb,[F,VAR v])))],
                              SETTER(P.sethdlr,[VAR k],convle(a,kont1)))))

       end
   | Lambda.WRAP(t,_,sv) => 
       let val w = convsv sv
           val t = grabty(w)
           val ct = ctype t
           val x = mkv(LT.ltc_void)
        in PURE(primwrap ct,[w],x,BOGt,c(VAR x))
       end
   | Lambda.UNWRAP(t,_,sv) => 
       let val t = LT.ltc_tyc t
           val ct = ctype t
           val w = convsv sv
           val x = mkv(t)
        in PURE(primunwrap ct,[w],x,ct,c(VAR x))
       end
   | _ => bug "convert.sml 7432894"


(***************************************************************************
 * genswitch : (Lambda.lexp * Access.conrep list * (Lambda.con *           *
 *                 Lambda.lexp) list * Lambda.lexp option) *               *
 *              (value -> cexp) -> cexp                                    *
 ***************************************************************************)
and genswitch ((sv, sign, l: (Lambda.con * Lambda.lexp) list, d),c) =
  let val df = mkv(ltc_cont [LT.ltc_int]) 
      val save = ref LT.ltc_void
      val k = mkLvar()
      fun kont1(z) = 
        let val t = grabty z
            val _ = (save := t)
            val (ul,header) = mkArgOut(t,z)
         in header(APP(VAR k,ul))
        end

      val l' = map (fn(c,e)=>(c,convle(e,kont1))) l

      val body=
        do_switch{sign=sign,exp=convsv sv,cases=l',default=APP(VAR df,[INT 0])}

      val body' = case d 
         of NONE => body
	  | SOME d' => FIX([(CONT,df,[mkv(LT.ltc_int)],[INTt],
                             convle(d',kont1))], body)

      val t = !save
      val v = mkv(t) 
      val _ = (addty(k, ltc_cont [t]))
      val (vl,cl,header) = mkArgIn(t,v)
   in FIX([(CONT,k,vl,cl,header(c(VAR v)))],body')
  end

val save = ref LT.ltc_void and k = mkLvar() and f = mkLvar() and v = mkLvar()
fun kont(w) = 
  let val t = grabty(w)
      val (t1,t2) = arrowLty(t)
      val _ = (addty(k, ltc_cont [t2]); addty(f,t); addty(v,t1); save := t1)
      val (ul,header) = mkArgOut(t1,VAR v)
   in header(APP(w,(VAR k)::ul))
  end

(**** We don't support unrolled lists for the time being ****
val lexp = 
  if (MachSpec.newListRep) 
  then (TransList.translist(MachSpec.listCellSz,lexp)) 
  else lexp
****)

(* val _ = MCprint.printLexp lexp *)
val body = convle(lexp,kont)
val (vl,cl,header) = mkArgIn(!save,v)

val bogus_knownf = mkv(lt_vcont)
val bogushead = 
     fn ce => FIX([(KNOWN,bogus_knownf,[mkv(LT.ltc_void)],[BOGt],
                    APP(VAR bogus_knownf,[STRING "bogus"]))],
                  FIX([(CONT,bogus_cont,[mkv(LT.ltc_void)],[BOGt],
                        APP(VAR bogus_knownf,[STRING "bogus"]))],ce))

in ((ESCAPE,f,k::vl,CNTt::cl,header(bogushead(body))),typtable)
end

end (* toplevel local *)
end (* functor Convert *)


