(* copyright 1998 YALE FLINT PROJECT *)

signature FCONTRACT =
sig
    
    (* needs Collect to be setup properly *)
    val contract : FLINT.fundec -> FLINT.fundec
	
end

(* All kinds of beta-reductions.  In order to do as much work per pass as
 * possible, the usage counts of each variable (maintained by the Collect
 * module) is kept as much uptodate as possible.  For instance as soon as a
 * variable becomes dead, all the variables that were referenced have their
 * usage counts decremented correspondingly.  This means that we have to
 * be careful to make sure that a dead variable will indeed not appear
 * in the output lexp since it might else reference other dead variables *)

(* things that lcontract did that fcontract doesn't do (yet):
 *
 * - inline across DeBruijn depths
 * - switch(con) concellation
 * - elimination of let [dead-vs] = pure in body *)

structure FContract :> FCONTRACT =
struct
local
    structure F  = FLINT
    structure M  = Intmap
    structure C  = Collect
    structure DI = DebIndex
    structure PP = PPFlint
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("FContract: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; bug msg)

(* fun sayexn e = app say (map (fn s => s^" <- ") (SMLofNJ.exnHistory e)) *)

fun ASSERT (true,_) = ()
  | ASSERT (FALSE,msg) = bug ("assertion "^msg^" failed")

datatype sval
  = Val    of F.value
  | Var    of F.lvar
  | Fun    of F.lvar * F.lexp * (F.lvar * F.lty) list * F.fkind * DI.depth
  | TFun   of F.lvar * F.lexp * (F.tvar * F.tkind) list * DI.depth
  | Record of F.lvar * F.value list
  | Select of F.lvar * F.value * int
  | Con    of F.lvar * F.value * F.dcon

exception NotFound
val m : sval M.intmap = M.new(128, NotFound)

fun cexp (cfg as (d,od)) le = let

    val loop = cexp cfg

    fun used lv = C.usenb lv > 0
    
    fun impurePO po = true		(* if a PrimOP is pure or not *)

    fun lookup lv = M.map m lv
(* 			handle e as NotFound => *)
(* 			(say (concat ["\nlooking up unbound ", *)
(* 				      !PP.LVarString lv]); *)
(* 			 raise e) *)

    fun sval2val sv =
	case sv
	 of (Var lv | Fun{1=lv,...} | TFun{1=lv,...} | Record{1=lv,...}
	  | Select{1=lv,...} | Con{1=lv,...} | Val (F.VAR lv)) => F.VAR lv
	  | Val v => v
			 
    fun val2sval (F.VAR ov) = ((lookup ov) handle x => raise x)
      | val2sval v = Val v

    fun bugsv (msg,sv) = bugval(msg, sval2val sv)

    fun subst ov = sval2val (lookup ov)
    val substval = sval2val o val2sval
    fun substvar lv =
	case substval (F.VAR lv)
	 of F.VAR lv => lv
	  | v => bugval ("unexpected val", v)

    fun unuseval f (F.VAR lv) = C.unuse f false lv
      | unuseval f _ = ()

    (* called when a variable becomes dead.
     * it simply adjusts the use-counts *)
    fun undertake lv =
	(case lookup lv
	 of Val v		=> unuseval undertake v
	  | Var nlv		=> ASSERT(nlv = lv, "nlv = lv")
	  | ( Fun {1=lv,2=le,...} | TFun{1=lv,2=le,...}	) =>
	     C.inside lv (fn()=> C.unuselexp undertake le)
	  | ( Select {2=v,...} | Con {2=v,...} ) =>
	     unuseval undertake v
	  | Record {2=vs,...}	=> app (unuseval undertake) vs)
	     handle x =>
	     (say "while undertaking "; PP.printSval(F.VAR lv); say "\n";
	      raise x)

    fun addbind (lv,sv) =
	let fun eqsv (sv1,sv2) = (sval2val sv1) = (sval2val sv2)
	    fun correct (Val v) = true
	      | correct sv =
		let val F.VAR lv = sval2val sv
		in eqsv(sv, M.map m lv)
		end handle NotFound => true
	in  ASSERT(correct sv, "addbind");
	    M.add m (lv, sv)
	end

    (* substitute a value sv for a variable lv and unuse value v *)
    fun substitute (lv1, sv, v) =
	(case sval2val sv of F.VAR lv2 => C.transfer(lv1,lv2) | v2 => ();
	 unuseval undertake v;
	 addbind (lv1, sv)) handle x => raise x

    (* common code for all the lexps "let v = <op>[v1,...] in ..." *)
    fun clet1 (svcon,lecon) (lv,vs,le) =
	if used lv then
	    let val nvs = map substval vs
		val _ = addbind (lv, svcon(nvs))
		val nle = loop le
	    in if used lv then lecon(nvs, nle) else nle
	    end
	else loop le

    (* common code for primops *)
    fun cpo (SOME{default,table},po,lty,tycs) =
	(SOME{default=substvar default,
	      table=map (fn (tycs,lv) => (tycs, substvar lv)) table},
	 po,lty,tycs)
      | cpo po = po

    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) =
	(s, Access.EXN(Access.LVAR(substvar lv)), lty)
      | cdcon dc = dc

in
    case le
     of F.RET vs => (F.RET (map substval vs) handle x => raise x)

      | F.LET (lvs,le,body) =>
	(let fun clet (F.LET(lvs1,le1,le2)) = F.LET(lvs1, le1, clet le2)
	       (* let associativity
		* !!BEWARE!! applying the associativity rule might
		* change the liveness of the bound variables *)
	       | clet (F.FIX(fdecs,le)) =
		 let val nbody = clet le
		     val nfdecs = List.filter (used o #2) fdecs
		 in if null nfdecs then nbody else F.FIX(nfdecs, nbody)
		 end
	       | clet (F.TFN(tfdec,le)) =
		 let val nbody = clet le
		 in if used (#1 tfdec) then F.TFN(tfdec, nbody) else nbody
		 end
	       | clet (F.CON(dc,tycs,v,lv,le)) =
		 let val nbody = clet le
		 in if used lv then F.CON(dc, tycs, v, lv, nbody) else nbody
		 end
	       | clet (F.RECORD(rk,vs,lv,le)) =
		 let val nbody = clet le
		 in if used lv then F.RECORD(rk, vs, lv, nbody) else nbody
		 end
	       | clet (F.SELECT(v,i,lv,le)) =
		 let val nbody = clet le
		 in if used lv then F.SELECT(v, i, lv, nbody) else nbody
		 end
	       | clet (F.PRIMOP(po,vs,lv,le)) =
		 let val nbody = clet le
		 in if impurePO po orelse used lv
		    then F.PRIMOP(po, vs, lv, nbody)
		    else nbody
		 end
	       (* F.RAISE never returns so the body of the let could be
		* dropped on the floor, but since I don't propagate
		* types I can't come up with the right return type
		*  | F.RAISE(v,ltys) =>
		*    (C.unuselexp undertake body;
		*     F.RAISE(v, ?????)) *)
	       | clet (F.RET vs) =
		 (* LET[lvs] = RET[vs] is replaced by substitutions *)
		 (app (fn (lv,v) => substitute (lv, val2sval v, v))
		      (ListPair.zip(lvs, vs));
		      loop body)
	       | clet le =
		 (app (fn lv => addbind (lv, Var lv)) lvs;
		  case loop body
		   of F.RET vs => if vs = (map F.VAR lvs) then le
				  else F.LET(lvs, le, F.RET vs)
		    | nbody => F.LET(lvs, le, nbody))
	in
	    clet (loop le)
	end handle x => raise x)
	
      | F.FIX (fs,le) =>
	(let fun cfun [] acc = rev acc
	       | cfun (fdec as (fk,f,args,body)::fs) acc =
		 if used f then
		     let (* make up the bindings for args inside the body *)
			 val _ = app (fn lv => addbind (lv, Var lv))
				     (map #1 args)
			 (* contract the body and create the resulting fundec *)
			 val nbody = C.inside f (fn()=> loop body)
			 val nsv = Fun(f, nbody, args, fk, od)
				      
			 val _ = (* update the subst with the new code *)
			     case nbody (* look for eta redex *)
			      of F.APP(g,vs) =>
				 if vs = (map (F.VAR o #1) args)
				 then substitute (f, val2sval g, g)
				 else addbind (f, nsv)
			       | _ => addbind (f, nsv)
		     in cfun fs ((fk, f, args, nbody)::acc)
		     end
		 else cfun fs acc

	     (* register the new bindings. We register them
	      * uncontracted first, for the needs of mutual recursion,
	      * and then we replace the contracted versions as they
	      * become available *)
	     val _ = app (fn fdec as (fk,f,args,body) =>
			  addbind (f, Fun(f, body, args, fk, od)))
			 fs
			    
	     (* recurse on the bodies *)
	     val fs = cfun fs []
				
	     val nle = loop le		(* contract the main body *)
	     val fs = List.filter (used o #2) fs (* junk newly unused funs *)
	in
	    if List.null fs
	    then nle
	    else F.FIX(fs,nle)
	end handle x => raise x)
	    
      | F.APP (f,vs) =>
	(let val nvs = map substval vs
	in case val2sval f
	    of Fun(g,body,args,fk,od) =>
	       (ASSERT(C.usenb g > 0, "C.usenb g > 0");
		if C.usenb g = 1 andalso od = d andalso not (C.recursive g)

		(* simple inlining:  we should copy the body and then
		 * kill the function, but instead we keep the body
		 * and kill only the function name *)
		then (C.unuse (fn lv => ()) true g;
		      cexp (d,od) (F.LET(map #1 args, F.RET nvs, body)))

		(* no inlining: just substitute the vars and vals *)
		else F.APP(F.VAR g, nvs))
		   
	     | sv => F.APP(sval2val sv, nvs)
	end handle x => raise x)
	    
      | F.TFN ((f,args,body),le) =>
	((if used f then
	     let (* val _ = addbind (f, TFun(f, body, args, od)) *)
		 val nbody = cexp (DI.next d, DI.next od) body
		 val _ = addbind (f, TFun(f, nbody, args, od))
		 val nle = loop le
	     in
		 if used f
		 then F.TFN((f, args, nbody), nle)
		 else nle
	     end
	 else loop le) handle x => raise x)

      | F.TAPP(f,tycs) => F.TAPP(substval f, tycs)

      | F.SWITCH (v,ac,arms,def) =>
	(let val nv = substval v
	     fun carm (F.DATAcon(dc,tycs,lv),le) =
		 (addbind(lv, Var lv);
		  (F.DATAcon(cdcon dc, tycs, lv), loop le))
	       | carm (con,le) = (con, loop le)
	     val narms = map carm arms
	     val ndef = Option.map loop def
	in
	    F.SWITCH(nv,ac,narms,ndef)
	end handle x => raise x)

      | F.CON (dc,tycs,v,lv,le) =>
	let val ndc = cdcon dc
	in clet1 (fn [nv] => Con(lv, nv, ndc),
	          fn ([nv],nle) => F.CON(ndc, tycs, nv, lv, nle))
		 (lv,[v],le)
	end

      | F.RECORD (rk,vs,lv,le) =>
	clet1 (fn nvs => Record(lv, nvs),
               fn (nvs,nle) => F.RECORD(rk, nvs, lv, nle))
	      (lv,vs,le)

      | F.SELECT (v,i,lv,le) =>
	if used lv then
	    case val2sval v
	     of Record (lvr,vs) =>
		(let val sv = val2sval (List.nth(vs, i))
		in substitute (lv, sv, F.VAR lvr);
		     loop le
		end handle x => raise x)
	      | sv =>
		(let val nv = sval2val sv
		    val _ = addbind (lv, Select(lv, nv, i))
		    val nle = loop le
		in if used lv then F.SELECT(nv, i, lv, nle) else nle
		end handle x => raise x)
	else loop le
		     
      | F.RAISE (v,ltys) => F.RAISE(substval v, ltys)

      | F.HANDLE (le,v) => F.HANDLE(loop le, substval v)

      | F.BRANCH (po,vs,le1,le2) =>
	(let val nvs = map substval vs
	     val npo = cpo po
	     val nle1 = loop le1
	     val nle2 = loop le2
	in F.BRANCH(npo, nvs, nle1, le2)
	end handle x => raise x)

      | F.PRIMOP (po,vs,lv,le) =>
	(let val nvs = map substval vs
	     val npo = cpo po
	     val _ = addbind(lv, Var lv)
	     val nle = loop le
	in if impurePO po orelse used lv
	   then F.PRIMOP(npo, nvs, lv, nle)
	   else nle
	end handle x => raise x)

end handle x => raise x
		 
fun contract (fdec as (_,f,_,_)) =
    let val _ = M.clear m
	val F.FIX([fdec], F.RET[F.VAR f]) =
	    cexp (DI.top,DI.top) (F.FIX([fdec], F.RET[F.VAR f]))
	val _ = M.clear m
    in fdec
    end

end
end
