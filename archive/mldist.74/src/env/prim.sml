(* Copyright 1989 by AT&T Bell Laboratories *)
structure Prim : sig val primTypes : Modules.Structure
		     val inLine : Modules.Structure
		     val inLineName : Access.primop -> string
		     val pure : Access.primop -> bool
		     val mayRaise : Access.primop -> bool
		     val special : Access.access -> bool
		 end = 
struct
   open Access Modules Variables Types BasicTypes Symbol Fixity

(* primTypes structure *)

   val typesEnv = ref (Env.empty : Modules.env)

   val env =
       (fold (fn ((s, t), e) => Env.bind(tycSymbol s, TYCbind t, e))
	     [("bool", boolTycon),
	      ("list", listTycon),
	      ("ref", refTycon),
	      ("unit", unitTycon),
	      ("int", intTycon),
	      ("real", realTycon),
	      ("cont", contTycon),
	      ("array", arrayTycon),
	      ("string", stringTycon),
	      ("exn", exnTycon),
	      ("option", optionTycon)]
	     Env.empty)

   val env =
       (fold (fn ((s,c),e) => Env.bind(varSymbol s, CONbind c, e))
	     [("true", trueDcon),
	      ("false", falseDcon),
	      ("::", consDcon),
	      ("nil", nilDcon),
	      ("ref", refDcon),
	      ("SOME", SOMEDcon),
	      ("NONE", NONEDcon)]
	     env)


   val env = 
       let val fixcons = fixSymbol "::"
        in Env.bind(fixcons,FIXbind(FIXvar{name=fixcons,
					   binding=infixright 5}),
		    env)
       end

   val primTypes =
       ModuleUtil.mkStructure(Env.consolidate env,[])

(* inLine structure *)

   val bottom = POLYty{sign=[{weakness=infinity,eq=false}], abs=0,
		       tyfun=TYFUN{arity=1,body=VARty(mkTyvar(IBOUND 0))}}

   val primopNames = [
        ("capture",P.capture),
        ("callcc",P.callcc),
        ("throw",P.throw),
	("delay",P.delay),
	("force",P.force),
	("!",P.!),
	("*",P.* ),
	("+",P.+),
	("-",P.-),
	(":=",P.:=),
	("<",P.<),
	("<=",P.<=),
	(">",P.>),
	(">=",P.>=),
  	("lessu",P.lessu),
  	("gequ",P.gequ),
	("alength",P.alength),
	("boxed",P.boxed),
	("unboxed",P.unboxed),
	("div",P.div),
	("orb",P.orb),
	("andb",P.andb),
	("xorb",P.xorb),
	("lshift",P.lshift),
	("rshift",P.rshift),
	("notb",P.notb),
	("cast",P.cast),
	("=",P.eql),
	("fadd",P.fadd),
  	("floor",P.floor),
  	("round",P.round),
  	("real",P.real),
  	("subscriptf",P.subscriptf),
  	("updatef",P.updatef),
  	("inlsubscriptf",P.inlsubscriptf),
  	("inlupdatef",P.inlupdatef),
  	("subscriptv",P.subscriptv),
	("fdiv",P.fdiv),
	("feql",P.feql),
	("fge",P.fge),
	("fgt",P.fgt),
	("fle",P.fle),
	("flt",P.flt),
	("fmul",P.fmul),
	("fneq",P.fneq),
	("fsub",P.fsub),
	("gethdlr",P.gethdlr),
	("ieql",P.ieql),
	("ineq",P.ineq),
	("<>",P.neq),
	("makeref",P.makeref),
	("ordof",P.ordof),
	("profile",P.profile),
	("sethdlr",P.sethdlr),
	("slength",P.slength),
	("store",P.store),
	("subscript",P.subscript),
	("unboxedassign",P.unboxedassign),
	("unboxedupdate",P.unboxedupdate),
	("update",P.update),
        ("inlsubscript",P.inlsubscript),
        ("inlupdate",P.inlupdate),
        ("inlbyteof",P.inlbyteof),
        ("inlstore",P.inlstore),
        ("inlordof",P.inlordof),
  	("~",P.~),
  	("getvar",P.getvar),
  	("setvar",P.setvar),
  	("uselvar",P.uselvar),
  	("deflvar",P.deflvar)]


   fun enter((s : string, p : primop), env) =
       let val name = varSymbol s
        in Env.bind(name,
		    VARbind(VALvar{access=INLINE p, name=[name],
				   typ= ref bottom}),
		    env)
       end

   val inLine =
      ModuleUtil.mkStructure(Env.consolidate(fold enter primopNames
					     Env.empty),[])

   fun inLineName p =
       let fun find [] = ErrorMsg.impossible "Prim.inLineName - bad primop name"
	     | find ((s,p1)::rest) = if p1=p then s else find rest
        in find primopNames
       end

   val pure =
     fn P.:= => false
      | P.! => false (****)
      | P.subscript => false (****)
      | P.store => false
      | P.unboxedassign => false
      | P.unboxedupdate => false
      | P.update => false
      | P.capture => false
      | P.callcc => false
      | P.~ => false (* these must be here because they may raise *)
      | P.+ => false
      | P.- => false
      | P.* => false
      | P.div => false
      | P.fadd => false
      | P.fsub => false
      | P.fmul => false
      | P.fdiv => false
      | P.lshift => false
      | P.force => false
      | P.subscriptf => false (****)
      | P.updatef => false
      | _ => true
  
   val mayRaise =
     fn P.~ => true
      | P.+ => true
      | P.- => true
      | P.* => true
      | P.div => true
      | P.fadd => true
      | P.fsub => true
      | P.fmul => true
      | P.fdiv => true
      | P.lshift => true
      | P.floor => true
      | P.round => true
      | _ => false

   fun special(INLINE P.eql) = true
     | special(INLINE P.neq) = true
     | special(INLINE P.:=) = true
     | special(INLINE P.update) = true
     | special _ = false

end (* structure Prim *)

