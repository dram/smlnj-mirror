(* misc.sml *)

structure Misc =
struct

local open ErrorMsg Symbol PrintUtil Access Basics BasicTypes
           TypesUtil Absyn EnvAccess EnvAccess.Env
in

  fun for l f = app f l

  val sort3 = Sort.sort (fn ((a,_,_),(b,_,_)) => name a > name b)

  fun protect((enter,exit),doit) =
      let val t = enter()
       in (doit() before exit t)
           handle exn => (exit t; raise exn)
      end

  val protectScope = (openScope,resetEnv)

  (* structure mode *)
  datatype strmode = REGULAR | FCTBODY
  val strMode = ref REGULAR

  (* functor parameter defaults *)
  datatype sigContext = SIGN | FCTPARAM

  (* following could go in Absyn *)
  val bogusID = Symbols.stringToSymbol "bogus"
  val bogusExnID = Symbols.stringToSymbol "Bogus"
  val bogusExp = VARexp(ref(VALvar{access=LVAR(mkLvar()),
				   name=bogusID,
				   vtype=ref ERRORty}))

  val anonName = Symbols.stringToSymbol "Anon"
  val anonParamName = Symbols.stringToSymbol "AnonParam"
  val nullSigStamp = genSigStamp()
  val nullSig = 
      STRstr{stamp = 0,
             sign = nullSigStamp,
	     table = newTable(),
	     env = emptyStrenv,
	     kind = SIGkind{share={s=[],t=[]},
		            bindings=[],
			    stampcounts={s=1,t=0}}}
  val nullStr = 
      STRstr{stamp = genStrStamp(),
             sign = nullSigStamp,
	     table = newTable(),
	     env = emptyStrenv,
	     kind = STRkind{path=[]}}
  val nullParamVar = STRvar{name=anonParamName,
			    access=LVAR(namedLvar(anonParamName)),
			    binding=nullSig}

  fun discard _ = ()

  fun single x = [x]

  fun varcon (VARbind v) = VARexp(ref v)
    | varcon (CONbind d) = CONexp d
    | varcon _ = impossible "parse.39"

  fun lookID(id : symbol): exp = 
       varcon (lookVARCON id handle Unbound => unboundVAR id)
       handle Unboundrec => VARexp(getPatchVar id)

  val lookIDinStr = varcon o lookVARCONinStr

  (* the following two functions belong in TypesUtil *)
  fun checkNonCircular(l : tycon list) =
      let fun less(TYCON{path=a::_,...},TYCON{kind=DEFtyc(TYFUN{body,...}),...}) =
		let fun find(CONty(ref(TYCON{path=b::_,...}), args)) = 
			    Symbol.eq(a,b) orelse exists find args
		      | find(CONty(_, args)) = exists find args
		      | find _ = false
		 in find body
		end
	    | less _ = impossible "Misc.checkNonCircular"
       in (Topsort.topsort2 less l; ())
	   handle Topsort.Cycle => complain "circular withtype declaration"
      end

  fun makeAbstract(datatycs,withtycs) =
      let val (stamps,abstycs,dconss) =
	      let fun loop((tr as ref(TYCON{stamp,arity,eq,path,
				  	    kind=DATAtyc dcons}))
			   ::rest,stamps,abstycs,dconss) =
		      let val abstyc = TYCON{stamp=stamp,arity=arity,path=path,
					     eq=ref NO,kind=ABStyc}
		       in tr := abstyc;
			  loop(rest,stamp::stamps,abstyc::abstycs,dcons::dconss)
		      end
		    | loop([],stamps,abstycs,dconss) = (stamps,abstycs,dconss)
		    | loop _ = impossible "Misc.makeAbstract.loop"
	       in loop(datatycs,[],[],[])
	      end
	  fun subst(tycref as ref(TYCON{stamp,...})) =
		let fun find(stamp'::stamps,tyc::tycs) =
			  if stamp = stamp' then tycref := tyc else find(stamps,tycs)
		      | find([],_) = ()
		      | find _ = impossible "Misc.makeAbstract.subst.find"
		 in find(stamps,abstycs)
		end
	    | subst _ = ()
	  fun substType(CONty(reftyc,args)) =
		(subst reftyc; app substType args)
	    | substType(POLYty{tyfun=TYFUN{body,...},...}) = substType body
	    | substType _ = ()
       in for dconss (app (fn DATACON{typ,...} => substType typ));
	  for withtycs
	      (fn ref(TYCON{kind=DEFtyc(TYFUN{body,...}),...}) => substType body
	        | _ => impossible "Misc.makeAbstract.fn")
      end

  fun dumpStructure(STRvar{access=PATH p,binding,...}) =
      let val STRstr{table,env as {t,...},...} = binding
	  val vbs = ref ([]: vb list)
	  and strbs = ref([]: strb list)
	  and tbs = ref([]: tb list)
	  and ebs = ref([]: eb list)
	  fun rebind(index,strg,VARbind(var)) =
		(case varApplied(var,{path=p,strenv=env})
		  of oldvar as VALvar{name,vtype,...} =>
		     let val newvar = mkVALvar(name,vtype)
		         val vb = VB{pat = VARpat(newvar),
				     exp = VARexp(ref oldvar),
				     tyvars = []}
		      in vbs := vb :: !vbs;
		         ibindVAR(index,strg,newvar)
		     end
		   | oldvar as OVLDvar _ => ibindVAR(index,strg,oldvar)
		   | _ => impossible "Misc.dumpStructures.rebind")
	    | rebind(index,strg,STRbind(strvar)) =
		let val oldstrvar as STRvar{name,binding,...} =
			  strApplied(strvar,{path=p,strenv=env})
		    val newstrvar = STRvar{access=LVAR(namedLvar(name)),
					   name=name,
					   binding=binding}
		    val strb = STRB{strvar=newstrvar,
				    def=VARstr oldstrvar,
				    thin=NONE,
				    constraint=NONE}
		 in strbs := strb :: !strbs;
		    ibindSTR(index,strg,newstrvar)
		end
	    | rebind(index,strg,TYCbind(reftyc as ref tycon)) =
		let val reftyc = case tycon
			           of INDtyc[i] => ref(t sub i)
				    | _ => reftyc
		    val tb = TB{tyc = reftyc, def = CONty(reftyc,[])}
			       (* bogus args in def field *)
		 in tbs := tb :: !tbs;
		    ibindTYC(index,strg,reftyc)
		end
	    | rebind(index,strg,CONbind(dcon)) =
		let val olddcon as DATACON{name,const,typ,rep,sign} =
			  dconApplied(dcon,{path=p,strenv=env})
		 in case rep
		      of VARIABLE _ =>
			   let val newdcon =
				 DATACON{name=name,const=const,typ=typ,sign=sign,
					 rep=VARIABLE(LVAR(namedLvar(name)))}	
			       val eb = EBdef{exn=newdcon,edef=olddcon}
			    in ebs := eb :: !ebs;
			       ibindCON(index,strg,newdcon)
			   end
		       | _ => ibindCON(index,strg,olddcon)
		end
	    | rebind(index,strg,FIXbind(fixvar)) =
		ibindFIX(index,strg,fixvar)
	    | rebind _ = ()
       in IntStrMap.app rebind table;
	  SEQdec [STRdec(!strbs),TYPEdec(!tbs),EXCEPTIONdec(!ebs),VALdec(!vbs)]
      end
    | dumpStructure _ = impossible "Misc.dumpStructure"

end (* local *)

end (* structure Misc *)
