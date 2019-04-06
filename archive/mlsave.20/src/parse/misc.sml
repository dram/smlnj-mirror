(* misc.sml *)

structure Misc =
struct

local open ErrorMsg Symbol PrintUtil Access Basics BasicTypes
           TypesUtil Absyn EnvAccess EnvAccess.Env
in

  fun for l f = app f l

  (* copyarray -- used in sign to copy environment arrays *)
  fun copyarray(a,n) =
      (* assume n <= length a *)
      let val new = array(n,a sub 0)
	  fun loop i = (update(new,i,a sub i); loop(i+1))
       in loop 0
	  handle Subscript => new
      end

  val sort3 = Sort.sort (fn ((a,_,_),(b,_,_)) => name a > name b)

  fun protect((enter,exit),doit) =
      let val t = enter()
       in (doit() before exit t)
           handle exn => (exit t; raise exn)
      end

  val protectScope = (mark,close)

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
  val nullStamp = genStrStamp()
  val nullSig = STRstr{stamp=0,sign=SOME nullStamp,table=Table.new(),
		       env=emptyStrenv,
		       kind=SIGkind{share={s=[],t=[]},
		       		    bindings=[],
				    stampcounts={s=1,t=0}}}
  val nullStr = STRstr{stamp=genStrStamp(),sign=SOME nullStamp,table=Table.new(),
		       env=emptyStrenv, kind=STRkind{path=[]}}
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
	       in loop(datatycs,[],[],[])
	      end
	  fun subst(tycref as ref(TYCON{stamp,...})) =
		let fun find(stamp'::stamps,tyc::tycs) =
			  if stamp = stamp' then tycref := tyc else find(stamps,tycs)
		      | find([],_) = ()
		 in find(stamps,abstycs)
		end
	    | subst _ = ()
	  fun substType(CONty(reftyc,args)) =
		(subst reftyc; app substType args)
	    | substType(POLYty{tyfun=TYFUN{body,...},...}) = substType body
	    | substType _ = ()
       in for dconss (app (fn DATACON{typ,...} => substType typ));
	  for withtycs
	      (fn ref(TYCON{kind=DEFtyc(TYFUN{body,...}),...}) => substType body)
      end

  fun dumpStructure(STRvar{access=PATH p,binding,...}) =
      let val STRstr{table,env as {t,...},...} = binding
	  val vbs = ref ([]: vb list)
	  and strbs = ref([]: strb list)
	  and tbs = ref([]: tb list)
	  and ebs = ref([]: eb list)
	  fun rebind(id,VARbind(var)) =
		let val oldvar as VALvar{vtype,...} = varApplied(var,(p,env))
		    val newvar = mkVALvar(id,vtype)
		    val vb = VB{pat = VARpat(newvar),
				exp = VARexp(ref oldvar),
				tyvars = []}
		 in vbs := vb :: !vbs;
		    bindVAR(id,newvar)
		end
	    | rebind(id,STRbind(strvar)) = 
		let val oldstrvar as STRvar{binding,...} =
			  strApplied(strvar,(p,env))
		    val newstrvar = STRvar{access=LVAR(namedLvar(id)),
					   name=id,
					   binding=binding}
		    val strb = STRB{strvar=newstrvar,
				    def=VARstr oldstrvar,
				    thin=NONE,
				    constraint=NONE}
		 in strbs := strb :: !strbs;
		    bindSTR(id,newstrvar)
		end
	    | rebind(id,TYCbind(reftyc as ref tycon)) = 
		let val reftyc = case tycon
			           of INDtyc[i] => ref(t sub i)
				    | _ => reftyc
		    val tb = TB{tyc = reftyc, def = CONty(reftyc,[])}
			       (* bogus args in def field *)
		 in tbs := tb :: !tbs;
		    bindTYC(id,reftyc)
		end
	    | rebind(id,CONbind(dcon)) = 
		let val olddcon as DATACON{const,typ,rep,sign,...} =
			  dconApplied(dcon,(p,env))
		 in case rep
		      of VARIABLE _ =>
			   let val newdcon =
				 DATACON{name=id,const=const,typ=typ,sign=sign,
					 rep=VARIABLE(LVAR(namedLvar(id)))}	
			       val eb = EBdef{exn=newdcon,edef=olddcon}
			    in ebs := eb :: !ebs;
			       bindCON(id,newdcon)
			   end
		       | _ => bindCON(id,olddcon)
		end
	    | rebind(id, FIXbind fixity) = 
		bindFIX(id,fixity)
	    | rebind _ = ()
       in Table.app(table,rebind);
	  SEQdec [STRdec(!strbs),TYPEdec(!tbs),EXCEPTIONdec(!ebs),VALdec(!vbs)]
      end
end (* local *)

end (* structure Misc *)