(* equal.sml *)

structure Equal =
struct
local open Basics Lambda Access BasicTypes ErrorMsg


fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = ErrorMsg.Impossible "equal.translatepath nil"

  type tyCl = ( (tyvar->ty) * ty )

  fun bind(env,TYVAR{stamp=v,...}::rv,t::rt) = 
		let val e = bind(env,rv,rt)
		 in (fn (x as TYVAR{stamp,...}) => if stamp=v then t else e x)
		end
    | bind(env,nil,nil) = env
    | bind _ = Impossible "234 in equal"

  fun same((ae,CONty(ref(TYPEtyc{params,def,...}),tyl)), b1) =
		same((bind(ae,params,tyl),def),b1)
    | same(a2,(be,CONty(ref(TYPEtyc{params,def,...}),tyl))) =
		same(a2,(bind(be,params,tyl),def))
    | same((ae,CONty(ref(tyca),al)),
	   (be,CONty(ref(tycb),bl))) =
	     let fun each(at::ar, bt::br) =
			 same((ae,at),(be,bt)) andalso each(ar,br)
		   | each(nil,nil) = true
		   | each _ =  Impossible "mismatch 23 in equal"
	      in eqTycon(tyca,tycb) andalso each(al,bl)
	     end
    | same((ae,VARty(TYVAR{status=ref(INSTANTIATED a3),...})),b4) = 
			    same((ae,a3),b4)
    | same(a5,(be,VARty(TYVAR{status=ref(INSTANTIATED b6),...}))) =
		same(a5,(be,b6))
    | same((ae,VARty(av as TYVAR{status=ref(BOUND),...})),b7) =
		 same((ae, ae av),b7)
    | same(a8,(be,VARty(bv as TYVAR{status=ref(BOUND),...}))) = 
		same(a8,(be, be bv))
    | same _ = false

in

  fun equal (concreteType : ty) : lexp =
   let
    val cache : (tyCl * lexp * lexp ref) list ref = ref nil
    fun enter tycl = let val v = VAR(mkLvar())
			       val r = ref v
		      in cache := (tycl, v, r) :: !cache; (v,r)
		     end
    exceptionx notfound
    exceptionx unboundTyvarInEqual

    fun find tycl =
      let fun f ((t,v,e)::r) = if same(tycl,t) then v else f r
            | f nil = raisex notfound
       in f (!cache)
      end
    fun test(tycl as (env, ty)) =
     case ty
      of VARty(TYVAR{status=ref(INSTANTIATED t),...}) => test(env,t)
       | VARty(tyv as TYVAR{status=ref(BOUND),...}) => test(env, env tyv)
       | VARty _ => Condemn "Attempt to compare polymorphic\
				       \ types for equality"
       | CONty(ref(ATOMtyc{name,...}),tyl) => atomeq (Symbol.Name name,tyl,env)
       | CONty(ref(TYPEtyc{params,def,...}),tyl) =>
			     test(bind(env,params,tyl),def)
       | CONty(ref(VARtyc{name,...}),tyl) =>
	    Condemn ("Can't compare signature type for equality: "^
			Symbol.Name name)
       | CONty(ref(DATAtyc{dcons=ref[DATACON{const=false,rep=ref TRANSPARENT,
		vtype=CONty(_,[ty,_]),...}],params,...}), tyl) =>
			  test(bind(env,params,tyl),ty)
       | CONty(tr as ref tyc,  tyl) =>
	    if (case (tyc,!refTycon)
	         of (DATAtyc{stamp=i,...},DATAtyc{stamp=j,...}) => i=j
		  | _ => false)
	       (*tr=refTycon *)
	     then atomeq("ref",tyl,env)
	    else (find tycl
	          handlex notfound =>
	   let val v = mkLvar() and x=mkLvar() and y=mkLvar()
	       val (eqv, patch) = enter tycl
	       fun inside env (DATACON{const=true,...}) = 
				CON(TRUEdcon,RECORD[])
 	         | inside env (c as DATACON{vtype=CONty(_,[ty,_]),
					     const=false,...})
	             = APP(test(env,ty),
			RECORD[DECON(c, VAR x),
			       DECON(c, VAR y)])
		 | inside _ _ = Impossible "1298 in equal"
	       val body = case tyc
		     of DATAtyc{params,dcons=ref[dcon],...} =>
			  inside (bind(env,params,tyl)) dcon	
		      | DATAtyc{params,dcons=ref dcons,...} =>
			  let val env = bind(env,params,tyl)
	                      fun concase dcon =
		                  (DATAcon(dcon),
				   SWITCH(VAR y,[(DATAcon(dcon), inside env dcon)],
					SOME(CON(FALSEdcon,RECORD[]))))
	                  in SWITCH(VAR x,map concase dcons,NONE)
			 end
		     | RECORDtyc _ =>
			let fun loop(n,[ty]) =
			             APP(test(env,ty), RECORD[SELECT(n, VAR x),
					    SELECT(n, VAR y)])
		              | loop(n,ty::r) =
			          SWITCH(loop(n,[ty]),
				   [(DATAcon(TRUEdcon), loop(n+1,r)),
				    (DATAcon(FALSEdcon),
				       CON(FALSEdcon,RECORD[]))],
				   NONE)
		              | loop(_,nil) = CON(TRUEdcon,RECORD[])
	                in loop(0,tyl)
	               end
		    | _ => Impossible "21 in equal"
	    in patch := FN(v,APP(FN(x,APP(FN(y,body),SELECT(1,VAR v))),
						SELECT(0,VAR v)));
               eqv
	   end)
       | _ => Impossible "28 in equal"
    and atomeq(name,tyl,env) =
	 let val eqsym = SymbolTable.StringToSymbol( name ^ "equal")
	     val VALvar{access=PATH p,...} = EnvAccess.lookVARinBase eqsym
          in translatepath p
	 end handlex Table.notfound => 
		    Condemn ("Attempt to test opaque type for equality: "
			        ^ name)
    val body = test( (fn x => Condemn "attempt to test polymorphic type\
				     \ for equality"), concreteType )
   in FIX(map (fn (_,VAR v,_) => v) (!cache),
	  map (fn (_,_,e) => !e) (!cache),
	  body)
  end
  handlex Syntax =>
	 (print "type = "; PrintType.printType concreteType; print"\n";
	 RECORD[])
		
end (* local *)
end (* struct *)

