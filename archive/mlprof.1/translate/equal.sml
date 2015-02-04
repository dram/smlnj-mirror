(* equal.sml *)
signature EQUAL =
sig
  structure Basics : BASICS
  structure Lambda : LAMBDA
  val equal : Basics.ty -> Lambda.lexp
end

structure Equal : EQUAL =
struct

  structure Basics : BASICS = Basics
  structure Lambda : LAMBDA = Lambda

open ErrorMsg Basics Lambda Access BasicTypes TypesUtil

fun translatepath [v] = VAR v
  | translatepath (x::p) = SELECT(x,translatepath p)
  | translatepath nil = ErrorMsg.impossible "equal.translatepath nil"

fun argType(POLYty(TYFUN{arity,body=CONty(_,[domain,_])}), args: ty list) =
      applyTyfun(TYFUN{arity=arity,body=domain},args)
  | argType(CONty(_,[argty,_]), []) = argty
  | argType _ = impossible "Equal.argType"

fun atomeq name =
    (case EnvAccess.lookVARinBase(Symbols.stringToSymbol(name ^ "equal"))
       of VALvar{access=PATH p,...} => translatepath p
	| VALvar{access=INLINE n,...} => SELECT(n,VAR 0)
	| _ => impossible "Equal #560")
    handle Table.Notfound_Table =>
      condemn("Attempt to test opaque type for equality: " ^ name)

exception Notfound

val trueLexp = CON(trueDcon,RECORD[])
val falseLexp = CON(falseDcon,RECORD[])

fun equal (concreteType : ty) : lexp =
    let val polyeq =
	    let val VALvar{access=PATH p,...} =
		  EnvAccess.lookVARinBase(Symbols.stringToSymbol "polyequal")
	     in translatepath p
	    end

	val cache : (ty * lexp * lexp ref) list ref = ref nil
	fun enter ty =
	    let val v = VAR(mkLvar())
		val r = ref v
	     in cache := (ty, v, r) :: !cache; (v,r)
	    end
	fun find ty =
	  let fun f ((t,v,e)::r) = if equalType(ty,t) then v else f r
		| f nil = raise Notfound
	   in f (!cache)
	  end

	fun test(ty) =
	 case ty
	  of VARty(ref(INSTANTIATED t)) => test(t)
	   | VARty _ => polyeq
	   | FLEXRECORDty(ref(CLOSED t)) => test t
	   | CONty(ref(TYCON{name,kind=ABStyc,...}),tyl) =>
	       atomeq(Symbol.name name)
	   | CONty(ref(TYCON{kind=DEFtyc _,...}),tyl) => test(reduceType ty)
	   | CONty(ref(TYCON{kind=DATAtyc(ref[DATACON{const=false,rep,typ,...}]),
			     arity,...}), tyl) =>
	       (case rep
		  of TRANSPARENT => test(argType(typ,tyl))
		   | REF => atomeq("ref")
		   | _ => impossible "Equal #498")
	   | CONty(ref(TYCON{kind,...}), tyl) =>
	      (find ty
	       handle Notfound =>
	       let val v = mkLvar() and x=mkLvar() and y=mkLvar()
		   val (eqv, patch) = enter ty
		   fun inside (DATACON{const=true,...}) = trueLexp
		     | inside (c as DATACON{typ, const=false,...}) =
			 APP(test(argType(typ,tyl)),
			     RECORD[DECON(c, VAR x),
				    DECON(c, VAR y)])
		   val body = 
		       case kind
			 of DATAtyc(ref[dcon]) =>
			      inside dcon	
			  | DATAtyc(ref dcons) =>
			      let fun concase dcon =
				      (DATAcon(dcon),
				       SWITCH(VAR y,[(DATAcon(dcon), inside dcon)],
					    SOME(falseLexp)))
			       in SWITCH(VAR x,map concase dcons,NONE)
			      end
			  | RECORDtyc _ =>
			    let fun loop(n,[ty]) =
					 APP(test(ty), RECORD[SELECT(n, VAR x),
						SELECT(n, VAR y)])
				  | loop(n,ty::r) =
				      SWITCH(loop(n,[ty]),
				       [(DATAcon(trueDcon), loop(n+1,r)),
					(DATAcon(falseDcon), falseLexp)],
				       NONE)
				  | loop(_,nil) = trueLexp
			     in loop(0,tyl)
			    end
			  | _ => impossible "21 in equal"
		in patch := FN(v,APP(FN(x,APP(FN(y,body),
					      SELECT(1,VAR v))),
				     SELECT(0,VAR v)));
		   eqv
	       end)
	   | _ => impossible "28 in equal"

	val body = test(concreteType)

     in FIX(map (fn (_,VAR v,_) => v | _ => impossible "Equal #324") (!cache),
	    map (fn (_,_,e) => !e) (!cache),
	    body)
    end
    handle Syntax =>
	   (print "equal: type = ";
	    PrintType.resetPrintType();
	    PrintType.printType concreteType; print"\n";
	    RECORD[])
		
end (* struct *)

