sig
   val instrumrDec: Absyn.dec -> Absyn.dec
end

To Be Done:

use a variant version of hcreater that also causes an event (a la IO events)
and integrate this new special event in everywhere.
- a possible alternative: use a counter for created references (recorded
and reset at suitable places, of course), or (even) a simple log, which
would seem to have better properties than our hash table...?

fun instrumrDec absyn =
let 
(* Because ref is a constructor that cannot be replaced at top level, 
    we replace it explicitly here: *)

val hcreater = makevar ("hcreater", UNDEFty)
val HREFexp = VARexp(ref hcreater)

(* For efficiency, we implement

fun hass (objr,value) =
        (updatedRList := (weak objr) :: (!updatedList);
	 objr := value)
	
in-line. Note we maintain type information in opr used by the 
garbage collector.
*)


val weakvar = makevar ("weak",UNDEFty)
val udl = makevar ("udl",UNDEFty)
fun HASSexp opr = 
     let val objvar = makevar ("obj",UNDEFty)
	 val valvar = makevar ("val",UNDEFty)
	 val objexp = VARexp(ref objvar)
         val valexp = VARexp(ref valvar)
      (* val newobj = APPexp(VARexp(ref weakvar),objexp)  *)
         val newobj = APPexp(VARexp(ref delayop), 
			     TUPLEexp[INTexp (System.Tags.desc_weak div 2),
				      objexp]) 
	 val oldlist = APPexp(VARexp(ref derefop),VARexp(ref udl))
     in FNexp [RULE(TUPLEpat[VARpat objvar,VARpat valvar],
	            SEQexp[APPexp(VARexp(ref assop),
	                          TUPLEexp[VARexp(ref udl),
			                   APPexp(CONexp(consDcon),
				           TUPLEexp[newobj,oldlist])]),
	                   APPexp(VARexp(ref opr), 
		                  TUPLEexp[objexp,valexp])]),
	       RULE(WILDpat,INTexp 0)]
     end


fun instrexp (RECORDexp lexpl) = 
	           RECORDexp (map (fn (lab,exp) => (lab,instrexp exp)) lexpl)
  | instrexp (SEQexp expl) = SEQexp (map instrexp expl)
  | instrexp (CONexp(DATACON{rep=REF,...})) = HREFexp
  | instrexp (VARexp(ref(assopr as VALvar{access=INLINE(P.:=),...}))) =
		HASSexp assopr
  | instrexp (exp as APPexp(opr,arg)) = APPexp(instrexp opr,instrexp arg)
  | instrexp (CONSTRAINTexp (e,c)) = CONSTRAINTexp(instrexp e,c)
  | instrexp (RAISEexp exp) = RAISEexp(instrexp exp)
  | instrexp (LETexp (ldec,lexp)) = LETexp(instrdec ldec,instrexp lexp)
  | instrexp (CASEexp(exp,rl)) = CASEexp(instrexp exp,map instrrule rl)
  | instrexp (HANDLEexp (e, HANDLER(FNexp body))) = 
                HANDLEexp(instrexp e, HANDLER(FNexp (map instrrule body)))
  | instrexp (FNexp body) = FNexp(map instrrule body)
  | instrexp (MARKexp (exp,s,e)) = MARKexp(instrexp exp,s,e)
and instrrule (RULE(pat,exp)) = RULE(pat,instrexp exp))
and instrdec (VALdec vbl) =
       let fun f (VB{pat,exp,tyvars}) =
	          VB{pat=pat,exp=instrexp exp,tyvars=tyvars}
       in VALdec (map f vbl)
       end
  | instrdec (VALRECdec rvbl) =
       let fun f (RVB{var,exp,resultty,tyvars}) =
	          RVB{var=var,exp=instrexp exp,resultty=resultty,tyvars=tyvars}
       in VALRECdec (map f rvbl)
  | instrdec (SEQdec decl) = SEQdec (map instrdec decl)
  | instrdec (LOCALdec(localdec, visibledec)) =
       LOCALdec(instrdec localdec,instrdec visibledec)
  | instrdec (ABSTYPEdec {abstycs,withtycs,body}) =
       ABSTYPEdec {abstycs=abstycs,withtycs=withtycs,body=instrdec body}
  | instrdec (STRdec strbl) = STRdec (map instrstrb strbl)
  | instrdec (ABSdec strbl) = ABSdec (map instrstrb strbl)
  | instrdec (FCTdec fctbl) = FCTdec (map instrfctb fctbl)
  | instrdec (MARKdec(dec,s,e)) = MARKdec(instrdec dec,s,e)
  | instrdec dec = dec
and instrstrb (STRB{strvar,def,thin,constraint}) =
       STRB{strvar=strvar,def=instrstrexp def,thin=thin,constraint=constraint}
and instrfctb (FCTB{fctvar,param,def,thin,constraint}) =
       FCTB{fctvar=fctvar,param=param,def=instrstrexp def,thin=thin,
	    constraint=constraint}
and instrstrexp(VARstr v) = VARstr v
  | instrstrexp(STRUCTstr{body,str,locations}) = 
       STRUCTstr{body=map instrdec body,str=str,locations=locations}
  | instrstrexp(APPstr{oper,argexp,argthin,str}) =
       APPstr{oper=oper,argexp=instrstrexp argexp,argthin=argthin,str=str}
  | instrstrexp(LETstr(dec,strexp)) = LETstr(instrdec dec,instrstrexp strexp)
  | instrstrexp(MARKstr(strexp,s,e)) = MARKstr(instrstrexp strexp,s,e)

in instrdec absyn
end

 
