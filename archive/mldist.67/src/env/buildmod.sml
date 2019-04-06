(* Copyright 1989 by AT&T Bell Laboratories *)
(* buildmod.sml *)

(* building structures *)

structure BuildMod : BUILDMOD = struct

open ErrorMsg Symbol Access Basics TypesUtil

(* ordering of binders -- comparing by bound symbol for runtime components *)
fun binderGt(bind1: binder, bind2: binder) =
    case (bind1,bind2)
      of ((n1,FIXbind _),(n2,FIXbind _)) => symbolGt(n1,n2)
       | ((_,FIXbind _),_) => true
       | (_,(_,FIXbind _)) => false
       | ((n1,VARbind _),(n2,VARbind _)) => symbolGt(n1,n2)
       | ((_,VARbind _),_) => true
       | (_,(_,VARbind _)) => false
       | ((n1,CONbind _),(n2,CONbind _)) => symbolGt(n1,n2)
       | ((_,CONbind _),_) => true
       | (_,(_,CONbind _)) => false
       | ((n1,TYCbind _),(n2,TYCbind _)) => symbolGt(n1,n2)
       | ((_,TYCbind _),_) => true
       | (_,(_,TYCbind _)) => false
       | ((n1,STRbind _),(n2,STRbind _)) => symbolGt(n1,n2)
       | ((_,STRbind _),_) => true
       | (_,(_,STRbind _)) => false
       | ((n1,FCTbind _),(n2,FCTbind _)) => symbolGt(n1,n2)
       | ((_,FCTbind _),_) => true
       | (_,(_,FCTbind _)) => false
       | ((n1,SIGbind _),(n2,SIGbind _)) => symbolGt(n1,n2)

fun last (x as [_]) = x
  | last(a::b) = last b
  | last [] = impossible "BuildMod.last"

fun buildStrTable (env: Basics.env) : trans list * Basics.env =
    let val newenv = ref(Env.empty : Basics.env)
	fun look s = Env.look (!newenv) s
	fun add(s,b) = newenv := Env.bind(s,b,!newenv)
	val bindings = 
	    (* no sorting done, except chronological by Env.app *)
	    let val r = ref (nil : (symbol * binding) list)
		fun add (s,b) = r := (s,b) :: !r
	     in Env.app add env; (* r now sorted youngest first *)
		!r
	    end
	fun fill (nil,count) = nil
	  | fill ((bdg as (s,binding))::rest,count) =
	       (look s; fill(rest,count)) handle Unbound =>	
	     case binding
	      of VARbind(var as VALvar{access,name,typ}) =>
		   (add(s,
		        VARbind(
			  case access
			   of INLINE(_) => var
			    | _ =>
				VALvar{access = SLOT count,
				       typ = typ,
				       name = last name}));
		    VALtrans(access)::fill(rest,count+1))
	       | CONbind(DATACON{name,const,sign,typ,rep}) =>
		   let val dcon = DATACON{name=name,const=const,sign=sign,
					  typ=typ,
					  rep=case rep
					        of VARIABLE _ => VARIABLE(SLOT count)
						 | _ => rep}
		    in add(s,CONbind(dcon));
		       case rep
			 of VARIABLE access => (* exception constructor *)
			     VALtrans(access)::fill(rest,count+1)
			  | _ => fill(rest,count) (* datatype constructor *)
		   end
	       | STRbind(STRvar{name,access,binding}) =>
		      (add(s, STRbind(STRvar{name=last name,
				               binding=binding,
				               access=SLOT(count)}));
		       VALtrans(access)::fill(rest,count+1))
	       | TYCbind tycon =>
		   let fun newDcon(DATACON{name,const,typ,rep,sign}) =
		           DATACON{name=name,const=const,rep=rep,sign=sign,
				   typ=typ}
		       val tycon' =
		           case tycon
			     of GENtyc{stamp,arity,eq,path,kind=ref(DATAtyc dcons)} =>
				 GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
				        kind=ref(DATAtyc(map newDcon dcons))}
			      | tyc => tyc
		    in add(s, TYCbind tycon');
		       fill(rest,count)
		   end
	       | _ => (add bdg; fill(rest,count))
     in (fill(bindings,0), Env.consolidate(!newenv))
    end

end (* structure BuildMod *)
