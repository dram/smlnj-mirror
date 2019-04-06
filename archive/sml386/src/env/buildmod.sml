(* Copyright 1989 by AT&T Bell Laboratories *)
(* buildmod.sml *)

(* building structures *)

structure BuildMod : BUILDMOD =
struct

open ErrorMsg Symbol Access Basics TypesUtl Env NmSpace

(* ordering of binders -- comparing by bound symbol for runtime components *)
fun binderGt(bind1: binder, bind2: binder) =
    case (bind1,bind2)
      of ((ind1,_,FIXbind(_)),(ind2,_,FIXbind(_))) => ind1 > ind2
       | ((_,_,FIXbind(_)),_) => true
       | (_,(_,_,FIXbind(_))) => false
       | ((_,n1,VARbind(_)),(_,n2,VARbind(_))) => n1 > n2
       | ((_,_,VARbind(_)),_) => true
       | (_,(_,_,VARbind(_))) => false
       | ((_,n1,CONbind(_)),(_,n2,CONbind(_))) => n1 > n2
       | ((_,_,CONbind(_)),_) => true
       | (_,(_,_,CONbind(_))) => false
       | ((ind1,_,TYCbind(_)),(ind2,_,TYCbind(_))) => ind1 > ind2
       | ((_,_,TYCbind(_)),_) => true
       | (_,(_,_,TYCbind(_))) => false
       | ((_,n1,STRbind(_)),(_,n2,STRbind(_))) => n1 > n2
       | ((_,_,STRbind(_)),_) => true
       | (_,(_,_,STRbind(_))) => false
       | ((ind1,_,FCTbind(_)),(ind2,_,FCTbind(_))) => ind1 > ind2
       | ((_,_,FCTbind(_)),_) => true
       | (_,(_,_,FCTbind(_))) => false
       | ((ind1,_,SIGbind(_)),(ind2,_,SIGbind(_))) => ind1 > ind2

fun extendPath(LVAR(v): access, []: path) = PATH[v] (* locally defined *)
  | extendPath(SLOT(n), p) = PATH(n::p)  (* element of opened structure *)
  | extendPath(x as PATH _, _) = x  (* defined exception *)
  | extendPath(x as INLINE _, _) = x
  | extendPath(access,path) = impossible "extendPath in envaccess"

fun last (x as [_]) = x | last(a::b) = last b | last [] = impossible "last"

fun buildStrTable () : trans list * symtable =
    let val newtable = newTable()
	val add = IntStrMp.add newtable
	val look = IntStrMp.map newtable
	fun getBindings() =
	    (* no sorting done, except chronological by collectTable *)
	    let val r = ref (nil : (binder * info) list)
		fun add x = r := x :: !r
	     in collectTable add;
		!r
	    end
	fun fill (nil,count) = nil
	  | fill ((bdg as (i,s,binding),{path,strenv})::rest,count) =
	       (look(i,s); fill(rest,count)) handle UnboundTable =>	
	     case binding
	      of VARbind(var as VALvar{access,name,typ}) =>
		   (add(i,s,
		        VARbind(
			  case access
			   of INLINE(_) => var
			    | _ =>
				VALvar{access = SLOT count,
				       typ = ref(typeInContext(!typ,strenv)),
				       name = last name}));
		    VALtrans(extendPath(access,path))::fill(rest,count+1))
	       | CONbind(DATACON{name,const,sign,typ,rep}) =>
		   let val dcon = DATACON{name=name,const=const,sign=sign,
					  typ=typeInContext(typ,strenv),
					  rep=case rep
					        of VARIABLE _ => VARIABLE(SLOT count)
						 | _ => rep}
		    in add(i,s,CONbind(dcon));
		       case rep
			 of VARIABLE access => (* exception constructor *)
			     VALtrans(extendPath(access,path))::fill(rest,count+1)
			  | _ => fill(rest,count) (* datatype constructor *)
		   end
	       | STRbind(STRvar{name,access,binding}) =>
		   let val newbinding =
			   case binding
			    of INDstr i =>
			       (case strenv
			         of REL{s=senv,...} => senv sub i
				  | DIR => impossible "buildStrTable.fill 1")
			     | SHRstr(i::r) =>
			       (case strenv
			         of REL{s=senv,...} => getEpath(r,senv sub i)
				  | DIR => impossible "buildStrTable.fill 2")
			     | _ => binding
		    in add(i,s, STRbind(STRvar{name=last name,
				               binding=newbinding,
				               access=SLOT(count)}));
		       VALtrans(extendPath(access,path))::fill(rest,count+1)
		   end
	       | TYCbind tycon =>
		   let fun newDcon(DATACON{name,const,typ,rep,sign}) =
		           DATACON{name=name,const=const,rep=rep,sign=sign,
				   typ=typeInContext(typ,strenv)}
		       val tycon' =
		           case tyconInContext strenv tycon
			     of GENtyc{stamp,arity,eq,path,kind=ref(DATAtyc dcons)} =>
				 GENtyc{stamp=stamp,arity=arity,eq=eq,path=path,
				       kind=ref(DATAtyc(map newDcon dcons))}
			      | tyc => tyc
		    in add(i,s, TYCbind tycon');
		       fill(rest,count)
		   end
	       | _ => (add bdg; fill(rest,count))
     in (fill(getBindings(),0), newtable)
    end

end (* structure BuildMod *)
