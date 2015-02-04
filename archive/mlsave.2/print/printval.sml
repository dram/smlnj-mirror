(* printval.sml *)

structure PrintVal: PRINTVAL =
struct

structure Object = Boot
structure Basics = Basics
open Object Basics BasicTypes TypesUtil ErrorMsg PrintUtil
type object = Object

 fun gettag obj = int (tuple obj sub 1)

 fun switch(obj, DATAtyc{dcons=ref dcons,...}) =
	let fun try ((d as DATACON{rep = ref rep,...})::r) = 
		(case rep
		 of TAGGED i => if gettag obj = i then d else raisex boxity
		  | CONSTANT i => if int obj = i then d else raisex boxity
		  | TRANSPARENT => d
		  | TRANSB => (tuple obj; d)
		  | TRANSU => ((tuple obj; try r) handlex boxity => d)
		  | _ => ErrorMsg.Impossible "funny datacon in print")
	        handlex boxity => try r
	      | try nil = ErrorMsg.Impossible "none of the datacons matched"
	 in try dcons
	end
	    

 fun decon(obj, DATACON{rep = ref rep,...}) =
     case rep
      of UNDECIDED => ErrorMsg.Impossible "undecided datacon in decon"
       | TAGGED _ => tuple obj sub 0
       | CONSTANT _ => ErrorMsg.Impossible "constant datacon in decon"
       | TRANSPARENT => obj
       | TRANSU => obj
       | TRANSB => obj
       | REF => tuple obj sub 0
       | VARIABLE _ => tuple obj sub 0

fun printVal(_,_,0) = prstr "#"
  | printVal(obj: object, ty: ty, depth: int) : unit =
    case ty
      of VARty(TYVAR{status=ref(INSTANTIATED t),...}) => printVal(obj,t,depth)
       | VARty _ => Impossible("impossible type for printing value")
       | CONty(ref tyc, argtys) =>
	   (case tyc
	      of ATOMtyc{name,...} =>
		   prstr(case (Symbol.Name name)
		      of "int" => makestring(int obj)
		       | "string" => string(obj)
		       | "real" => makestring(real obj)
		       | "instream" => "<instream>"
		       | "outstream" => "<outstream>"
		       | _ => "<unknown>")
               | TYPEtyc _ =>
		   printVal(obj, expandTy ty, depth)
	       | DATAtyc{params,name,...} =>
		   if eqTycon(tyc,!listTycon)
		     then printList(obj, hd argtys, depth)
		     else let val dcon as DATACON{name,const,vtype,...} =
				    switch(obj,tyc)
		           in print(Symbol.Name name);
			      if const
			        then ()
				else let val CONty(_,dom::_) = vtype
				      in prstr " ";
					 printVal(decon(obj,dcon),
					      CONty(ref(mkTYPEtyc(name,params,dom)),argtys),
(*			                      instance(dom,argtys,params), *)
					      depth-1)
				     end
		          end
	       | RECORDtyc{labels=[],...} => (print "()"; ())
	       | RECORDtyc{labels,...} =>
		   if Tuples.isTUPLEtyc tyc
		     then printTuple(tuple(obj), argtys, depth)
		     else printRecord(tuple(obj), labels, argtys, depth))

and printList(obj:object, ty:ty, depth:int) : unit =
    let fun printTail(separator, p) =
            let val dcon as DATACON{name,...} = switch(p, !listTycon)
	     in case (Symbol.Name name)
	          of "nil" => prstr "]"
		   | "::" => 
		       let val pair = tuple(decon(p, dcon))
		        in prstr separator;
		           printVal(pair sub 0, ty, depth-1);
			   printTail (",", pair sub 1)
		       end
            end
     in print "["; printTail("",obj)
    end

and printTuple(objs: object array, tys: ty list, depth:int) : unit =
    let fun printFields(nf,[ty]) = printVal(objs sub nf,ty,depth-1)
	  | printFields(nf, ty::restty) = 
	      (printVal(objs sub nf,ty,depth-1); print(",");
	       printFields(nf+1,restty))
	  | printFields(nf,[]) = ()
     in print("("); printFields(0,tys); prstr(")")
    end

and printRecord(objs: object array, labels: label list, tys: ty list, depth:int) =
    let fun printFields(nf,[l],[ty]) = 
	      (print(Symbol.Name l); print("="); printVal(objs sub nf,ty,depth-1))
	  | printFields(nf, l::restl, ty::restty) = 
	      (print(Symbol.Name l); print("="); printVal(objs sub nf,ty,depth-1);
	       print(","); printFields(nf+1,restl,restty))
	  | printFields(nf,[],[]) = ()
     in print("{"); printFields(0,labels,tys); prstr("}")
    end

end (* structure PrintVal *)
