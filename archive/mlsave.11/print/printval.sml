(* printval.sml *)

structure PrintVal: PRINTVAL =
struct

structure Basics = Basics
type object = System.object

open System Basics BasicTypes TypesUtil ErrorMsg PrintUtil

 fun gettag obj = int (tuple obj sub 1)

 fun switch(obj, DATAtyc{dcons=ref dcons,...}) =
	let fun try ((d as DATACON{rep = ref rep,...})::r) = 
		((case rep
		   of TAGGED i => if gettag obj = i then d else raise Boxity
		    | CONSTANT i => if int obj = i then d else raise Boxity
		    | TRANSPARENT => d
		    | TRANSB => (tuple obj; d)
		    | TRANSU => ((tuple obj; try r) handle Boxity => d)
		    | REF => d
		    | _ => ErrorMsg.impossible "funny datacon in print")
	         handle Boxity => try r)
	      | try nil = ErrorMsg.impossible "none of the datacons matched"
	 in try dcons
	end
	    

 fun decon(obj, DATACON{rep = ref rep,...}) =
     case rep
      of UNDECIDED => ErrorMsg.impossible "undecided datacon in decon"
       | TAGGED _ => tuple obj sub 0
       | CONSTANT _ => ErrorMsg.impossible "constant datacon in decon"
       | TRANSPARENT => obj
       | TRANSU => obj
       | TRANSB => obj
       | REF => tuple obj sub 0
       | VARIABLE _ => tuple obj sub 0

fun fullexpandTy (ty as CONty(ref(TYPEtyc _),_)) = fullexpandTy(expandTy ty)
  | fullexpandTy (VARty(TYVAR{status=ref(INSTANTIATED t),...})) =
	fullexpandTy t
  | fullexpandTy ty = ty

fun printVal(_,_,0) = prstr "#"
  | printVal(obj: object, ty: ty, depth: int) : unit =
    case ty
      of VARty(TYVAR{status=ref(INSTANTIATED t),...}) => printVal(obj,t,depth)
       | VARty _ => impossible("impossible type for printing value")
       | CONty(ref tyc, argtys) =>
	   (case tyc
	      of VARtyc _ => prstr "-"
	       | ATOMtyc{name,...} =>
		   (case (Symbol.name name)
		      of "int" => prstr(makestring(int obj))
		       | "string" => pr_mlstr(string obj)
		       | "real" => prstr(makestring(real obj))
		       | "->" => prstr "fn"
		       | _ => prstr "-")
               | TYPEtyc _ =>
		   printVal(obj, expandTy ty, depth)
	       | DATAtyc{params,name,...} =>
		   if eqTycon(tyc,!listTycon)
		     then printList(obj, hd argtys, depth)
		     else let val noparen = INfix(0,0)
			  in  printDcon(obj,ty,depth,noparen,noparen)
			  end
	       | RECORDtyc{labels=[],...} => (print "()"; ())
	       | RECORDtyc{labels,...} =>
		   if Tuples.isTUPLEtyc tyc
		     then printTuple(tuple(obj), argtys, depth)
		     else printRecord(tuple(obj), labels, argtys, depth))

and printDcon(obj:object,CONty(ref(tyc as DATAtyc{params,...}),argtys),depth:int,
		l:fixity,r:fixity) =
    let val dcon as DATACON{name,const,vtype,...} = switch(obj,tyc)
	val dname = Symbol.name name
	val fixity = EnvAccess.lookFIX name
	fun prdcon() =
	   let val CONty(_,dom::_) = vtype
	       val deconTy = fullexpandTy(CONty(ref(mkTYPEtyc(name,params,dom)),
						argtys))
	   in
	    case (fixity,deconTy)
	      of (INfix _,CONty(ref(deconTyc as RECORDtyc _),[tyL,tyR])) =>
		    let val twoTuple = tuple(decon(obj,dcon))
		    in  if Tuples.isTUPLEtyc deconTyc
			then (
			  printDcon(twoTuple sub 0,tyL,depth-1,NONfix,fixity);
			  print " "; print dname; print " ";
			  printDcon(twoTuple sub 1,tyR,depth-1,fixity,NONfix))
			else (
			  print dname; print " ";
			  printDcon(decon(obj,dcon),deconTy,depth-1,NONfix,NONfix))
		    end
		| _ =>
		   (print dname; print " ";
		    printDcon(decon(obj,dcon),deconTy,depth-1,NONfix,NONfix))
	   end
    in  if const then prstr dname
	else case(l,r,fixity) of
	      (NONfix,NONfix,_) => (print "("; prdcon(); prstr ")")
	    | (INfix _,INfix _,_) => prdcon() (* special case: only on first
						 iteration, for no parens *)
	    | (_,_,NONfix) => prdcon()
	    | (INfix(_,p1),_,INfix(p2,_)) =>
			if p1 >= p2 then (print "("; prdcon(); prstr ")")
			else prdcon()
	    | (_,INfix(p1,_),INfix(_,p2)) =>
			if p1 > p2 then (print "("; prdcon(); prstr ")")
			else prdcon()
    end
  | printDcon(obj,VARty(TYVAR{status=ref(INSTANTIATED t),...}),depth,l,r) =
	printDcon(obj,t,depth,l,r)
  | printDcon(obj,ty as CONty(ref(TYPEtyc _),_),depth:int,l,r) =
	printDcon(obj,fullexpandTy ty,depth,l,r)
  | printDcon(obj,ty,depth,_,_) =
	printVal(obj,ty,depth)

and printList(obj:object, ty:ty, depth:int) : unit =
    let fun printTail(separator, p) =
            let val dcon as DATACON{name,...} = switch(p, !listTycon)
	     in case (Symbol.name name)
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
	      (print(Symbol.name l); print("="); printVal(objs sub nf,ty,depth-1))
	  | printFields(nf, l::restl, ty::restty) = 
	      (print(Symbol.name l); print("="); printVal(objs sub nf,ty,depth-1);
	       print(","); printFields(nf+1,restl,restty))
	  | printFields(nf,[],[]) = ()
     in print("{"); printFields(0,labels,tys); prstr("}")
    end

end (* structure PrintVal *)
