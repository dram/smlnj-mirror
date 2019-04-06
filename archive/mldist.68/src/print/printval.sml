(* Copyright 1989 by AT&T Bell Laboratories *)
(* printval.sml *)

(* functor PrintVal (structure Env:ENV): PRINTVAL =
struct

structure Env = Env *)

structure PrintVal : PRINTVAL =

struct
open System System.Unsafe Basics BasicTypes TypesUtil ErrorMsg PrintUtil

fun gettag obj = int (tuple obj sub 1)

exception Switch

fun switch(obj, GENtyc{kind=ref(DATAtyc dcons),...}) =
    let open Access
        fun try ((d as DATACON{rep,...})::r) = 
	    (case rep
	       of TAGGED i => if (gettag obj = i handle Boxity => false)
			      then d else try r
		| CONSTANT i => if (int obj = i handle Boxity => false)
				then d else try r
		| TRANSPARENT =>
			    if ((tuple obj; true) handle Boxity => false)
			    then d else (try r handle Switch => d)
		| TRANSB => if ((tuple obj; true) handle Boxity => false)
			    then d else try r
		| TRANSU => if ((int obj; true) handle Boxity => false)
			    then d else try r
		| REF => d
		| _ => ErrorMsg.impossible "PrintVal.switch: funny datacon")
	  | try nil = raise Switch
    in  try dcons
	handle Switch => 
	  ErrorMsg.impossible "PrintVal.switch: none of the datacons matched"
    end

fun decon(obj, DATACON{rep,...}) =
  let open Access
   in  case rep
     of TAGGED _ => tuple obj sub 0
      | TRANSPARENT => obj
      | TRANSU => obj
      | TRANSB => obj
      | REF => tuple obj sub 0
      | VARIABLE _ => tuple obj sub 0
      | _ => ErrorMsg.impossible "83383 in printval"
  end

val noparen = INfix(0,0)

exception FAIL

fun is_relative(VARty(ref(INSTANTIATED ty))) = is_relative(ty)
  | is_relative(FLEXRECORDty(ref(CLOSED ty))) = is_relative(ty)
  | is_relative(CONty(RELtyc _, _)) = true
  | is_relative(CONty(tyc,args)) = exists is_relative args
  | is_relative _ = false

fun getSTRpath env [id] = (EnvAccess.lookSTR env id handle Env.Unbound => raise FAIL)
  | getSTRpath env [] = raise FAIL
  | getSTRpath env q = EnvAccess.lookPathSTR env (q,(fn s => raise FAIL))

fun printVal (env:Basics.env) (obj: object, ty: ty, depth: int) : unit =
      printVal'(env, obj, ty, depth, noparen, noparen)

and printVal' (_,_,_,0,_,_) = print "#"
  | printVal' (env: Basics.env,obj: object,ty: ty,depth: int,l: fixity,r: fixity) : unit =
    case ty
      of VARty(ref(INSTANTIATED t)) => printVal'(env,obj,t,depth,r,l)
       | FLEXRECORDty(ref(CLOSED t)) => printVal'(env,obj,t,depth,r,l)
       | CONty(tyc as GENtyc{kind=ref(PRIMtyc),...}, _) =>
	       if eqTycon(tyc,intTycon) then print(makestring(int obj))
	       else if eqTycon(tyc,realTycon) then print(makestring(real obj))
	       else if eqTycon(tyc,stringTycon) then pr_mlstr(string obj)
	       else if eqTycon(tyc,arrowTycon) then print "fn"
	       else if eqTycon(tyc,exnTycon) then print "exn"
	       else if eqTycon(tyc,contTycon) then print "cont"
	       else print "prim?"
       | CONty(tyc as GENtyc{kind=ref(ABStyc t),...}, _) => print "-"
       | CONty(tyc as GENtyc{kind=ref(DATAtyc _),...}, argtys) => (* wrong!? *)
    	   if eqTycon(tyc,listTycon)
    	     then printList(env, obj, hd argtys, depth,
    			    !System.Control.Print.printLength)
    	     else printDcon(env, obj, tyc, argtys, depth, l, r)
       | CONty(tyc as RECORDtyc [], _) => print "()"
       | CONty(tyc as RECORDtyc labels, argtys) =>
	       if Tuples.isTUPLEtyc tyc
		 then printTuple(env,tuple(obj), argtys, depth)
		 else printRecord(env,tuple(obj), labels, argtys, depth)
       | CONty(tyc as DEFtyc _, _) => 
	       printVal'(env,obj, reduceType ty, depth, l, r)
       | POLYty{tyfun=TYFUN{body,...},...} => printVal'(env,obj,body,depth,l,r)
       | _ => print "-"

and printDcon(_,_,_,_,0,_,_) = print "#"
  | printDcon(env:Basics.env,obj:object, tyc as GENtyc{arity,path,...}, argtys,
              depth:int, l:fixity, r:fixity) =
    let val dcon as DATACON{name,const,typ,...} = switch(obj,tyc)
	val dname = Symbol.name name
     in if const
	then print dname
	else
	  let val fixity = EnvAccess.lookFIX env name  (* may be inaccurate *)
	      val dom = case typ
			 of CONty(_,dom::_) => dom
			  | POLYty{tyfun=TYFUN{body=CONty(_,dom::_),...},...} =>
			      applyTyfun(TYFUN{arity=arity,body=dom},argtys)
	      val dom = if is_relative(dom)
			then let val name::path' = path
				 val STRvar{binding=STRstr{table,env=strenv as REL{t,...},
							   ...},
					    ...} =
				       getSTRpath env (rev path')
				 val tyc' = case EnvAccess.lookTYC table name
					      of INDtyc i => t sub i
					       | tyc'' => tyc''
			      in if eqTycon(tyc,tyc')
				 then typeInContext(dom,strenv)
				 else dom
			     end
			     handle Env.Unbound => (print "UnboundTable\n"; dom)
				  | FAIL => dom
				  | Bind => dom
			else dom
	      val dom = headReduceType(dom)
	      fun prdcon() =
		  case (fixity,dom)
		   of (INfix _,CONty(domTyc as RECORDtyc _, [tyL,tyR])) =>
			let val twoTuple = tuple(decon(obj,dcon))
			 in if Tuples.isTUPLEtyc domTyc
			    then (
			      printVal'(env,twoTuple sub 0,tyL,depth-1,NONfix,fixity);
			      print " "; print dname; print " ";
			      printVal'(env,twoTuple sub 1,tyR,depth-1,fixity,NONfix))
			    else (
			      print dname; print " ";
			      printVal'(env,decon(obj,dcon),dom,depth-1,NONfix,NONfix))
			end
		    | _ =>
		       (print dname; print " ";
			printVal'(env,decon(obj,dcon),dom,depth-1,NONfix,NONfix))
	      fun prpardcon() = (print "("; prdcon(); print ")")
	   in case(l,r,fixity)
	      of (NONfix,NONfix,_) => prpardcon()
	       | (INfix _,INfix _,_) => prdcon()
		  (* special case: only on first iteration, for no parens *)
	       | (_,_,NONfix) => prdcon()
	       | (INfix(_,p1),_,INfix(p2,_)) =>
		   if p1 >= p2 then prpardcon()
		   else prdcon()
	       | (_,INfix(p1,_),INfix(_,p2)) =>
		   if p1 > p2 then prpardcon()
		   else prdcon()
	  end
    end

and printList(env:Basics.env, obj:object, ty:ty, depth:int, length: int) : unit =
    let fun printTail(sep, p, len) =
	    if len <= 0 then (print sep; print "...]") else
            let val dcon as DATACON{name,...} = switch(p, listTycon)
	     in case (Symbol.name name)
	          of "nil" => print "]"
		   | "::" => 
		       let val pair = tuple(decon(p, dcon))
		        in print sep;
			   printVal env (pair sub 0, ty, depth-1);
			   printTail (",", pair sub 1,len-1)
		       end
            end
     in print "["; printTail("",obj,length)
    end

and printTuple(env:Basics.env, objs: object array, tys: ty list, depth:int) : unit =
    let fun printFields(nf,[ty]) = printVal env (objs sub nf,ty,depth-1)
	  | printFields(nf, ty::restty) = 
	      (printVal env (objs sub nf,ty,depth-1); print(",");
	       printFields(nf+1,restty))
	  | printFields(nf,[]) = ()
     in print("("); printFields(0,tys); print(")")
    end

and printRecord(env:Basics.env, objs: object array, labels: label list, tys: ty list, depth:int) =
    let fun printFields(nf,[l],[ty]) = 
	      (print(Symbol.name l); print("="); printVal env (objs sub nf,ty,depth-1))
	  | printFields(nf, l::restl, ty::restty) = 
	      (print(Symbol.name l); print("="); printVal env (objs sub nf,ty,depth-1);
	       print(","); printFields(nf+1,restl,restty))
	  | printFields(nf,[],[]) = ()
     in print("{"); printFields(0,labels,tys); print("}")
    end

end (* structure PrintVal *)
