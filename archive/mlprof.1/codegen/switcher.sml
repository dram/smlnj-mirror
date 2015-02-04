(* codegen.sml *)

signature SWITCHER =
sig
  structure Machine : MACHINE
  structure Lambda : LAMBDA
  val genswitch : ((Lambda.lexp * bool -> unit) *
		   bool *
		   (Lambda.lexp -> Machine.Label) *
		   (Lambda.lexp*(Lambda.con*Lambda.lexp) list * Lambda.lexp option)
	          ) -> unit
end


functor Switcher (Machine : MACHINE) : SWITCHER =
 struct

  structure Machine = Machine
  structure Lambda = Lambda

  open Basics Lambda

  val newlabel = Machine.newlabel

  fun translatepath [v] = VAR v
    | translatepath (x::p) = SELECT(x,translatepath p)
    | translatepath nil = ErrorMsg.impossible "nil in codegen.translatepath"

  fun sublist test =
    let fun subl(a::r) = if test a then a::(subl r) else subl r
          | subl nil = nil
    in  subl
    end

  fun count test =
    let fun subl(a::r) = if test a then 1+(subl r) else subl r
          | subl nil = 0
    in  subl
    end

  fun fst(a,b) = a

  val printSwitch =
     PrintUtil.printClosedSequence ("[",",","]") 
	(fn (DATAcon(DATACON{name,rep,sign,...}),_) =>
	      (PrintUtil.printSym name; print "/"; PrintBasics.printRep rep)
          | (INTcon i,_) => (print "INT"; ())
	  | (REALcon _,_) => (print "REAL"; ())
	  | (STRINGcon _,_) => (print "STRING"; ()))

  fun isboxedRep(CONSTANT _) = false
    | isboxedRep(TRANSU) = false
    | isboxedRep(_) = true

  fun isboxed (DATAcon(DATACON{rep,...})) = isboxedRep(rep)
    | isboxed (REALcon _) = true
    | isboxed (STRINGcon s) = (length s <> 1)
    | isboxed _ = false

  fun genswitch (gen,last,makecon,(a,l,default)) =
    let val join = newlabel()
	fun gencase (dc, b:lexp) =
	    let val skip = newlabel()
	    in  case dc
		 of DATAcon(DATACON{rep=TAGGED number,...}) =>
			Machine.testcase_int(number,skip)
		  | DATAcon(DATACON{rep=CONSTANT number,...}) =>
			Machine.testcase_int(number,skip)
		  | DATAcon(DATACON{rep=VARIABLE(Access.PATH p),
			    const=true,...}) => 
			(gen(SELECT(1,translatepath p),false);
			 Machine.testcase skip)
		  | DATAcon(DATACON{rep=VARIABLE(Access.PATH p),...}) => 
			(gen(translatepath p,false);
			 Machine.testcase skip)
		  | INTcon i =>
			Machine.testcase_int(i,skip)
		  | STRINGcon s =>
		     if length s = 1
		      then Machine.testcase_int(ord s,skip)
		      else Machine.testcase_string(makecon(STRING s),
						   skip, length s)
		  | REALcon s =>
		      Machine.testcase_real(makecon(REAL s),skip)
		  | _ => ();
		gen(b,last);
		Machine.endcase(join,skip)
	    end
	fun gencases (l as (_,e)::r, n) =
		if length l = n
		    then (app gencase r; 
			  gen(e,last); Machine.endcase(join,newlabel()))
		    else app gencase l
	  | gencases _ = ()
	val infinity = 10000000
	val (boxedinT,unboxedinT) = 
	    case l
	     of (DATAcon(DATACON{rep=VARIABLE _,...}),_)::_ => 
			(infinity,0)
	      | (DATAcon(DATACON{sign,...}),_)::_ =>
		    (count isboxedRep sign, 
		     count (not o isboxedRep) sign)
	      | (INTcon _,_) :: _ => (0,infinity)
	      | (REALcon _,_) :: _ => (infinity,0)
	      | (STRINGcon _,_) :: _ => (infinity, infinity)
	      |  nil => ErrorMsg.impossible "nil in codegen.genswitch"
	val boxed = sublist (isboxed o fst) l
	val unboxed = sublist (not o isboxed o fst) l
    in  gen(a,false);
	Machine.startswitch();
	case (boxed,boxedinT, unboxed, unboxedinT)
	 of (nil, 0, unboxed, n) => gencases(unboxed,n)
	  | (boxed as (REALcon _,_)::_, n, nil, 0) => gencases(boxed,n)
	  | (boxed, n, nil, 0) => (Machine.gettag(); gencases(boxed,n))
	  | (boxed as (STRINGcon _, _)::_, nb, unboxed, nu) =>
	            let val boxl = newlabel()
			and defl = newlabel()
		    in  Machine.testboxed boxl;
			gencases(unboxed,nu);
			Machine.boxed (boxl,defl);
			gencases(boxed,nb);
			Machine.endboxed defl
		    end
	  | (boxed,nb,unboxed,nu) =>
		    let val boxl = newlabel()
			and defl = newlabel()
		    in  Machine.testboxed boxl;
			gencases(unboxed,nu);
			Machine.boxed (boxl,defl);
			Machine.gettag();
			gencases(boxed,nb);
			Machine.endboxed defl
		    end;
	case default of NONE => Machine.const 0  (* to get offset *)
		      | SOME e => gen(e,last);
	Machine.endswitch(join)
    end
 end (* functor Switcher *)
