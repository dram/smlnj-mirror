signature INDEX = sig
  val report : ErrorMsg.inputSource -> (BareAbsyn.dec * Basics.env) -> unit
  val openIndexFile :string -> outstream option
end;

structure Index : INDEX = struct

open Basics BareAbsyn

val DEBUG = false

fun index_file_name (name :string) :string =
  let fun split (dirname as ("/"::rest)) filename = (rev dirname, filename)
        | split (c::rest) filename = split rest (c::filename)
	| split [] filename = ([], filename)
      val (dirname, filename) = (split (rev (explode name)) [])
  in implode(dirname @ ("."::"i"::"."::filename))
  end

fun openIndexFile (fname :string) =
 if !System.Control.indexing 
   then if !System.Control.markabsyn
        then SOME (open_out(index_file_name fname))
             handle Io s => (print("[cannot open index file, "^s^"]\n");
		  	     NONE)
        else (print("[indexing is turned on, but markabsyn is turned off]\n");
	      NONE)
   else NONE

fun report ({indexStream=NONE,...}:ErrorMsg.inputSource) _ = ()
  | report (inputSource as {fileName, indexStream = SOME istream,...}) 
           (absyn :BareAbsyn.dec, env: Basics.env) =
  let val current_pos = ref 0 and limit_pos = ref 0
    fun withpos(L1,L2,f) = let val c = !current_pos and l = !limit_pos
	                    in current_pos := L1; limit_pos := L2;
			       f() before (current_pos := c; limit_pos := l)
			       handle e => (current_pos := c; limit_pos := l;
					    raise e)
			   end 

    val and_seq :bool ref = ref(false)

    val formatQid = PrintUtil.formatQid
    val print = outputc istream

    fun nl () = (print "\n"; PrintType.resetPrintType())

    fun print_type (typ :ty) :unit =
      (print "(";
       PrintType.printType istream env typ;
       print ")")

    fun print_sym (s: Symbol.symbol) = print (Symbol.name s);

    fun comma_seq elems =
      let fun prElems [el] = print el
	    | prElems (el::rest) = (print el; print ", "; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

    fun print_and_seq pr elems =
      let val old_and_seq = (!and_seq)
          fun prElems (el::rest) = (pr el; and_seq := true; prElems rest)
	    | prElems [] = ()
       in prElems elems;
	  and_seq := old_and_seq
      end


    fun print_entry (name, f) = 
      let val (_,start_line,_) = ErrorMsg.filepos inputSource (!current_pos)
	  val (_,end_line,_) = ErrorMsg.filepos inputSource (!limit_pos) in
        print name; print " "; 
	print ((makestring start_line) ^ " ");
	print ((makestring (!current_pos)) ^ " ");
	print (if (!and_seq) then "A " else "X ");
	print ((makestring end_line) ^ " ");
	print ((makestring (!limit_pos)) ^ " ");
	print "\127 ";
        f();
        nl()
      end;

    fun printPat (VARpat (v as VALvar{typ=ref t,name,...})) =
	  print_entry (formatQid name, fn()=>(print "val "; print_type t))
      | printPat (LAYEREDpat (v,p)) = (printPat(v); printPat(p))
      | printPat (RECORDpat{fields,...}) = app (printPat o #2) fields
      | printPat (APPpat(_,p)) = printPat p
      | printPat (CONSTRAINTpat (p,_)) = printPat p
      | printPat _ = ()

    and printDec(VALdec vbs) = 
	       print_and_seq (fn VB{pat,...} => printPat pat) vbs
      | printDec(VALRECdec rvbs) = 
	       print_and_seq (fn RVB{var,...} => printPat(VARpat var)) rvbs
      | printDec(TYPEdec tbs) =
          (print_and_seq
             (fn (TB{tyc=DEFtyc{path=name::_, tyfun=TYFUN{arity,...},...},def}) =>
	       print_entry(Symbol.name name, 
			   fn()=>(print "type ";
				  case arity
				      of 0 => ()
				    | 1 => (print "'a ")
				    | n => (print "(";
					    comma_seq (PrintType.typeFormals n);
					    print ") ");
				  print_type def))
	       | _ => ErrorMsg.impossible "Index0")
	      tbs)
      | printDec(DATATYPEdec{datatycs,withtycs}) =
          (print_and_seq 
             (fn GENtyc{path=name::_, arity, kind=ref(DATAtyc dcons),...} =>
                  print_entry(Symbol.name name, fn()=>print "datatype")
                  (* app (fn DATACON{name,...} =>
		          print_entry(Symbol.name name,fn()=>()))
			 dcons *)
               | _ => ErrorMsg.impossible "Index3")
             datatycs)
      | printDec(ABSTYPEdec{abstycs, withtycs, body}) =
          (app (fn GENtyc{path=name::_, kind=ref(DATAtyc dcons),...} =>
                 (print_entry(Symbol.name name, fn()=>print "abstype ");
                  app (fn (DATACON{name,...}) => 
		          print_entry(Symbol.name name,fn()=>()))
		       dcons)
	       | GENtyc{path=name::_,...} =>
	          print_entry(Symbol.name name, fn()=> print "abstype ")
               | _ => ErrorMsg.impossible "Index4")
             abstycs;
	   printDec body)
      | printDec(EXCEPTIONdec ebs) =
          (print_and_seq
             (fn (EBgen{exn=DATACON{name,...},etype}) =>
                   print_entry(Symbol.name name,
			       fn()=>(print "exn";
				      case etype of NONE => ()
				     | SOME ty' => (print " of ";
						    print_type ty')))
               | (EBdef{exn=DATACON{name,...},edef=DATACON{name=dname,...}}) =>
                   print_entry(Symbol.name name,
			       fn()=>(print "exn "; print(Symbol.name dname))))
             ebs)
      | printDec(STRdec sbs) =
          (app (fn (STRB{strvar=STRvar{name,...},def,...}) =>
                 (print_entry(formatQid name, fn()=>print "structure");
                  printStrexp def))
             sbs)
      | printDec(ABSdec sbs) = printDec(STRdec sbs)
      | printDec(FCTdec fbs) =
          (app (fn (FCTB{fctvar=FCTvar{name=fname,...},
                       param=STRvar{name=pname,...},def,...}) =>
                 (print_entry(Symbol.name fname, fn()=> print "functor ");
                  printStrexp def))
             fbs)
      | printDec(SIGdec sigvars) =
          app (fn SIGvar{name,...} =>
	       print_entry(Symbol.name name, fn()=>print "signature"))
            sigvars
      | printDec(LOCALdec(inner,outer)) = printDec(outer)
      | printDec(SEQdec decs) = app printDec decs
      | printDec(OPENdec strVars) = ()
      | printDec(MARKdec(dec,L1,L2)) = withpos(L1,L2, fn()=>printDec(dec))
      | printDec(FIXdec _) = ()
      | printDec(OVLDdec _) = ()
      | printDec(IMPORTdec _) = ()
      | printDec _ = ErrorMsg.impossible "Index2"
    
    and printStrexp(VARstr(STRvar{name,...})) = ()
      | printStrexp(STRUCTstr{body,...}) = app printDec body
      | printStrexp(APPstr{oper=FCTvar{name,...}, argexp,...}) = ()
      | printStrexp(LETstr(dec,body)) = printStrexp(body)
      | printStrexp(MARKstr(body,L1,L2)) =withpos(L1,L2,fn()=>printStrexp body)
  in
    (printDec absyn; nl())
  end

end
