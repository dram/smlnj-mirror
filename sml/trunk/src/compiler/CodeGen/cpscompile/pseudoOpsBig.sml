(* PseudoOpsBig.sml -- pseudo ops for big endian machines.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor PseudoOpsBig(M : MACH_SPEC) = struct
  structure T = M.ObjDesc
  structure W = Word

  datatype pseudo_op = 
      ALIGN4
    | JUMPTABLE of {base:Label.label, targets:Label.label list}

  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &
  val itow  = Word.fromInt

  fun error msg = ErrorMsg.impossible ("PseudoOpsBig." ^ msg)

  (* since we never compile assembly code, we don't really care
   * about this, but it is good enough for debugging purposes.
   *)
  fun toString (JUMPTABLE{base, targets}) =
	Label.nameOf base ^ ":\t.jumptable " ^
	List.foldr (op ^) "" (map (fn l => Label.nameOf l ^ " ") targets) ^
	"\n"
    | toString ALIGN4 = "\t .align\n"

  fun emitValue {pOp = JUMPTABLE{base, targets}, loc, emit} = let
	fun emitByte n = emit(Word8.fromLargeWord(Word.toLargeWord n))
	fun emitWord w = (emitByte((w >> 0w8) & 0w255); emitByte(w & 0w255))
	fun emitLongX n = let val w = itow n
	in emitWord(w ~>> 0w16); emitWord(w & 0w65535)
	end
	val baseOff = Label.addrOf base
	fun emitOffset lab = emitLongX(Label.addrOf lab - baseOff)
      in
	app emitOffset targets
      end
    | emitValue{pOp=ALIGN4, loc, emit} = 
      (case Word.andb(itow loc, 0w3)
        of 0w0 => ()
         | _ => error "emitValue: ALIGN"
       (*esac*))
      
  fun align n = W.toIntX(W.andb(W.fromInt n + 0w3, W.notb 0w3))

  fun padding(loc) = align(loc) - loc

  fun sizeOf (JUMPTABLE {targets, ...}, loc) = 
        4 * length targets + padding(loc)
    | sizeOf (ALIGN4, loc) = padding loc

  fun adjustLabels (pOp, loc) = let
    fun setAddr(lab, new) = 
      if Label.addrOf(lab)=new then false else (Label.setAddr(lab, new); true) 
  in 
     case pOp
     of JUMPTABLE{base, ...} => setAddr(base, align(loc))
      | ALIGN4 => false
  end

end



	

(*
 * $Log: pseudoOpsBig.sml,v $
 * Revision 1.4  1998/12/21 17:04:55  jhr
 *   Got rid of "removable" function.
 *
 * Revision 1.3  1998/11/18 03:53:09  jhr
 *  New array representations.
 *
 * Revision 1.2  1998/10/28 18:20:40  jhr
 *   Removed code generator support for STRING/REAL constants.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
