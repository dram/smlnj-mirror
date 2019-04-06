(* sparcmcbase.sml
 *
 * J.H. Reppy (3/15/89)
 * Cornell University
 * Ithaca, NY  14853
 * jhr@cs.cornell.edu
 *)

structure SparcMCBase : sig

    datatype JumpKind
    =	Bcc of int	    (* branch on condition (includes fbcc).  The arg *)
			    (* is the opcode (w/o the offset).		   *)
    |	LOADadr of	    (* Load an address given by an offset from the *)
	    (int * int * int) (* start of the current code block.  The first *)
			  (* arg is an offset to the label, the second is the*)
			    (* register containing the base code address and *)
			    (* third is the register to load.		*)
    |	ADJUSTadr of int    (* Adjust the base code address register by a *)
			    (* given offset.  The arg is the base register. *)
    |	LABELptr of int

(*    val sizejump : JumpKind * int * int * int -> int
    val emitjump : JumpKind * int * int * int -> unit
*)
    val emitstring : string -> unit
    val emitWord : int -> unit
    val emitLong : int -> unit

  (* from Bits *)
    val << : (int * int) -> int	    (* Bits.lshift *)
    val >> : (int * int) -> int	    (* Bits.rshift *)
    val ++ : (int * int) -> int	    (* Bits.orb *)
    val & : (int * int) -> int	    (* Bits.andb *)
    infix << >> ++ &

  (* from Backpatch *)
    type Label
    val newlabel : unit -> Label
    val emitbyte : int -> unit
    val align : unit -> unit
    val define : Label -> unit
    val jump : (JumpKind * Label) -> unit
    val mark : unit -> unit
    val finish : unit -> string

end (* sig *) = struct

    structure Jumps = struct

	datatype JumpKind
	=   Bcc of int
	|   LOADadr of int * int * int
	|   ADJUSTadr of int
	|   LABELptr of int

(***********************)
    val << = Bits.lshift
    val >> = Bits.rshift
    val ++ = Bits.orb
    val & = Bits.andb
(************************)
    infix << >> ++ &

    datatype immed_sz = Immed13 | Immed32

    fun sizeInt n = if ((~4096 <= n) andalso (n < 4096)) then Immed13 else Immed32

  (* Emit a 16-bit word *)
    fun eword i = if i < 0
	then eword(65536 + i)
	else [chr(i >> 8), chr(i & 255)]

  (* Emit a 32-bit long word (really only get 31-bits) *)
    fun elong i = if i < 0
	then let
	    val a = ~i
	    val b = (a >> 16)
	    val c = (a & 65535);
	    in
		eword(~c + (if b = 0 then 0 else ~1)) @ eword (~b)
	    end
	else eword(i >> 16) @ eword (i & 65535)


  (* Return the size (in bytes) of a "jump" *)
    fun sizejump (Bcc _, _, _, _) = 4
    |	sizejump (LOADadr(k, _, _), _, _, dst) = (
	    case sizeInt ((dst + k) - 8)
	    of	Immed13 => 4
	    |	Immed32 => 12)
    |	sizejump (ADJUSTadr _, _, _, dst) = (
	    case sizeInt (dst - 8)
	    of	Immed13 => 4
	    |	Immed32 => 12)
    |	sizejump (LABELptr _, _, _, _) = 4

  (* We use %o4 as a temporary register for intermediate address computations.
   * Note: %o4 is also used as SparcCM.localTmp, but there should be no conflict.
   *)
    val tmpReg = 12

  (* Emit code for a jump *)
    fun emitjump (Bcc opcode, _, src, dst) = let
	    val disp = (dst - src) div 4
	    val d = if disp < 0 then (disp + 4194304) else disp
	    in
		if ((disp < ~2097152) orelse (2097152 <= disp))
		then ErrorMsg.impossible "[SparcMCBase.emitjump]" else ();
		implode(elong(opcode ++ d))
	    end
    |	emitjump (LOADadr(k, b, r), sz, _, dst) = let
	    val offset = (k + dst) - 8
	    in
		if (offset < 0) then ErrorMsg.impossible "[offset < 0]" else ();
		case sizeInt offset
		of  Immed13 => 
		      (* emit "add %rb,offset,%rr" *)
			implode(eword (32768 ++ (r << 9) ++ (b >> 2)) @
			        eword (((b & 3) << 14) ++ 8192 ++ offset))
		|   Immed32 => let
			val hi22 = (offset >> 10)
			val lo10 = (offset & 1023)
			in implode(
			  (* emit "sethi %hi(offset),%rr" *)
			    eword (256 ++ (r << 9) ++ (hi22 >> 16)) @
			    eword (hi22 & 65535) @
			  (* emit "add %rr,%lo(offset),%rr" *)
			    eword (32768 ++ (r << 9) ++ (r >> 2)) @
			    eword (((r & 3) << 14) ++ 8192 ++ lo10) @
			  (* emit "add %rb,%rr,%rr" *)
			    eword (32768 ++ (r << 9) ++ (b >> 2)) @
			    eword (((b & 3) << 14) ++ r))
			end
	    end
    |	emitjump (ADJUSTadr b, 4, _, dst) = (
	    if (dst < 8) then ErrorMsg.impossible "[dst < 8]" else ();
	  (* emit "sub %rb,dst,%rb" *)
	    implode(eword (32800 ++ (b << 9) ++ (b >> 2))@
	            eword (((b & 3) << 14) ++ 8192 ++ dst-8)))
    |	emitjump (ADJUSTadr b, 12, _, dst) = (
	    if (dst < 8) then ErrorMsg.impossible "[dst < 8]" else ();
	    let
	    val d = dst-8
	    val hi22 = (d >> 10)
	    val lo10 = (d & 1023)
	    in implode(
	      (* emit "sethi %hi(d),%o4" *)
		eword (256 ++ (tmpReg << 9) ++ (hi22 >> 16)) @
		eword (hi22 & 65535) @
	      (* emit "add %o4,%lo(d),%o4" *)
		eword (32768 ++ (tmpReg << 9) ++ (tmpReg >> 2)) @
		eword (((tmpReg & 3) << 14) ++ 8192 ++ (lo10 & 1023)) @
	      (* emit "sub %rb,%o4,%rb" *)
		eword (32800 ++ (b << 9) ++ (b >> 2)) @
		eword (((b & 3) << 14) ++ tmpReg))
	    end)
    |	emitjump (LABELptr k, _, src, dst) = implode(elong((dst - src) + k))
    |	emitjump _ = ErrorMsg.impossible "[SparcMCBase.emitjump]"

    fun emitlong x  = implode(elong x)

    end

    structure Emitter : BACKPATCH = Backpatch(Jumps)

    open Emitter
    open Jumps

   fun emitbyte s = emitstring(chr s)
   fun emitWord s = emitstring(implode(eword s))
   fun emitLong s = emitstring(implode(elong s))

(* +DEBUG
    fun prReg (i : int) = (if (i < 0) orelse (31 < i)
	    then ErrorMsg.impossible "[SparcMCBase.prReg]"
	    else if (i < 16)
		then if (i < 8)
		then (print "%g"; print i; ())
		else (print "%o"; print(i-8); ())
	    else if (i < 24)
		then (print "%l"; print(i-16); ())
		else (print "%i"; print(i-24); ()))
    local
	val xdigit = "0123456789abcdef"
	fun prHexDigit i = (print(chr(ordof(xdigit, i))); ())
	fun prHex i = let
		val d0 = (i & 15)   val d1 = ((i >> 4) & 15)
		val d2 = ((i >> 8) & 15)  val d3 = ((i >> 12) & 15)
		val d4 = ((i >> 16) & 15)  val d5 = ((i >> 20) & 15)
		in
		    prHexDigit d5; prHexDigit d4; prHexDigit d3;
		    prHexDigit d2; prHexDigit d1; prHexDigit d0
		end
	val byteCnt = ref 0
	val isFinishing = ref false
	fun prSp () = (print "\n**	 ")
	fun prJ (Bcc i) = (prSp(); print "Bcc("; print i; print ")")
	|   prJ (LOADadr(offset, a, b)) = (
		prSp(); print "LOADadr("; print offset;
		print ", "; prReg a; print ", "; prReg b; print ")")
	|   prJ (ADJUSTadr a) = (prSp(); print "ADJUSTadr("; prReg a; print")")
	|   prJ (LABELptr k) = (prSp(); print "LABELptr("; print k; print ")")
    in
    val emitbyte = fn i => (if not(!isFinishing)
	    then (
		if ((!byteCnt & 3) = 0)
		then (print "\n"; prHex(!byteCnt); print ":  "; ()) else ();
		prHexDigit((i >> 4) & 15);  prHexDigit(i & 15);
		inc byteCnt)
	    else ();
	    emitbyte i)

    fun finish args = let
	    val _ = (print "\n";  print(!byteCnt); print " bytes\n"; 
			isFinishing := true)
	    val res = Emitter.finish args
	    in
		isFinishing := false;  byteCnt := 0;
		res
	    end
    fun jump (args as (jmp, lab)) = (prJ jmp; Emitter.jump args)
    fun mark () = (prSp(); print "MARK"; Emitter.mark())
    fun align () = (
	    if ((!byteCnt & 3) <> 0)
	    then (byteCnt := !byteCnt + (4 - (!byteCnt & 3))) else ();
	    prSp(); print "ALIGN"; Emitter.align())
    end
-DEBUG *)


end (* structure SparcMCBase *)
