(* irConstant.sml --- 
 * 
 * This module has been written to be independent of whether the frame
 * grows from high to low memory or reverse.
 *)
functor IrConstant(Frame:FRAME) = struct
  structure F = Frame

  datatype const = 
      FRAMESIZE of F.frame 
    | FRAMESLOT of {slot:int, frame:F.frame} 
    | ARGSLOT of {slot:int, frame:F.frame}

  exception IrConstant
  fun error msg = (print("IrConstant." ^ msg ^ "\n"); raise IrConstant)

  fun log2(acc:int, n, ln) = 
    if acc > n then error "log: must be power of 2"
    else if acc = n then ln else log2(acc+acc, n, ln+1)

  val bits = Word.fromInt(log2(1, F.alignment, 0))

  (* stack allocation is in multiples of F.alignment *)
  fun multipleOfFrames n = let
    val nbyBits = Word.>>(Word.fromInt(n+F.alignment - 1), bits)
    val multbyBits = Word.<<(nbyBits, bits)
  in Word.toInt multbyBits
  end

  fun valueOf(FRAMESIZE frame) = let
        val slots = F.localAreaSz(frame) + F.fixedFrameArea + F.maxOutgoingArgs
      in multipleOfFrames(slots * 4)
      end
    | valueOf(FRAMESLOT{slot, frame}) = 
	if F.hiToLowStackGrowth then ~(slot + 1) * 4 else slot * 4
    | valueOf(ARGSLOT{slot, frame}) = let
	val offset = (F.argOffset + slot) * 4
      in if F.hiToLowStackGrowth then offset else ~offset
      end

  fun toString const = let
    fun ms n = if n<0 then ("-" ^ Int.toString (~n)) else Int.toString n
  in ms (valueOf const)
  end
end
