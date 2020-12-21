(* char-buffer-pp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that puts its output in a CharBuffer.buf object.  There
 * are no styles and tokens are strings.  You can use this module to pretty-print
 * into a string as follows:
 *
 *	val buf = CharBuffer.new 1024
 *	val ppStrm = CharBufferPP.openBuf {dst = buf, wid = 80}
 *	.... pretty printing ....
 *	val result = CharBuffer.contents buf
 *)

structure CharBufferPP : sig

    include PP_STREAM
      where type token = string

    val openBuf : {dst : CharBuffer.buf, wid : int} -> stream

  end = struct

    structure DevOps = struct
	type t = CharBuffer.buf
      (* no style support *)
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
      (* output some number of spaces to the device *)
        fun space (dst, n) = CharBuffer.addVec (dst, StringCvt.padLeft #" " n "")
	val indent = space
      (* output a new-line to the device *)
	fun newline dst = CharBuffer.add1 (dst, #"\n")
      (* output a string/character in the current style to the device *)
	fun string (dst, s) = CharBuffer.addVec (dst, s)
	fun char (dst, c) = CharBuffer.add1 (dst, c)
      (* nothing to flush *)
	fun flush dst = ()
      end

    structure Device = DefaultDeviceFn (DevOps)

    structure PP = PPStreamFn (
      structure Token = StringToken
      structure Device = Device)

    open PP

    fun openBuf {dst, wid} = PP.openStream (Device.newWithWidth (dst, wid))

  end;
