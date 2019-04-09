(*
 * driver/control.sml: Controlling levels of verbosity, etc.
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
structure Controls = struct
    val verbose = ref true
    val debug   = ref false
    val keep_going = ref false
    val show_exports = ref false
    val parse_caching = ref 100
end

functor ControlFun (Compiler: COMPILER): CONTROL = struct

    structure P = Compiler.Control.Print

    (*
     * By not having the controls within this functor we make sure that
     * they exist only once and are shared among all instances of CM.
     *)

    local
	val sem = Futures.semaphore ()
	fun control r x = let
	    fun access NONE = !r
	      | access (SOME n) = (!r before r := n)
	in
	    Futures.sequential (sem, access) x
	end
    in
	fun verbose x = control Controls.verbose x
	fun debug x = control Controls.debug x
	fun keep_going x = control Controls.keep_going x
	fun show_exports x = control Controls.show_exports x
	fun parse_caching x = control Controls.parse_caching x
    end

    local
	val sem = Futures.semaphore ()
	fun say' s = (P.say s; P.flush ())
    in
	fun say s = Futures.sequential (sem, say') s
    end

    fun csay r s = if !r then say s else ()
    val vsay = csay Controls.verbose
    val dsay = csay Controls.debug

end
