(* pre-os.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * This is the OS structure(s) with only types, for compiling signatures.
 *
 *)

structure OS =
    struct
	structure W32G = Win32_General

	type syserror = W32G.word
	    
	structure Process =
	    struct
		type status = W32G.word
	    end

	structure IO =
	    struct
		datatype iodesc = IODesc of Win32_General.hndl ref
	    end
    end

(*
 * $Log$
 *)
