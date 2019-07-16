functor MP () : MP =
struct

    local
      structure U = System.Unsafe
      val cfun = U.CInterface.c_function "SMLNJ-MP"
      fun cfun' s = (print ("looking for \""^s^"\"...");
		     cfun s
		     before
		     print (" found\n"))
      val cfun = cfun'
      val cfun_spin_lock = cfun "spin_lock"
    in

      type spin_lock = U.Assembly.A.spin_lock


      (* fast locks *)
      val spin_lock : unit -> spin_lock = U.cast (fn () => ref false)
      val try_lock = U.Assembly.A.try_lock  
      val unlock   = U.Assembly.A.unlock   

(*
      (* slow locks *)
      val spin_lock : unit -> spin_lock = cfun "spin_lock"
      val try_lock = cfun "c_try_lock"
      val unlock = cfun "c_unlock"
*)


      fun lock l : int = 
	  let fun aux () = if try_lock l then 0
			   else aux ()
	  in
	      aux ()
	  end

	  
      fun debuglock l (s : string) =
	  let fun spin (m,n) =
	      if try_lock l then
		m
	      else 
		  (print s;
		   if n = 100 then
		       spin (m+1,0)
		   else spin (m,n+1))
	  in
	      spin (0,0)
	  end

      exception ReleaseProc

      local
(** doesn't work in 108.2
	open System.Timer 
	val t0 = TIME {sec=0,usec=0}
**)
	val relProc : (unit -> 'a) = cfun "release_proc"
      in
	fun releaseProc () = 
	      ((** System.Unix.setitimer (0,t0,t0);  **)
	       relProc ();
	       raise ReleaseProc)
		   
      end


      local
	val relProc : (unit -> 'a) = cfun "release_proc"
	val max_procs : int = ((cfun "max_procs") : unit -> int) ()
	val acqProc : ('a * (unit -> unit)) -> bool = cfun "acquire_proc"
	fun uncaught exn = (print "uncaught exn in acquireProc\n";
			    relProc ();
			    releaseProc ())
      in
	fun acquireProc (v,f) =
	    (!U.Assembly.activeProcs) < max_procs
	    andalso
	    let fun f' () = (((f ()) handle exn => uncaught exn);
			     releaseProc ())
	    in
		acqProc (v,f')
	    end
      end


      val getvar = U.getvar
      val setvar = U.setvar

(****
      local
	  open System.Signals
	      
	  fun handler _ = 
	      callcc (fn c =>
		      (callcc (fn k =>
			       (throw c k));
		       (releaseProc ()) handle
		       ReleaseProc => 
			   (throw (!U.toplevelcont) ())))
      in
	  val _ = setHandler(SIGINT,SOME handler)
      end
****)

  
    end (* local *)
end (* struct *)

