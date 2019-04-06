(* User-level debugger code *)
use "dbguser/util.sml";
use "dbguser/interface.sml";
use "dbguser/breaks.sml";
use "dbguser/emacs.sml";
use "dbguser/commands.sml";
(* Debugger versions of pervasives *) 
(* following is to get reasonable definitions and icounting for
   libraries. *)
let open System.Control.CG
in (bodysize := 40;
    rounds := 3;
    reducemore := 15; 
    icount := true)
end;
use "dbguser/normperv.sml";
use "dbguser/system.sml";
use "dbguser/hsignals.sml";
use "dbguser/hio.sml";
use "dbguser/hstore.sml";
structure NULL_PERV = struct end;
(* Xuse_file (FULLDEBUG "NULL_PERV") "dbguser/list.sml";   (* ???? *) *)
structure DebugList = List;
use "dbguser/debugperv.sml";
