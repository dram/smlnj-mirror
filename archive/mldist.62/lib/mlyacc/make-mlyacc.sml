(* make-mlyacc.sml *)
(* create a stand-alone mlyacc image. 
   Based on code supplied by Lars Bo Nielsen *)

(* load into sml with current directory lib/mlyacc/src *)

use "load.sml";

loadAll();

fun main (argv, env) =
    let val argc = length argv
        val prog = hd argv
     in if (argc <> 2)
        then outputc std_out ("Usage: " ^ prog ^ " file\n")
        else let val [_,file] = argv
               in ParseGen.parseGen file
                  handle Io s =>
                    outputc std_out (prog ^ ": Couldn't open file: " ^ file ^ "\n")
             end
    end;

exportFn ("mlyacc", main);
