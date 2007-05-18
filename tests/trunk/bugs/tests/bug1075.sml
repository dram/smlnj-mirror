(* bug1075.sml *)
(* Profiler bug test case 
Compiler.Profile.setProfMode true;
fun f x = 3;
Compiler.Profile.reset();
f 3;
Compiler.Profile.report(TextIO.stdOut);
 *)
