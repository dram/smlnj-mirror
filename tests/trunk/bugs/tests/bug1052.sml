(* bug1052.sml *)
(* has to be executed in test directory *)

val ppconsumer = {consumer = Control.Print.say, 
		  linewidth = fn () => !Control.Print.linewidth, 
		  flush = Control.Print.flush};
val fname = "/dev/null";
val instream = TextIO.openIn fname;
val source = Source.newSource(fname,instream,false,ppconsumer);
Source.closeSource source;
