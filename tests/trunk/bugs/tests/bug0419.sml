(* bug0419.sml *)
(* requires CML *)

CM.autoload "$cml/basis.cm";
CM.autoload "$cml/cml.cm";

local open CML
in
fun placeBuffer () =
   let
	val c = channel ()
	val b = channel ()
	val a = channel ()
	fun input_int (s:string) = valOf(Int.fromString s)
	fun P1 x = (TextIO.print( "Waiting for Input on Channel a? \n");
		let
			val y = input_int "12"
		in
			s__3 x y
		end)
	and
 	    s__3 x y   = ( ( send (c,y ) ; P1 y  ))

	fun P2 z = (let val v = recv c in s__5 z v  end)
	and
  	   s__5 z v  =
	( (TextIO.print (" Output on Channel b!"^Int.toString(v)^"\n");
	P2 v  ))
   in
	 spawn (fn () => P1 4  );
	 spawn (fn () => P2 5  );
	()
   end
end; (* local *)

(* NOTE: to run, use the command
 *
 *	RunCML.doit (placeBuffer, NONE);
 *
 * but the program has an infinite loop.
 *)