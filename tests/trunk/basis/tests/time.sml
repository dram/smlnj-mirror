(* test/time.sml
   PS 1995-03-23
*)


local 
  infix 1 seq
  fun e1 seq e2 = e2;
  fun check b = if b then "OK" else "WRONG";
  fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

  fun range (from, to) p = 
      let open Int 
      in
          (from > to) orelse (p from) andalso (range (from+1, to) p)
      end;

  fun checkrange bounds = check o range bounds;

  fun fib n = if n < 2 then 1 else fib(n-1) + fib(n-2);

  open Time

  val bigt = fromSeconds 987654321 + fromMicroseconds 500012;
  val litt = fromSeconds 454 + fromMicroseconds 501701

  fun chk (s, r) = 
      check'(fn _ => 
             case fromString s of
                 SOME res => res = fromMicroseconds r
               | NONE     => false)

in

val test1 = 
    check'(fn _ => zeroTime + bigt = bigt andalso bigt - zeroTime = bigt);

val test2a = 
    check'(fn _ => toSeconds zeroTime = 0
	   andalso zeroTime = fromSeconds 0
	   andalso zeroTime = fromMilliseconds 0
	   andalso zeroTime = fromMicroseconds 0);
val test2b = 
    check'(fn _ => toSeconds bigt = 987654321
	   andalso toSeconds litt = 454
	   andalso toMilliseconds litt = 454501
	   andalso toMicroseconds litt = 454501701);
val test2c = check'(fn _ => (toString(fromSeconds ~1) = "~1.000"));
val test2d = check'(fn _ => (fmt 3 (fromMilliseconds ~1) = "~0.001"));
val test2e = check'(fn _ => (fmt 6 (fromMicroseconds ~1) = "~0.000001"));

val test3a = 
    check'(fn _ => fromReal 0.0 = zeroTime
	   andalso fromReal 10.25 = fromSeconds 10 + fromMilliseconds 250);
val test3b = 
    check'(fn _ => toString(fromReal ~1.0) = "~1.000")
val test3c =
    check'(fn _ => toString(fromReal 1E30) =
                   "1000000000000000042420637374017.962")

val test4a = 
    check'(fn _ => Real.==(toReal (fromReal 100.25), 100.25));
val test6a = 
    check'(fn _ => bigt + litt = litt + bigt
	   andalso (bigt + litt) - litt = bigt
	   andalso (bigt - litt) + litt = bigt);

val test7a = 
    check'(fn _ => litt <= litt andalso litt >= litt
	   andalso zeroTime < litt andalso litt > zeroTime
	   andalso litt < bigt andalso bigt > litt
	   andalso not (litt > bigt) 
	   andalso not (bigt < litt) 
	   andalso not(litt < litt)
	   andalso not(litt > litt));

val test8a = 
    check'(fn _ => now() <= now() 
	   andalso (now () before fib 27 seq ()) < now());

val test9a = 
    ((fmt ~1 litt  = "455") seq "WRONG")
     handle Size => "OK" | _ => "EXN"

val test9b = 
    check'(fn _ => fmt 0 litt  = "455"
	   andalso fmt 2 litt = "454.50");

val test9c = 
    check'(fn _ => fmt 1 litt = "454.5"
	   andalso fmt 2 litt = "454.50"
	   andalso fmt 3 litt = "454.502"
	   andalso fmt 4 litt = "454.5017"
	   andalso fmt 5 litt = "454.50170"
	   andalso fmt 6 litt = "454.501701");
    
val test10a = 
    List.map chk
         [("189", 189000000),
	  ("189.1", 189100000),
	  ("189.125125", 189125125),
	  (".1", 100000),
	  (".125125", 125125),
	  (" \n\t189crap", 189000000),
	  (" \n\t189.1crap", 189100000),
	  (" \n\t189.125125crap", 189125125),
	  (" \n\t.1crap", 100000),
	  (" \n\t.125125crap", 125125)];

val test10b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
         ["", "now", "Monday"];
end
