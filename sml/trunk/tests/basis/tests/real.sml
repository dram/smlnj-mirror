(* test/real.sml -- PS 1995-03-24, 1996-05-16, 1996-07-02, 1996-09-25 *)

infix 1 seq;
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;


local 
    open Real
in	

val test1 = check(sign ~57.0 = ~1 andalso sign 99.0 = 1 andalso sign 0.0 = 0);
val test2 = check(sameSign(~255.0, ~256.0) andalso sameSign(255.0, 256.0) 
		  andalso sameSign(0.0, 0.0));
val test3 = check(sign 1E~300 = 1 andalso sign ~1E~300 = ~1
		  andalso sign 1E300 = 1 andalso sign ~1E300 = ~1);

local 
    val args = [0.0, 99.0, ~5.0, 1.1, ~1.1, 1.9, ~1.9, 2.5, ~2.5, 
		1000001.4999, ~1000001.4999];
    val minInt = valOf Int.minInt;
    val maxInt = valOf Int.maxInt;
    val rminInt = real minInt;
    val rmaxInt = real maxInt;
    fun chkminmax f min max = 
	check'(fn _ => 
	       List.all (fn r => f r = minInt) (rminInt :: min)
	       andalso List.all (fn r => f r = maxInt) (rmaxInt :: max));
    fun chkfail f r =
	(f r; "WRONG") 
	handle Overflow => "OK" | _ => "WRONG"
in
val test4a = 
  check(map ceil args 
	= [0, 99, ~5, 2, ~1, 2, ~1, 3, ~2, 1000002, ~1000001]);
val test4b = chkminmax ceil [rminInt-0.9] [rmaxInt-0.1];
val test4c = map (chkfail ceil) [rminInt-1.0, rmaxInt+0.1];

val test5a = check(map floor args
		   = [0, 99, ~5, 1, ~2, 1, ~2, 2, ~3, 1000001, ~1000002]);
val test5b = chkminmax floor [rminInt+0.1] [rmaxInt+0.9];
val test5c = map (chkfail floor) [rminInt-0.1, rmaxInt+1.0];

val test6a = check(map trunc args
		   = [0, 99, ~5, 1, ~1, 1, ~1, 2, ~2, 1000001, ~1000001]);
val test6b = chkminmax trunc [rminInt-0.9] [rmaxInt+0.9];
val test6c = map (chkfail trunc) [rminInt-1.0, rmaxInt+1.0];

val test7a = check(map round args 
		   = [0, 99, ~5, 1, ~1, 2, ~2, 2, ~2, 1000001, ~1000001]);
val test7b = chkminmax round [rminInt-0.5, rmaxInt+0.4]
val test7c = map (chkfail round) [rminInt-0.6, rmaxInt+0.5];

end

val test8 = check(Real.==(0.0, real 0)
		  andalso Real.==(2.0, real 2) 
		  andalso Real.==(~2.0, real ~2));

fun chk(s, r) = 
    let val eps = abs r * 1E~10 
    in 
	check'(fn _ => 
	       case fromString s of
		   SOME res => abs(res - r) <= eps
                 | NONE     => false)
    end

fun chk(s, r) = 
    let val eps = abs r * 1E~10 
    in 
	check'(fn _ => 
	       case fromString s of
		   SOME res => abs(res - r) <= eps
                 | NONE     => false)
    end
  

val test9a = 
    List.map chk[("12.", 12.0),
		 ("12.E", 12.0),
		 ("12.E+", 12.0),
		 ("12.E-", 12.0),
		 ("12.E2", 12.0),
		 ("12.E+2", 12.0),
		 ("12.E-2", 12.0),
		 ("12E+", 12.0),
		 ("12E-", 12.0)];

val test9b = 
    List.map chk[("0", 0.0),
		 ("156", 156.0),
		 ("+156", 156.0), 
		 ("~156", ~156.0), 
		 ("-156", ~156.0), 
		 ("156.25", 156.25), 
		 ("+156.25", 156.25), 
		 ("~156.25", ~156.25), 
		 ("-156.25", ~156.25),
		 (".25", 0.25),
		 ("+.25", 0.25),
		 ("~.25", ~0.25),
		 ("-.25", ~0.25)];

val test9c = 
    List.map chk[ ("156E024", 156E024),
		  ("+156E024",  156E024),
		  ("~156E024", ~156E024),
		  ("-156E024", ~156E024),
		  ("156.25E024", 156.25E024),
		  ("+156.25E024", 156.25E024),
		  ("~156.25E024", ~156.25E024),
		  ("-156.25E024", ~156.25E024),
		  (".25E024", 0.25E024),
		  ("+.25E024", 0.25E024),
		  ("~.25E024", ~0.25E024),
		  ("-.25E024", ~0.25E024)];

val test9d = 
    List.map chk[ ("156E+024", 156E024),
		  ("+156E+024", 156E024),
		  ("~156E+024", ~156E024),
		  ("-156E+024", ~156E024),
		  ("156.25E+024", 156.25E024),
		  ("+156.25E+024", 156.25E024),
		  ("~156.25E+024", ~156.25E024),
		  ("-156.25E+024", ~156.25E024),
		  (".25E+024", 0.25E024),
		  ("+.25E+024", 0.25E024),
		  ("~.25E+024", ~0.25E024),
		  ("-.25E+024", ~0.25E024)];

val test9e = 
    List.map chk[ ("156E~024", 156E~024),
		  ("+156E~024", 156E~024),
		  ("~156E~024", ~156E~024),
		  ("-156E~024", ~156E~024),
		  ("156.25E~024", 156.25E~024),
		  ("+156.25E~024", 156.25E~024),
		  ("~156.25E~024", ~156.25E~024),
		  ("-156.25E~024", ~156.25E~024),
		  (".25E~024", 0.25E~024),
		  ("+.25E~024", 0.25E~024),
		  ("~.25E~024", ~0.25E~024),
		  ("-.25E~024", ~0.25E~024)];

val test9f = 
    List.map chk[ ("156E-024", 156E~024),
		  ("+156E-024", 156E~024),
		  ("~156E-024", ~156E~024),
		  ("-156E-024", ~156E~024),
		  ("156.25E-024", 156.25E~024),
		  ("+156.25E-024", 156.25E~024),
		  ("~156.25E-024", ~156.25E~024),
		  ("-156.25E-024", ~156.25E~024),
		  (".25E-024", 0.25E~024),
		  ("+.25E-024", 0.25E~024),
		  ("~.25E-024", ~0.25E~024),
		  ("-.25E-024", ~0.25E~024)];

val test9g = 
    List.map chk[ ("156e024", 156E024),
		  ("+156e024", 156E024),
		  ("~156e024", ~156E024),
		  ("-156e024", ~156E024),
		  ("156.25e024", 156.25E024),
		  ("+156.25e024", 156.25E024),
		  ("~156.25e024", ~156.25E024),
		  ("-156.25e024", ~156.25E024),
		  (".25e024", 0.25E024),
		  ("+.25e024", 0.25E024),
		  ("~.25e024", ~0.25E024),
		  ("-.25e024", ~0.25E024)];

val test9h = 
    List.map chk[ ("156e+024", 156E024),
		  ("+156e+024", 156E024),
		  ("~156e+024", ~156E024),
		  ("-156e+024", ~156E024),
		  ("156.25e+024", 156.25E024),
		  ("+156.25e+024", 156.25E024),
		  ("~156.25e+024", ~156.25E024),
		  ("-156.25e+024", ~156.25E024),
		  (".25e+024", 0.25E024),
		  ("+.25e+024", 0.25E024),
		  ("~.25e+024", ~0.25E024),
		  ("-.25e+024", ~0.25E024)];

val test9i = 
    List.map chk[ ("156e~024", 156E~024),
		  ("+156e~024", 156E~024),
		  ("~156e~024", ~156E~024),
		  ("-156e~024", ~156E~024),
		  ("156.25e~024", 156.25E~024),
		  ("+156.25e~024", 156.25E~024),
		  ("~156.25e~024", ~156.25E~024),
		  ("-156.25e~024", ~156.25E~024),
		  (".25e~024", 0.25E~024),
		  ("+.25e~024", 0.25E~024),
		  ("~.25e~024", ~0.25E~024),
		  ("-.25e~024", ~0.25E~024)];

val test9j = 
    List.map chk[ ("156e-024", 156E~024),
		  ("+156e-024", 156E~024),
		  ("~156e-024", ~156E~024),
		  ("-156e-024", ~156E~024),
		  ("156.25e-024", 156.25E~024),
		  ("+156.25e-024", 156.25E~024),
		  ("~156.25e-024", ~156.25E~024),
		  ("-156.25e-024", ~156.25E~024),
		  (".25e-024", 0.25E~024),
		  ("+.25e-024", 0.25E~024),
		  ("~.25e-024", ~0.25E~024),
		  ("-.25e-024", ~0.25E~024)];

val test10 = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG") 
             ["e10", "E10", 
	      "+e10", "+E10", 
	      "~e10", "~E10", 
	      "-e10", "-E10"];

(* Note: There is some unclarity concerning rounding.  Should 1.45,
rounded to two significant digits, be "1.4" (nearest even digit) or
"1.5" (new greater digit) in case of a tie?  PS 1996-05-16
C printf rounds to "1.4". DBM 9/3/1999 *)

val test11a = 
    (fmt (StringCvt.FIX (SOME ~1)) 12.3456; "WRONG")
    handle Size => "OK" | _ => "WRONG";

val test11b = 
    (fmt (StringCvt.FIX (SOME 100000)) 12.3456)
    handle Size => "OK" | _ => "WRONG";

fun chkFIX (r, s0, s1, s2, s6) = 
    fmt (StringCvt.FIX (SOME 0)) r = s0
    andalso fmt (StringCvt.FIX (SOME 1)) r = s1
    andalso fmt (StringCvt.FIX (SOME 2)) r = s2
    andalso fmt (StringCvt.FIX (SOME 6)) r = s6
    andalso fmt (StringCvt.FIX NONE) r = s6;

fun chkFIX' (r, s0, s1, s2, s6) = 
    chkFIX(r, s0, s1, s2, s6) 
    andalso (Real.==(r, 0.0) 
	     orelse chkFIX(~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6));

val test11c = 
    check'(fn _ =>
	   List.all chkFIX'
	   [(0.0, "0", "0.0", "0.00", "0.000000"),
	    (1.0, "1", "1.0", "1.00", "1.000000"),
	    (1.4, "1", "1.4", "1.40", "1.400000"),
	    (1.5, "2", "1.5", "1.50", "1.500000"),
	    (2.5, "2", "2.5", "2.50", "2.500000"),
	    (1.6, "2", "1.6", "1.60", "1.600000"),
    	    (1.45, "1", "1.4", "1.45", "1.450000"),
	    (3.141592653589, "3", "3.1", "3.14", "3.141593"),
	    (91827364509182.0, "91827364509182", "91827364509182.0", 
	     "91827364509182.00", "91827364509182.000000")]);

val test12a = 
    (fmt (StringCvt.SCI (SOME ~1)) 12.3456; "WRONG")
    handle Size => "OK" | _ => "WRONG";

val test12b = 
    (fmt (StringCvt.SCI (SOME 100000)) 12.3456)
    handle Size => "OK" | _ => "WRONG";

fun chkSCI (r, s0, s1, s2, s6) = 
    fmt (StringCvt.SCI (SOME 0)) r = s0
    andalso fmt (StringCvt.SCI (SOME 1)) r = s1
    andalso fmt (StringCvt.SCI (SOME 2)) r = s2
    andalso fmt (StringCvt.SCI (SOME 6)) r = s6
    andalso fmt (StringCvt.SCI NONE) r = s6;

fun chkSCI' (r, s0, s1, s2, s6) = 
    chkSCI(r, s0, s1, s2, s6) 
    andalso (Real.==(r, 0.0)
	     orelse chkSCI(~r, "~"^s0, "~"^s1, "~"^s2, "~"^s6));

val test12c = 
    check'(fn _ => 
	   List.all chkSCI'
	   [(0.0, "0E0", "0.0E0", "0.00E0", "0.000000E0"),
	    (0.0012345678, "1E~3", "1.2E~3", "1.23E~3", "1.234568E~3"),
	    (1.0, "1E0", "1.0E0", "1.00E0", "1.000000E0"),
	    (1.4, "1E0", "1.4E0", "1.40E0", "1.400000E0"),
	    (1.5, "2E0", "1.5E0", "1.50E0", "1.500000E0"),
	    (2.5, "2E0", "2.5E0", "2.50E0", "2.500000E0"),
	    (1.6, "2E0", "1.6E0", "1.60E0", "1.600000E0"),
    	    (1.45, "1E0", "1.4E0", "1.45E0", "1.450000E0"),
	    (3.141592653589, "3E0", "3.1E0", "3.14E0", "3.141593E0"),
	    (91827364509182.0, "9E13", "9.2E13", "9.18E13", "9.182736E13")]);

val test13a = 
    (fmt (StringCvt.GEN (SOME 0)) 12.3456; "WRONG")
    handle Size => "OK" | _ => "WRONG";

val test13b = 
    (fmt (StringCvt.GEN (SOME 100000)) 12.3456)
    handle Size => "OK" | _ => "WRONG";

fun chkGEN (r, s1, s2, s6, s12) = 
    fmt (StringCvt.GEN (SOME 1)) r = s1
    andalso fmt (StringCvt.GEN (SOME 2)) r = s2
    andalso fmt (StringCvt.GEN (SOME 6)) r = s6
    andalso fmt (StringCvt.GEN (SOME 12)) r = s12
    andalso fmt (StringCvt.GEN NONE) r = s12
    andalso toString r = s12;

fun chkGEN' (r, s1, s2, s6, s12) = 
    chkGEN(r, s1, s2, s6, s12) 
    andalso (Real.==(r, 0.0) orelse 
	     chkGEN(~r, "~"^s1, "~"^s2, "~"^s6, "~"^s12));

val test13c = 
    check'(fn _ =>
	   List.all chkGEN'
	   [(0.0,               "0.0", "0.0",     "0.0", "0.0"),
	    (0.0012345678,    "0.001", "0.0012", "0.00123457", 
	     "0.0012345678"),
	    (1.0,              "1.0", "1.0",  "1.0", "1.0"),
	    (1.4,              "1.0", "1.4",  "1.4", "1.4"),
	    (1.5,              "2.0", "1.5",  "1.5", "1.5"),
	    (2.5,              "2.0", "2.5",  "2.5", "2.5"),
	    (1.6,              "2.0", "1.6",  "1.6", "1.6"),
    	    (1.45,             "1.0", "1.4",  "1.45", "1.45"),
	    (3.141592653589,   "3.0", "3.1",  "3.14159", "3.14159265359"),
	    (91827364509182.0, "9E13", "9.2E13",  "9.18274E13", 
							"9.18273645092E13")]);
end

(* 
fun f r n = Real.fmt (StringCvt.GEN (SOME n)) r;
fun ff r = map (f r) [1,2,6,12];
 *)
