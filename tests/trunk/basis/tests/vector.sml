(* test/vector.sml -- some test cases for Vector 
   PS 1994-12-10, 1995-06-14, 1997-03-07 *)

local
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

  structure VS = VectorSlice
  open Vector;
  infix 9 sub;

  fun extract (a: 'a vector, di: int, len: int option): 'a vector =
      VS.vector(VS.slice(a,di,len));

  val a = fromList [0,1,2,3,4,5,6];
  val b = fromList [44,55,66];
  val c = fromList [0,1,2,3,4,5,6];
  val d = tabulate(100, fn i => i mod 7);

  val e = concat [d, b, d];
  val f = extract (e, 100, SOME 3);

  fun chkiter iter f vec reslast =
      check'(fn _ =>
             let val last = ref ~1
                 val res = iter (fn x => (last := x; f x)) vec
             in (res, !last) = reslast end)

  fun chkiteri iter f vecsl reslast =
      check'(fn _ =>
             let val last = ref ~1
                 val res = iter (fn (i, x) => (last := i; f x)) (VS.slice vecsl)
             in (res, !last) = reslast end)

in

val test1 = check'(fn _ => a<>b);
val test2 = check'(fn _ => a=c);

val test3 = check'(fn _ => d sub 27 = 6);

val test4a = (tabulate(maxLen+1, fn i => i) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4b = (tabulate(~1, fn i => i)       seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4c = check'(fn _ => length (tabulate(0, fn i => i div 0)) = 0);

val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn _ => c sub 0 = 0);

val test7 = check'(fn _ => length e = 203);

val test8 = check'(fn _ => length (concat []) = 0);

val test9 = check'(fn _ => f = b);

val test9a = check'(fn _ => e = extract(e, 0, SOME (length e)) 
		    andalso e = extract(e, 0, NONE));
val test9b = check'(fn _ => fromList [] = extract(e, 100, SOME 0));
val test9c = (extract(e, ~1, SOME (length e))  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9d = (extract(e, length e + 1, SOME 0)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9e = (extract(e, 0, SOME (length e+1)) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9f = (extract(e, 20, SOME ~1)        seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9g = (extract(e, ~1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9h = (extract(e, length e + 1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9i = check'(fn _ => fromList [] = extract(e, length e, SOME 0)
		    andalso fromList [] = extract(e, length e, NONE));

val test10a = 
    chkiter map (fn x => 2*x) b (fromList [88,110,132], 66)

val test11a = 
    chkiteri VS.mapi (fn x => 2*x) (b, 0, NONE) (fromList [88,110,132], 2)
val test11b = 
    chkiteri VS.mapi (fn x => 2*x) (b, 1, NONE) (fromList [110,132], 1)
val test11c = 
    chkiteri VS.mapi (fn x => 2*x) (b, 1, SOME 0) (fromList [], ~1)
val test11d = 
    chkiteri VS.mapi (fn x => 2*x) (b, 1, SOME 1) (fromList [110], 0)
val test11e = 
    chkiteri VS.mapi (fn x => 2*x) (b, 3, NONE) (fromList [], ~1)

val test11f =
    (VS.mapi #2 (VS.slice(b, 0, SOME 4)) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11g =
    (VS.mapi #2 (VS.slice(b, 3, SOME 1)) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11h =
    (VS.mapi #2 (VS.slice(b, 4, SOME 0)) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11i =
    (VS.mapi #2 (VS.slice(b, 4, NONE)) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
end;


