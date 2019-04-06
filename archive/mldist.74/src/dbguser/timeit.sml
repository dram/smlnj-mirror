fun timeit (f:unit ->'a) =
  let open System.Timer
      val start = start_timer()
      val result = f()
  in print (makestring(check_timer start)); print "\t";
     print (makestring(check_timer_gc start)); print "\n";
     result
  end
