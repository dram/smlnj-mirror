(* Copyright 1996 by AT&T Bell Laboratories. *)
(* persmap.sml *)

structure PersMap : ORD_MAP = 
  BinaryMapFn
    (struct
       type ord_key = PersStamps.persstamp
       val compare = PersStamps.compare
     end)


