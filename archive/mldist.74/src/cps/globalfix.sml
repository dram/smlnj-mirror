(* globalfix.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *)
signature GLOBALFIX =
  sig val globalfix : CPS.function -> CPS.function list
  end

structure GlobalFix : GLOBALFIX =
struct
open CPS
fun globalfix(f,vl,cexp) =
let
fun gfix ce =
  case ce of
    FIX(fl,c) =>
    let val (n,c') = gfix c
	val l' =
	revfold (fn((v,a,c),m) => let val (l,d) = gfix c in (v,a,d)::l@m end) fl n
    in (l',c')
    end
  | APP _ => ([],ce)
  | SWITCH(v,l) =>
    let val (f,l') =
	fold (fn(c,(fl,cl)) => let val (f,d) = gfix c in (f@fl,d::cl) end) l ([],[])
    in  (f,SWITCH(v,l'))
    end
  | RECORD(l,v,c) => let val (f,c') = gfix c in (f,RECORD(l,v,c')) end
  | SELECT(i,v,w,c) => let val (f,c') = gfix c in (f,SELECT(i,v,w,c')) end
  | OFFSET(i,v,w,c) => let val (f,c') = gfix c in (f,OFFSET(i,v,w,c')) end
  | PRIMOP(i,args,ret,l) =>
    let val (f,m) =
	fold (fn(c,(fl,cl)) => let val (f,c) = gfix c in (f@fl,c::cl) end) l ([],[])
    in (f,PRIMOP(i,args,ret,m))
    end
val (l,body) = gfix cexp
in  (f,vl,body) :: l
end
end (* structure GlobalFix *)
