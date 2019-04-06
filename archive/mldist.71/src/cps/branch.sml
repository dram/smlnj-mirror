structure Branch : sig val branch: CPS.function list -> CPS.function list end =
struct open CPS Access

val infinity = 1000000000

fun minl l =
 let fun f(i,nil) = i | f(i,j::rest) = if i<j then f(i,rest) else f(j,rest)
  in f(infinity,l)
 end

val opp = fn P.boxed => P.unboxed | P.unboxed => P.boxed 
           | P.< => P.>= | P.>= => P.< | P.> => P.<= | P.<= => P.>
           | P.lessu => P.gequ | P.gequ => P.lessu
           | P.ieql => P.ineq | P.ineq => P.ieql 
           | P.feql => P.fneq | P.fneq => P.feql 
	   | P.fge => P.flt | P.flt => P.fge 
	   | P.fle => P.fgt | P.fgt => P.fle
           | _ => ErrorMsg.impossible "3893 in Branch"

fun minl l = let fun f(m,a::r) = if m<a then f(m,r) else f(a,r)
                   | f(m,nil) = m
              in f(infinity,l)
             end

fun all (a::rest) = a andalso all rest | all nil = true

fun scc funs = 
let exception Unseen
    type info = {dfsnum: int ref, sccnum: int ref, body: cexp} 
    val m : info Intmap.intmap = Intmap.new(32,Unseen)
    val lookup = Intmap.map m

    val compmap : int Intmap.intmap = Intmap.new(32,Unseen)
    
    val comps = ref 0 and id = ref 0

    val stack : (int * int ref) list ref = ref nil

    fun scc (node, {dfsnum as ref d, sccnum, body}) =
       if d >= 0 then d else
        let fun g (RECORD(_,_,e)) = g e
              | g (SELECT(_,_,_,e)) = g e
              | g (OFFSET(_,_,_,e)) = g e
              | g (SWITCH(_,el)) = minl (map g el)
              | g (PRIMOP(_,_,_,[e])) = g e
              | g (PRIMOP(_,_,_,[a,b])) = min(g a, g b)
              | g (APP(LABEL w, _)) = scc(w, lookup w)
              | g _ = infinity
          
            fun newcomp(c,(n,sccnum)::rest) = 
			(sccnum := c;
(*                         print n; print "  "; print c; print "\n"; *)
                         if n=node then rest else newcomp(c,rest))

	    val v = !id 
            val _ = (id := v+1; 
		     stack := (node, sccnum) :: !stack;
                     dfsnum := v)
            val gb = g body
        in if v <= gb
             then (stack := newcomp(!comps before comps := !comps + 1, !stack);
                   v)
             else gb
       end
 in app (fn (f,_,body) => Intmap.add m 
			         (f,{dfsnum=ref ~1, sccnum=ref ~1, body=body}))
        funs;
    app (fn (f,_,_) => scc(f, lookup f)) funs;
    ! o #sccnum o lookup
end

fun branch funs = 
let val sccnum = scc funs
    fun rewrite(f,vl,body) =
        let val n = sccnum f
            fun g (RECORD(r,w,e)) = 
			let val (d,e') = g e in (d, RECORD(r,w, e')) end
              | g (SELECT(i,v,w,e)) = 
			let val (d,e') = g e in (d, SELECT(i,v,w, e')) end
              | g (OFFSET(i,v,w,e)) =
			let val (d,e') = g e in (d, OFFSET(i,v,w, e')) end
              | g (SWITCH(v,el)) = 
			let val ge = map g el
                         in (all (map #1 ge), SWITCH(v, map #2 ge))
                        end
              | g (PRIMOP(p,vl,wl,[e])) = 
			let val (d,e') = g e in (d, PRIMOP(p,vl,wl,[e'])) end
              | g (PRIMOP(p,vl,wl,[e1,e2])) = 
		      (case (g e1, g e2)
                        of ((false, e1'), (true, e2')) =>
				(true, PRIMOP(opp p, vl,wl,[e2',e1']))
                         | ((c1, e1'), (c2, e2')) =>
				(c1 orelse c2, PRIMOP(p, vl,wl,[e1',e2'])))
              | g (e as APP(LABEL w, _)) = (sccnum w = n, e)
              | g e = (false, e)
          in (f, vl, #2(g body))
         end
 in map rewrite funs
end

end


	


