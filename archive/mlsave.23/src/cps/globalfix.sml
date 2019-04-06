signature GLOBALFIX =
  sig structure CPS : CPS
      val globalfix : (CPS.function * (CPS.lvar -> bool)) ->
			(CPS.function * bool) list
  end



structure GlobalFix : GLOBALFIX =
struct
structure CPS = CPS
open CPS
fun globalfix((f,vl,cexp),known) =
let
fun gfix ce =
  case ce
    of FIX(fl,c) =>
	let val (l',c') = gfix c
	    val l'' = fold (fn ((v,args,c),l) =>
					let val (l',c') = gfix c
					in  (v,args,c')::l@l'
					end)
			   fl l'
	in  (l'',c')
	end
     | APP _ => (nil,ce)
     | SWITCH(v,l) =>
	let val (f,l') = fold (fn (c,(fl,cl)) =>
				let val (f',c') = gfix c
				in  (f'@fl,c'::cl)
				end)
			      l (nil,nil)
	in  (f,SWITCH(v,l'))
	end
     | RECORD(l,v,c) =>
	let val (f,c') = gfix c
	in  (f,RECORD(l,v,c'))
	end
     | SELECT(i,v,w,c) =>
	let val (f,c') = gfix c
	in  (f,SELECT(i,v,w,c'))
	end
     | OFFSET(i,v,w,c) =>
	let val (f,c') = gfix c
	in  (f,OFFSET(i,v,w,c'))
	end
     | PRIMOP(i,args,ret,l) =>
	let val (f,l') = fold (fn (c,(fl,cl)) =>
				let val (f',c') = gfix c
				in  (f'@fl,c'::cl)
				end)
			      l (nil,nil)
	in  (f,PRIMOP(i,args,ret,l'))
	end

val (l,body) = gfix(cexp) (* THROW AWAY bogus body. *)
in  ((f,vl,body),false) :: map (fn x as (lb,_,_) => (x,known lb)) l
end


end (* structure GlobalFix *)
