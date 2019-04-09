(* ulist.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure UrList =
struct
  open UrList  (* this should be UrList *)
  exception Hd
  exception Tl
  exception Nth
  exception NthTail
  exception Last
  fun hd (a::r) = a | hd nil = raise Hd
  fun tl (a::r) = r | tl nil = raise Tl    
  fun null nil = true | null _ = false
  fun length l = 
      let fun j(k,nil) = k
	    | j(k, a::x) = j(k+1,x)
       in j(0,l)
      end
  fun op @(x,nil) = x
    | op @(x,l) =
    let fun f(nil,l) = l
          | f([a],l) = a::l
        | f([a,b],l) = a::b::l
        | f([a,b,c],l) = a::b::c::l
        | f(a::b::c::d::r,l) = a::b::c::d::f(r,l)
     in f(x,l)
    end
  fun rev l =
      let fun f (nil, h) = h
	    | f (a::r, h) = f(r, a::h)
      in  f(l,nil)
      end
  fun map f =
      let fun m nil = nil
            | m [a] = [f a]
            | m [a,b] = [f a, f b]
            | m [a,b,c] = [f a, f b, f c]
            | m (a::b::c::d::r) = f a :: f b :: f c :: f d :: m r
      in  m
      end
  fun fold f [] = (fn b => b)
    | fold f (a::r) = (fn b => let fun f2(e,[]) = f(e,b)
				     | f2(e,a::r) = f(e,f2(a,r))
			       in f2(a,r)
			       end)
  fun revfold f [] = (fn b => b)
    | revfold f (a::r) = (fn b => let fun f2(e,[],b) = f(e,b)
					| f2(e,a::r,b) = f2(a,r,f(e,b))
				  in f2(a,r,b)
				  end)	
  fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
  fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
  fun nthtail(e,0) = e 
    | nthtail(e::r,n) = nthtail(r,n-1)
    | nthtail _ = raise NthTail
  fun nth x = hd(nthtail x) handle NthTail => raise Nth | Hd => raise Nth
  fun exists pred =
      let fun f nil = false
	    | f (hd::tl) = pred hd orelse f tl
      in  f
      end
  fun last [] = raise Last
    | last (x::nil) = x
    | last (_::r) = last r
end (* structure List *)


(*
 * $Log: ulist.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:39  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:17  george
 *   Version 109.24
 *
 *)
