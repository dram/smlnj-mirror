(* cse.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature CSE = sig fun cse_elim: {function: CPS.function,
				   click: string -> unit} -> CPS.function
                end


structure CSE : CSE = 
struct

  open CPS

fun cse_elim{function,click} = 

  exception CSE_map
  val vm : int Intmap.intmap = Intmap.new(16,CSE_map)
  fun ren v = Intmap.map vm v handle CSE_map => v
  val enter = Intmap.add vm

  fun sameval(VAR v, VAR w) = ren v= ren w
    | sameval(LABEL v, LABEL w) = ren v= ren w
    | sameval(INT i, INT j) = i=j
    | sameval(INT i, INT j) = i=j
    | sameval(INT32 i, INT32 j) = i=j
    | sameval(REAL i, REAL j) = i=j
    | sameval(STRING i, STRING j) = i=j
    | sameval _ = false

  fun all2 f (i::ir,j::jr) = f(i,j) andalso all2 f (ir,jr)
    | all2 f (nil,nil) = true
    | all2 f _ = false


  fun samepath(SELp(i,a),SELp(j,b)) = i=j andalso samepath(a,b)
    | samepath(OFFp i, OFFp j) = i=j
    | samepath _ = false

  fun samefield((v1,p1),(v2,p2)) = sameval(v1,v2) andalso samepath(p1,p2)

  fun same(RECORD(k1,vl1,_,_), RECORD(k2,vl2,_,_)) = 
                 k1=k2 andalso all2 samefield(vl1,vl2)
    | same(SELECT(i1,v1,_,cty1,_), SELECT(i2,v2,_,cty2,_)) = 
                 i1=i2 andalso sameval(v1,v2) andalso cty1=cty2
    | same(OFFSET(i1,v1,_,_), SELECT(i2,v2,_,_)) = 
                 i1=i2 andalso sameval(v1,v2)
    | same(PURE(p1,vl1,_,cty1,_),PURE(p2,vl2,_,cty2,_)) = 
                 p1=p2 andalso all2 sameval (vl1,vl2) andalso cty1=cty2

  fun hashval(VAR v) = ren v
    | hashval(LABEL v) = ren v + 789
    | hashval(INT i) = i
    | hashval(INT32 i) = i+1000
    | hashval(REAL i) = floor(i*1000) handle Overflow => 101
    | hashval(STRING i) = 484
    | hashval _ = 474

  fun hashlist(i::rest) = (hashlist rest * 37 + 89 + i) mod 1079
    | hashlist nil = 0

  fun hashpath(SELp(i,rest)) = hashlist[i,hashpath rest]
    | hashpath(OFFp(i)) = i

  fun hashfield(v,p) = hashlist[hashval v, hashpath p]

  fun hash(RECORD(_,vl,_,_)) = hashlist (map hashfield vl)
    | hash(SELECT(i,v,_,_,_)) = hashlist[5,i,hashval v]
    | hash(OFFSET(i,v,_,_,_)) = hashlist[7,i,hashval v]
    | hash(PURE(_,vl,_,_,_)) = hashlist(map hashval v)
    | hash _ = raise Hash

  type expmap = cexp list IntmapF.intmap
  fun add(m: expmap, e : cexp) =
    let val h = hash e
        val el = IntmapF.lookup m h  handle IntmapF.IntmapF => nil
     in IntmapF.add(m,h,e::el)
    end
  fun look(m: expmap, e: cexp) =
    let val h = hash e
        val el = IntmapF.lookup m h handle IntmapF.IntmapF => nil
        fun match(a::rest) = if same(e,a) then SOME a else match rest
          | match nil = NONE
     in match el
    end
  val empty = IntmapF.empty

  fun renval(VAR v) = VAR(ren v)
    | renval(LABEL v) = LABEL(ren v)
    | renval x = x

  fun renfield(v,p) = (renval v, p)

  fun elim (m:expmap, e: cexp) =
   case e
    of RECORD(k,vl,w,ce) => 
	  (case look(m,e)
            of SOME(RECORD(_,_,w',_)) => (enter(w,w'); elim(m,ce))
             | _ => RECORD(k,map renfield vl,w,elim(add(m,e),ce)))
     | SELECT(i,v,w,t,ce) =>
	   (case look(m,e)
             of SOME(SELECT(_,_,w',_,_)) => (enter(w,w'); elim(m,ce))
              | _ => SELECT(i,renval v, w, t, elim(m,ce)))
     | OFFSET(i,v,w,ce) =>
	   (case look(m,e)
             of SOME(OFFSET(_,_,w',_)) => (enter(w,w'); elim(m,ce))
              | _ => OFFSET(i,renval v, w, elim(m,ce)))
     | PURE(p,vl,w,t,ce) =>
	   (case look(m,e)
             of SOME(PURE(_,_,w',_,_)) => (enter(w,w'); elim(m,ce))
              | _ => PURE(p,map renval vl, w, elim(m,ce)))
     | APP(v,vl) => APP(renval v, map renval vl)
     | FIX(fundefs,ce) =>
	   let fun g(k,f,vl,tl,b)=>(k,f,vl,tl,elim(m,b))
            in FIX(map g fundefs, elim(m,ce))
           end
     | SWITCH(v,c,ce) => SWITCH(renval v, c, elim(m,ce))
     | BRANCH(p,vl,c,e1,e2) => BRANCH(p,map renval vl,c,elim(m,e1),elim(m,e2))
     | SETTER(p,vl,ce) => SETTER(p,map renval vl, elim(m,ce))
     | LOOKER(p,vl,w,t,ce) => LOOKER(p,map renval vl, w, t, elim(m,ce))
     | ARITH(p,vl,w,t,ce) => ARITH(p, map renval vl, w, t, elim(m,ce))

    val (k,f,vl,tl,b) = function
  in (k,f,vl,tl,elim(empty,b))
 end

(*
 * $Log: cse.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:30  george
 *   Version 109.24
 *
 *)
