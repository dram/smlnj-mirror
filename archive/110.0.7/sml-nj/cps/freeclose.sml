(* Copyright 1996 by Bell Laboratories *)
(* freeclose.sml *)

(*****************************************************************************
 *                                                                           *
 * freemapClose                                                              *
 *                                                                           *
 *    Produces a free variable mapping at each function binding.             *
 *    The mapping includes the functions bound at the FIX, but not the       *
 *    arguments of the function.                                             *
 *                                                                           *
 *    All function bindings that are never called are eliminated.            *
 *                                                                           *
 *****************************************************************************)

signature FREECLOSE =
  sig
    val freemapClose : CPS.function -> (CPS.function * (CPS.lvar -> 
                              (CPS.lvar list * CPS.lvar list option)))
  end

structure FreeClose : FREECLOSE = struct

  open Access CPS SortedList

(*****************************************************************************
 *  Misc and utility functions                                               *
 *****************************************************************************)
val say = Control.Print.say

fun addvL(v,NONE) = NONE
  | addvL(v,SOME l) = SOME(enter(v,l))

val enter = fn (VAR x,y) => enter(x,y) | (_,y) => y
val error = ErrorMsg.impossible

fun addL(v,NONE) = NONE
  | addL(v,SOME l) = SOME(enter(v,l))

fun overL(r,NONE) = NONE
  | overL(r,SOME l) = SOME (merge(r,l))

fun mergeL(NONE,r) = r
  | mergeL(l,NONE) = l
  | mergeL(SOME l,SOME r) = SOME (merge(l,r))

fun removeL(vl,NONE) = NONE
  | removeL(vl,SOME r) = SOME (remove(vl,r))

fun rmvL(v,NONE) = NONE 
  | rmvL(v,SOME r) = SOME (rmv(v,r))

fun clean l = 
  let fun vars(l, VAR x :: rest) = vars(x::l, rest)
	| vars(l, _::rest) = vars(l,rest)
	| vars(l, nil) = uniq l
   in vars(nil, l)
  end

fun filter p vl = 
  let fun f(x::r,l) = if p x then f(r,x::l) else f(r,l)
        | f([],l) = rev l
   in f(vl,[])
  end

fun unzip l = 
  let fun f((a,b)::r,x,y) = f(r,a::x,b::y)
        | f([],x,y) = (rev x,rev y)
   in f(l,[],[])
  end

fun exists pred l = 
  let fun f(a::r) = if pred a then true else f r
        | f [] = false
   in f l
  end

fun partition f l = 
  foldr (fn (e,(a,b)) => if f e then (e::a,b) else (a,e::b)) ([], []) l

val infinity = 1000000000
fun minl l = 
  let fun f(i,nil) = i 
        | f(i,j::r) = if i < j then f(i,r) else f(j,r)
   in f(infinity,l)
  end

fun freemapClose fe = let

(*****************************************************************************
 *  Fix the kind information for each function defintion                     * 
 *****************************************************************************)

val escapes = Intset.new()
val escapesP = Intset.mem escapes
fun escapesM(VAR v) = Intset.add escapes v
  | escapesM _ = ()

val known = Intset.new()
val knownP = Intset.mem known
val knownM = Intset.add known

val contset = Intset.new()
val contP = Intset.mem contset
val contM = Intset.add contset

fun fixkind(fe as (CONT,f,vl,cl,ce)) = 
       if escapesP f then (contM f; fe)
       else (knownM f; (KNOWN,f,vl,cl,ce))
  | fixkind(fe as (fk,f,vl,cl,ce)) = 
       if escapesP f then (ESCAPE,f,vl,cl,ce)
       else (knownM f; (KNOWN_REC,f,vl,cl,ce))

fun procfix(fk,f,vl,cl,ce) = (fk,f,vl,cl,proc ce)
and proc(ce) =
      case ce 
       of FIX(fl,body) =>
	   let val body' = proc body
               val nfl = map fixkind (map procfix fl)
               val (fl1,fl2) = partition (contP o #2) nfl 
            in case (fl1,fl2) 
                of ([],_) => FIX(fl2,body')
                 | (_,[]) => FIX(fl1,body')
                 | _ => FIX(fl2,FIX(fl1,body'))
	   end
        | APP(v,args) => (app escapesM args; ce)
	| SWITCH(v,c,l) => SWITCH(v,c,map proc l)
	| RECORD(rk,l,w,ce) => 
           (app (escapesM o #1) l; RECORD(rk,l,w,proc ce))
	| SELECT(i,v,w,t,ce) => SELECT(i,v,w,t,proc ce)
	| OFFSET(i,v,w,ce) => OFFSET(i,v,w,proc ce)
	| LOOKER(p,vl,w,t,ce) => 
           (app escapesM vl; LOOKER(p,vl,w,t,proc ce))
	| ARITH(p,vl,w,t,ce) => 
           (app escapesM vl; ARITH(p,vl,w,t,proc ce))
	| PURE(p,vl,w,t,ce) => 
           (app escapesM vl; PURE(p,vl,w,t,proc ce))
	| SETTER(p,vl,ce) => 
           (app escapesM vl; SETTER(p,vl,proc ce))
	| BRANCH(p,vl,c,e1,e2) =>
           (app escapesM vl; BRANCH(p,vl,c,proc e1,proc e2))

val fe' = procfix fe

(*****************************************************************************
 * Find out the strongly connected component information                     *
 *****************************************************************************)

exception Unseen
type info = {dfsnum : int ref, sccnum : int ref, edges : lvar list}
val m : info Intmap.intmap = Intmap.new(32,Unseen)
val lookup = Intmap.map m
val total : lvar list ref = ref nil

fun addinfo(f,vl) = (total := (f :: (!total));
                     Intmap.add m (f,{dfsnum=ref ~1,sccnum=ref ~1,edges=vl}))
fun KC x = (contP x) orelse (knownP x)
fun EC x = (contP x) orelse (escapesP x)

fun makenode (_,f,_,_,body) =
  let fun edges (RECORD(_,_,_,e)) = edges e
	| edges (SELECT(_,_,_,_,e)) = edges e
	| edges (OFFSET(_,_,_,e)) = edges e
	| edges (SWITCH(_,_,el)) = foldmerge (map edges el) 
	| edges (SETTER(_,_,e)) = edges e
	| edges (LOOKER(_,_,_,_,e)) = edges e
	| edges (ARITH(_,_,_,_,e)) = edges e
        | edges (PURE(_,_,_,_,e)) = edges e
	| edges (BRANCH(_,_,_,a,b)) = merge(edges a,edges b)
        | edges (APP(u, ul)) = filter KC (clean (u::ul))
	| edges (FIX(fl,b)) = (app makenode fl; edges b)
   in addinfo(f,edges body)
  end 

fun vp v = say(LambdaVar.lvarName(v))

val compnums = ref 0 and id = ref 0
val stack : (int * int ref) list ref = ref nil
fun scc nodenum =
  let fun newcomp(c,(n,sccnum)::rest) = 
	    (sccnum := c; 
(***
             say "*** "; vp n; say "  "; say (Int.toString(c)); say "\n"; 
***)
             if n=nodenum then rest else newcomp(c,rest))
        | newcomp _ = error "newcomp in freeclose in the closure phase"

      val info as {dfsnum as ref d, sccnum, edges} = lookup nodenum

   in if d >= 0 then if (!sccnum >= 0) then infinity else d 
      else (let val v = !id before (id := !id+1)
(***
                val _ = (say "###start of "; vp nodenum; say "  "; 
                         say (Int.toString(v)); say "\n")
***)
                val _ = (stack := (nodenum, sccnum) :: !stack;
                         dfsnum := v)
                val b = minl (map scc edges)
(***
                val _ = (say "###end of "; vp nodenum; say "  "; 
                         say (Int.toString(b)); say "\n")
***)
             in if v <= b 
                then let val c = !compnums before (compnums := !compnums+1)
                         val _ = (stack := newcomp(c,!stack))
                      in infinity (* v *)
                     end
                else b
            end)
  end

(*** Make the call graph ***)
val _ = makenode(fe')

(*** Get the scc information ***)
val _ = app (fn x => (scc x; ())) (!total)

val sccnum = ! o #sccnum o lookup
(***
fun plist p l = (app (fn v => (say " "; p v)) l; say "\n")
val ilist = plist vp
val _ = app (fn v => (vp v; say " edges : " ;
                      ilist(#edges(lookup v));
                      say "****   sccnum is   "; 
                      say (Int.toString(sccnum v)); say "\n")) (!total)
***)
fun samescc(x,n) = if n < 0 then false else ((sccnum x) = n)

(*****************************************************************************
 * Get all the free variable information and also do branch prediction       *
 *****************************************************************************)

exception FREEVMAP
val vars : (lvar list * (lvar list option)) Intmap.intmap 
                                               = Intmap.new(32, FREEVMAP)
val freeV = Intmap.map vars 
fun loopV v = (#2 (freeV v)) handle FREEVMAP => error "loopV in closure"

(*** split the not-really-recursive bindings, a temporary hack ***)
(*** need to add code on identify those KNOWN_REC kind functions ***)

fun knownOpt ([],_,_) = error "knownOpt in closure 4354"
  | knownOpt (flinfo,died,freeb) = 
      let val newflinfo = 
            let val roots = filter (member died) freeb
                val graph = map (fn ((_,f,_,_,_),free) => 
                                   (f,filter (member died) free)) flinfo
                
                fun loop(old) = 
                  let val new = 
                        foldr (fn ((f,free),total) => 
                           if member old f then merge(free,total) else total)
                        old graph
                   in if length(new) = length(old) then new else loop(new)
                  end

                val nroots = loop(roots)
             in filter (fn ((_,f,_,_,_),_) => member nroots f) flinfo
            end

          val (nfl,freel) =
            let val (known,other) = 
                       partition (fn ((KNOWN_REC,_,_,_,_),_) => true
                                   | _ => false) newflinfo
                val known' = 
                  case known 
                   of u as [((_,v,args,cl,body),free)] => 
                         (if member free v then u
                          else [((KNOWN,v,args,cl,body),free)])
                    | z => z
             in unzip(known'@other)
            end

       in case nfl of [] => (fn ce => ce, freel)
                    | _ => (fn ce => FIX(nfl,ce), freel)
      end

(***>> 
  (*** splitting the pseudo-mutually-recursive FIX bindings ***)
  (*** currently turned off ***)
  | knownOpt (flinfo,roots) = 
     (let val (bindings,freel) = unzip(flinfo)
          val fixV = uniq(map #2 bindings)
          val knownLvar = member (filter knownP fixV) 
          fun adjust_kind (fe as (KNOWN,v,args,cl,ce)) = 
                if (exists knownLvar (#1(freeV v))) 
                then (KNOWN_REC,v,args,cl,ce) else fe
            | adjust_kind fe = fe

          val bindings = map adjust_kind bindings
       in if (exists EC fixV) then (fn b => FIX(bindings,b))
          else 
           (let fun nearby f = 
                      let val free = filter knownLvar (#1 (freeV f))
                       in rmv(sccnum f,uniq (map sccnum free))
                      end
                fun gather((k,nl,fl),z as ((n,nb,l)::r)) = 
                        if n = k then ((n,merge(nl,nb),fl@l)::r)
                        else ((k,nl,fl)::z)
                  | gather(x,[]) = [x] 

                fun topsort([],hdr) = hdr
                  | topsort(l : (int * int list * function list) list,hdr) = 
                      let val (leaves,rest) = partition (null o #2) l
                          val (dies,newh) = 
                            foldr (fn ((n,_,fl),(l,h)) =>
                                  (n::l,fn b => h(FIX(fl,b)))) ([],hdr) leaves
                          val rest = map (fn (n,nl,fl) => 
                                                 (n,remove(dies,nl),fl)) rest
                       in topsort(rest,newh)
                      end


                val graph = Sort.sort (fn ((x : int,_,_),(y,_,_)) => (x < y)) 
                               (map (fn (fe as (_,f,_,_,_)) => 
                                         (sccnum f,nearby f,[fe])) bindings)

             in topsort(foldr gather [] graph, fn x => x)
            end)
      end)
<<***)


(*** Find out free variale info and do branch-prediction transformation ***)
fun freefix(fk,f,vl,cl,ce) =
     let val (ce',ul,wl,etag) = if KC f then freevars(sccnum f,ce)
                               else freevars(~1,ce)
         val args = uniq vl
         val l = remove(args,ul)
         val z = removeL(args,wl)
         val nz = if (knownP f andalso etag) then overL(l,z) else z
         val _ = Intmap.add vars (f,(l,nz))
      in ((fk,f,vl,cl,ce'),l)
     end

and freevars(n,ce) =
  case ce 
   of FIX(fl,body) =>
       let val died = uniq(map #2 fl)  
           val flinfo = map freefix fl
           val (body',freeb,wl,_) = freevars(n,body)
           val (header,freel) = knownOpt(flinfo,died,freeb)

           val free = remove(died,merge(freeb,foldmerge freel))
           fun h(x,l) = if member died x then mergeL(loopV x,l) 
                        else addvL(x,l) 
           val nwl = case wl of NONE => NONE
                              | SOME l => foldr h (SOME []) l
           val nwl = removeL(died,nwl)

        in (header(body'),free,nwl,false)
       end
    | APP(v,args) => 
       let val free = clean(v::args)
           val fns = filter KC free
           val wl = if (exists (fn x => samescc(x,n)) fns) then SOME free
                    else NONE
        in (ce,free,wl,true)
       end
    | SWITCH(v,c,l) => 
       let fun freelist(ce,(el,free,wl,etag)) =
             let val (ce',free',wl',etag') = freevars(n,ce)
                 val newetag = case wl' of NONE => etag
                                         | SOME _ => (etag' andalso etag)
              in (ce'::el,merge(free',free),mergeL(wl',wl),newetag)
             end
           val (l',freel,wl,etag) = foldr freelist ([],[],NONE,true) l
        in (SWITCH(v,c,l'),enter(v,freel),addL(v,wl),etag)
       end
    | RECORD(rk,l,w,ce) => 
       let val (ce',free,wl,etag) = freevars(n,ce)
           val new = clean (map #1 l)   
           val free' = merge(new, rmv(w,free))
           val wl' = overL(new, rmvL(w,wl))
        in (RECORD(rk,l,w,ce'),free',wl',etag)
       end
    | SELECT(i,v,w,t,ce) =>
       let val (ce',free,wl,etag) = freevars(n,ce)
        in (SELECT(i,v,w,t,ce'),enter(v,rmv(w,free)),addL(v,rmvL(w,wl)),etag)
       end
    | OFFSET(i,v,w,ce) =>
       let val (ce',free,wl,etag) = freevars(n,ce)
        in (OFFSET(i,v,w,ce'),enter(v,rmv(w,free)),addL(v,rmvL(w,wl)),etag)
       end 
    | LOOKER(p,vl,w,t,ce) => 
       let val (ce',free,wl,etag) = freevars(n,ce)
           val new = clean vl
        in (LOOKER(p,vl,w,t,ce'),merge(new,rmv(w,free)),
            overL(new,rmvL(w,wl)),etag)
       end
    | ARITH(p,vl,w,t,ce) => 
       let val (ce',free,wl,etag) = freevars(n,ce)
           val new = clean vl
        in (ARITH(p,vl,w,t,ce'),merge(new,rmv(w,free)),
            overL(new,rmvL(w,wl)),etag)
       end
    | PURE(p,vl,w,t,ce) => 
       let val (ce',free,wl,etag) = freevars(n,ce)
           val new = clean vl
        in (PURE(p,vl,w,t,ce'),merge(new,rmv(w,free)),
            overL(new,rmvL(w,wl)),etag)
       end
    | SETTER(p,vl,ce) => 
       let val (ce',free,wl,etag) = freevars(n,ce)
           val new = clean vl
        in (SETTER(p,vl,ce'),merge(new,free),overL(new,wl),etag)
       end
    | BRANCH(p,vl,c,e1,e2) =>
       let val (e1',free1,wl1,etag1) = freevars(n,e1)
           val (e2',free2,wl2,etag2) = freevars(n,e2)
           val new = clean vl
           val free = merge(new,merge(free1,free2))
           val wl = overL(new,mergeL(wl1,wl2))
        in case (wl1,wl2)
            of (NONE,SOME _) => (BRANCH(P.opp p,vl,c,e2',e1'),free,wl,etag2)
             | (SOME _,NONE) => (BRANCH(p,vl,c,e1',e2'),free,wl,etag1)
             | (SOME _,SOME _) => 
                   (BRANCH(p,vl,c,e1',e2'),free,wl,etag1 andalso etag2)
             | _ => (BRANCH(p,vl,c,e1',e2'),free,wl,true)
       end

 in (#1(freefix fe'), freeV)
end (* function freemapClose *)

val freemapClose = Stats.doPhase(Stats.makePhase "Compiler 079 freemapClose")
                     freemapClose

end (* structure FreeClose *)


(*
 * $Log: freeclose.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:31  george
 *   Version 109.24
 *
 *)
