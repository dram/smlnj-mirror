(* Copyright 1996 by AT&T Bell Laboratories *)
(* interp.sml *)

(* 
 * This file is not currently installed. It is not up to date
 * to the changes in other parts of the compiler.  (ZHONG)
 *)

signature INTERP = 
sig
exception NotImplemented
val interp : Lambda.lexp -> 'a 
end 

structure Interp : INTERP = 
struct

exception NotImplemented
fun interp lexp = raise NotImplemented

(*

open Array Types Lambda 
structure P = PrimOp
infix 9 sub
structure U = System.Unsafe

val cast = U.cast

fun err s = ErrorMsg.impossible ("Interp: "^s)

datatype 'a env = BIND of 'a env * lvar * 'a

val MTENV : 'a env = cast()

fun realc s =
  let val (sign,s) = (case explode s
	    of (#"~" :: rest) => (~1.0,rest)
	     | s => (1.0,s)
	  (* end case *))
      fun j(exp,d::dl,mant) = j(exp,dl,mant * 0.1 + real(d))
        | j(0,nil,mant) = mant*sign
        | j(exp,nil,mant) = if exp>0 then j(exp-1,nil,mant*10.0)
				     else j(exp+1,nil,mant*0.1)
      fun h(esign,wholedigits,diglist,exp,nil) = 
			j(esign*exp+wholedigits-1,diglist,0.0)
        | h(es,fd,dl,exp,d::s) = h(es,fd,dl,exp*10+(Char.ord d - Char.ord #"0"),s)
      fun g(i,r, #"E" :: #"~" :: s)=h(~1,i,r,0,s)
	| g(i,r, #"E" :: s)=h(1,i,r,0,s)
	| g(i,r,d :: s) = g(i, (Char.ord d - Char.ord #"0")::r, s)
	| g(i,r,nil) = h(1,i,r,0,nil)
      fun f(i,r, #"." :: s)=g(i,r,s)
        | f(i,r,s as #"E" :: _)=g(i,r,s)
        | f(i,r,d :: s) = f(i+1,(Char.ord(d)-Char.ord #"0")::r,s)
   in f(0,nil,s)
  end

fun look v =
 let fun f(BIND(e,w,x)) = if v=w then x else f e
  in f
 end

val upd = BIND

val rec M = 
   fn APP(FN(v,_,b),a) => let val a' = M a and b' = M b
                           in fn r => cast(b' (upd(r,v, cast (a' r))))
                          end
    | APP(a,b) => cast let val a' = cast(M a) and b' = M b
                   in fn r => cast((a' r) (b' r))
                  end
    | FN(v,_,b) => let val b' = M b
                    in fn r => cast (fn x =>  b' (upd(r,v,x)))
                   end
    | DECON((_, rep, _, _),b) =>
        (case rep
          of UNTAGGED => let val b' = cast(M b)
	                  in fn r => let val {x} = (cast(b' r))
	        	              in x
				     end
			 end
           | TAGGED _ => let val b' = cast(M b)
                          in fn r => cast(U.subscript(b' r, 1))
		         end
(* ZHONG?
           | TAGGEDREC(_,1) =>
               let val b' = M b
  	        in fn r => let val (i,x) = cast(b' r)
	    	            in cast{x=x} 
  			   end
  	       end
           | TAGGEDREC(_,2) => 
               let val b' = M b
 	        in fn r => let val (i,x,y) = cast(b' r)
	                    in cast(x,y)
			   end
	       end
           | TAGGEDREC(_,3) =>
               let val b' = M b
	        in fn r => let val (i,x,y,z) = cast(b' r)
			    in cast(x,y,z)
	        	   end
	       end
           | TAGGEDREC(_,4) => 
               let val b' = M b
  	        in fn r => let val (i,w,x,y,z) = cast(b' r)
		 	    in cast(w,x,y,z)
			   end
	       end
           | TAGGEDREC(_,5) => 
               let val b' = M b
	        in fn r => let val (i,v,w,x,y,z) = cast(b' r)
			    in cast(v,w,x,y,z)
			   end
	       end
           | TAGGEDREC(_,j) => 
               let val b' = M b
  	           fun f(z,n) = if n=j then nil
		                else U.subscript(z,j+1)::f(z,n+1)
	        in fn r => cast(Vector.fromList(f(cast(b' r),0)))
   	       end
           | UNTAGGEDREC _ => M b
*)
           | EXNFUN _ => let val b' = cast(M b)
	                  in fn r => cast(U.subscript(b' r, 1))
	                 end
           | TRANSPARENT => M b
           | LISTNIL => (fn r => cast 0)
           | LISTCONS => M b
           | _ => err "interp on DECON")

    | CON((_, rep, _, _), b) => 
        (case rep 
          of UNTAGGED => let val b' = M b
	                  in fn r => cast {a= b' r}
			 end
           | CONSTANT i => (fn r => cast i)
           | LISTNIL => (fn r => cast 0)
           | LISTCONS => M b
           | TAGGED i => let val b' = M b
	                        in fn r => cast (i, b' r)
			       end
           | EXNFUN z => 
               (*  (case z of SOME p =>  ZHONG? *)
	       let val b' = M b
		   fun g (LVAR v) = VAR v
		     | g (PATH(i,r)) = SELECT(i, g r)
		   val gp = M(g z)
	        in fn r => cast (gp r, b' r)
	       end
               (* | NONE => err "EXNFUN has no access path") *)

           | EXNCONST z =>
               (* (case z of SOME p => ZHONG? *)
	       let fun g (LVAR v) = VAR v
		     | g (PATH(i,r)) = SELECT(i, g r)
	        in M(g z)
	       end
               (* | NONE => err "EXNCONST has no access path") *)
           | TRANSPARENT => M b
(* ZHONG?
           | TAGGEDREC(i,1) => 
                let val b' = M b
     	         in fn r => let val {1=x} = cast(b' r)
       		             in cast(i,x)
		            end        
                end
           | TAGGEDREC(i,2) => 
                let val b' = M b
                 in fn r => let val (x,y) = cast(b' r)
			     in cast(i,x,y)
			    end         
                end
           | TAGGEDREC(i,3) => 
                let val b' = M b
   	         in fn r => let val (x,y,z) = cast(b' r)
			     in cast(i,x,y,z)
			    end        
                end
           | TAGGEDREC(i,4) => 
                let val b' = M b
  	         in fn r => let val (w,x,y,z) = cast(b' r)
  			     in cast(i,w,x,y,z)
			    end         
                end
           | TAGGEDREC(i,5) => 
                let val b' = M b
	         in fn r => let val (v,w,x,y,z) = cast(b' r)
	    	             in cast(i,v,w,x,y,z)
			    end         
                end
           | TAGGEDREC(i,j) => 
                let val b' = M b
	            fun f(z,n) = if n=j then nil
			         else U.subscript(z,n)::f(z,n+1)
	         in fn r => cast(Vector.fromList(i::f(cast(b' r),0)))
	        end
           | UNTAGGEDREC _ => M b
*)
           | _ => err "interp on CON")

    | SELECT(i,a) => let val a' = cast(M a) 
		      in fn r => cast(U.subscript(a' r, i))
		     end
    | RECORD [] => (fn r => cast())
    | RECORD [a,b] => let val a' = M a and b' = M b
		       in fn r => cast (a' r, b' r)
		      end
    | RECORD [a,b,c] => let val a' = M a and b' = M b and c' = M c
		         in fn r => cast (a' r, b' r, c' r)
		        end
    | RECORD [a,b,c,d] => let val a' = M a and b' = M b
			      and c' = M c and d' = M d
		           in fn r => cast (a' r, b' r, c' r, d' r)
		          end
    | RECORD [a,b,c,d,e] => let val a' = M a and b' = M b
			        and c' = M c and d' = M d
			        and e' = M e
		             in fn r => cast (a' r, b' r, c' r, d' r, e' r)
		            end
    | RECORD [a,b,c,d,e,f] => let val a' = M a and b' = M b
			          and c' = M c and d' = M d
			          and e' = M e and f' = M f
		               in fn r => cast (a' r, b' r, c' r, d' r, 
                                                e' r, f' r)
		              end
    | RECORD l => let val l' = map M l
		   in fn r => cast(Vector.fromList(map (fn x => x r) l'))
		  end
    | SRECORD [] => (fn r => cast())
    | SRECORD l => let val l' = map M l
                    in fn r => cast(Vector.fromList(map (fn x => x r) l'))
                   end
    | VECTOR [] => (fn r => cast())
    | VECTOR [a,b] => let val a' = M a and b' = M b
		       in fn r => cast(Vector.fromList[a' r, b' r])
		      end
    | VECTOR [a,b,c] => let val a' = M a and b' = M b and c' = M c
		         in fn r => cast (a' r, b' r, c' r)
		        end
    | VECTOR [a,b,c,d] => let val a' = M a and b' = M b
			      and c' = M c and d' = M d
		           in fn r => cast (a' r, b' r, c' r, d' r)
		          end
    | VECTOR [a,b,c,d,e] => let val a' = M a and b' = M b
			        and c' = M c and d' = M d
			        and e' = M e
		             in fn r => cast (a' r, b' r, c' r, d' r, e' r)
		            end
    | VECTOR [a,b,c,d,e,f] => let val a' = M a and b' = M b
			          and c' = M c and d' = M d
			          and e' = M e and f' = M f
		               in fn r => cast (a' r, b' r, c' r, d' r, 
                                                e' r, f' r)
		              end
    | VECTOR l => let val l' = map M l
		   in fn r => cast(Vector.fromList(map (fn x => x r) l'))
		  end

    | INT i => (fn r => cast i)
    | STRING s => (fn r => cast s)
    | REAL s => let val x = realc s in fn r => cast x end
    | PRIM(P.CAST,_) => (fn r => cast(fn x => x))
    | PRIM(P.ARITH{oper=P.+,overflow=true,kind=P.INT 31},_) => (fn r => cast Int.+)  
    | PRIM(P.ARITH{oper=P.-,overflow=true,kind=P.INT 31},_) => (fn r => cast Int.-)
    | PRIM(P.ARITH{oper=P.*,overflow=true,kind=P.INT 31},_) => (fn r => cast Int.* )
    | PRIM(P.ARITH{oper=P./,overflow=true,kind=P.INT 31},_) => (fn r => cast Int.div)
    | PRIM(P.ARITH{oper=P.ORB,kind=P.INT 31,...},_) => (fn r => cast Word.orb)
    | PRIM(P.ARITH{oper=P.ANDB,kind=P.INT 31,...},_) => (fn r => cast Word.andb)
    | PRIM(P.ARITH{oper=P.XORB,kind=P.INT 31,...},_) => (fn r => cast Word.xorb)
    | PRIM(P.ARITH{oper=P.NOTB,kind=P.INT 31,...},_) => (fn r => cast Word.notb)
    | PRIM(P.ARITH{oper=P.RSHIFT,kind=P.INT 31,...},_) => (fn r => cast Word.~>>)
    | PRIM(P.ARITH{oper=P.LSHIFT,overflow=false,kind=P.INT 31},_) => (fn r => cast Word.<<)
    | PRIM(P.DEREF,_) => (fn r => cast !)
    | PRIM(P.MAKEREF,_) => (fn r => cast ref)
    | PRIM(P.ARITH{oper=P.~,overflow=true,kind=P.INT 31},_) => (fn r => cast Int.~)
    | PRIM(P.CMP{oper=P.EQL,kind=P.INT 31},_) => (fn r => cast (fn(a:int,b) => a=b))
    | PRIM(P.CMP{oper=P.NEQ,kind=P.INT 31},_) => (fn r => cast (fn(a:int,b) => a<>b))
    | PRIM(P.CMP{oper=P.>,kind=P.INT 31},_) => (fn r => cast Int.>)
    | PRIM(P.CMP{oper=P.<,kind=P.INT 31},_) => (fn r => cast Int.<)
    | PRIM(P.CMP{oper=P.>=,kind=P.INT 31},_) => (fn r => cast Int.>=)
    | PRIM(P.CMP{oper=P.<=,kind=P.INT 31},_) => (fn r => cast Int.<=)
    | PRIM(P.CMP{oper=P.LTU,kind=P.INT 31},_) =>  (* this is buggy if b<0 *)
	(fn r => cast (fn (a, b) => ((0 <= a) andalso (a < b))))
    | PRIM(P.CMP{oper=P.GEU,kind=P.INT 31},_) => (* this is buggy if b<0 *)
	(fn r => cast (fn (a, b) => ((0 > a) orelse (a >= b))))
    | PRIM(P.SUBSCRIPT,_) => (fn r => cast U.subscript)
    | PRIM(P.UPDATE,_) => (fn r => cast U.update)
    | PRIM(P.BOXEDUPDATE,_) => (fn r => cast U.update)
    | PRIM(P.UNBOXEDUPDATE,_) => (fn r => cast U.update)
    | PRIM(P.SUBSCRIPTV,_) => (fn r => cast U.subscriptv)
    | PRIM(P.NUMSUBSCRIPT{kind=P.FLOAT 64,checked=false,immutable=false},_) => (fn r => cast U.subscriptf)
    | PRIM(P.NUMUPDATE{kind=P.FLOAT 64,checked=false},_) => (fn r => cast U.updatef)
    | PRIM(P.LENGTH,_) => (fn r => cast Array.length)
    | PRIM(P.OBJLENGTH,_) => (fn r => cast U.objLength)
    | PRIM(P.NUMUPDATE{kind=P.INT 8,checked=false},_) => (fn r => cast U.store)
    | PRIM(P.NUMSUBSCRIPT{kind=P.INT 8,checked=false,immutable=true},_) => (fn r => cast U.ordof)
    | PRIM(P.ARITH{oper=P.+,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real.+)
    | PRIM(P.ARITH{oper=P./,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real./)
    | PRIM(P.ARITH{oper=P.*,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real.* )
    | PRIM(P.ARITH{oper=P.-,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real.-)
    | PRIM(P.ARITH{oper=P.~,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real.~)
    | PRIM(P.ARITH{oper=P.ABS,overflow=true,kind=P.FLOAT 64,...},_) => (fn r => cast Real.abs)
    | PRIM(P.REAL{fromkind=P.INT 31,tokind=P.FLOAT 64},_) => (fn r => cast Real.real)
    | PRIM(P.CMP{oper=P.EQL,kind=P.FLOAT 64},_) => (fn r => cast (fn(a:real,b) => a=b))
    | PRIM(P.CMP{oper=P.NEQ,kind=P.FLOAT 64},_) => (fn r => cast (fn(a:real,b) => a<>b))
    | PRIM(P.CMP{oper=P.>,kind=P.FLOAT 64},_) => (fn r => cast Real.>)
    | PRIM(P.CMP{oper=P.>=,kind=P.FLOAT 64},_) => (fn r => cast Real.>=)
    | PRIM(P.CMP{oper=P.<=,kind=P.FLOAT 64},_) => (fn r => cast Real.<=)
    | PRIM(P.CMP{oper=P.<,kind=P.FLOAT 64},_) => (fn r => cast Real.<)
    | PRIM(P.BOXED,_) => (fn r => cast U.boxed)
    | PRIM(P.UNBOXED,_) => (fn r => cast (not o U.boxed))
    | PRIM(P.CALLCC,_) => (fn r => cast SMLofNJ.Cont.callcc)
(*    | PRIM(P.CAPTURE,_) => (fn r => cast U.PolyCont.capture)  ZHONG? obsolete? *)
    | PRIM(P.THROW,_) => (fn r => cast SMLofNJ.Cont.throw)
    | PRIM(P.GETVAR,_) => (fn r => cast U.getvar)
    | PRIM(P.GETTAG,_) => (fn r => cast U.getObjTag)
    | PRIM(P.MKSPECIAL,_) => (fn r => cast U.special)
    | PRIM(P.SETSPECIAL,_) => (fn r => cast U.setSpecial)
    | PRIM(P.GETSPECIAL,_) => (fn r => cast U.getSpecial)
    | PRIM(P.SETVAR,_) => (fn r => cast U.setvar)
    | PRIM(P.SETPSEUDO,_) => (fn r => cast U.setpseudo)
    | PRIM(P.GETPSEUDO,_) => (fn r => cast U.getpseudo)
    | PRIM(P.SETMARK,_) => (fn r => cast U.setmark)
    | PRIM(P.DISPOSE,_) => (fn r => cast U.dispose)
    | PRIM(P.GETHDLR,_) => (fn r => cast U.gethdlr)
    | PRIM(P.SETHDLR,_) => (fn r => cast U.sethdlr)
    | PRIM(P.MARKEXN,_) => (fn r => cast(fn es =>
			    let val (exn: exn, s: string) = cast es
				val (e,v,l: string list) = cast exn
			       in cast(e,v,s::l) end))
    | PRIM(p,_) => err(concat["bad primop ", 
                              P.prPrimop p, " in interp"])
    | EXNF(a,t) => let val a' = cast (M a) 
                    in fn r => ((cast ref) (a' r))
                   end
    | EXNC a => 
       let val a' = cast (M a)
        in fn r => cast (Vector.fromList [((cast ref) (a' r)), cast ()])
       end
    | VAR v => look v
    | HANDLE(a,b) => let val a' = cast (M a) and b' = cast(M b)
                      in fn r => (a' r handle e => b' r e)
                     end
    | RAISE(a,_) => let val a' = cast (M a) in fn r => raise(a' r) end
    | FIX(nl,_,fl,b) => 
         let fun g(n::nl,f::fl) = let val f' = M f
			              val fl' = g(nl,fl)
                                   in fn rr => cast (upd(fl' rr,n, 
						fn x => cast(f'(!rr)) x))
                                  end
                | g(nil,_) = cast(fn rr => !rr)
             val l = g(nl,fl)
             val b' = cast(M b)
          in fn r => cast (let val rr = ref (cast r)
		            in rr := l (cast rr); b'(!rr)
                           end)
         end
   | SWITCH(e, _, l as (DATAcon(_, EXNFUN _, _, _), _)::_, SOME d) => 
                exnswitch(e,l,d)
   | SWITCH(e, _, l as (DATAcon(_, EXNCONST _, _, _), _)::_, SOME d) => 
                exnswitch(e,l,d)
   | SWITCH(e, _, l as (REALcon _, _)::_, SOME d) =>
       let fun trans(REALcon i, a)= (realc i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast (let val e'':real = cast(e' r)
   		             fun find ((i, answer)::rest) =
			           if i=e'' then answer r else find rest
	                       | find [] = d' r
		          in find cases
		         end)
       end
   | SWITCH(e,_,l as (INTcon _, _)::_, SOME d) =>
       let fun trans(INTcon i, a)= (i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast (let val e'':int = cast(e' r)
		             fun find ((i, answer)::rest) =
			           if i=e'' then answer r else find rest
	                       | find nil = d' r
      		          in find cases
		         end)
       end

   | SWITCH(e,_,l as (INT32con _, _)::_, SOME d) =>
       let fun trans(INT32con i, a)= (i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast (let val e'':Int32.int = cast(e' r)
		             fun find ((i, answer)::rest) =
			           if i=e'' then answer r else find rest
	                       | find nil = d' r
      		          in find cases
		         end)
       end

   | SWITCH(e,_,l as (WORDcon _, _)::_, SOME d) =>
       let fun trans(WORDcon i, a)= (i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast (let val e'':word = cast(e' r)
		             fun find ((i, answer)::rest) =
			           if i=e'' then answer r else find rest
	                       | find nil = d' r
		          in find cases
		         end)
       end
   | SWITCH(e,_,l as (WORD32con _, _)::_, SOME d) =>
       let fun trans(WORD32con i, a)= (i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast (let val e'':Word32.word = cast(e' r)
		    fun find ((i, answer)::rest) =
			 if i=e'' then answer r else find rest
	              | find nil = d' r
		 in find cases
		end)
       end
   | SWITCH(e,_,l as (STRINGcon _, _)::_, SOME d) =>
       let fun trans(STRINGcon i, a)= (i, M a)
           val cases = map trans l and d' = M d and e' = M e
        in fn r => cast(let val e'':string = cast(e' r)
		    fun find ((i, answer)::rest) =
			 if i=e'' then answer r else find rest
	              | find nil = d' r
		 in find cases
		end)
       end
   | SWITCH(e,_, l as (DATAcon _, _)::_, d) =>
       let val d' = case d of SOME d0 => M d0
                            | NONE => fn r =>  err "no default in interp"
           val e' = M e
           fun f((DATAcon(_, rep, _, _), ans) :: rest) = 
                (case rep
                  of CONSTANT i => 
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => if x=i then ans' else rest' x
		       end
                   | TAGGED i => 
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => 
                              if U.boxed x andalso U.subscript(cast x,0)=i 
			      then ans' else rest' x
		       end
(* ZHONG?
                   | TAGGEDREC(i,_) =>
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => 
                              if U.boxed x andalso U.subscript(cast x,0)=i 
			      then ans' else rest' x
		       end
*)
                   | UNTAGGED =>
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => if U.boxed x then ans' else rest' x
		       end
(* ZHONG?
                   | UNTAGGEDREC _ =>
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => if U.boxed x then ans' else rest' x
		       end
*)
                   | TRANSPARENT =>
		       let val rest' = f rest
		           val ans' = M ans
	                in fn x => if U.boxed x
				then ans' else rest' x
		       end
                   | _ => err "unexpected conrep for SWITCH")
(* ZHONG? -- missing cases *)
             | f (_::rest) = err "unexpected access for SWITCH"
             | f nil = fn x => d'
  	   val cases = f l
        in fn r => cases(e' r) r
       end
   | WRAP(t,e) => M e
   | UNWRAP(t,e) => M e
   | _ => err "bad lexp in interp"

 and exnswitch = fn (e,l,d) =>
     let fun rev'(PATH(i,p),l) = rev'(p,i::l)
           | rev'(LVAR v, l) = v::l

         fun trans(DATAcon(_, EXNCONST p, _, _), a) =
		(rev'(PATH(0,p),nil), M a)
           | trans(DATAcon(_, EXNFUN p, _, _), a) =
		(rev'(p,nil), M a)
           | trans _ = err "unexpected conrep in exnswitch"

         val cases = map trans l and d' = M d and e' = M e

      in fn r => cast(let val e'' : int = U.subscript(cast(e' r),0)
		          fun select(x,i::rest) = 
                                select(U.subscript(cast x,i),rest)
		            | select(x,nil) = cast x
		          
                          fun find ((v::path, answer)::rest) =
			        if select(look v r,path)=e'' then answer r
		                else find rest
	                    | find nil = d' r
   		       in find cases
		      end)
     end

 fun interp lexp = cast(M lexp MTENV)

*)
end (* structure Interp *)

(*
 * $Log: interp.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:48  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.2  1997/03/03 17:10:46  george
 * moved callcc related functions to SMLofNJ.Cont
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
