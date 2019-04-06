signature HISTORYSTORE = sig
  datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist
  val updatedAList: (System.Unsafe.object array Weak.weak,int) plist ref
  val updatedRList: System.Unsafe.object array Weak.weak list ref
  val createdList: System.Unsafe.object array Weak.weak list ref
  val hcreatea: ((int * System.Unsafe.object) -> (System.Unsafe.object array))
  val hcreater: System.Unsafe.object -> (System.Unsafe.object array)
  type storehandle
  val remember: (unit -> storehandle)
  val restore: (storehandle -> unit)
  val reset: (unit -> unit)
  val zap: (int -> unit)
  val activate:  bool ref
end

(*  The following are intended usages for making and updating historical
	refs and arrays:

fun href (obj) = hcreater (obj)

fun harray(cnt,obj) = hcreatea(cnt,obj)

fun hass (objr, value) = 
        (objr := value;
	 updatedRList := (weak objr) :: (!updatedRList))

fun hupd (arr, offset, value) =
	(Array.update (arr, offset, value);
	 updatedAList := PCONS(weak arr, offset,!updatedAList))

*)

abstraction HistoryStore : HISTORYSTORE =
struct

open Weak System.Unsafe DebugUtil DebugKernel

(* for inlining *)
val weak_desc = System.Tags.desc_weak div 2

val activate = ref true

datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist

val updatedAList = ref (PNIL: (object array weak,int) plist)
val updatedRList = ref (nil:object array weak list)
val createdList = ref (nil:object array weak list)

(* reference hash table stuff *)

local 
  datatype hlink =  NILhlink
	          | HLINK of int * object array weak * hlink
  val initialsize = 32
  val size = ref initialsize
  val size1 = ref (initialsize-1)
  val table = ref (array(initialsize, NILhlink))
  val count = ref 0
  fun growtable() =
    let val oldsize = !size
	val newsize = oldsize + oldsize
	val newsize1 = newsize - 1
        val newtable = array(newsize,NILhlink)
        fun bucket n = 
	     let fun add'(a,b,HLINK(time,wobj,nlink)) =
		      (case (strong wobj) of
			 NONE => (dec count; add'(a,b,nlink))
		       | SOME obj => if Bits.andb(time,newsize1) = n then
				       add'(HLINK(time,wobj,a),b,nlink)
				     else add'(a,HLINK(time,wobj,b),nlink))
		   | add'(a,b,NILhlink) =
		      ((*unsafe*)update(newtable,n,a);
		       (*unsafe*)update(newtable,n+oldsize,b);
		       bucket(n+1))
             in add'(NILhlink,NILhlink,!table sub n)
	     end
    in bucket 0 handle Subscript => ();
       table := newtable;
       size := newsize;
       size1 := newsize1
    end

  fun searchchain time =
    let fun f (HLINK(time',wobj,nlink)) =
		if time = time' then strong wobj
		else f nlink
	  | f NILhlink = NONE
    in f 
    end

  fun cleanchain ch =
    let fun f (HLINK(time,wobj,nlink),acc) =
		     (case (strong wobj) of
		        NONE => (count := !count - 1; f(nlink,acc))
		      | SOME _ => f(nlink,HLINK(time,wobj,acc)))
          | f (NILhlink,acc) = acc
    in f (ch,NILhlink)
    end
in
  fun hcreatea(cnt,value) =
    let val time = (*unsafe*) subscript(times,0)
	fun reinit obj = 
	  let fun d (~1) = ()
	  	| d n = ((*unsafe*)update(obj,n,value); d (n-1)) 
	  in d (cnt-1)
	  end
        fun dochain (link as HLINK(time',wobj,nlink)) =
		(case (strong wobj) of 
		   NONE => (dec count;dochain nlink)
		 | SOME obj => if time = time' 
			       then (reinit obj;
		    		     (obj, link))
			       else let val (obj', nlink') = dochain nlink
				    in (obj', HLINK(time',wobj,nlink'))
				    end)
	  | dochain (NILhlink) =
		let val newobj = array(cnt,value)
		in inc count;
		   (newobj, HLINK(time, weak newobj, NILhlink))
		end
        fun insert() = 
          let val addr = Bits.andb(time,!size1)
              val (r,ch) = dochain ((*unsafe*) subscript(!table,addr))
          in (*unsafe*)update (!table,addr,ch);
	     if (cnt > 0) then
	       createdList := weak r :: (!createdList)
	     else ();
	     r
	  end
    in if (!count) >= (!size) then growtable() else ();
       insert()
    end

  fun hcreater(value) =
    let val time = (*unsafe*) subscript(times,0)
        val addr = Bits.andb(time,!size1)
	val ch = (*unsafe*) subscript(!table,addr)
	val obj = 
              case (searchchain time ch) of
	        SOME obj => ((*unsafe*) update(obj,0,value); (* reinitialize *)
			    (* (*unsafe*) update(!table,addr,cleanchain ch); *)
			     obj)
	      | NONE => let val obj = array(1,value)
			in count := !count + 1;
			   (*unsafe*) update (!table,addr,
					   HLINK(time,weak obj,cleanchain ch));
			   if (!count) > (!size) then growtable() else ();
			   obj
			end
    in createdList := weak obj :: (!createdList);
       obj
    end

  fun hzap t = (* remove all entries with times >= t *)
     let fun dochain (HLINK(time,wobj,nlink)) = 
	       (case (strong wobj) of 
	          NONE => (dec count; dochain nlink)
	        | SOME obj => if time >= t 
			      then (dec count; dochain nlink)
			      else HLINK(time,wobj,dochain nlink))
           | dochain (NILhlink) = NILhlink
         fun dobucket n = ((*unsafe*)update(!table,n,dochain(!table sub n));
			   dobucket (n+1)) handle Subscript => ()
     in dobucket 0
     end

  fun hreset() = (size := initialsize;
		  size1 := initialsize - 1;
		  table := array(initialsize, NILhlink))
end	

fun reset() = (updatedAList := PNIL;
	       updatedRList := nil;
	       createdList := nil;
	       hreset())

fun zap t = hzap t

datatype storecreation = STORECREATION of object array * object array
				* storecreation | NILstorecreation
datatype storeupdate = STOREUPDATE of object array * int  * object 
				* storeupdate | NILstoreupdate
datatype storedelta = STOREDELTA of storecreation list * storeupdate
				    * storedelta * int ref * storehandle weak
		    | NILstoredelta
and	 storehandle = STOREHANDLE of storedelta ref

val lastsd = ref NILstoredelta

val unique0 = ref () (* unique values *)
val unique1 = ref () 

(* some array utilities *)

fun copyarray olda = 
  let val newa = array (Array.length olda, (*unsafe*) subscript(olda,0))
      fun d n = ((*unsafe*)update(newa,n,olda sub n); d (n+1))
  in (d 1) handle Subscript => newa
  end

fun resetarray target source =
  let fun d n = ((*unsafe*)update(target,n, source sub n); d (n+1))
  in (d 0) handle Subscript => ()
  end



fun remember () =
  let val h = STOREHANDLE(ref NILstoredelta)
      val _ = forcegc() 
      (*val _ = (print "remembering..."; flush_out std_out)  *)
      fun docl (warr::next,acc) =
(*	    (case (strong warr) of
	       SOME a => let val a' = copyarray a
			 in (*unsafe*) update (a,0,cast unique0);
			    docl (next,STORECREATION(a,a',acc))
			 end
	     | NONE => docl (next,acc))
inlined version follows : *)
	    (case (cast(System.Unsafe.subscript(cast warr,0)),
		   cast(System.Unsafe.subscript(cast warr,~1))) of
	       (a,weak_desc) => let val a' = copyarray a
			 in (*unsafe*) update (a,0,cast unique0);
			    docl (next,STORECREATION(a,a',acc))
			 end
	     | (_,tag) => ((*print "weak:"; print tag;*)
			   docl (next,acc)))
	| docl (nil,acc) = acc
(*      val _ = (print "cl original = "; print (length (!createdList));print "\n") *)
      val cl = if !activate then 
		 docl (!createdList,NILstorecreation)
	       else NILstorecreation
      fun sizecl (STORECREATION (_,_,next)) = 1 + (sizecl next)
	| sizecl (NILstorecreation) = 0
(*      val _ = (print "condensed to "; print (sizecl cl); print "\n") *)
      fun douAl (PCONS(warr,i,next),acc) =
(*	    (case (strong warr) of
	       SOME a => if (not(cast((*unsafe*) subscript(a,0))=unique0)) then
			   let val ai' = (*unsafe*) subscript(a,i)
			   in if (not (cast ai' = unique1)) then
			        ((*unsafe*) update(a,i,cast unique1);
			         douAl (next,STOREUPDATE(a,i,ai',acc)))
			      else douAl (next,acc)
			   end
			 else douAl (next,acc)
	     | NONE => douAl (next,acc)) 
inlined version follows: *)
	    ((*print " "; print i; print ":";*)
	     case (cast(System.Unsafe.subscript(cast warr,0)),
		   cast(System.Unsafe.subscript(cast warr,~1))) of
	       (a,weak_desc) => if (cast ((*unsafe*) subscript(a,0)) = unique0) then
			   ((*print "U0";*)
			    douAl (next,acc))
			 else
			   let val ai' = (*unsafe*) subscript(a,i)
			   in if (cast ai' = unique1) then
				((*print "U";*)
				 douAl (next,acc))
			      else
			        ((*unsafe*) update(a,i,cast unique1);
			         douAl (next,STOREUPDATE(a,i,ai',acc)))
			   end
	     | (_,tag)  => ((*print "weak:"; print tag; *)
			    douAl (next,acc)))
	| douAl (PNIL,acc) = acc
      fun douRl (warr::next,acc) =
(*	    (case (strong warr) of
	       SOME a => let val a0' = a sub 0
			 in if (not(cast a0' = unique0)) then
			      ((*unsafe*) update(a,0,cast unique0);
			       douRl (next,STOREUPDATE(a,0,a0',acc)))
			    else douRl (next,acc)
			 end
	     | NONE => douRl (next,acc))
inlined version follows: *)
	    (case (cast(System.Unsafe.subscript(cast warr,0)),
		   cast(System.Unsafe.subscript(cast warr,~1))) of
	       (a,weak_desc) => let val a0' = (*unsafe*) subscript(a,0)
			 in if cast a0' = unique0 then
			      douRl (next,acc)
			    else
			      ((*unsafe*) update(a,0,cast unique0);
			       douRl (next,STOREUPDATE(a,0,a0',acc)))
			 end
	     | (_,tag) => ((*print "weak:";print tag; print "\n";*)
			   douRl (next,acc))) 
	| douRl (nil,acc) = acc
      fun plength (PCONS(_,_,next)) = 1 + (plength next)
        | plength (PNIL) = 0
(*      val _ = (print "A R update original = "; print (plength (!updatedAList));
		print " "; print (length (!updatedRList)); print "\n") *)
      val ul = if !activate then
		 douRl (!updatedRList, douAl (!updatedAList,NILstoreupdate)) 
               else NILstoreupdate
      fun sizeul (STOREUPDATE(_,off,_,next)) = 
		((*print off; print " "; *)
		 1 + (sizeul next))
	| sizeul (NILstoreupdate) = ((*print "total ";*) 0)
(*      val _ = (print "condensed to "; print (sizeul ul); print "\n") *)
      fun resetc (STORECREATION(a,a',next)) = 
	    ((*unsafe*) update (a,0,(*unsafe*) subscript(a',0));
	     resetc next)
	| resetc nNILstorecreation = ()
      val _ = resetc cl
      fun resetu (STOREUPDATE(a,i,ai',next)) =
	    ((*unsafe*) update(a,i,ai');
	     resetu next)
	| resetu NILstoreupdate = ()
      val _ = resetu ul
      val _ = case (!lastsd) of 
                STOREDELTA(_,_,_,rc,_) => inc rc
	      | NILstoredelta => ()
      val newsd = STOREDELTA([cl],ul,!lastsd,ref 0,weak h)
  in  (fn (STOREHANDLE(sdr)) => sdr := newsd) h;
      lastsd := newsd;
      createdList := nil;
      updatedAList := PNIL;
      updatedRList := nil;
(*      print "done\n"; *)
      h
  end

fun recreate (STORECREATION(a,v,rest)) = 
	((*print "recreate\n"; *)
	 resetarray a v; recreate rest)
  | recreate NILstorecreation = ()

fun reupdate (STOREUPDATE(a,i,v,rest)) = 
	((*print "reupdate "; print i; *)
	 (*unsafe*)update(a,i,v); reupdate rest)
  | reupdate NILstoreupdate = ()

(* -- this version condenses update lists against each other where
	possible (but not update lists against create lists...) -- *) 
fun redo (STOREDELTA (cls,ul,sd',rc,wh)) =
      let val (oldcls,olduls,oldsd) = redo sd'
      in case (rc,strong wh) of
	   (ref 1,NONE) => (cls @ oldcls,ul::olduls,oldsd)
         | (_,_) =>
	     let val cls' = cls @ oldcls
		 fun markprune (STOREUPDATE(a,i,v,rest),acc) =
		      if not (cast ((*unsafe*) subscript(a,i)) = unique0) then
			((*unsafe*) update (a,i,cast unique0);
			 markprune (rest, STOREUPDATE(a,i,v,acc)))
		      else markprune (rest,acc)
		   | markprune (NILstoreupdate,acc) = acc
		 val ul' = if null olduls then
			     ul 
			   else revfold markprune (ul::olduls) NILstoreupdate
	     in
	       app recreate cls';
	       reupdate ul';
	       (nil,nil,STOREDELTA(cls',ul',oldsd,rc,wh))
             end
      end
   | redo NILstoredelta = (nil,nil,NILstoredelta)  

(*	-- simple version -- 
fun redo (STOREDELTA (cls,ul,sd',_,_)) =
     (redo sd'; app recreate cls; reupdate ul)
  | redo NILstoredelta = ()
*)

fun restore (STOREHANDLE (ref sd)) =
    ((*print "restoring..."; flush_out std_out; *)
     redo sd;
     (*print "done\n"; *)
     lastsd := sd)

end (* struct *)

