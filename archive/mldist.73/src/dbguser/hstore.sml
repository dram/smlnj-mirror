(* hstore.sml *)

structure HistoricalArray : ARRAY =
struct
  local
    open System.Unsafe System.Unsafe.Weak
    (* plist is shared with DebugStore *)
    datatype ('a,'b) plist = PNIL | PCONS of 'a * 'b * ('a,'b) plist
    val (updatedAList: (object Array.array weak,int) plist ref,_,_,
	 hcreatea: ((int * object) -> (object array)),_) = 
		  cast (!System.Control.Debug.interface 16)
  in
    open Array
    fun array(cnt,obj) = let val r = cast (hcreatea(cnt,cast obj))
			 in (*print "hcreatea "; print cnt; print "\n";*)
			    r
			 end

    fun arrayoflist nil = cast(hcreatea(0,cast 0 (* ?? *)))
      | arrayoflist (l as (e::r)) =
	    let val a = array(List.length l, e)
	        fun init ((e::r),n) = (update(a,n,e); init(r,n+1))
  	          | init (nil,_) = ()
  	    in  init(r,1); a
            end

    fun update (arr, offset, value) =
	  (Array.update(arr,offset,value);
	   (* print "update "; print offset;print "\n"; *)
	   updatedAList := PCONS(weak(cast arr),offset,!updatedAList))
  end (*local*)
end (*struct*)


structure HistoricalRef =
struct
  local
    open System.Unsafe System.Unsafe.Weak
    val (_,updatedRList: object Array.array weak list ref,_,_,_) =
		  cast (!System.Control.Debug.interface 16)
  in
    open Ref
    (* We handle op := directly in the instrumenter for efficiency;
	  it could be handled here as follows:
      val op := = fn (r,value) => 
		  (Ref.: (r,value);
		   updatedRList := weak (cast r) :: (!updatedRList)) *)
  
    (* We handle inc and dec here for now, because it is difficult to
	  arrange to instrument them. *)
    fun inc r = (Ref.inc r; 
		 updatedRList := weak (cast r) :: (!updatedRList))
    fun dec r = (Ref.dec r;
		 updatedRList := weak (cast r) :: (!updatedRList))
  end (*local*)
end (*struct*)



