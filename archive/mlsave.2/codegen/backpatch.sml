signature backpatch =
sig
    type Label
    val newlabel : unit -> Label
    type JumpKind
    val emitbyte : int -> unit
    val align : unit -> unit
    val define : Label -> unit
    val jump : JumpKind*Label -> unit
    val mark : unit -> unit
    val finish : ( (JumpKind*int*int*int->int)
	          *(JumpKind*int*int*int->unit)
		  *(int->unit) )
		 -> (int * ((int->unit)->unit))
end

functor backpatch(Kind: sig type JumpKind end) : backpatch =
struct 
      structure DBA = Dynamic(struct local open Byte_array
					in type array=byte_array
					   exceptionx subscript = byte_array
					   type elem = int
					   val array = create
					   val op sub = fetch
					   val update = store
					   val length = length
				       end
			      end)

      type Label = int ref;
      fun newlabel () = ref 0;

      type JumpKind=Kind.JumpKind;

      datatype Descriptor
	 = JUMP of (JumpKind * Label * int ref)
	 | LABEL of Label | ALIGN | MARK

      val initialOffset = 4

    fun SizeJumps (sizejump : JumpKind * int * int * int -> int)
		  (L : (int * Descriptor) list) =
     let val changed = ref false;
         fun labels (offset, (pos, JUMP(k, l, r as ref size))::rest) =
		  labels(offset+size, rest)
	   | labels (offset, (pos, LABEL l)::rest) =
	        (l := offset+pos; labels(offset,rest))
	   | labels (offset, L as (pos,ALIGN)::rest) =
		if (offset+pos) mod 4 = 0 then labels(offset,rest)
		    else labels(offset+1, L)
	   | labels (offset, (pos,MARK)::rest) =
	        labels(offset+4, rest)
	   | labels (offset, nil) = ()
         fun adjust (offset, (pos, JUMP(k, l, r as ref size))::rest) =
		  let val s = sizejump(k, size, offset+pos, !l)
	           in if s > size then (r := s; changed := true) else ();
                      adjust(offset+size, rest)
		  end
	   | adjust (offset, (pos, LABEL l)::rest) =
	         adjust(offset,rest)
	   | adjust (offset, L as (pos,ALIGN)::rest) =
		if (offset+pos) mod 4 = 0 then adjust(offset,rest)
		    else adjust(offset+1, L)
	   | adjust (offset, (pos,MARK)::rest) =
	        adjust(offset+4, rest)
	   | adjust (offset, nil) = ()
      in labels(initialOffset, L);
	 adjust(initialOffset, L);
         while !changed
	   do (changed := false;
	       labels(initialOffset, L);
	       adjust(initialOffset, L))
     end

  val bytes = DBA.array(0);
  val position = ref 0;
  fun emitbyte i = (  (* printhex i; print" "; *)
		     (DBA.update(bytes, !position, i)
		      handlex Byte_array.byte_array => (print
 ("error in byte_array update, position " ^ makestring (!position) ^
", value " ^ makestring i); raisex Byte_array.byte_array))
		     ; inc position);
  val refs : (int * Descriptor) list ref = ref nil;
  fun addref(d) = refs := (!position, d) :: !refs;
  fun align() = addref(ALIGN);
  fun define L = addref(LABEL L);
  fun jump(k,L) = addref(JUMP(k,L,ref 0));
  fun mark() = addref(MARK);
  fun finish(sizejump,emitjump,emitlong) =
   let val endlabel = newlabel()
       val _ = addref(LABEL endlabel)
       val reflist = rev (!refs)
       val scratchpos = !position
val _ = print " relocating...\n"
       val _ = SizeJumps sizejump reflist
    in (!endlabel - initialOffset,
        (fn output =>
	  let val _ = (print "about to output\n")
	      fun copy(start,until) =
		      if start=until then ()
		      else (output(DBA.sub(bytes,start)); copy(start+1,until))
	      fun chunk(lastp, g, (p, JUMP(k, l, ref size))::rest) =
		    (copy(lastp, p);
		     position := scratchpos;
		     emitjump(k,size,p+g,!l);
		     copy(scratchpos,!position);
		     chunk(p, g+size, rest))
	        | chunk(lastp, g, (p, LABEL l)::rest) =
		    (copy(lastp, p);
		     chunk(p, g, rest))
	        | chunk(lastp, g, L as (p, ALIGN)::rest) =
	            (copy(lastp, p);
		     if (p+g) mod 4 = 0 then chunk(p, g, rest)
		        else (output 0; chunk(p, g+1, L)))
	        | chunk(lastp, g, L as (p, MARK)::rest) =
	            (copy(lastp, p);
		     position := scratchpos;
		     emitlong ( (2*(p+g+4)) + 7 );
		     copy(scratchpos,!position);
		     chunk(p, g+4, rest))
		| chunk(lastp, g, nil) =
		    copy(lastp, scratchpos)
           in chunk(0,initialOffset,reflist);
              position := 0;
              refs := nil
	  end))
   end
end;
