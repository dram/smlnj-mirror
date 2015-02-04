signature BACKPATCH =
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

functor Backpatch(Kind: sig type JumpKind end) : BACKPATCH =
  struct 
    structure DBA = Dynamic(struct
			     local open ByteArray in
				   type array = bytearray
				   type elem = int
				   exception Subscript = Subscript
				   val array = array
				   val op sub = op sub
				   val update = update
				   val length = length
			     end
			    end)

    open System.Tags
    type Label = int ref
    fun newlabel() = ref 0

    type JumpKind = Kind.JumpKind

    datatype Descriptor
	 = JUMP of (JumpKind * Label * int ref)
	 | LABEL of Label | ALIGN | MARK

    val initialOffset = 0

    fun sizeJumps (sizejump : JumpKind * int * int * int -> int)
		  (lab : (int * Descriptor) list) =
     let val changed = ref false
         fun labels (offset, (pos, JUMP(k, l, r as ref size))::rest) =
		  labels(offset+size, rest)
	   | labels (offset, (pos, LABEL l)::rest) =
	        (l := offset+pos; labels(offset,rest))
	   | labels (offset, lab as (pos,ALIGN)::rest) =
		if (offset+pos) mod 4 = 0 then labels(offset,rest)
		    else labels(offset+1, lab)
	   | labels (offset, (pos,MARK)::rest) =
	        labels(offset+4, rest)
	   | labels (offset, nil) = ()
         fun adjust (offset, (pos, JUMP(k, l, r as ref size))::rest) =
		let val s = sizejump(k, size, offset+pos, !l)
	        in  if s > size then (r := s; changed := true) else ();
                    adjust(offset+size, rest)
		end
	   | adjust (offset, (pos, LABEL l)::rest) =
	        adjust(offset,rest)
	   | adjust (offset, lab as (pos,ALIGN)::rest) =
		if (offset+pos) mod 4 = 0 then adjust(offset,rest)
		else adjust(offset+1, lab)
	   | adjust (offset, (pos,MARK)::rest) =
	        adjust(offset+4, rest)
	   | adjust (offset, nil) = ()
     in  labels(initialOffset, lab);
	 adjust(initialOffset, lab);
         while !changed
	   do (changed := false;
	       labels(initialOffset, lab);
	       adjust(initialOffset, lab))
     end

  val bytes = DBA.array 0
  val position = ref 0
  fun emitbyte i = (DBA.update(bytes, !position, i); inc position)
  val refs : (int * Descriptor) list ref = ref nil
  fun addref(d) = refs := (!position, d) :: !refs
  fun align() = addref ALIGN
  fun define lab = addref(LABEL lab)
  fun jump(k,lab) = addref(JUMP(k,lab,ref 0))
  fun mark() = addref MARK
  fun finish(sizejump,emitjump,emitlong) =
   let val endlabel = newlabel()
       val _ = addref(LABEL endlabel)
       val reflist = rev (!refs)
       val scratchpos = !position
       val _ = ErrorMsg.debugmsg "relocating..."
       val _ = sizeJumps sizejump reflist
   in  (!endlabel - initialOffset,
        (fn output =>
	  let val _ = ErrorMsg.debugmsg "about to output"
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
	        | chunk(lastp, g, lab as (p, ALIGN)::rest) =
	            (copy(lastp, p);
		     if (p+g) mod 4 = 0 then chunk(p, g, rest)
		        else (output 0; chunk(p, g+1, lab)))
	        | chunk(lastp, g,(p, MARK)::rest) =
	            (copy(lastp, p);
		     position := scratchpos;
		     emitlong (((p+g+4) div 4) * power_tags
				+ tag_backptr);
		     copy(scratchpos,!position);
		     chunk(p, g+4, rest))
		| chunk(lastp, g, nil) =
		    copy(lastp, scratchpos)
          in  chunk(0,initialOffset,reflist);
              position := 0;
              refs := nil
	  end))
   end
  end (* functor BackPatch *)
