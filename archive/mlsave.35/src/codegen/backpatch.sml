signature BACKPATCH =
sig
    eqtype Label
    val newlabel : unit -> Label
    type JumpKind
    val emitstring : string -> unit
    val align : unit -> unit
    val define : Label -> unit
    val jump : JumpKind*Label -> unit
    val mark : unit -> unit
    val finish : unit -> string
end

signature JUMPS =
sig
  type JumpKind
  val sizejump : JumpKind*int*int*int -> int
  val emitjump : JumpKind*int*int*int -> string
  val emitlong : int -> string
end

functor Backpatch(Kind: JUMPS) : BACKPATCH =
struct 
  open Kind System.Tags
  type Label = int ref
  fun newlabel() = ref 0

  datatype Desc
	 = BYTES of string * Desc | JUMP of JumpKind * Label * int ref * Desc
	 | LABEL of Label * Desc | ALIGN of Desc | MARK of Desc | NIL

  val refs = ref(fn z:Desc => z)
  fun emitstring s = let val z = !refs in refs := fn r => z(BYTES(s,r)) end
  fun align() = let val z = !refs in refs := fn r => z(ALIGN r) end
  fun mark() = let val z = !refs in refs := fn r => z(MARK r) end
  fun define lab = let val z = !refs in refs := fn r => z(LABEL(lab, r)) end
  fun jump(k,lab) = 
	let val z = !refs in refs := fn r => z(JUMP(k,lab,ref 0, r)) end

  fun finish() =
   let val changed = ref true

       fun labels (pos, BYTES(s,rest)) = labels(pos+size s,rest)
         | labels (pos, JUMP(k,l,ref size, rest)) = labels(pos+size, rest)
	 | labels (pos, LABEL(l,rest)) = (l := pos; labels(pos,rest))
	 | labels (pos, lab as ALIGN rest) = labels(((pos+3)div 4)*4, rest)
	 | labels (pos, MARK rest) = labels(pos+4, rest)
	 | labels (pos, NIL) = ()

       fun adjust (pos, BYTES(s,rest)) = adjust(pos+size s,rest)
	 | adjust (pos, JUMP(k, l, r as ref size, rest)) =
		let val s = sizejump(k, size, pos, !l)
	        in  if s > size then (r := s; changed := true) else ();
                    adjust(pos+size, rest)
		end
	 | adjust (pos, LABEL(l,rest)) = adjust(pos,rest)
	 | adjust (pos, ALIGN rest) = adjust(((pos+3)div 4)*4, rest)
	 | adjust (pos, MARK rest) = adjust(pos+4, rest)
	 | adjust (pos, NIL) = ()

       fun chunk(pos, BYTES(s,r)) = s :: chunk(pos+size s,r)
	 | chunk(pos, JUMP(k,l,ref size, r)) =
		    emitjump(k,size,pos,!l) :: chunk(pos+size,r)
	 | chunk(pos, LABEL(l, rest)) = chunk(pos,rest)
	 | chunk(pos, ALIGN rest) =
		(case pos mod 4
		  of 0 => chunk(pos,rest)
		   | 1 => "\000\000\000" :: chunk(pos+3,rest)
		   | 2 => "\000\000" :: chunk(pos+2,rest)
		   | 3 => "\000" :: chunk(pos+1,rest))
	 | chunk(pos, MARK r) =
     	       emitlong(((pos+4)div 4)*power_tags+tag_backptr) 
		:: chunk(pos+4, r)
	 | chunk(pos, NIL) = nil

       val reflist = !refs (ALIGN NIL) before refs := (fn z=>z)
    in  ErrorMsg.debugmsg "relocating...";
        while !changed
	   do (changed := false; labels(0, reflist); adjust(0, reflist));
        ErrorMsg.debugmsg "about to output";
	implode(chunk(0, reflist))
   end
end (* functor BackPatch *)
