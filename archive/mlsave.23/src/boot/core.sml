(*
The following are already in the symbol table:
     1) Magical words that can be free in signatures (from PrimTypes):
		int string bool unit real list array ref exn
     2) Built-in constructors (from PrimTypes):
		:: nil ref true false
     3) Built-in structures:
		PrimTypes InLine
	The InLine structure is not typed (all values have type alpha).
All matches in this file should be exhaustive; the match and bind exceptions
 are not defined at this stage of bootup, so any uncaught match will cause
 an unpredictable error.
*)


functor CoreFunc(Assembly : ASSEMBLY) =
struct
    structure Assembly = Assembly

    exception Bind
    exception Match

  val bufsize = 1024
    val zcurrent = Assembly.C.current
    val zother = Assembly.create_b 20

  val std_in = {filid = 0, pos = ref 0, len = ref 0, 
		closed = ref false, buf = Assembly.create_b bufsize,
		tty = ref true}
  val std_out ={filid = 1, pos = ref 0, 
		closed = ref false, buf = Assembly.create_b bufsize,
		tty = ref true}

structure Refs = struct
    val interactive = ref true
    val prLambda = ref (fn () => ())
    val zerotime = (0,0)
    val lines = ref 0
    val parse = ref zerotime
    val translate = ref zerotime
    val codeopt = ref zerotime
    val convert = ref zerotime
    val cpsopt = ref zerotime
    val closure = ref zerotime
    val globalfix = ref zerotime
    val spill = ref zerotime
    val codegen = ref zerotime
    val freemap = ref zerotime
    val execution = ref zerotime
    val outstreams = ref [std_out]
    val instreams = ref [std_in]
    val reduce_r = ref (fn a : (unit -> unit) => a)
    val printArgs = ref false
    val printRet = ref false
    val bindContainsVar = ref true
    val bindExhaustive = ref true
    val matchExhaustive = ref true
    val matchRedundant = ref true
    val expandResult = ref true
    val trapv = ref true
    val tailrecur = ref true
    val recordopt = ref true
    val tail = ref true
    val profile = ref false
    val closureprint = ref false
    val closureStrategy = ref 1
    val path = ref false
    val hoist = ref true
    val reduce = ref true
    val bodysize = ref 0
    val reducemore = ref 0
    val alphac = ref true
    val comment = ref false
    val knowngen = ref 0
    val stdgen = ref 0
    val knowncl = ref 0
    val foldconst = ref true
    val etasplit = ref true
    val printit = ref false
    val printsize = ref false
    val printDepth = ref 5
    val stringDepth = ref 70
    val signatures = ref true
    val profiling = ref false
    val globalProfile = ref [zother]
    val debugging = ref false
    val primaryPrompt = ref "- "
    val secondaryPrompt = ref "= "
    val internals = ref false
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
    val saveLambda = ref false
    val saveLvarNames = ref false
    val timings = ref false
    val reopen = ref false
    val pstruct = ref {core = (), initial=(), math=()}
end

(* someday modify this function to use sceql, after old code generator
	(which doesn't implement sceql) is thrown away *)
    fun stringequal(a,b) =
     if InLine.ieql(a,b) then true
     else InLine.boxed a andalso InLine.boxed b andalso
      let val len = InLine.slength a
       in if InLine.ieql(len,InLine.slength b)
	   then let fun f 0 = true
		      | f i = let val j = InLine.-(i,1)
			       in if InLine.ieql(InLine.ordof(a,j),
				                 InLine.ordof(b,j))
			           then f j else false
			      end
		 in f len
		end
	   else false
       end

    local
	val width_tags = 4
	val power_tags = 16
	val tag_record =	1
	val tag_array =	9
	val tag_bytearray =	11
	val tag_string =	15
	val tag_embedded =	7
	val tag_closure =	13
	val tag_backptr =	5
	val tag_forwarded =	3

       val ieql = InLine.ieql
       val cast = InLine.cast
       val sub = InLine.subscript
       val boxed = InLine.boxed
       val / = InLine.div
       val op * = InLine.*
       val + = InLine.+
       val - = InLine.-
     in fun polyequal(a : 'a, b : 'a) = ieql(a,b)
      orelse
      (boxed a andalso boxed b
       andalso
	  (* assumes identical tags, with string=embedded *)
	  let val tag : int = InLine.subscript(a,~1)
	      val tag = +(+(tag,tag),1)
	      val tag = -(tag,op *(/(tag,power_tags),power_tags))
	      val alen = InLine.alength a
	      fun maprec () =
		    let fun m i = if ieql(i,alen) then true
	 			  else if polyequal(sub(a,i), sub(b,i))
				    then m(InLine.+(i,1))
				  else false
		    in  m 0
		    end
	  in  if ieql(tag,tag_array) then false
	      else if ieql(tag,tag_bytearray) then false
	      else ieql(alen,InLine.alength(b)) 
		   andalso  if ieql(tag,tag_record) then maprec()
			    else stringequal(cast a, cast b)
	  end)
  end
end
