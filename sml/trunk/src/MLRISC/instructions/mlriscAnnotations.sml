(*
 * These are some basic annotations understood by the MLRISC system
 *
 * -- Allen
 *)

structure MLRiscAnnotations : MLRISC_ANNOTATIONS =
struct

   structure A = Annotations
   structure C = CellsBasis

    (* the branch probability of conditional branches *)
    (* in percentage *) 
   exception BRANCHPROB of int
   val BRANCH_PROB = A.new'{create=BRANCHPROB, 
                            get=fn BRANCHPROB b => b | e => raise e,
                            toString=fn b => "branch("^Int.toString b^"%)"}

    (* the execution frequency of a basic block *)
   exception EXECUTIONFREQ of int
   val EXECUTION_FREQ = A.new'{create=EXECUTIONFREQ,
                               get=fn EXECUTIONFREQ x => x | e => raise e,
                               toString=fn r => "freq("^Int.toString r^")"}

    (* no effect at all; just allows you to insert comments *)
   val COMMENT = A.new(SOME(fn s => s))

   fun listify f =
   let fun g [] = ""
         | g [x] = f x
         | g (x::xs) = f x^" "^g xs
   in  g end

    (* control dependence use *)
   exception CTRLDEF of C.cell 
   exception CTRLUSE of C.cell
   val CTRL_USE = A.new'{create=CTRLUSE, 
                         get=fn CTRLUSE x => x | e => raise e, 
                         toString=C.toString}
   val CTRL_DEF = A.new'{create=CTRLDEF, 
                         get=fn CTRLDEF x => x | e => raise e, 
                         toString=C.toString}

   val NO_OPTIMIZATION = A.new(SOME(fn () => "NO_OPTIMIZATION"))
   val CALLGC = A.new(SOME(fn () => "CALLGC"))
   val GCSAFEPOINT = A.new(SOME(fn s => "GCSAFEPOINT: "^s))
   val GC_INFO = A.new(SOME(fn () => "GC_INFO"))

   exception BLOCKNAMES of A.annotations
   val BLOCK_NAMES = A.new'{create=BLOCKNAMES,
                            get=fn BLOCKNAMES n => n | e => raise e,
                            toString=fn _ => "BLOCK_NAMES"}

   exception EMPTYBLOCK 
   val EMPTY_BLOCK = A.new'{create=fn () => EMPTYBLOCK,
                            get=fn EMPTYBLOCK => () | e => raise e,
                            toString=fn () => "EMPTY_BLOCK"}

   exception MARKREG of C.cell -> unit
   val MARK_REG = A.new'{toString=fn _ => "MARK_REG",
                         create=MARKREG,
                         get=fn MARKREG f => f | e => raise e
                        }
   val PRINT_CELLINFO = A.new(SOME(fn _ => "PRINT_CELLINFO"))
           : (C.cell -> string) A.property
         
   val NO_BRANCH_CHAINING = A.new(SOME(fn () => "NO_BRANCH_CHAINING"))

    (* 
     * Define return arguments of a call (hack for x86)
     *)
   val RETURN_ARG = A.new(SOME(C.toString))

end
