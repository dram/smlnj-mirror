(*
 * These are some basic annotations understood by the MLRISC system.
 * The MLRISC client can create its own annotations and propagate them
 * to MLRISC.  Client-defined annotations are ignored by MLRISC. 
 *
 * -- Allen
 *)

signature MLRISC_ANNOTATIONS =
sig

    (* 
     * The branch probability of conditional branches. 
     * The client can attach this with conditional branches.
     * This has no effect otherwise. 
     *
     * Currently, the annotation is recognized by the static branch prediction
     * module. 
     *)
   val BRANCH_PROB : int Annotations.property (* in percentage (0-100)*) 

    (* The execution frequency of a basic block 
     * You can attach this at a basic block.
     *)
   val EXECUTION_FREQ : int Annotations.property

    (* No effect at all; this just allows you to insert comments *)
   val COMMENT : string Annotations.property

    (* 
     * Control dependence definition and use.
     *
     * To use these, the client should generate
     * control dependence virtual registers via Cells.newCell Cells.CTRL
     * and attach these annotations to instructions and basic blocks.
     *
     * These annotations are currently recognized by the SSA optimization
     * modules.
     *)
   exception CTRLDEF of int
   exception CTRLUSE of int
   val CTRL_DEF : int Annotations.property
   val CTRL_USE : int Annotations.property

    (*
     * This annotation can be used specify a pretty printing function for
     * assemblers
     *)
   val REGINFO : ((int -> int) * int -> string) Annotations.property

    (*
     * Disable all optimizations in the cluster
     *)
   val NO_OPTIMIZATION : unit Annotations.property

    (*
     * Mark basic block that is used for calling the GC
     *)
   val CALLGC : unit Annotations.property
   val GCSAFEPOINT : string Annotations.property

    (*
     * Insert block names
     *)
   val BLOCK_NAMES : Annotations.annotations Annotations.property

    (*
     * This annotation inserts an empty basic block
     *)
   val EMPTY_BLOCK : unit Annotations.property

    (* 
     * Enter information for a register.
     *)
   exception MARKREG of int -> unit
   val MARK_REG : (int -> unit) Annotations.property

    (*
     * Disable branch chaining optimization on a jump
     *)
   val NO_BRANCH_CHAINING : unit Annotations.property

end
