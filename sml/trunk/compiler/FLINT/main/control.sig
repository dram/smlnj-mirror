(* control.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature FLINTCONTROL =
sig

    val print		: bool ref   (* show IR *)
    val printPLambda    : bool ref   (* show PLambda.lexp after Translate *)
    val printPhases	: bool ref   (* show phases *)
    val printFctTypes   : bool ref   (* show functor types *)
    val currentPhase    : string ref (* the name of the current FLINT optimization phase *)
    val phases		: string list ref  (* determines phases and their order *)
    val insertPhase     : string * int -> unit

    val tmdebugging     : bool ref   (* TransTypes debugging *)
    val trdebugging     : bool ref   (* Translate debugging *)
    val mcdebugging     : bool ref   (* MatchComp debugging *)
    val nmdebugging     : bool ref   (* Plambda normalization (FlintNM) *)
    val fcdebugging     : bool ref   (* fcontract phase debugging (FContract) *)
    val rcdebugging     : bool ref   (* recover phase debugging (Recover) *)
    val spdebugging     : bool ref   (* specialize phase debugging (Specialize) *)
    val wrdebugging     : bool ref   (* wrap phase debugging (Wrapping) *)
    val redebugging     : bool ref   (* reify phase debugging (Reify) *)
    val rtdebugging     : bool ref   (* runtime types debugging (RuntimeType) *)

    val lineWidth	: int ref    (* FLINT pretty printer line width (def 200) *)
    val printDepth	: int ref    (* FLINT pretty printer print depth (def 200) *)

    val inlineThreshold	: int ref    (* inline threshold *)
    (* val splitThreshold : int ref *)
    val unrollThreshold	: int ref    (* unroll threshold *)
    val maxargs		: int ref    (* to put a cap on arity raising *)
    val dropinvariant	: bool ref

    val specialize	: bool ref   (* whether to specialize *)
    (* val liftLiterals	: bool ref *)
    val sharewrap	: bool ref   (* whether to share wrappers *)
    val saytappinfo	: bool ref   (* for verbose typelifting *)

    (* FLINT internal type-checking controls *)
    val check		: bool ref    (* typecheck IR? *)
    val checkDatatypes	: bool ref    (* typecheck datatypes *)
    val checkKinds	: bool ref    (* check kinds *)
    val checkPLambda    : bool ref    (* type check plambda after translate *)

    (* for use in FLINT/main/flintcomp.sml *)
    val recover : (LambdaVar.lvar -> unit) ref

    (* only for temporary debugging *)
    val misc		: int ref

end (* signature FLINTCONTROL *)
