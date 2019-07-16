(*
 * tools/tools.sig: managing source classes and tools
 *
 *   Copyright (c) 1996 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

signature CMTOOLS = sig

    exception ToolError of { tool: string, msg: string }

    type fname = string
    type class = string

    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of fname -> class option

    type target = fname * class option

    (* A rule takes a file name (relative to the directory of the
     * corresponding description file) and a rulecontext.  In general,
     * when coding a rule one would write a rule function and pass it to
     * the context, which will temporarily change the current working
     * directory to the one that holds the description file ("the context").
     * If this is not necessary for the rule to work correctly, then
     * one can simply ignore the context (this saves system call overhead
     * during dependency analysis). To simplify the latter case one can
     * also write a simplerule and turn it into a rule by applying
     * dontcare.  Similarly, function withcontext converts a simplerule
     * into a rule that always runs in the proper context, thereby also always
     * incurring the associated system call overhead. *)
    type rulefn = unit -> target list
    type rulecontext = rulefn -> target list
    type rule = fname * rulecontext -> target list

    type simplerule = fname -> target list

    val dontcare: simplerule -> rule
    val withcontext: simplerule -> rule

    (* validators and processors always run in the correct context *)
    type validator = { source: fname, targets: target list } -> bool
    type processor = { source: fname, targets: target list } -> unit

    val addClassifier: classifier -> unit

    val addSmlClass: class -> unit
    val addCmClass: class -> unit
    val addScGClass: class -> unit
    val addScLClass: class -> unit

    val addToolClass:
	{ class: class, rule: rule,
	  validator: validator, processor: processor } -> unit

    val defaultClassOf: fname -> class option

    (*
     * A couple of convenience routines useful for installing tools.
     *)

    (* make a classifier which looks for a specific file name suffix *)
    val stdSfxClassifier: { sfx: string, class: class } -> classifier

    (* two validators -- one that checks time stamp consistency, the
     * other one only for the existence of the targets *)
    val stdTStampValidator: validator
    val stdExistenceValidator: validator

    (* make a processor that runs a simple shell command *)
    val stdShellProcessor: { command: string, tool: string } -> processor
end

signature TOOLS = sig

    include CMTOOLS

    exception UnknownClass of class

    type abstarget = AbsPath.t * class option
    
    datatype classification =
	SMLSOURCE | CMFILE | SCGROUP | SCLIBRARY
      | TOOLINPUT of 
	{ targets: abstarget list, validate: unit -> bool, make: unit -> unit }

    val classify: AbsPath.t * class option -> classification
end
