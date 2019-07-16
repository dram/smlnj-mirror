(*
 * cm/cm.sig: `CM' Compilation Manager (layout of main structure)
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature COMPILATION_MANAGER = sig

    structure Compiler: COMPILER

    structure Tools: TOOLS

    structure SymVal: SYMVAL

    (* current CM version *)
    val version: string

    (* controlling verbosity and debugging *)
    val verbose: bool option -> bool
    val debug: bool option -> bool

    (* keep going after compile errors or don't *)
    val keep_going: bool option -> bool

    (* control caching of parse trees *)
    val parse_caching: int option -> int

    (* list exported names for entities without filter *)
    val show_exports: bool option -> bool

    (* change default rootfile *)
    val set_root: string -> unit

    (* change default search path *)
    val set_path: string list option -> string list

    (*
     * dot': rootdescriptiondesc * dotfile -> unit
     * dot: dotfile -> unit
     *  Produce a DOT graph (the input for DOT) showing the dependency
     *  graph.
     *)
    val dot': string * string -> unit
    val dot: string -> unit

    (*
     * standalone stuff
     *)
    val sa': string * string -> unit
    val sa: string -> unit

    (*
     * names', binfiles', strings': rootdescriptiondesc -> string list
     * names, binfiles, strings: unit -> string list
     *  Produce various topologically sorted lists of strings:
     *   names - SML filenames
     *   binfiles - binfile names
     *   strings - descriptive strings for SML files
     *)
    val names': string -> string list
    val names: unit -> string list
    val binfiles': string -> string list
    val binfiles: unit -> string list
    val strings': string -> string list
    val strings: unit -> string list

    (*
     * recompile': rootdescriptiondesc -> unit
     * recompile: unit -> unit
     *  Recompile what's necessary, update cache and binfiles.
     *)
    val recompile': string -> unit
    val recompile: unit -> unit

    (*
     * make': rootdescriptiondesc -> unit
     * make: unit -> unit
     *  Recompile what's necessary, execute code in all modules,
     *  introduce new toplevel bindings.
     *)
    val make': string -> unit
    val make: unit -> unit

    (*
     * mkusefile': rootdescriptiondesc * usefile -> unit
     * mkusefile: usefile -> unit
     *  Create a file (usefile) containing a topologically sorted list
     *  of SML `use' commands necessary to boot a system in the absence
     *  of CM.
     *)
    val mkusefile': string * string -> unit
    val mkusefile: string -> unit

    (*
     * Stabilize entity
     *)
    val stabilize': string * bool -> unit
    val stabilize: bool -> unit

    (*
     * Destabilize entity
     *)
    val destabilize': string -> unit
    val destabilize: unit -> unit

    (* prepare for autoloading *)
    val autoload': string -> unit
    val autoload: unit -> unit

    (* autoloading mode on/off *)
    val autoloading: bool option -> bool

    (* forget all autoloaded entities *)
    val clearAutoList: unit -> unit

    val autoList: unit -> string list

    (*
     * register and unregister cleanup code
     *)
    val initCleanup: unit -> unit
    val uninitCleanup: unit -> unit

    (*
     * Remove things from the cache, which can't possibly be valid anymore:
     *)
    val sweep: unit -> unit

    (*
     * Empty the cache:
     *)
    val clear: unit -> unit

    (*
     * The `testbed' for compiler hackery...
     *)
    val testbed': string * string list -> unit
    val testbed: string list -> unit

    (*
     * Timing statistics...
     *)
    val withTiming: ('a -> 'b) -> 'a -> 'b

    (*
     * Command-line processing...
     *)
    val procCmdLine: unit -> unit
end

signature FULL_CM = sig

    structure SysEnv: SYS_ENV
    structure Control: CONTROL
    structure EntityDesc: ENTITY_DESC
    structure CUnit: CUNIT
    structure Recompile: RECOMPILE
    structure Driver: DRIVER
    structure Lists: LISTS
    structure ModuleName: MODULE_NAME
    structure SysDag: SYS_DAG
    structure Complain: COMPLAIN

    include COMPILATION_MANAGER

    sharing
	Compiler =
	CUnit.Compiler =
	Recompile.SysDag.GroupDag.ModuleName.Compiler =
	Driver.Compiler
    sharing SysDag = Recompile.SysDag = Lists.SysDag = Driver.SysDag
    sharing ModuleName = SysDag.ModuleName

    (* sharing type desc = SysDag.desc *)

end
