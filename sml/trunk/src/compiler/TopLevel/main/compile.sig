(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compile.sig *)

signature COMPILE0 = 
sig

exception Compile of string            (* raised during compilation only *)
exception SilentException              (* raised by CM *)
exception TopLevelException of exn     (* raised during executation only *)
exception TopLevelCallcc               (* raised during executation only *)
val architecture: string               (* machine architecture *)

(** important intermediate formats used during the compilations *)
type source     = CompBasic.source     (* the input file *)
type ast        = CompBasic.ast        (* concrete syntax *)
type absyn      = CompBasic.absyn      (* abstract syntax *)
type flint      = CompBasic.flint      (* intermediate code *)
type csegments  = CompBasic.csegments  (* binary code segments *)
type executable = CompBasic.executable (* machine executables *)
type object     = CompBasic.object         (* runtime object *)

(** environments and contexts used during the compilation *)
type statenv    = StaticEnv.staticEnv  (* static env   : symbol -> binding *)
type dynenv     = DynamicEnv.dynenv    (* dynamic env  : pid -> object *)
type symenv     = SymbolicEnv.symenv   (* symbolic env : pid -> flint *)

type compInfo   = CompBasic.compInfo   (* general compilation utilities *)
val mkCompInfo  : source * (absyn -> absyn) -> compInfo
val anyErrors   : compInfo -> bool

type lvar       = Access.lvar          (* local id *)
type pid        = PersStamps.persstamp (* persistant id *)
type import     = pid * CompBasic.importTree  (* import specification *)
type pickle                            (* pickled format *)
type hash                              (* environment hash id *)

(** take the input source and turn it into the concrete syntax *)
val parseOne    : source -> unit -> ast option (* incremental version *)
val parse       : source -> ast

(** take ast, do semantic checks, then output the new env, absyn and pickles *)
(*
 * "statenv" must be the combination of all import environments.
 * Each individual import environment must have been treated by
 * GenLinkPath.start prior to being put into "statenv".
 * "getMap" fetches the linkpath map to be used for pickling.
 * "getEnv" fetches back environments during unpickling.  (We should not
 * use "statenv" here because the linkpath generator will cause it to 
 * be a partial copy of the original environments.)
 *)
val elaborate   : {ast: ast,
		   statenv: statenv,
		   compInfo: compInfo} 
                   -> {absyn: absyn, newstatenv: statenv,
 	               exportLvars: lvar list, exportPid: pid option,
		       staticPid: hash, pickle: pickle }

(** elaborate as above, then keep on to compile into the binary code *)
val compile     : {source: source, ast: ast,
		   statenv: statenv,
                   symenv: symenv, compInfo: compInfo, 
                   checkErr: string -> unit,
                   splitting: bool}
                   -> {csegments: csegments, newstatenv: statenv,
                       absyn: absyn (* for pretty printing only *),
                       exportPid: pid option, exportLvars: lvar list,
                       staticPid: hash, pickle: pickle, 
                       inlineExp: flint option, imports: import list }

(** build the new symbolic environment *)
val mksymenv    : pid option * flint option -> symenv 

(** turn the byte-vector-like code segments into an executable closure *)
val mkexec      : csegments -> executable

(** just like f x, except that it catches top-level callcc's *)
val isolate     : ('a -> 'b) -> 'a -> 'b

(** perform the execution of the excutable, output the new dynenv *)
val execute     : {executable: executable, imports: import list, 
                   exportPid: pid option, dynenv: dynenv} -> dynenv

end (* signature COMPILE0 *)

signature COMPILE = COMPILE0 where type pickle = Word8Vector.vector
                               and type hash = PersStamps.persstamp

signature TOP_COMPILE = COMPILE0
