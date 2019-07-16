(*
 * entity/description.sig: dealing with entity description files
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ENTITY_DESC = sig

    structure Tools: TOOLS

    exception BadEntityDescription of string * string
    exception FileNotFound of string
    exception AliasNestingTooDeep of string

    datatype member = M of { name: AbsPath.t,
			     history: string list,
			     classification: Tools.classification }

    datatype export =
	SIG of string
      | STR of string
      | FCT of string
      | FSIG of string

    datatype elab_params =
	EP of {
	       path: AbsPath.t list,
	       lparm: {
		       strdef: string -> bool,
		       sigdef: string -> bool,
		       fctdef: string -> bool,
		       fsigdef: string -> bool,
		       symval: string -> int option
		      }
	      }

    (* `location' is the directory in which the description file resides *)
    datatype description =
	ENTITY of {
		   lib: bool,
		   exports: export list,
		   members: member list,
		   location: AbsPath.t,
		   stable: bool
		  }
       
    (* read: EP -> FILENAME -> description * FILENAME *)
    val read: elab_params -> AbsPath.t -> description

    (* SC backward compatibility *)
    val readSCGroup: elab_params -> AbsPath.t -> description
    val readSCLibrary: elab_params -> AbsPath.t -> description

    (* clear internal cache for stable descriptions *)
    val clear: unit -> unit

end
