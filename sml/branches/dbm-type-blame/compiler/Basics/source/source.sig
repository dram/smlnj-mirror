(* source.sig
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *)

signature SOURCE =
  sig
    type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream, 
        anyErrors: bool ref,
        errConsumer: PrettyPrintNew.device
      }

    val newSource : (string * int * TextIO.instream * bool * PrettyPrintNew.device)
          -> inputSource

    val closeSource: inputSource -> unit

    val filepos: inputSource -> SourceMap.charpos -> SourceMap.sourceloc

  end

(*
The fileOpened field contains the name of the file that was opened to
produce a particular inputSource.  It is used only to derive related
file names.  (For an example, see CompileF.codeopt and CompileF.parse
in build/compile.sml.)

newSource has some old warts build in.  It takes as argument a file
and line number, and it assumes column~1.  The reason we don't simply
pass a SourceMap.sourcemap is that we have to hide the awful truth
about the beginning position according to ml-lex (it's 2).  That
position, and therefore the creation of the source map, are
encapsulated inside newSource.

filepos is kept around for historical reasons, to avoid having to
change lots of code elsewhere in the compiler; it wraps a call to
SourceMap.filepos and massages the return type.  It probably should be
eliminated, but then somebody would have to fix all those call sites.
*)

