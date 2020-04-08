This directory tree contains documentation for the
various libraries that comprise the **Standard ML of
Ney Jersey Library**.  The documentation is written
using [AscciDoctor](https://asciidoctor.org).

Naming conventions for files.  All source files have
the `.adoc` file suffix.  For each individual library,
there is a file `foo-lib.adoc`, where `foo-lib.cm` is
the name of the CM file for the library.  Files that
document modules (*i.e.*, signatures, structures, or
functors) begin with a prefix, which is one of `sig-`,
`str-`, or `fun-`, followed by the module name and
the `.adoc` file suffix.  For example, consider the
the `ORD_SET` signature.  It's documentation will live
in a file named `sig-ORD_SET.adoc`.  That file will
document the signature as well as its various instances
(*e.g.*, `AtomSet`, `RedBlackSetFn`, *etc*.).
