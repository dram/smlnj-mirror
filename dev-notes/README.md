This directory tree contains various notes about the
implementation of SML/NJ.

* README.md -- this file

* TODO-64bit.md -- a rough plan for the port to support 64-bit targets

* TODO.md -- "TODO" list for the compiler

* amd64-stack-frame.numbers -- Numbers spreadsheet that describes the layout of
  the ML stack frame for the AMD64 target

* conversions.md -- describes the algebra of integer/word conversions used to implement
  the Basis Library

* cps-primops.md -- describes the semantics of the CPS primitive operations

* new-literals.md -- describes a new representation for the literal bytecode that is used
  to generate the literals record for a module.

* primop-list.md -- describes the primitive operations that are exposed by the compiler

* primops.txt -- describes internal representation of primitive operations as of 110.82

* register-assignments.numbers -- Numbers spreadsheet that describes the mapping from
  the CPS register model to hardware registers for the supported architectures.

* runtime-rep.md -- describes the runtime representation of basic SML values.

* tycons-ptnums.txt -- description of FLINT tycons and ptnums
