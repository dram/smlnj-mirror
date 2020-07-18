This directory contains development code for a new **LLVM**-based
code generator for *SML/NJ*.

To build the *LLVM* libraries, use the *build-llvm.sh* script.

It is also necessary to build and install *asdlgen* and its supporting
include files and libraries.  To do so, run the command

````
make install
````

in the `asdl` directory.  Note that the *SML/NJ* `config/install.sh`
script builds *asdlgen*, but does not install the libraries.
