/// \file codegen.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Interface to the code generator library (for testing purposes).
///
/// \author John Reppy
///

#ifndef _CODEGEN_HXX_
#define _CODEGEN_HXX_

enum class output { PrintAsm, AsmFile, ObjFile, Memory };

void codegen (std::string const & src, output out);

// Some global flags for controlling the code generator.
// These are just for testing purposes

#endif // !_CODEGEN_HXX_
