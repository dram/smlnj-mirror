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

void codegen (std::string const & src, bool emitLLVM, output out);

#endif // !_CODEGEN_HXX_
