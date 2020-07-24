/// \file codegen.cxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Main code generator code.
///
/// \author John Reppy
///

#include "code-buffer.hxx"
#include "cfg.hxx"

static code_buffer *CodeBuf;

void codegen (asdl::instream &inS)
{
    CFG::comp_unit *cu = CFG::comp_unit::read (inS);

    if (CodeBuf == nullptr) {
	CodeBuf = code_buffer::create ("AMD64");
    }

    CodeBuf->initModule (cu->get_srcFile());

    cu->codegen (CodeBuf);

    CodeBuf->dump ();

}
