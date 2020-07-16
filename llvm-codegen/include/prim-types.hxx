/// \file prim-types.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief The definitions of the primitive modules iun cps.asdl
///
/// \author John Reppy
///

#ifndef __PRIM_TYPES_HXX__
#define __PRIM_TYPES_HXX__

#include <vector>

namespace LambdaVar {

    typedef int64_t lvar;

    lvar read_lvar (asdl::instream & is);
    std::vector<lvar> read_lvar_seq (asdl::instream & is);

} // namespace LambdaVar

#endif // !__PRIM_TYPES_HXX__
