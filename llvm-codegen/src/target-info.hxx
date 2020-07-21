/// \file target-info.hxx
///
/// \copyright 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief Information about the target architecture and how SML is mapped
///        onto it.
///
/// \author John Reppy
///

#ifndef _TARGET_INFO_HXX_
#define _TARGET_INFO_HXX_

struct target_info {
    int		wordSz;		// size in bits of ML word (should also be native pointer size)
};

#endif // !_TARGET_INFO_HXX_
