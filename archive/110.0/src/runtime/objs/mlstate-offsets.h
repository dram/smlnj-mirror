/* mlstate-offsets.h
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 *
 * NOTE: this file is generated --- do not edit!!!
 */

#ifndef _MLSTATE_OFFSETS_
#define _MLSTATE_OFFSETS_

#define VProcOffMSP 4
#define AllocPtrOffMSP 8
#define LimitPtrOffMSP 12
#define StorePtrOffMSP 108
#define RootsOffMSP 16
#define PCOffMSP (RootsOffMSP+16)
#define StdArgOffMSP (RootsOffMSP+0)
#define StdContOffMSP (RootsOffMSP+4)
#define StdClosOffMSP (RootsOffMSP+8)
#define ExnPtrOffMSP (RootsOffMSP+12)
#define BasePtrOffMSP (RootsOffMSP+80)
#define VarPtrOffMSP (RootsOffMSP+76)
#define LinkRegOffMSP (RootsOffMSP+20)
#define MiscRegOffMSP(i) (40+(4*(i)))
#define PseudoReg1OffMSP 100
#define PseudoReg2OffMSP 104
#define MaskOffMSP 112
#define InMLOffVSP 8
#define HandlerPendingOffVSP 12
#define InSigHandlerOffVSP 16
#define NPendingSysOffVSP 20
#define NPendingOffVSP 24

#endif /* !_MLSTATE_OFFSETS_ */
