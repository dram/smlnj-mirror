#define _div_e (_div_e0+4)
#define _float_e (_float_e0+4)
#define _interrupt_e (_interrupt_e0+4)
#define _overflow_e (_overflow_e0+4)
#define _systemcall_e (_systemcall_e0+4)

#ifdef BSD
#define FIONREAD 0x4004667f
#define TIOCGETP 0x40067408
#endif
#ifdef V9
#define FIONREAD 0x667f
#define TIOCGETP 0x7408
#endif
