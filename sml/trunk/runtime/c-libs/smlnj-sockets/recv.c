/* recv.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_recv : (sock * int * bool * bool) -> int
 *
 * The arguments are: socket, number of bytes, OOB flag and peek flag; the
 * result is the vector of bytes received.
 */
ml_val_t _ml_Sock_recv (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    int		nbytes = REC_SELINT(arg, 1);
    int		flag = 0;
    ml_val_t	vec, res;
    int		m, n;
    char	*s;

    if (REC_SEL(arg, 2) == ML_true) flag |= MSG_OOB;
    if (REC_SEL(arg, 3) == ML_true) flag |= MSG_PEEK;

  /* allocate the vector; note that this might cause a GC */
    vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS(nbytes));

    s = PTR_MLtoC(char, vec);
    n = recv (sock, s, nbytes, flag);

    if (n < 0)
	return RAISE_SYSERR(msp, sts);
    else if (n == 0)
	return ML_string0;

  // pad the last word of the vector with zero bytes so that string pattern
  // matching on the result will work.
    for (m = n;  (m & (WORD_SZB-1)) != 0;  m++) {
	s[m] = '\0';
    }

    if (n < nbytes) {
      /* we need to shrink the vector */
	ML_ShrinkRaw32 (msp, vec, BYTES_TO_WORDS(n));
    }

    SEQHDR_ALLOC (msp, res, DESC_string, vec, n);

    return res;

} /* end of _ml_Sock_recv */

