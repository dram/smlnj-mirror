/* prim.h
 * Created 7/15/90 by David Tarditi for ML->C
 *
 * Define types of assembly language or C values to be used directly by
 * ML.
 *
 */

#ifndef _PRIM_
#define _PRIM_
extern int datalist[];
extern int array_v[];
extern int callc_v[];
extern int create_b_v[];
extern int create_s_v[];
extern int create_v_v[];
extern int floor_v[];
extern int handle_c[];
extern int logb_v[];
extern int return_c[];
extern int scalb_v[];
extern int try_lock_v[];
extern int unlock_v[];
extern int sigh_return_c[];

#ifdef C
extern int sigh_resume();
extern unsigned int Cmask;
extern int *plimit;
#else
extern int sigh_resume[];
#endif

extern int arctan_v[],cos_v[],exp_v[],ln_v[],sin_v[],sqrt_v[];
#endif !_PRIM_
