#include "tags.h"
#include "descriptor.h"
#include "ml.h"
#define refcell(z) int z[2] = {mak_desc(1,tag_array), ML_INT(0)};

refcell(collected0)
refcell(collectedfrom0)
refcell(gcmessages0)
refcell(majorcollections0)
refcell(minorcollections0)
int collected[] = &collected0[1];
int collectedfrom[] = &collectedfrom0[1];
int *gcmessages = &gcmessages0[1];
int *majorcollections = &majorcollections0[1];
int *minorcollections = &minorcollections0[1];
int *control_v0[] = {(int*)mak_desc(6,tag_record),
		     collected, collectedfrom,
		     gcmessages,
		     majorcollections,minorcollections};
int *control_v[] = &control_v[1];
main()
{ 0; }

/*
int *runvec[] = {&div_e,
		 &float_e,
		 &interrupt_e,
		 &overflow_e,
		 &systemcall_e,
		 &array_v,
		 &array0_v,
		 &boot_v,
		 &byte_array0_v,
		 &chdir_v,
		 &close_v,
		 &control_v,
		 &create_b_v,
		 &create_s_v,
		 &dup2_v,
		 &execute_v,
		 &exportFn_v,
		 &exportML_v,
		 &fionread_v,
		 &floor_v,
		 &fork_v,
		 &isatty_v,
		 &logb_v,
		 &openf_v,
		 &pipe_v,
		 &profvec_v,
		 &pstruct_v,
		 &read_v,
		 &scalb_v,
		 &seql_v,
		 &system_v,
		 &timer_v,
		 &write_v};
int *runvec2 = &runvec[1];

*/
	.align 2
	.long mak_desc(2,tag_record)
_div_e:	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,3,"Div\0")

	.align	2
	.long mak_desc(2,tag_record)
_overflow_e:
	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,8,"Overflow")
