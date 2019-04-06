#include "tags.h"
#include "prof.h"
#include "ml.h"

extern int datalist[];

struct {int tag; char s[4]} div_s = {mak_desc(3,tag_string), "Div\0"};

int div_e0[]={mak_desc(2,tag_record),
	     1,
	     (int) (div_e0+4),
	     mak_desc(1,tag_array),
	     (int) div_s.s};

struct {int tag; char s[8]} overflow_s = {mak_desc(8,tag_string), "Overflow"};

int overflow_e0[]={mak_desc(2,tag_record),
	     1,
	     (int) (overflow_e0+4),
	     mak_desc(1,tag_array),
	     (int) overflow_s.s};

struct {int tag; char s[12]} interrupt_s = 
	{mak_desc(9,tag_string), "Interrupt\0\0\0"};

int interrupt_e0[]={mak_desc(2,tag_record),
	     1,
	     (int) (interrupt_e0+4),
	     mak_desc(1,tag_array),
	     (int) interrupt_s.s};

struct {int tag; char s[12]} systemcall_s = 
	{mak_desc(10,tag_string), "SystemCall\0\0"};

int systemcall_e0[]={mak_desc(1,tag_array),
	            (int) systemcall_s.s};

struct {int tag; char s[12]} float_s = 
	{mak_desc(5,tag_string), "Float\0\0\0"};

int float_e0[]={mak_desc(1,tag_array),
	            (int) float_s.s};

int array0_v[]={mak_desc(0,tag_array)};

int bytearray0_v[]={mak_desc(0,tag_bytearray)};

extern int collected0[];
extern int collectedfrom0[];
extern int current0[];
extern int gcmessages0[];
extern int majorcollections0[];
extern int minorcollections0[];
extern int pstruct0[];
extern int ratio0[];

int *cstruct[]={ 
   (int*)mak_desc(9,tag_record),
   div_e0+1,
   float_e0+1,
   interrupt_e0+1,
   overflow_e0+1,   		
   systemcall_e0+1,
   array0_v+1,
   bytearray0_v+1,
   collected0+1,
   collectedfrom0+1,
   current0+1,
   datalist,
   (int*)1,  /* external list */
   gcmessages0+1,
   majorcollections0+1,
   minorcollections0+1,
#ifdef V9
   (int*)7,
#else
#ifdef VAX
   (int*)3,
#else
   (int*)5,
#endif
#endif
   pstruct0+1,
   ratio0+1
};
