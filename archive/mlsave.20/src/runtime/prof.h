/* modifications here must be duplicated in codegen/codegen.sml,
   vax/vaxprim.sml, and m68/m68prim.sml */
#define ARRAYS		0
#define ARRAYSIZE	1
#define STRINGS		2
#define STRINGSIZE	3
#define REFCELLS	4
#define REFLISTS	5
#define CLOSURES	6
#define CLOSURESLOTS	21
#define CLOSUREOVFL	(CLOSURES + CLOSURESLOTS)
#define RECORDS		(CLOSUREOVFL + 1)
#define RECORDSLOTS	21
#define RECORDOVFL	(RECORDS + RECORDSLOTS)
#define SPILLS		(RECORDOVFL + 1)
#define SPILLSLOTS	21
#define SPILLOVFL	(SPILLS + SPILLSLOTS)
#define KNOWNCALLS	(SPILLOVFL + 1)
#define STDKCALLS	(KNOWNCALLS + 1)
#define STDCALLS	(STDKCALLS + 1)
#define PROFSIZE	(STDCALLS+1)
