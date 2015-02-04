/* modifications here must be duplicated in codegen/codegen.sml,
   vax/vaxprim.sml, and m68/m68prim.sml */
#define ARRAYS 0
#define ARRAYSIZE 1
#define STRINGS 2
#define STRINGSIZE 3
#define REFCELLS 4
#define REFLISTS 5
#define LINKS 6
#define LINKSLOTS 256
#define BIGLINKS (LINKS+LINKSLOTS)
#define CLOSURES (BIGLINKS+1)
#define CLOSURESLOTS 256
#define BIGCLOSURES (CLOSURES+CLOSURESLOTS)
#define RECORDS (BIGCLOSURES+1)
#define RECORDSLOTS 5
#define BIGRECORDS (RECORDS+RECORDSLOTS)
#define PROFSIZE (BIGRECORDS+1)
