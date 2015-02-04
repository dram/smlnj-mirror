extern chatting();
#define printf chatting
#define plural(a,b,c) (((a)==1)?(b):(c))

#ifdef PROFILE

#include <stdio.h>
#include "prof.h"

int profvec[PROFSIZE];

int num_closures = 0;
int space_closures = 0;
int num_closure_accesses = 0;
int num_links_traced = 0;
int num_records = 0;
int space_records = 0;
int total = 0;
int descriptors = 0;

#define links(i) (profvec[LINKS+i])
#define closures(i) (profvec[CLOSURES+i])
#define records(i) (profvec[RECORDS+i])

find_profile_info()
{
	int i;

	for (i = 0; i < CLOSURESLOTS; i++)
		num_closures += closures(i);
	for (i = 1; i < CLOSURESLOTS; i++)
		space_closures += closures(i) * (i+1);
	space_closures += profvec[BIGCLOSURES] + closures(0);

	for (i = 0; i < LINKSLOTS; i++)
		num_closure_accesses += links(i);
	for (i = 1; i < LINKSLOTS; i++)
		num_links_traced += links(i) * i;
	num_links_traced += profvec[BIGLINKS];

	for (i = 0; i < RECORDSLOTS; i++)
		num_records += records(i);
	for (i = 1; i < RECORDSLOTS; i++)
		space_records += records(i) * (i+1);
	space_records += profvec[BIGRECORDS] + records(0);

	total = space_closures + space_records
		+ profvec[ARRAYSIZE] + profvec[ARRAYS]
		+ profvec[STRINGSIZE] + profvec[STRINGS]
		+ profvec[REFCELLS] * 2
		+ profvec[REFLISTS] * 2;

	descriptors = num_closures + num_records
		 + profvec[ARRAYS] + profvec[STRINGS]+ profvec[REFCELLS];
}

print_alloc_info()
{
	int k; float f;
	char *s,*s1;

	printf("\nProfiling information:\n\n");

	s = plural(num_closure_accesses,"time","times");
	printf("Closure elements were accessed %d %s:\n",num_closure_accesses,s);
	for (k = 1; k < LINKSLOTS; k++) {
		if (links(k) > 0) {
			s = plural(links(k),"time","times");
			s1 = plural(k,"link","links");
			printf("%d %s through %d %s;\n",links(k),s,k,s1);
		}
	}
	s = plural(links(0),"time","times");
	if (links(0) > 0) printf("%d %s through greater than %d links;\n",
					links(0),s,LINKSLOTS - 1);
	s1 = plural(num_links_traced,"link","links");
	printf("a total of %d %s were traced.\n",num_links_traced,s1);
	f = (float)num_links_traced/(float)num_closure_accesses;
	s = plural(f,"link was","links were");
	printf("%g %s traced per access on average.\n\n", f, s);


	printf("Heap allocations: (only total sizes include descriptors)\n");
	printf("Total size %d; %d descriptors accounted for %.1f%%.\n\n",
		total, descriptors, 100.0*descriptors/total);

	printf("  Size   Number   %% total   Total size    %% total\n\n");
	printf("Closures:\n");
	for (k = 1; k < CLOSURESLOTS; k++)
		if (closures(k) > 0)
		printf("%6d%9d%9.1f%%%13d%10.1f%%\n",
			k,
			closures(k),
			100.0*closures(k)/num_closures,
			closures(k) * (k+1),
			100.0*(closures(k)*(k+1))/total);
	if (closures(0) > 0)
		printf(">%5d%9d%9.1f%%%13d%10.1f%%\n",
			CLOSURESLOTS - 1,
			closures(0),
			100.0*closures(0)/num_closures,
			profvec[BIGCLOSURES]+closures(0),
			100.0*(profvec[BIGCLOSURES]+closures(0))/total);
	printf("Total:%9d%23d%10.1f%%  Average size %.2f\n\n",
	    num_closures, space_closures, 100.0*space_closures/total,
	    (float)(space_closures-num_closures)/num_closures);

	printf("Records:\n");
	for (k = 1; k < RECORDSLOTS; k++)
		if (records(k) > 0)
		printf("%6d%9d%9.1f%%%13d%10.1f%%\n",
			k,
			records(k),
			100.0*records(k)/num_records,
			records(k) * (k+1),
			100.0*(records(k) * (k+1)) / total);
	if (records(0) > 0)
		printf(">%5d%9d%9.1f%%%13d%10.1f%%\n",
			RECORDSLOTS - 1,
			records(0),
			100.0*records(0)/num_records,
			profvec[BIGRECORDS] + records(0),
			100.0*(profvec[BIGRECORDS] + records(0)) / total);
	printf("Total:%9d%23d%10.1f%%  Average size %.2f\n\n",
	    num_records,space_records,100.0*space_records/total,
	    (float)(space_records-num_records)/num_records);

	if (!profvec[ARRAYS]) printf("Arrays:  0 created.\n");
	else printf("Arrays:%8d%23d%10.1f%%  Average size %.2f\n",
		profvec[ARRAYS],
		profvec[ARRAYSIZE] + profvec[ARRAYS],
		100.0*(profvec[ARRAYSIZE] + profvec[ARRAYS]) / total,
		(float)profvec[ARRAYSIZE]/profvec[ARRAYS]);

	if (!profvec[STRINGS]) printf("Strings:  0 created.\n");
	else printf("Strings:%7d%23d%10.1f%%  Average size %.2f\n",
		profvec[STRINGS],
		profvec[STRINGSIZE] + profvec[STRINGS],
		100.0*(profvec[STRINGSIZE] + profvec[STRINGS]) / total,
		(float)profvec[STRINGSIZE]/profvec[STRINGS]);

	if (!profvec[REFCELLS]) printf("Refs:  0 created.\n");
	else printf("Refs:%10d%23d%10.1f%%\n",
		profvec[REFCELLS],
		profvec[REFCELLS]*2,
		100.0*(profvec[REFCELLS]*2) / total);

	if (!profvec[REFLISTS]) printf("Reflists:  0 created.\n");
	else printf("Reflist\ncells:%9d%23d%10.1f%%\n",
		profvec[REFLISTS],
		profvec[REFLISTS]*2,
		100.0*(profvec[REFLISTS]*2) / total);

}


print_profile_info()
{
	find_profile_info();
	if (num_closures > 0 || num_records > 0) print_alloc_info();
}

#endif

#ifdef GCPROFILE

extern int collected,collectedfrom,minorcollections,majorcollections;
extern timer();
extern int g_sec,g_usec,t_sec,t_usec;

print_gcprof()
{
 int g,gu,t,tu;
 char *s0,*s1;

 printf("Garbage collection information:\n");
 s0 = plural(minorcollections,"","s");
 s1 = plural(majorcollections,"","s");
 printf("%d minor collection%s, %d major collection%s\n",
	minorcollections,s0,majorcollections,s1);
 if (collectedfrom)
 printf("%d collected from %d possible (%.1f%%)\n",
	collected,collectedfrom,100.0*collected/collectedfrom);
 timer();
 g = (g_sec-1)/2; gu = (g_usec-1)/2;
 t = (t_sec-1)/2; tu = (t_usec-1)/2;
 printf("Total process time was %d.%.6d\n",t,tu);
 if (t+tu)
 printf("Total garbage collection time was %d.%.6d (%.1f%%)\n",
	g,gu,100.0*(1000000.0*g+gu)/(1000000.0*t+tu));
}

#endif
