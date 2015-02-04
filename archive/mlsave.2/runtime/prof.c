#include <stdio.h>
#define printf chatting

#define ARRAYS 0
#define ARRAYSIZE 1
#define STRINGS 2
#define STRINGSIZE 3
#define LINKS 4
#define LINKSLOTS 256
#define BIGLINKS (LINKS+LINKSLOTS)
#define CLOSURES (BIGLINKS+1)
#define CLOSURESLOTS 256
#define BIGCLOSURES (CLOSURES+CLOSURESLOTS)
#define RECORDS (BIGCLOSURES+1)
#define RECORDSLOTS 5
#define BIGRECORDS (RECORDS+RECORDSLOTS)
#define PROFSIZE (BIGRECORDS+1)

int profvec[PROFSIZE];

int num_closures = 0;
int space_closures = 0;
int num_closure_accesses = 0;
int num_links_traced = 0;
int num_records = 0;
int space_records = 0;


#define links(i) (profvec[LINKS+i])
#define closures(i) (profvec[CLOSURES+i])
#define records(i) (profvec[RECORDS+i])

find_profile_info()
{
	int i;

	for (i = 0; i < CLOSURESLOTS; i++)
		num_closures += closures(i);
	for (i = 1; i < CLOSURESLOTS; i++)
		space_closures += closures(i) * i;
	space_closures += profvec[BIGCLOSURES];

	for (i = 0; i < LINKSLOTS; i++)
		num_closure_accesses += links(i);
	for (i = 1; i < LINKSLOTS; i++)
		num_links_traced += links(i) * i;
	num_links_traced += profvec[BIGLINKS];

	for (i = 0; i < RECORDSLOTS; i++)
		num_records += records(i);
	for (i = 1; i < RECORDSLOTS; i++)
		space_records += records(i) * i;
	space_records += profvec[BIGRECORDS];

}

#define plural(a,b,c) (((a)==1)?(b):(c))

print_alloc_info()
{
	int k;
	char *s,*s1;

	printf("\nProfiling information: (sizes do not include descriptor)\n\n");
	s = plural(num_closures,"closure","closures");
	printf("%d %s created,\n",num_closures,s);
	for (k = 1; k < CLOSURESLOTS; k++) {
		if (closures(k) > 0)
			printf("%d of size %d,\n",closures(k),k);
	}
	if (closures(0) > 0) printf("%d of size greater than %d,\n",
					closures(0),CLOSURESLOTS - 1);
	printf("total size %d words, average size %g words.\n\n",
	    space_closures,
	    (float)space_closures/(float)num_closures);

	s = plural(num_closure_accesses,"time","times");
	printf("Closure elements were accessed %d %s\n",num_closure_accesses,s);
	for (k = 1; k < LINKSLOTS; k++) {
		if (links(k) > 0) {
			s = plural(links(k),"time","times");
			s1 = plural(k,"link","links");
			printf("%d %s through %d %s\n",links(k),s,k,s1);
		}
	}
	s = plural(links(0),"time","times");
	if (links(0) > 0) printf("%d %s through greater than %d links\n",
					links(0),s,LINKSLOTS - 1);
	s1 = plural(num_links_traced,"link","links");
	printf("A total of %d %s were traced.\n",num_links_traced,s1);
	printf("%g links were traced per access on average.\n",
		(float)num_links_traced/(float)num_closure_accesses);

	printf("\n");

	s = plural(num_records,"record","records");
	printf("%d %s created,\n",num_records,s);
	for (k = 1; k < RECORDSLOTS; k++) {
		if (records(k) > 0)
			printf("%d of size %d,\n",records(k),k);
	}
	if (records(0) > 0) printf("%d of size greater than %d,\n",
					records(0),RECORDSLOTS - 1);
	printf("total size %d words, average size %g words.\n\n",
	    space_records,
	    (float)space_records/(float)num_records);

	if (!profvec[ARRAYS]) printf("Arrays:  %d created\n",
					profvec[ARRAYS]);
	else printf("Arrays:  %d created, total size %d words, average size %g words.\n",
		profvec[ARRAYS],
		profvec[ARRAYSIZE],
		(float)profvec[ARRAYSIZE]/(float)profvec[ARRAYS]);

	if (!profvec[STRINGS]) printf("Strings:  %d created\n",
					profvec[STRINGS]);
	else printf("Strings:  %d created, total size %d words, average size %g words.\n",
		profvec[STRINGS],
		profvec[STRINGSIZE],
		(float)profvec[STRINGSIZE]/(float)profvec[STRINGS]);

}


print_profile_info()
{
	find_profile_info();
	if (num_closures > 0 || num_records > 0) print_alloc_info();
}
