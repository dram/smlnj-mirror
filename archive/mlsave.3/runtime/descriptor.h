/* needs tags.h */

#define is_ptr(x) (!((int)(x)&1))
#define mak_int(x) ((int)(x)*2+1)
#define mask_tags (power_tags-1)
#define get_len(x) (*(int *)(x)>>width_tags)
#define get_tag(x) (*(int *)(x)&mask_tags)

typedef struct mem {
#ifdef VAX
	unsigned int flg:width_tags;
	int len:32-width_tags;
#endif
#ifdef M68
	int len:32-width_tags;
	unsigned int flg:width_tags;
#endif
} mem;

struct string {
    int n;
    char s[4];
};
#define get_strlen(x) (is_ptr(x) ? ((struct string *)(x))->n : 1)
extern char *get_str();
