/* linkdata.c -- group files into the text segment of a .o file */

#define	SYMBOL	"_datalist"

#include <stdio.h>
#include <a.out.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>

#define	LONG		(sizeof (long))
#define	align2(x)	(((x) + LONG - 1) &~ (LONG - 1))

struct relocation_info rconst, *rel;

void die(s)
	char *s;
{
	perror(s);
	exit(1);
}

long datalist = 1;

extern int brk();
extern long *sbrk();	/* wrong but convenient */


int link(file, pc, last)
	char *file; long pc; int last;
{
	int fd;
	long i, len, n, nbytes;
	char *s;
	struct stat st;
	static int bufsize = 0;
	static long *buf;

	if (datalist == 1)
		datalist = 4;

	if ((fd = open(file, 0)) == -1)
		die("open");
	if (fstat(fd, &st) == -1)
		die("stat");

	n = st.st_size;
	len = strlen(file);
	nbytes = align2(n) + align2(len) + 6 * LONG;

	if (n + len + 40 > bufsize) {
		bufsize = n + len + 1024;
		if (buf == 0) {
			if ((buf = sbrk(bufsize)) == (long *) -1)
				die("sbrk");
		} else if (brk(buf + bufsize) == -1)
			die("brk");
	}

	buf[0] = 0x31;
	buf[1] = pc + 5 * LONG;
	buf[2] = buf[1] + LONG + align2(len);
	if (last)
		buf[3] = 1;
	else
		buf[3] = pc + nbytes + LONG;
	buf[4] = len * 16 + 15;

	strcpy(&buf[5], file);
	s = ((char *) &buf[5]) + len;
	for (i = align2(len); i > len; i--)
		*s++ = 0;
	*((long *) s) = n * 16 + 15;
	s += LONG;

	if (read(fd, s, n) != n)
		die("read");
	s += n;
	for (i = align2(n); i > n; i--)
		*s++ = 0;

	if (close(fd) == -1)
		die("close");

#define	REL(addr)	(*rel = rconst, rel++->r_address = (addr))

	REL(pc + LONG);
	REL(pc + 2 * LONG);
	if (!last)
		REL(pc + 3 * LONG);

	assert(s - nbytes == (char *) buf);
	write(1, buf, nbytes);
	return nbytes;
}

int main(argc, argv)
	int argc;
	char *argv[];
{
	long pc = 0;
	struct exec exec;
	struct nlist nl;
	struct relocation_info *relbase;
	struct {
		long len;
		char data[100];
	} str;

	/* fake a relocation constant to assign from */
	rconst.r_address   = 0;
	rconst.r_symbolnum = 4; /* i don't know why this works.  magic? */
	rconst.r_pcrel     = 0;
	rconst.r_length    = 2; /* long */
	rconst.r_extern    = 0;

	/* fill the rest in later */
#ifdef sun
	exec.a_machtype	= M_68020;
#endif
	exec.a_magic	= OMAGIC;
	exec.a_data	= 0;
	exec.a_bss	= 0;
	exec.a_syms	= sizeof nl;
	exec.a_entry	= 0;
	exec.a_drsize	= 0;
	if (write(1, &exec, sizeof exec) == -1)
		die("write");

	rel = relbase = (struct relocation_info *) sbrk(3 * argc * sizeof *relbase);
	if ((long) rel == -1)
		die("sbrk(rel)");

	{int i;
	 for (i=1;i<argc;i++)
		pc += link(argv[i], pc, i==argc-1);
        }

	exec.a_text	= pc;
	exec.a_trsize	= (rel - relbase) * sizeof *rel;

	if (write(1, relbase, exec.a_trsize) == -1)
		die("write(rel)");

	nl.n_un.n_strx = 4;		/* first symbol table entry */
	nl.n_other = 0;
	nl.n_desc  = 0;

	if (argc>1)
	 {nl.n_type = N_EXT | N_TEXT;	/* global in text area */
	  nl.n_value = relbase[0].r_address;	/* why bother? it's 4 */
	 }
	else {nl.n_type = N_EXT | N_ABS;
	      nl.n_value = 1;
	     }

	if (write(1, &nl, sizeof nl) == -1)
		die("write(nl)");

	strcpy(str.data, SYMBOL);
	str.len = LONG + strlen(SYMBOL) + 1;
	if (write(1, &str, str.len) == -1)
		die("write(str)");

	if (lseek(1, 0, 0L) == -1)
		die("lseek");
	if (write(1, &exec, sizeof exec) == -1)
		die("write(exec)");
	return 0;
}
