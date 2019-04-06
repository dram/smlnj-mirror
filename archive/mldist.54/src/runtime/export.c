/* Copyright 1989 by AT&T Bell Laboratories */
#include "tags.h"
#ifdef NeXT
#include <sys/loader.h>
#else
#include <a.out.h>
#endif NeXT

#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

#if MIPSEL || MIPSEB
#define MIPS 1
#endif

#ifdef MIPS
#define N_DATADDR 0x10000000
#define N_TXTADDR 0x400000 /* start of headers; text segment follows */
#endif

#ifdef HPUX
#define N_DATADDR DATA_OFFSET
#endif HPUX



#ifdef NS32
#define N_DATADDR(x)	0x400000
#endif NS32

#ifndef N_DATADDR
#define N_DATADDR(x)  CEIL((x).a_text,getpagesize())
#endif

#ifndef N_TXTADDR
#ifdef NeXT
#include <machine/vm_param.h>
#ifdef USRTEXT
#define N_TXTADDR(x)  USRTEXT
#endif USRTEXT
#endif
#ifdef VAX
#define N_TXTADDR(x)  0
#endif
#ifdef NS32
#define N_TXTADDR(x)  0
#define exec aouthdr
#define a_magic		magic
#define	a_text		tsize
#define a_data		dsize
#define	a_bss		bsize
#define	a_entry		entry
#define	a_trsize	text_start
#define	a_drsize	data_start
#endif NS32
#ifndef sony_news
#ifdef M68
#ifdef HPUX
#define N_TXTADDR TEXT_OFFSET
#else
#ifdef MORE
#define N_TXTADDR(x) 0
#else
#define N_TXTADDR(x)  getpagesize()
#endif MORE
#endif HPUX
#endif
#else sony_news
#define N_TXTADDR(x)  0
#endif sony_news
#ifdef SPARC
#include <machine/vmparam.h>
#define N_TXTADDR(x)  USRTEXT
#endif
#endif

#ifdef HPUX
#define ZMAGIC SHARE_MAGIC
#endif HPUX

 
/* Garbage collection is already done.
 * Data to be saved is:
 *  0 -> ceil(etext)		 text
 *  ceil(etext) -> arenabase	 data
 *  arenabase -> old_high	 heap
 *  
 *  > set a_entry as address of start procedure
 */

extern int etext;   /* &etext is just beyond the end of the text segment */
extern int old_high;

static int textstart,datastart;

#ifdef MIPS
extern long startptr[];		/* fool damn global pointer stuff */
#else
extern int startptr;
#endif

#ifdef HPUX
getpagesize(){return 4096;}
#endif HPUX

#ifdef V9
getpagesize(){return 1024;}
#endif

#ifndef NeXT
#ifndef MIPS
export (filid)
    int filid;
{
    int bytcount;
   static struct exec E;  /* make it static so all fields=0 */

#ifdef HPUX
    memset(&E,0,sizeof(E));
    E.a_magic.file_type = ZMAGIC;
#else
#if SUN3 || SPARC
    E.a_magic = NMAGIC;
#else
#ifdef NS32
    E.a_magic = NS32GMAGIC;
#else
    E.a_magic = ZMAGIC;
#endif
#endif
#endif

#ifdef HPUX
    E.a_magic.system_id = HP9000S200_ID;
#else
#ifdef M68
#ifndef sony_news
    E.a_machtype = 2;	/* M_68020 */
#endif sony_news
#ifdef MORE
    E.a_machtype = MID_HP300; /* M_68020 */
#else
    E.a_machtype = M_68020; /* M_68020 */
#endif MORE
#endif
#ifdef SPARC
    E.a_toolversion = 1;
    E.a_machtype = M_SPARC;
#endif
#endif HPUX

    textstart = N_TXTADDR(E);
#ifdef HPUX
    E.a_text = (int) CEIL(((int)&etext+textstart),getpagesize())-textstart;
#else
    E.a_text = (int) CEIL(((int)&etext),getpagesize())-textstart;
    datastart = N_DATADDR(E);
    E.a_bss = 0;
#ifndef NS32
    E.a_syms = 0;
#endif NS32
#endif HPUX
    E.a_entry = startptr;
    E.a_trsize = 0;
    E.a_drsize = 0;
#ifdef HPUX
    E.a_data = CEIL(old_high-(int)&etext, getpagesize());
#else
    E.a_data = CEIL(old_high-datastart, getpagesize());
#endif HPUX
    filid >>= 1;
    fchmod(filid,0755);
#ifdef NS32
    coff_cough(filid,&bytcount,E.a_text,E.a_data,E.a_entry);
#else
    bulletproofWrite(filid,&E,sizeof(E));
    bytcount = sizeof(E);
#endif NS32
#if VAX || NS32
    {int i, nzeros = getpagesize()-sizeof(E);
	char zeros[1024];
        for(i=0;i<nzeros;i++) zeros[i]=0;
        bulletproofWrite(filid,zeros,nzeros);

    }
#endif
#ifdef sony_news || MORE
    {int i, nzeros = getpagesize()-bytcount;
     char zeros[4096];
     for(i=0;i<nzeros;i++) zeros[i]=0;
     bulletproofWrite(filid,zeros,nzeros);
    }
#endif sony_news
#ifdef HPUX
    lseek(filid,N_TXTADDR(E),0);
    bulletproofWrite(filid,0,E.a_text);
    lseek(filid,N_DATADDR(E),0);
    bulletproofWrite(filid,CEIL((int)&etext,getpagesize()),E.a_data);
#else
    bulletproofWrite(filid,textstart,E.a_text);
    bulletproofWrite(filid,datastart,E.a_data);
#endif HPUX
}
#ifdef NS32
coff_cough(fd,countp,tsize,dsize,entry)
int fd, *countp, tsize, dsize, entry;
{
static struct filehdr fhdr;
static struct aouthdr ahdr;
static struct scnhdr thdr;
static struct scnhdr dhdr;
static struct scnhdr bhdr;
int allhdrsize = sizeof(fhdr) + sizeof(ahdr) + 3*sizeof(thdr);
int pagesize = getpagesize();

fhdr.f_magic = NS32GMAGIC;
fhdr.f_nscns = 3;
fhdr.f_timdat = /* don't care */ 0;
fhdr.f_symptr = /* null? */ 0;
fhdr.f_nsyms = 0;
fhdr.f_opthdr = sizeof(struct aouthdr);
fhdr.f_flags = F_RELFLG|F_EXEC|F_LNNO|F_LSYMS|F_AR32WR;

ahdr.magic = /* OOPS */ 0x010b;
ahdr.vstamp = /* don't care */ 0;
ahdr.tsize = tsize;
ahdr.dsize = dsize;
ahdr.bsize = 0;
ahdr.msize = /* OOPS */ 0x10;
ahdr.mod_start = /* OOPS */ 0x20;
ahdr.entry = entry;
ahdr.text_start = 0;
ahdr.data_start = 0x400000;
ahdr.entry_mod = /* unused? */ 0;
ahdr.flags = U_SYS_42|U_AL_4096;

strncpy(thdr.s_name,_TEXT,8);
thdr.s_paddr = thdr.s_vaddr = ahdr.data_start;
thdr.s_size = ahdr.tsize;
thdr.s_scnptr = CEIL(allhdrsize,pagesize);
thdr.s_relptr = /* null? */ 0;
thdr.s_lnnoptr = /* null? */ 0;
thdr.s_nreloc = 0;
thdr.s_nlnno = 0;
thdr.s_flags = STYP_TEXT;
thdr.s_symptr = /* null? */ 0;
thdr.s_modno = /* OOPS */ 0;
thdr.s_pad = /* don't care */ 0;

strncpy(dhdr.s_name,_DATA,8);
dhdr.s_paddr = dhdr.s_vaddr = ahdr.data_start;
dhdr.s_size = ahdr.dsize;
dhdr.s_scnptr = thdr.s_scnptr + CEIL(thdr.s_size,pagesize);
dhdr.s_relptr = /* null? */ 0;
dhdr.s_lnnoptr = /* null? */ 0;
dhdr.s_nreloc = 0;
dhdr.s_nlnno = 0;
dhdr.s_flags = STYP_DATA;
dhdr.s_symptr = /* null? */ 0;
dhdr.s_modno = /* OOPS */ 0;
dhdr.s_pad = /* don't care */ 0;

strncpy(bhdr.s_name,_BSS,8);
bhdr.s_paddr = bhdr.s_vaddr = ahdr.data_start + ahdr.dsize;
bhdr.s_size = /* none */ 0;
bhdr.s_scnptr = /* null */ 0;
bhdr.s_relptr = /* null? */ 0;
bhdr.s_lnnoptr = /* null? */ 0;
bhdr.s_nreloc = 0;
bhdr.s_nlnno = 0;
bhdr.s_flags = STYP_BSS;
bhdr.s_symptr = /* null? */ 0;
bhdr.s_modno = /* OOPS */ 0;
bhdr.s_pad = /* don't care */ 0;
 bulletproofWrite(fd,&fhdr,sizeof(fhdr));
 bulletproofWrite(fd,&ahdr,sizeof(ahdr));
 bulletproofWrite(fd,&thdr,sizeof(thdr));
 bulletproofWrite(fd,&dhdr,sizeof(dhdr));
 bulletproofWrite(fd,&bhdr,sizeof(bhdr));
 *countp = allhdrsize;
}
#endif NS32
#else MIPS

/* See MIPS Assembly language programmer's guide, 
   Chapter 9 (Object file format) */

#include <stdio.h>
/* following macro works only with the old-style preprocessor */
#define insist(condition) do { if (!(condition)) { \
    chatting("==> Inconsistent object for export: !(%s)\n", "condition"); \
    return; } } while(0)
/* #define verbose chatting */
static verbose(){}

#undef export

export (filid)
    int filid;
{
     /* plan: increase .data section to get up to old_high, make following
       sections zero */

    int bytcount;
    int i;
    int dataremaining;		/* # of bytes left in data *segment* */
    int textcount;		/* # of bytes in text segment */
    int seendata;		/* true iff have seen .data *section* hdr */
#define HDBUF 512
   char headerbuffer[HDBUF];
    

    /* MIPS stores header information at N_TXTADDR */
    struct filehdr *fh = (struct filehdr *) headerbuffer;
    struct aouthdr *ah = (struct aouthdr *) (headerbuffer + sizeof(*fh));
    struct scnhdr *sh = (struct scnhdr *) (headerbuffer + sizeof(*fh) + sizeof(*ah));

    int headersize = sizeof(*fh)+sizeof(*ah)+
                       (((struct filehdr *) N_TXTADDR)->f_nscns * sizeof(*sh));



    headersize = CEIL(headersize,16);

    insist(headersize <= HDBUF);

    memcpy(headerbuffer, N_TXTADDR, headersize);  /* copy headers for mods */

    /* validate & adjust file header */
#ifdef MIPSEB
    insist(fh->f_magic == MIPSEBMAGIC);
#else
    insist(fh->f_magic == MIPSELMAGIC);
#endif MIPSEB

    if (fh->f_symptr || fh->f_nsyms) {
	verbose("Discarding %d bytes of symbolic header & symbol table info\n",
		fh->f_nsyms);
	fh->f_symptr = 0;
	fh->f_nsyms = 0;
    }
    insist(fh->f_opthdr == sizeof(*ah));
    fh->f_flags = F_RELFLG | F_EXEC | F_LNNO | F_LSYMS; /* stripped */
    verbose("validated file header\n");

    /* validate & adjust optional header */
    insist(ah->magic == ZMAGIC);
    insist(ah->tsize == CEIL (((int) &etext)-N_TXTADDR, getpagesize()));
    ah->dsize = CEIL(old_high - N_DATADDR,getpagesize());
    ah->bsize = 0;
    insist(ah->entry == startptr[0]);
    insist(ah->text_start == N_TXTADDR);
    insist(ah->data_start == N_DATADDR);
    ah->bss_start = CEIL(ah->data_start+ah->dsize, getpagesize());
    /* don't change masks or gp value */
    verbose("validated optional header\n");

    /* adjust section headers */
    textcount = 0;
    dataremaining = ah->dsize;
    seendata = 0;
    for (i=0; i<fh->f_nscns; i++, sh++) {
	insist(sh->s_paddr == sh->s_vaddr);
	insist(sh->s_relptr == 0);
	sh->s_lnnoptr = 0;	/* Destroy gp tables */
	insist(sh->s_nreloc == 0);
	insist(sh->s_nlnno == 0);
	switch(sh->s_flags) {
	  case STYP_TEXT:
	    insist(sh->s_scnptr == headersize);	/* fall through */
	  case STYP_INIT:
	    textcount += sh->s_size;
	    break;
	  case STYP_RDATA:
	  case STYP_DATA:
	  case STYP_LIT8:
	  case STYP_LIT4:
	  case STYP_SDATA:
	  case STYP_SBSS:
	  case STYP_BSS:
	    if (seendata) { /* turn into a dummy section */
		sh->s_paddr = sh->s_vaddr = ah->data_start + ah->dsize;
		sh->s_size = 0;
		sh->s_scnptr = 0;
	    } else {
		insist((sh->s_flags & (STYP_SBSS | STYP_BSS)) == 0);
		if (sh->s_flags == STYP_DATA) {
		    sh->s_size = dataremaining;
		    seendata = 1;
		} else {
		    dataremaining -= sh->s_size;
		}
	    }
	    break;
	  default: insist(0);
	}
	verbose("Adjusted %s section\n", sh->s_name);
    }
    insist(CEIL(textcount + headersize,getpagesize())==ah->tsize);

    filid >>= 1; /* ditch tag bit */
    fchmod(filid,0755);
    bulletproofWrite(filid,fh,headersize); /* headers */
    bulletproofWrite(filid,N_TXTADDR+headersize,ah->tsize-headersize); /* text */
    bulletproofWrite(filid,N_DATADDR,ah->dsize); /* data */
}
#endif MIPS
#else NeXT
extern int mach_maplimit;
export(filid) int filid;
{static struct mach_header E;  /* make it static so all fields=0 */
static struct segment_command tcmd;
static struct section tsectn;
static struct segment_command dcmd;
static struct section dsectn;
static struct section bsectn;
static struct thread_command uthr;
static unsigned long thflavor;
static unsigned long thcount;
static struct NeXT_thread_state_regs ntregs;
static unsigned int hdrsize;
int datasize, bsssize;

E.magic = MH_MAGIC;
E.cputype = CPU_TYPE_MC68030;
E.cpusubtype = CPU_SUBTYPE_NeXT;
E.filetype = MH_EXECUTE;
E.ncmds = 3;
E.sizeofcmds = sizeof(tcmd) + sizeof(tsectn)
	+ sizeof(dcmd) + sizeof(dsectn) + sizeof(bsectn)
	+ sizeof(uthr) + sizeof(thflavor) + sizeof(thcount) + sizeof(ntregs);
E.flags = MH_NOUNDEFS;

hdrsize = E.sizeofcmds + sizeof(E);
 textstart = N_TXTADDR(E);


tcmd.cmd = LC_SEGMENT;
tcmd.cmdsize = sizeof(tcmd) + sizeof(tsectn);
strcpy(tcmd.segname,SEG_TEXT);
tcmd.vmaddr = textstart;
tcmd.vmsize = (int) CEIL(((int)&etext),getpagesize())-textstart;
tcmd.fileoff = 0;
tcmd.filesize = tcmd.vmsize;
tcmd.maxprot = VM_PROT_ALL;
tcmd.initprot = VM_PROT_READ|VM_PROT_EXECUTE;
tcmd.nsects = 1;
tcmd.flags = 0;

strcpy(tsectn.sectname,SECT_TEXT);
strcpy(tsectn.segname,tcmd.segname);
tsectn.addr = tcmd.vmaddr + hdrsize;
tsectn.size = tcmd.vmsize - hdrsize;
tsectn.offset = hdrsize;
tsectn.align = 2;
tsectn.reloff = 0;
tsectn.nreloc = 0;
tsectn.flags = 0;
tsectn.reserved1 = tsectn.reserved2 = 0;


dcmd.cmd = LC_SEGMENT;
dcmd.cmdsize = sizeof(dcmd) + sizeof(dsectn) + sizeof(bsectn);
strcpy(dcmd.segname,SEG_DATA);
dcmd.vmaddr = tcmd.vmaddr + tcmd.vmsize;
datasize = CEIL(old_high-dcmd.vmaddr, getpagesize());
bsssize = mach_maplimit-old_high;
dcmd.vmsize = bsssize+datasize;
dcmd.fileoff = tcmd.fileoff + tcmd.filesize;
dcmd.filesize = datasize;
dcmd.maxprot = VM_PROT_ALL;
dcmd.initprot = VM_PROT_ALL;
dcmd.nsects = 2;
dcmd.flags = 0;

strcpy(dsectn.sectname,SECT_DATA);
strcpy(dsectn.segname,dcmd.segname);
dsectn.addr = dcmd.vmaddr;
dsectn.size = datasize;
dsectn.offset = dcmd.fileoff;
dsectn.align = 2;
dsectn.reloff = 0;
dsectn.nreloc = 0;
dsectn.flags = 0;
dsectn.reserved1 = dsectn.reserved2 = 0;

strcpy(bsectn.sectname,SECT_BSS);
strcpy(bsectn.segname,dcmd.segname);
bsectn.addr = dsectn.addr + dsectn.size;
bsectn.size = bsssize;
bsectn.offset = 0;
bsectn.align = 2;
bsectn.reloff = 0;
bsectn.nreloc = 0;
bsectn.flags = S_ZEROFILL;
bsectn.reserved1 = bsectn.reserved2 = 0;

uthr.cmd = LC_UNIXTHREAD;
uthr.cmdsize = sizeof(uthr) + sizeof(thflavor) + sizeof(thcount) + sizeof(ntregs);

thflavor = NeXT_THREAD_STATE_REGS;

thcount = NeXT_THREAD_STATE_REGS_COUNT;

ntregs.areg[0] = 0xfeadface;
ntregs.pc = startptr;

 filid >>= 1;
 fchmod(filid,0755);
 bulletproofWrite(filid,&E,sizeof(E));
 bulletproofWrite(filid,&tcmd,sizeof(tcmd));
 bulletproofWrite(filid,&tsectn,sizeof(tsectn));
 bulletproofWrite(filid,&dcmd,sizeof(dcmd));
 bulletproofWrite(filid,&dsectn,sizeof(dsectn));
 bulletproofWrite(filid,&bsectn,sizeof(bsectn));
 bulletproofWrite(filid,&uthr,sizeof(uthr));
 bulletproofWrite(filid,&thflavor,sizeof(thflavor));
 bulletproofWrite(filid,&thcount,sizeof(thcount));
 bulletproofWrite(filid,&ntregs,sizeof(ntregs));
 bulletproofWrite(filid,tsectn.addr,tsectn.size);
 bulletproofWrite(filid,dsectn.addr,dsectn.size);
}
#endif NeXT


/* NICK: A Bullet-proof write to retry on NFS systems */
/* JHR: Added retry for NFS timeout errors. */
#include <errno.h>
bulletproofWrite(fid, buf, total)
int fid;
char *buf;
int total;
{
   int bytesWritten = 0;
#ifdef M68
   int retries = 0;
#endif M68
   int i;
   do
      {	 i = write(fid, buf, total-bytesWritten);
#ifdef M68
#ifndef sony_news
     if (i < 0) {
	if (errno == ETIMEDOUT) {
	   /* NFS timeout error, so try again. */
	   if (retries++ > 5)
	      die("export, NFS timeout");
	   chatting("[Write timeout, retrying]\n");
	   continue;
	}
	else die("export");
     }
     else retries = 0;
#else sony_news
	 if (i < 0) die("export");
#endif sony_news
#else
     if (i < 0) die("export");
#endif M68
     bytesWritten += i;
     buf += i;
     if (bytesWritten < total)
	chatting("[Write incomplete (%d/%d), retrying]\n",
	     bytesWritten, total);
      }
   while (bytesWritten < total);
}
