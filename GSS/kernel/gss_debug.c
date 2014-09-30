/*
BeginILUCopyright

Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

Unlimited use, reproduction, modification, and distribution of this
software and modified versions thereof is permitted.  Permission is
granted to make derivative works from this software or a modified
version thereof.  Any copy of this software, a modified version
thereof, or a derivative work must include both the above copyright
notice of Xerox Corporation and this paragraph.  Any distribution of
this software, a modified version thereof, or a derivative work must
comply with all applicable United States export control laws.  This
software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
OF THE POSSIBILITY OF SUCH DAMAGES.
  
EndILUCopyright

$Id: gss_debug.c,v 1.14 1999/09/17 17:44:50 janssen Exp $
*/
/*
 * debug.c -- general debugging package for use with GSS API
 *
 */

#ifndef WIN32
#include <unistd.h>
#else
#include <process.h>
#endif
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "ilugss_debug.h"

static unsigned _ilugss_debuglevel;
static FILE *_ilugss_debugfp=NULL;

static int virgin=1;

struct debugentry {
	char *name;
	unsigned value;
};

static struct debugentry debugs[]={
	{"PACKET",PACKET_DB},
	{"INIT",INIT_DB},
	{"PROTO",PROTO_DB},
	{"ERROR",ERR_DB},
	{"CALL",CALL_DB},
	{"ALL",0xffff},
};


char *_ilugss_sys_strerror(int errnum)
{
  return strerror(errnum);
}

void _ilugss_set_GSS_debuglevel()
{
	char *s, *tmp;
	int debugcount,i;

	debugcount=sizeof(debugs)/sizeof(struct debugentry);
	virgin=0;
	_ilugss_debuglevel=0;
	if ((s=getenv("GSSDEBUG"))==NULL)
		return;
	while (*s!='\0') {
		tmp=strchr(s,',');
		if (tmp!=NULL) 
			*tmp='\0';
		for (i=0; i < debugcount; i++) {
			if (strcmp(debugs[i].name,s)==0) {
				_ilugss_debuglevel |= debugs[i].value;
				break;
			}
		}
		if (i >= debugcount) {
			fprintf(stderr,"_ilugss_set_GSS_debuglevel(): bad debug option \"%s\": valid options are: ",s);
			for (i=0; i < debugcount; i++) {
				fprintf(stderr,"%s ",debugs[i].name);
			}
			fprintf(stderr,"\n");
		}
		if (tmp==NULL)
			break;
		s=tmp+1;
	}
}

void ilugss_debug(unsigned level,...)
{
	va_list ap;
	char *fmt;
	char *debugfile;
	static int mypid;

	if (virgin) {
		_ilugss_set_GSS_debuglevel();
		if ((debugfile=getenv("GSSDEBUGFILE"))!=NULL) {
			if ((_ilugss_debugfp=fopen(debugfile,"a+"))==NULL) 
				fprintf(stderr,"could not open debug file %s, using stderr...\n", debugfile);
		}
		if (_ilugss_debugfp==NULL)
			_ilugss_debugfp=stderr;
		mypid=getpid();	
	}
	if ((_ilugss_debuglevel & level)==0)
		return;
	va_start(ap,level);
	fmt=va_arg(ap,char *);
	fprintf(_ilugss_debugfp,"GSS (pid %d): ",mypid);
	vfprintf(_ilugss_debugfp,fmt,ap);
	va_end(ap);
	fflush(_ilugss_debugfp);
}

#define MAXDUMP	4096

void ilugss_dump_packet(void *packet,unsigned len)
{
	unsigned char *buf;
	char c;
	int dumplen;
	int i,j,n;

	if ((_ilugss_debuglevel & PACKET_DB)!=PACKET_DB)
		return;
	buf=packet;
	if (len > MAXDUMP) {
		fprintf(_ilugss_debugfp,"Request to dump packet of %u bytes, only %u bytes being dumped.\n",len,MAXDUMP);
		dumplen=MAXDUMP;
	} else {
		dumplen=len;
	}
	if (packet==NULL) {
		fprintf(_ilugss_debugfp,"attempt to dump NULL packet.\n");
		return;
	}
	for (i = 0;  i < dumplen;  i += 16) {
		fprintf (_ilugss_debugfp, "%6u:  ", i);
		for (j = 0;  j < 16 && (i + j) < dumplen;  j++)
			fprintf(_ilugss_debugfp, "%02x%s ", buf[i + j],
				 ((j % 4) == 3) ? " " : "");
		/* compute n, number of space of padding before ascii dump */
		n = (((16-j)/4)*(13)+((16-j)%4)*3)+1;
		fprintf (_ilugss_debugfp, "% *.*s", n, n, "");
		for (j = 0;  j < 16 && (i + j) < dumplen;  j++) {
			c = buf[i + j];
			fprintf (_ilugss_debugfp, "%c", ((c >= ' ') && (c <= '~')) ?
					 (char) c : '.');
		}
		fprintf(_ilugss_debugfp,"\n");
	}
}
