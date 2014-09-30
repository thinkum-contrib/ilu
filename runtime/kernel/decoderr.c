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
*/
/*
 */
/* $Id: decoderr.c,v 1.7 1999/08/03 01:53:01 janssen Exp $ */
/* Last edited by Mike Spreitzer February 5, 1996 7:32 am PST */

#include <stdio.h>

typedef struct {
  long unsigned   hash;
  const char     *filename;
}               hashnote;

static hashnote hashnotes[] = {
#include "errhashs.c"
	{0, NULL}
};

int
main(int argc, char **argv)
{
  int             i, j, ans = 0;
  long unsigned   code, hash, line;
  for (i = 1; i < argc; i++) {
    if (sscanf(argv[i], "%lu", &code) != 1) {
      fprintf(stderr, "Failed to parse \"%s\" as a long unsigned int!\n",
	      argv[i]);
      ans = -1;
    } else if (code < 1000)
      fprintf(stderr, "%lu isn't an internal check failure minor code!\n",
	      code);
    else {
      int             found = 0;
      code -= 1000;
      hash = code / 10000;
      line = code - hash * 10000;
      fprintf(stderr, "%lu = line %lu in", code + 1000, line);
      for (j = 0; hashnotes[j].filename != NULL; j++)
	if (hashnotes[j].hash == hash) {
	  printf(" %s $ILUSRC/runtime/kernel/%s",
		 (found ? "or" : "file"),
		 hashnotes[j].filename);
	  found = 1;
	}
      if (found)
	printf("\n");
      else {
	printf(" unknown file (that hashes to %lu)\n", hash);
	ans = -1;
      }
    }
  }
  return ans;
}
