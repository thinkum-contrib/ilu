/** 
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
$Id: util.c,v 1.23 1999/08/05 00:12:20 janssen Exp $
*/
/* Chris Jacobi, April 17, 1998 12:57 pm PDT */

#include <string.h>
#include <stdio.h>	/* for FILE */
#include <stdlib.h>	/* for getenv() */
#include <version.h>	/* for ILU_VERSION_STRING */

#include "iluptype.h"

#ifdef _IS_POSIX
#include <sys/types.h>
#include <unistd.h>
#endif /* _IS_POSIX */
#include <time.h>	/* for ctime() */

#include <sys/stat.h>	/* for stat() */


char* replaceBackslashes(char* x)

    /* Replace backslashes with forward slashes.
     * Allocates new string if some replacement happend, returns the 
     * input string if no replacement happend. 
     *
     * The only place local to the parser where this is used is in file 
     * names in comments.  This is used for the benefit of java which
     * gives a special interpretation to backslashes even in comments.
     */
{
    char backslash = '\\';
    char* p;
    p = strchr(x, backslash);
    if (p) {
        x = ilu_strdup(x);
        p = strchr(x, backslash);
        while (p) {
            *p = '/';
            p++;
            p = strchr(p, backslash);
        }
    }
    return x;
} /*replaceBackslashes*/



/* MED: Windows doesn't have pwd.h (or getpwuid()) */
#ifdef _IS_POSIX
#include <pwd.h>	/* for getpwuid() */
#endif

static char *ModTime (char *path)
{
  static char timebuf[30];
  struct stat statbuf;

  stat (path, &statbuf);
  strcpy (timebuf, (char *) ctime(&statbuf.st_mtime));
  timebuf[24] = '\0';
  return (timebuf);
}

static char *GoodGetLogin (void)
{
#ifdef _IS_POSIX
  char *name;
  struct passwd *pw;

  if ((name = (char *) getlogin()) == NULL
      && (name = (char *) (((pw = getpwuid(getuid())) == NULL) ? NULL : pw->pw_name)) == NULL
      && (name = (char *) getenv("USER")) == NULL)
    return "nobody";
  else
    return name;
#else
  return "nobody";
#endif
}

typedef struct {
  FILE *file;
  char *buffer;
  char *prefix;
} PrintInfo;

static void PrintImportedFileInfo (Imported s, PrintInfo *info)
{
  Interface i = GetInterface (s->name, s->filename);
  char *filename;

  if (i == NULL)
    {
      fprintf (stderr, "Couldn't find interface \"%s\".\n", s->name);
      exit(1);
    }
  filename = (s->filename && *s->filename) ? s->filename : i->filename;
  if (filename)
    fprintf (info->file, ",\n%s and \"%s\" of %s", info->prefix, replaceBackslashes(filename), ModTime(filename));
}

static void ListImportedFileInfo (Imported s, PrintInfo *info)
{
  Interface i = GetInterface (s->name, s->filename);
  char *filename;

  if (i == NULL)
    {
      fprintf (stderr, "Couldn't find interface \"%s\".\n", s->name);
      exit(1);
    }
  filename = (s->filename && *s->filename) ? s->filename : i->filename;
  if (filename)
    sprintf (info->buffer + strlen(info->buffer), ",\n%s and \"%s\" of %s",
	     info->prefix, replaceBackslashes(filename), ModTime(filename));
}

char * iluparser_BoilerPlateString (Interface parse, char *programName, char *prefixes[2])
{
  PrintInfo info;
  time_t clock = time(0);
  char *now;
  char buf[2048];

  info.buffer = buf;
  info.prefix = prefixes[1];

  now = ilu_strdup(ctime(&clock));
  now[strlen(now) - 1] = '\0';

  buf[0] = 0;
  
  sprintf (buf + strlen(buf), "%s This file was automatically generated with ILU (version %s) tools\n",
	   prefixes[0], ILU_VERSION_STRING);
  sprintf (buf + strlen(buf), "%s at %s by `%s'\n%s running \"%s\" of %s\n",
	   prefixes[1], now, GoodGetLogin(), prefixes[1], replaceBackslashes(programName), ModTime(programName));
  sprintf (buf + strlen(buf), "%s on \"%s\" of %s", prefixes[1], replaceBackslashes(parse->filename), ModTime(parse->filename));
  if (list_size(parse->imports) > 0)
    list_enumerate (parse->imports, (iluparser_EnumProc) ListImportedFileInfo, &info);
  sprintf (buf + strlen(buf), ".\n%s\n%s ILU is Copyright 1991-1999 Xerox Corporation, All Rights Reserved.\n",
	   prefixes[1], prefixes[1]);
  sprintf (buf + strlen(buf), "%s ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.\n",
	   prefixes[1]);
  return ilu_strdup(buf);
}

void iluparser_GenerateBoilerplate (FILE *file, Interface parse, char *programName, char *prefixes[2])
{
  char *bp = iluparser_BoilerPlateString(parse, programName, prefixes);
  fprintf (file, "%s", bp);
  free(bp);
}

static void PrintInterfaceInfo (Interface parse, PrintInfo *info)
{
  fprintf (info->file, "%s on \"%s\" of %s", info->prefix, replaceBackslashes(parse->filename), ModTime(parse->filename));
  if (list_size(parse->imports) > 0)
    list_enumerate (parse->imports, (iluparser_EnumProc) PrintImportedFileInfo, info);
  fprintf (info->file, "\n");
}

void iluparser_MultipleInterfaceBoilerplate (FILE *file, list interfaces, char *programName, char *prefixes[2])
{
  PrintInfo info;
  time_t clock = time(0);
  char *now;

  info.file = file;
  info.prefix = prefixes[1];

  now = ilu_strdup(ctime(&clock));
  now[strlen(now) - 1] = '\0';
  
  fprintf (file, "%s This file was automatically generated with ILU (version %s) tools\n",
	   prefixes[0], ILU_VERSION_STRING);
  fprintf (file, "%s at %s by `%s'\n%s running \"%s\" of %s\n",
	   prefixes[1], now, GoodGetLogin(), prefixes[1], replaceBackslashes(programName), ModTime(programName));
  list_enumerate (interfaces, (iluparser_EnumProc) PrintInterfaceInfo, &info);
  fprintf (file, "%s\n%s ILU is Copyright 1991-1997 Xerox Corporation, All Rights Reserved.\n",
	   prefixes[1], prefixes[1]);
  fprintf (file, "%s ILU information:  ftp://ftp.parc.xerox.com/pub/ilu/ilu.html.\n",
	   prefixes[1]);
}

string iluparser_GetILUVersionString ()
{
  return (ILU_VERSION_STRING);
}
