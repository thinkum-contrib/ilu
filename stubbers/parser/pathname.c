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
$Id: pathname.c,v 1.19 1999/08/03 01:50:15 janssen Exp $
*/

#include <stdio.h>

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include	<unistd.h>
#include	<ilumac.h>
#else
#include <iluconf.h>
#endif

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

#include <string.h>
#include <stdlib.h>	/* for getenv() */
#ifdef _IS_POSIX
#include <unistd.h>
#endif

#if (defined(WIN32) || defined(WIN16))
/* for getcwd */
#include <direct.h>
/* for _access */
#include <io.h>
/* differences in directory separator chars */
#define DIR_SEPARATOR_CHAR '\\'
#define DIR_SEPARATOR_STR "\\"
#define PATH_SEPARATOR_CHAR ';'
#elif defined( macintosh )
#define DIR_SEPARATOR_CHAR ':'
#define DIR_SEPARATOR_STR ":"
#define PATH_SEPARATOR_CHAR ','
#else
#define DIR_SEPARATOR_CHAR '/'
#define DIR_SEPARATOR_STR "/"
#define PATH_SEPARATOR_CHAR ':'
#endif /* (defined(WIN32) || defined(WIN16)) */

extern void *iluparser_Malloc(unsigned long size);
extern void *iluparser_Realloc(void *, unsigned long size);
extern void iluparser_Free(void *ptr);
extern char *ilu_strdup(char *);


char *iluparser_CanonicalPathname (char *filename);
char *iluparser_GetProgramName (char *shortName);

static char *gnu_getcwd (void)
     /* from the info pages to glibc */
{
#if (defined(_IS_POSIX) || (defined(WIN32) || defined(WIN16) || defined(macintosh)))

  int size = 100;
  char *buffer = (char *) iluparser_Malloc (size);
  
  while (1)
    {
      char *value = getcwd (buffer, size);
      if (value != 0)
	return buffer;
           size *= 2;
      iluparser_Free (buffer);
      buffer = (char *) iluparser_Malloc (size);
    }

#elif defined(HAVE_GETWD)

  char pathname[MAXPATHLEN];

  if (getwd(pathname) != NULL)
    return (ilu_strdup(pathname));
  else
    {
      fprintf (stderr, "ILU(pathname.c):  Couldn't execute 'getwd'.  Defaulting to \".\".\n");
      return (ilu_strdup("."));
    }

#else

  fprintf (stderr, "ILU(pathname.c):  Don't know how to get current working directory, defaulting to \".\".\n");
  return (ilu_strdup("."));
#endif
}

char *iluparser_CanonicalPathname (char *file)
{
#if (defined(_IS_BSD) || defined(_IS_POSIX) || (defined(WIN32) || defined(WIN16) || defined(macintosh)))	/* in short, IS_UNIX or WIN32 */
  char *pathname;
  char *wd;
  unsigned int wdlen;

#if (defined(WIN32) || defined(WIN16))
  if ((*file == DIR_SEPARATOR_CHAR) || ((*(file+1)) == ':')) /* path already starts at the root or has drive id*/
#elif defined( macintosh )
  if (*file != DIR_SEPARATOR_CHAR)	 /* path already starts at the root */
#else
  if (*file == DIR_SEPARATOR_CHAR)	 /* path already starts at the root */
#endif
    {
      pathname = (char *) iluparser_Malloc(strlen(file) + 1);
      strcpy (pathname, file);
    }
  else
    {
      wd = gnu_getcwd();
      wdlen = strlen(wd);

#if defined( macintosh )
      if (*file == DIR_SEPARATOR_CHAR) /* path relative to where we are */
#else
      if (*file == '.' && file[1] == DIR_SEPARATOR_CHAR) /* path relative to where we are */
#endif
	{
	  pathname = (char *) iluparser_Malloc (strlen(file+1) + wdlen + 1);
	  strcpy (pathname, wd);
	  strcpy (pathname+wdlen, file+1);
	}
      else
	{
	  pathname = (char *) iluparser_Malloc (strlen(file) + wdlen + 2);
	  strcpy (pathname, wd);
	  pathname[wdlen] = DIR_SEPARATOR_CHAR;
	  strcpy (pathname+wdlen+1, file);
	}
    }

#ifdef REMOVE_AUTOMOUNT_TMP_MNT
  if (strncmp("/tmp_mnt", pathname, 8) == 0)
    memmove (pathname, pathname + 8, strlen(pathname+7));
#endif

  return (pathname);

#else	/* not BSD or POSIX */

  return (ilu_strdup(file));

#endif
}

/* dll -- searches path to find where shortname is */
#if (defined(WIN32) || defined(WIN16))
/* for SearchPath */
#include <windows.h>
#endif

char * /* result is NULL or full pathname */
  iluparser_GetProgramName (char *shortName)
{
#if defined(_IS_POSIX)

  char *pathEnv;
  char *cwd;
  char *p, *q, *buf;
  int pathLen, shortNameLen, cwdLen, bufLen;
#define INIT_BUFLEN 100

  if ((cwd = gnu_getcwd()) != NULL)
    cwdLen = strlen(cwd);
  else
    cwdLen = 0;

  if( shortName[0] == DIR_SEPARATOR_CHAR )
    {
      pathEnv = DIR_SEPARATOR_STR;
    }
  else if ( shortName[0] == '.' || ( strchr(shortName, DIR_SEPARATOR_CHAR) != NULL ))
    {
      if( cwdLen == 0 )
	return NULL;
      pathEnv = cwd;
    }
  else
    {
      if( (pathEnv = (char *)(getenv("PATH"))) == NULL )
	return NULL;
    }
  buf = (char *)iluparser_Malloc(bufLen = INIT_BUFLEN);
  if( buf == NULL ) return NULL;
  shortNameLen = strlen(shortName);
  q = pathEnv;
  for(;;) {
    p = q;
    while( *p == PATH_SEPARATOR_CHAR )
      p++;
    if( *p == 0 )
      return NULL;
    q = p;
    while( (*q != PATH_SEPARATOR_CHAR) && (*q != 0) )
      q++;
    pathLen = (q - p);
    if( p[0] == DIR_SEPARATOR_CHAR )
      {
	if( pathLen >= bufLen )
	  {
	    bufLen += pathLen;
	    if( (buf = (char *)iluparser_Realloc(buf, bufLen)) == NULL )
	      return NULL;
	  }
	memcpy(buf, p, pathLen);
      }
    else
      {
	if( cwdLen == 0 )
	  continue;
	if( p[0] == '.' )
	  { p++; pathLen--; }
	if( (cwdLen + 1 + pathLen) >= bufLen )
	  {
	    bufLen += (cwdLen + 1 + pathLen);
	    if( (buf = (char *)iluparser_Realloc(buf, bufLen)) == NULL )
	      return NULL;
	  }
	memcpy(buf, cwd, cwdLen);
	buf[cwdLen] = DIR_SEPARATOR_CHAR;
	if( pathLen > 0 )
	  memcpy(&(buf[cwdLen+1]), p, pathLen);
	pathLen += (cwdLen+1);
      }
    if( (pathLen + 1 + shortNameLen) >= bufLen )
      continue;
    buf[pathLen] = DIR_SEPARATOR_CHAR;
    if (strncmp(shortName, "./", 2) == 0)
      strcpy(buf+pathLen+1, shortName+2);
    else
      strcpy(buf+pathLen+1, shortName);
    if( buf[0] == DIR_SEPARATOR_CHAR && buf[1] == DIR_SEPARATOR_CHAR)
      {
	char *s = buf + 1;

	while (s[1] == DIR_SEPARATOR_CHAR)
	  s++;
	memmove (buf, s, strlen(s) + 1);
      }

    if( access(buf, X_OK) != 0 )
      continue;

#ifdef REMOVE_AUTOMOUNT_TMP_MNT
    if (strncmp(buf, "/tmp_mnt/", 9) == 0)
      memmove (buf, buf+8, strlen(buf) - 7);
#endif

    return buf;
  }
  /*NOTREACHED*/

#elif (defined(WIN32))

/* shortname is something like foo.exe */
char* pc_filepart;
char* pc_fullpath = (char*) iluparser_Malloc(_MAX_PATH);

if (SearchPath(getenv("PATH"), shortName, ".exe", 
		_MAX_PATH, pc_fullpath, &pc_filepart) != 0)
	return pc_fullpath;

 if (SearchPath(NULL, shortName, ".exe", 
		_MAX_PATH, pc_fullpath, &pc_filepart) != 0)
	return pc_fullpath;

iluparser_Free(pc_fullpath);

return NULL;

#else

  return (ilu_strdup(shortName));

#endif
}
