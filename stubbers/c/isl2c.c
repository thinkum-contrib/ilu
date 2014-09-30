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
/* Last edited by Mike Spreitzer September 15, 1995 5:01 pm PDT */


#include <stdlib.h>
#include <stdarg.h>
#include "cstubber.h"

#ifdef _IS_POSIX
#include <unistd.h>	/* for unlink */
#include <errno.h>	/* for errno, ENOENT */
#endif /* _IS_POSIX */

#if defined( WIN32 ) || defined( macintosh )
/* for stat */
#include <sys/types.h>
#include <sys/stat.h>
#endif /* WIN32 */

static boolean makedir (char *);
static void usage(void);

static boolean GenerateTrue = FALSE;
static boolean GenerateCommon = FALSE;
static boolean GenerateSurrogate = FALSE;
static boolean GenerateHeaders = FALSE;
static boolean RemoveFirst = FALSE;
static boolean NoDefaultTrueClass = FALSE;

static list Includes = NULL;
static list NoDefaultTrueClassList = NULL;

char *NamesFile = NULL;
char *TrueFile = NULL;
char *SurrogateFile = NULL;
char *HeaderFile = NULL;
char *CommonFile = NULL;
char *InterfaceHeaderTranslationFile = NULL;


#if (defined(WIN32) || defined(WIN16))
#define DIR_SEPARATOR_STR "\\"
#elif defined( macintosh )
#define DIR_SEPARATOR_STR ""
#else
#define DIR_SEPARATOR_STR "/"
#endif /* (defined(WIN32) || defined(WIN16)) */


#if defined( macintosh )
static char *OutputDirectory = "";
#else
static char *OutputDirectory = ".";
#endif

static char *ProgramName = "c-stubber";

static void generateCBoilerplate (FILE *file, Interface parse)
{
  static char *prefixes[2] = { " *", " *" };

  fprintf (file, "/*\n");
  iluparser_GenerateBoilerplate (file, parse, ProgramName, prefixes);
  fprintf (file, " */\n\n");
}

static void possibleUnlink (const char *filename)
{
#ifdef _IS_POSIX
  if (RemoveFirst)
    {
      if (unlink(filename) < 0)
	{
	  int e = errno;
	  if (e != ENOENT)
	    fprintf (stderr, "Warning:  couldn't delete existing file \"%s\", %s.\n",
		     filename, strerror(e));
	}
    }
#endif /* _IS_POSIX */
}

static void generateCommon (Interface parse)
{
  FILE *file;
  char filename[1000];

  MethodRecordID = 0;

  if (CommonFile == NULL) 
    sprintf(filename, "%s%s%s-common.c", OutputDirectory, DIR_SEPARATOR_STR, c_interface_name(parse));
  else 
    sprintf(filename, "%s%s%s.c", OutputDirectory, DIR_SEPARATOR_STR, CommonFile);

  possibleUnlink (filename);

  if ((file = fopen(filename, "w")) == NULL)
    fatal("Couldn't open client stubs output file %s.\n", filename);
  printf("common code for interface %s to %s...\n", c_interface_name(parse), filename);
  setFile(file);
  generateCBoilerplate (file, parse);
  generateCommonCode(parse, file);
  fclose (file);
}

static void generateSurrogate (Interface parse)
{
  FILE *file;
  char filename[1000];

  MethodRecordID = 0;

  if (SurrogateFile == NULL) 
    sprintf(filename, "%s%s%s-surrogate.c", OutputDirectory, DIR_SEPARATOR_STR, c_interface_name(parse));
  else 
    sprintf(filename, "%s%s%s.c", OutputDirectory, DIR_SEPARATOR_STR, SurrogateFile);

  possibleUnlink (filename);

  if ((file = fopen(filename, "w")) == NULL)
    fatal("Couldn't open surrogate stubs output file %s.\n", filename);
  printf("code for surrogate stubs of interface %s to %s...\n", c_interface_name(parse), filename);
  setFile(file);
  generateCBoilerplate (file, parse);
  generateClientCode(parse, file);
  fclose (file);
}

static void generateTrue (Interface parse)
{
  FILE *file;
  char filename[1000];

  if (TrueFile == NULL) 
    sprintf(filename, "%s%s%s-true.c", OutputDirectory, DIR_SEPARATOR_STR, c_interface_name(parse));
  else 
    sprintf(filename, "%s%s%s.c", OutputDirectory, DIR_SEPARATOR_STR, TrueFile);

  possibleUnlink (filename);

  if ((file = fopen(filename, "w")) == NULL)
    fatal("Couldn't open true stubs output file %s.\n", filename);
  printf("code for true stubs of interface %s to %s...\n", c_interface_name(parse), filename);
  setFile(file);
  generateCBoilerplate (file, parse);
  generateServerCode(parse, NoDefaultTrueClass, NoDefaultTrueClassList, file);
  fclose(file);
}

static void generateHeaders(Interface parse)
{
  FILE *file;
  char filename[1000];

  if (HeaderFile == NULL) 
    sprintf(filename, "%s%s%s.h", OutputDirectory, DIR_SEPARATOR_STR, c_interface_name(parse));
  else 
    sprintf(filename, "%s%s%s.h", OutputDirectory, DIR_SEPARATOR_STR, HeaderFile);

  possibleUnlink (filename);

  if ((file = fopen(filename, "w")) == NULL) {
    fatal("Couldn't open output file %s.\n", filename);
  }
  printf("header file for interface %s to %s...\n", c_interface_name(parse), filename);
  setFile(file);
  generateCBoilerplate (file, parse);
  generateCHeaders(parse, file);
  fclose(file);
}

static void generateStubs (Interface parse)
{
  if (!makedir(OutputDirectory))
    fatal("Couldn't create output directory \"%s\".\n", OutputDirectory);
  if (GenerateHeaders)
    generateHeaders (parse);
  if (GenerateCommon)
    generateCommon (parse);
  if (GenerateSurrogate)
    generateSurrogate (parse);
  if (GenerateTrue)
    generateTrue (parse);
}

#if (defined(_IS_BSD) || defined(_IS_POSIX))
#include <sys/stat.h>
#ifdef _IS_POSIX
#include <unistd.h>
#endif
#endif

static boolean makedir (char *dirname)
{
#if (defined(_IS_BSD) || defined(_IS_POSIX) || defined( macintosh ))

  struct stat s;

  if (stat(dirname, &s) == 0)
    {
      if (S_ISDIR(s.st_mode))
	{

#ifdef _IS_POSIX

	  return (access (dirname, W_OK) == 0);

#else /* not POSIX */

	  return (TRUE);

#endif
	  
	}
      return (FALSE);
    }
  else
    {
      char buf[1000];
      sprintf(buf, "mkdir %s", dirname);
      return((system(buf) >> 8) == 0);
    }

#elif WIN32 /* not _IS_BSD or _IS_POSIX */
  struct _stat s;

  if (_stat(dirname, &s) == 0)  /* if it exists */
    { /* xxx dll note should we check for writeability if on NTFS? */
      if ((s.st_mode) & _S_IFDIR)  /* if its a directory */
	    return (TRUE);
	  else
		return (FALSE);
    }
  else		   /* it doesn't exist */
    {
      char buf[1000];
      sprintf(buf, "mkdir %s", dirname);
      return((system(buf) >> 8) == 0);
    }
#else

  fprintf (stderr, "%s:  Can't test directory \"%s\".\n",
	   ProgramName, dirname);
  return (TRUE);

#endif
}

int main(int ac, char **av, char **envp)
{
  list s;
  char **interfacename;
  int i_num_isl_files_processed = 0;
  int i_renaming_files = 0;

  if (ac < 2) {
    usage();
    return 1;
  }
  Includes = new_list();
  NoDefaultTrueClassList = new_list();

  if ((ProgramName = iluparser_GetProgramName(*av)) == NULL)
#if (defined(WIN32) && defined(_WINIO))
    ProgramName = "wcstub";
#else
    ProgramName = "c-stubber";
#endif /* (defined(WIN32) && defined(_WINIO)) */
  av++;
  while(*av != NULL && *av[0] == '-') {
    if (strcmp(*av, "-dir") == 0) {
      if (*++av != NULL)
	OutputDirectory = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-I") == 0) {
      if (*++av != NULL)
	list_insert(Includes, *av);
      else
	usage();
    }
    else if (strcmp(*av, "-true") == 0)
      GenerateTrue = TRUE;
    else if (strcmp(*av, "-renames") == 0) {
      if (*++av != NULL)
	NamesFile = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-surrogate") == 0)
      GenerateSurrogate = TRUE;
    else if (strcmp(*av, "-headers") == 0)
      GenerateHeaders = TRUE;
    else if (strcmp(*av, "-removefirst") == 0)
      RemoveFirst = TRUE;
    else if (strcmp(*av, "-nodefaulttrueclass") == 0)
      NoDefaultTrueClass = TRUE;
    else if (strcmp(*av, "-nodefaulttrueclassfor") == 0) {
      if (*++av != NULL)
	list_insert(NoDefaultTrueClassList, *av);
      else
	usage();
    }
    else if (strcmp(*av, "-common") == 0)
      GenerateCommon = TRUE;
    else if (strcmp(*av, "-tname") == 0) {
      if (*++av != NULL)
	TrueFile = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-sname") == 0) {
      if (*++av != NULL)
	SurrogateFile = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-hname") == 0) {
      if (*++av != NULL)
	HeaderFile = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-cname") == 0) {
      if (*++av != NULL)
	CommonFile = *av;
      else
	usage();
    }
    else if (strcmp(*av, "-hdrmap") == 0) {
      if (*++av != NULL)
	InterfaceHeaderTranslationFile = *av;
      else
	usage();
    }
    else {
      usage();
      fatal("%s: Invalid switch \"%s\".\n", ProgramName, *av);
    }
    av++;
  }
  if (!(GenerateTrue || GenerateHeaders || GenerateSurrogate || GenerateCommon)) {
    /* do everything, if nothing is specified */
    GenerateCommon = TRUE;
    GenerateHeaders = TRUE;
    GenerateSurrogate = TRUE;
    GenerateTrue = TRUE;
  }
  if (NamesFile != NULL) {
    if (!ReadSynonyms(NamesFile)) {
      fatal("Couldn't read names file %s.\n", NamesFile);
    }
  }

  iluparser_RegisterInterfaceDirectories(Includes);

  if (TrueFile || SurrogateFile || HeaderFile || CommonFile)
  	i_renaming_files = 1;

  for(interfacename = av; *interfacename != NULL; interfacename++) {
  	if (i_renaming_files && (i_num_isl_files_processed > 0))
      fatal("Can't use -tname -sname -hname or -cname with more than 1 isl file.\n");
    if ((s = ParseFile(*interfacename)) == NULL) {
      fatal("Couldn't find or parse %s.\n", *interfacename);
    }
    list_enumerate(s, (void (*)(refany, refany)) generateStubs, NULL);
	i_num_isl_files_processed++;
  }
  return 0;
}

static void usage()
{
  fprintf(stderr, "Usage: %s [-I ISLDIRECTORY] [-dir OUTPUTDIRECTORY] [-true] [-surrogate] [-headers] [-common] \
[-renames NAMESFILE] [-tname TRUEFILENAME] [-sname SURROGATEFILENAME] [-hname HEADERFILENAME] [-removefirst] \
[-cname COMMONFILENAME] [-nodefaulttrueclass] [-nodefaulttrueclassfor OBJECT-TYPE] [-hdrmap INTERFACEHEADERTRANSLATIONFILE ] ISLFILE [ISLFILE ...]\n", ProgramName);
}
