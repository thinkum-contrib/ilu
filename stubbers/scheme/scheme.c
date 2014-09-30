/*  -*- Mode: C; -*-
 *
 * Support for Guile Scheme has been contributed by Siemens Corporate Research, Inc.
 */

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

#ifdef MACOS
#pragma segment islscmmain
#endif

#include <stdlib.h>
#include "scheme.h"

#ifdef _IS_POSIX
#include <unistd.h>
#include <errno.h>
#endif /* _IS_POSIX */

#ifdef WIN32
/* for stat */
#include <sys/types.h>
#include <sys/stat.h>
#endif /* WIN32 */

char *OutputDirectory = ".";
char *ProgramName = NULL;
char *NamesFile = NULL;
char *StubsFile = NULL;
char *IncludePath = NULL;
char *ClientCommonFile = NULL;
char *InterfaceHeaderTranslationFile = NULL;
boolean GenerateImakefile = FALSE;
boolean GenerateServerSkeleton = FALSE;
boolean GenerateClasses = FALSE;
boolean RemoveFirst = FALSE;

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

static void generate_scm_boilerplate(FILE *file, Interface parse);

static void GenerateCode (Interface parse)
{
  char filename[1000];
  FILE *file;

  if (ClientCommonFile == NULL) {
#ifdef MACOS
  sprintf (filename, "%s.scm", scheme_interface_name(parse));
#else
  sprintf (filename, "%s/%s.scm", OutputDirectory, scheme_interface_name(parse));
#endif
  }
  else {
#ifdef MACOS
    sprintf (filename, "%s.scm", ClientCommonFile);
#else
    sprintf (filename, "%s/%s.scm", OutputDirectory, ClientCommonFile);
#endif
  }

  possibleUnlink (filename);

  if ((file = fopen (filename, "w")) == NULL)
    {
      fprintf (stderr, "Couldn't open client stubs output file %s.\n", filename);
      exit (1);
    }
  printf ("code for interface %s to %s...\n", scheme_interface_name(parse), filename);
  generate_scm_boilerplate (file, parse);
  generate_code (parse, file);
  fclose (file);

  if (StubsFile == NULL) {
#ifdef MACOS
  sprintf (filename, "%s-server-stubs.scm", scheme_interface_name(parse));
#else
  sprintf (filename, "%s/%s-server-stubs.scm", OutputDirectory, scheme_interface_name(parse));
#endif
  }
  else {
#ifdef MACOS
    sprintf (filename, "%s.scm", StubsFile);
#else
    sprintf (filename, "%s/%s.scm", OutputDirectory, StubsFile);
#endif
  }

  possibleUnlink (filename);

  if ((file = fopen (filename, "w")) == NULL)
    {
      fprintf (stderr, "Couldn't open server stubs output file %s.\n", filename);
      exit (1);
    }
  printf ("code for server stubs of interface %s to %s...\n", scheme_interface_name(parse), filename);
  generate_scm_boilerplate (file, parse);
  generate_server_code (parse, file);
  fclose (file);
}

static void generate_imakefile (Interface parse, FILE *file)
{
  struct context_s c;

  c.interface = parse;
  c.file = file;
}

#if (defined(_IS_BSD) || defined(_IS_POSIX))
#include <sys/stat.h>
#ifdef _IS_POSIX
#include <unistd.h>
#endif
#endif

static boolean makedir (char *dirname)
{
#if (defined(_IS_BSD) || defined(_IS_POSIX))

  struct stat s;

  if (stat(dirname, &s) == 0)
    {
      if (S_ISDIR(s.st_mode))
	{

#ifdef _IS_POSIX

	  return (access (dirname, W_OK) == 0);

#else /* not POSIX */

	  return (TRUE);

#endif /* _IS_POSIX */
	  
	}
      else
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

static void GenerateStubs (Interface parse)
{
  char filename[1000];
  FILE *file;

  if (! makedir (OutputDirectory))
    {
      fprintf (stderr, "Couldn't create output directory %s.\n", OutputDirectory);
      exit (1);
    }

  if (GenerateClasses)
    {
      GenerateCode (parse);
    }

  if (GenerateImakefile)
    {
      sprintf (filename, "%s/Imakefile", OutputDirectory);
      possibleUnlink (filename);
      if ((file = fopen (filename, "w")) == NULL)
	{
	  fprintf (stderr, "Couldn't open output file %s.\n", filename);
	  exit (1);
	}
      printf ("ILU-style Imakefile to %s...\n", filename);
      generate_imakefile (parse, file);
      fclose (file);
    }
}

static void PrintUsage (void)
{
  fprintf (stderr, "Usage:  %s [-I PATH] [-dir OUTPUTDIRECTORY] \
[-renames NAMESFILE] [-sname STUBSFILENAME] [-cname CLIENTFILENAME] \
[-removefirst] ISLFILE [ISLFILE ...]\n", ProgramName);
}

static void AppendIncludePath(char** inc, char* path)
{
  if(*inc) {
    *inc = realloc(*inc, strlen(*inc) + strlen("-I") + strlen(path) + 2);
    strcat(strcat(*inc, " -I"), path);
  } else {
    *inc = strcat(strcpy(malloc(strlen("-I") + strlen(path) + 1), "-I"), path);
  }
}

static boolean ParseArgs (int *pac, char ***pav)
{
  int ac = *pac;
  char **av = *pav;

  ac--;
  av++;
  while (*av[0] == '-')
    {
      if (strcmp(*av, "-dir") == 0)
	OutputDirectory = *++av;
      else if (strcmp(*av, "-imakefile") == 0)
	GenerateImakefile = TRUE;
      else if (strcmp(*av, "-serverskeleton") == 0)
	GenerateServerSkeleton = TRUE;
      else if (strcmp(*av, "-renames") == 0)
	NamesFile = *++av;
      else if (strcmp(*av, "-classes") == 0)
	GenerateClasses = TRUE;
      else if (strcmp(*av, "-removefirst") == 0)
	RemoveFirst = TRUE;
      else if (strcmp(*av, "-sname") == 0)
	StubsFile = *++av;
      else if (strcmp(*av, "-cname") == 0)
	ClientCommonFile = *++av;
      else if (strcmp(*av, "-hdrmap") == 0)
	InterfaceHeaderTranslationFile = *++av;
      else if (strncmp(*av, "-I", 2) == 0)
	{
	  if((*av)[2]) {
	    AppendIncludePath(&IncludePath, &(*av)[2]);
	  } else {
	    AppendIncludePath(&IncludePath, *++av);
	  }
	}
      else
	{
	  fprintf (stderr, "%s:  Invalid switch \"%s\".\n", ProgramName, *av);
	  PrintUsage();
	  return (FALSE);
	}
      ac--;
      av++;
    }

  if (!(GenerateServerSkeleton || GenerateImakefile || GenerateClasses))
    {
      GenerateClasses = TRUE;
    }

  *pac = ac;
  *pav = av;
  return (TRUE);
}

void
main (int ac, char **av, char **envp)
{
  list s;
  char **filename;

  int i_num_isl_files_processed = 0;
  int i_renaming_files = 0;

  if ((ProgramName = iluparser_GetProgramName(av[0])) == NULL)
#if (defined(WIN32) && defined(_WINIO))
    ProgramName = "wscmstub";
#else
    ProgramName = "scheme-stubber";
#endif /* (defined(WIN32) && defined(_WINIO)) */

  if (ac < 2)
    {
      PrintUsage();
      exit(1);
    }

  if (!ParseArgs (&ac, &av))
    exit(1);

  if (NamesFile != NULL)
    {
      if (!ReadSynonyms (NamesFile))
	{
	  fprintf (stderr, "Couldn't read names file %s.\n", NamesFile);
	  exit (1);
	};
    };      

  if (StubsFile || ClientCommonFile)
  	i_renaming_files = 1;

  for (filename = av;  *filename != NULL;  filename += 1)
    {
      if (i_renaming_files && (i_num_isl_files_processed > 0))
	{
	  fprintf (stderr, "Can't use -sname or -cname with more than 1 isl file.\n");
	  exit(1);
	}
      if(IncludePath)
	iluparser_SetIdlIncludePath(IncludePath);
      if ((s = ParseFile (*filename)) == NULL)
	{
	  fprintf (stderr, "Couldn't find or parse %s.\n", *filename);
	  exit (1);
	}
      list_enumerate (s, (iluparser_EnumProc) GenerateStubs, NULL);
      i_num_isl_files_processed++;
    }
  exit(0);
}

static void generate_scm_boilerplate (FILE *file, Interface parse)
{
  static char *prefixesForSCM[2] = { ";;", ";;" };

  iluparser_GenerateBoilerplate(file, parse, ProgramName, prefixesForSCM);
  fprintf (file, "\n");
}

/* makes up an interface_header_translation_list based on what's in
   InterfaceHeaderTranslationFile. Each entry in the list consists of
   the two null terminated names in one buffer e.g. fromname\0toname\0  
   - added by Dan Larner
*/ 
static list make_header_translation_list(char* pc_translation_file_name) {

  char  trans_delims[4] = {' ', '\t', '\n', '\0'};
  list  list_translations;
  FILE* pfile_translations;
  char* pc_translation_line;
  char* pc_from_name;
  char* pc_to_name;
  char* pc_list_entry;

  /* open up the file */
  if ((pfile_translations = fopen(pc_translation_file_name, "r")) == NULL)
  	return NULL;

  /* make a new empty list */
  list_translations = (list) new_list();
  list_clear (list_translations, FALSE);

  /* get some space to read lines into */
  pc_translation_line = (char*) iluparser_Malloc(256);

  /* read in a line at a time, creating the pairs - anything bad and we bomb */
  while (fgets(pc_translation_line, 256, pfile_translations) != NULL) {

	 /* ignore comment lines and simple spacing */
	 if ((pc_translation_line[0] == '#') || (pc_translation_line[0] == '\n'))
	 	continue;

  	 /* get the names */
	 pc_from_name = strtok(pc_translation_line, trans_delims);
	 pc_to_name = strtok(NULL, trans_delims);

	 if ((pc_from_name == NULL) || (pc_to_name == NULL)) { 
	 	/* some problem reading the names cleanup and return failure */
		list_clear (list_translations, TRUE);
		iluparser_Free(list_translations);
		iluparser_Free(pc_translation_line);
		fclose(pfile_translations);
		return NULL;
	 }

   /* make an entry to hold the two names, and add it to the list 
      note that an entry is two null terminated strings next to each other */
   pc_list_entry = iluparser_Malloc(strlen(pc_from_name) + strlen(pc_to_name) + 2);
   strcpy (pc_list_entry, pc_from_name);
   strcpy (pc_list_entry + strlen(pc_from_name) + 1, pc_to_name);
   list_insert(list_translations, pc_list_entry);
   
  } /* end while */

  /* cleanup */
  iluparser_Free(pc_translation_line);
  fclose(pfile_translations);

  return list_translations;
}


/* given a translation list and a name to look for, returns the translation
   for the name if one exists, else NULL - added by Dan Larner
*/
static char* get_translation(list list_translations, char* pc_interfacename) {

  listElement *pelement;
  char* pc_match;

  if (list_translations == NULL || list_translations->count < 1)
    return NULL;  /* null or empty list! */

  /* walk down the list, checking for a matching translation */
  for (pelement = list_translations->head;  pelement != NULL;  
  	   pelement = pelement->next) {
    if (strcmp(pelement->data, pc_interfacename) == 0) {
		/* found a matching translation */
		pc_match = ((char*)(pelement->data)) + strlen(((char*)(pelement->data))) + 1;
		return (pc_match);
	}
  }
  return NULL; /* no translation found */
}


/* given an interface name, checks the interface to header name
   translation list for a match.  If found, returns the translation,
   else returns interfacename. added by Dan Larner
*/
char* interface_header_name(char* pc_interfacename) {
	static list  list_translations = NULL;
	static int   i_list_creation_attempted = 0;
	char*        pc_translated_name ;

	if (InterfaceHeaderTranslationFile == NULL)
		return pc_interfacename;	/* there is no translations file */

	if (list_translations == NULL) {  /* first time through (or bad file) */

		if (i_list_creation_attempted == 1) 
			return pc_interfacename;	/* already tried to create and failed */

		list_translations = 
			make_header_translation_list(InterfaceHeaderTranslationFile);

		i_list_creation_attempted = 1;  

		if (list_translations == NULL)	{
  			fprintf (stderr, 
  			 		"Couldn't make translations from file %s - ignoring file",
			 		InterfaceHeaderTranslationFile);
			return pc_interfacename;  /* problem making translation list */
		}
	} /* end of creating translation list */

  /* return the translation if there is one */
  if ((pc_translated_name = get_translation(list_translations, pc_interfacename)) != NULL)
  	return pc_translated_name;

  return pc_interfacename;
}
