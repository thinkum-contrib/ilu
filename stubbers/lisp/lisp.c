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

/* Last edited by Mike Spreitzer December 6, 1994 3:44 pm PST */

/*
$Id: lisp.c,v 1.25 1999/08/03 01:50:08 janssen Exp $
*/

#include <stdlib.h>

#include "lisp.h"

typedef int Boolean;

#define EOS  '\0'
#define DOT	'.'

extern string lisp_interface_name(Interface i);

char *OutputDirectory = ".";
char *ProgramName = NULL;
char *FileExtension = "lisp";
boolean GenerateSysdcl = FALSE;
boolean GenerateServerSkeleton = FALSE;
boolean GenerateServerStubs = FALSE;
boolean GenerateBasics = FALSE;

extern void GenerateSStubs(Interface interface, FILE *file);
extern void GenerateTypeStubs();
extern void GenerateClassStubs();
extern void set_default_interface(Interface i);
static void generate_boilerplate(FILE *file, Interface parse);
static void generate_c_boilerplate(FILE *file, Interface parse);
extern void ExportPackage (Interface, FILE *);
extern void GenerateTypeDefinitions (Interface, FILE *);
extern void GenerateTypeCode (Interface, FILE *);
extern void GenerateExceptionDefinitions (Interface, FILE *);
extern void GenerateExceptionCode (Interface, FILE *);
extern void GenerateSystemDeclaration (Interface, char *directory_name);

static void DoClasses (Interface parse)
{
  char filename[1000];
  FILE *file;

  if (GenerateBasics)
    {
      sprintf (filename, "%s/%s-basics.%s", OutputDirectory, lisp_interface_name(parse), FileExtension);
      if ((file = fopen (filename, "w")) == NULL)
	{
	  fprintf (stderr, "Couldn't open output file %s.\n", filename);
	  exit (1);
	}
      printf ("basics file for interface %s to %s...\n", interface_name(parse), filename);
      generate_boilerplate (file, parse);
      ExportPackage (parse, file);
      GenerateTypeDefinitions (parse, file);
      GenerateTypeCode (parse, file);
      GenerateExceptionDefinitions (parse, file);
      GenerateExceptionCode (parse, file);
      GenerateConstants (parse, file);
      fclose (file);
    }

  if (GenerateServerStubs && (list_size(parse->classes) > 0))
    {
      sprintf (filename, "%s/%s-server-procs.%s", OutputDirectory, lisp_interface_name(parse), FileExtension);
      if ((file = fopen (filename, "w")) == NULL)
	{
	  fprintf (stderr, "Couldn't open output file %s.\n", filename);
	  exit (1);
	}
      printf ("server stubs for interface %s to %s...\n", interface_name(parse), filename);
      generate_boilerplate (file, parse);
      GenerateSStubs (parse, file);
      fclose (file);
    }
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

#endif
	  
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

#else

  fprintf (stderr, "%s:  Can't test directory \"%s\".\n",
	   ProgramName, dirname);
  return (TRUE);

#endif
}

static void GenerateStubs (Interface parse)
{
  set_default_interface (parse);

  if (! makedir (OutputDirectory))
    {
      fprintf (stderr, "Couldn't create output directory %s.\n", OutputDirectory);
      exit (1);
    }

  DoClasses(parse);

  if (GenerateSysdcl)
    GenerateSystemDeclaration(parse, OutputDirectory);

#if 0
  GenerateCompileDriver (parse, output_dir);
#endif
}

static void PrintUsage (void)
{
  fprintf (stderr, "Usage:  %s [-dir OUTPUTDIRECTORY] [-extension FILEEXTENSION] [-sysdcl] [-serverskeleton] [-serverstubs] [-basics] ISLFILE [ISLFILE ...]\n", ProgramName);
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
      else if (strcmp(*av, "-sysdcl") == 0)
	GenerateSysdcl = TRUE;
      else if (strcmp(*av, "-extension") == 0)
	FileExtension = *++av;
      else if (strcmp(*av, "-serverskeleton") == 0)
	GenerateServerSkeleton = TRUE;
      else if (strcmp(*av, "-serverstubs") == 0)
	GenerateServerStubs = TRUE;
      else if (strcmp(*av, "-basics") == 0)
	GenerateBasics = TRUE;
      else
	{
	  fprintf (stderr, "%s:  Invalid switch \"%s\".\n", ProgramName, *av);
	  PrintUsage();
	  return (FALSE);
	}
      ac--;
      av++;
    }

  if (!(GenerateServerSkeleton || GenerateSysdcl || GenerateBasics || GenerateServerStubs))
    {
      GenerateBasics = TRUE;
      GenerateServerStubs = TRUE;
      GenerateSysdcl = TRUE;
    }

  *pac = ac;
  *pav = av;
  return (TRUE);
}

int main (int ac, char **av, char **envp)
{
  list s;
  char **filename;
  extern Interface GetInterface(string, string);

  if ((ProgramName = iluparser_GetProgramName(av[0])) == NULL)
    ProgramName = av[0];

  if (ac < 2)
    {
      PrintUsage();
      return 1;
    }

  if (!ParseArgs (&ac, &av))
    return 1;

  for (filename = av;  *filename != NULL;  filename += 1)
    {
      if ((s = ParseFile(*filename)) == NULL)
	{
	  fprintf (stderr, "Couldn't find or parse %s.\n", *filename);
	  exit (1);
	}
      list_enumerate(s, (EnumProc) GenerateStubs, NULL);
    }
  return 0;
}

static void generate_boilerplate (FILE *file, Interface parse)
{
  char *prefixes[2] = { ";;;", ";;;" };
  
  fprintf (file, ";;; -*- Mode:Lisp;  Package: %s;  Syntax: COMMON-LISP;  Base: 10 -*-\n",
	   lisp_interface_name(parse));
  fprintf (file, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n");
  iluparser_GenerateBoilerplate (file, parse, ProgramName, prefixes);
  fprintf (file, ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n");
}

static void generate_c_boilerplate (FILE *file, Interface parse)
{
  char *prefixes[2] = { " * ", " * " };

  fprintf (file, "/*\n");
  iluparser_GenerateBoilerplate (file, parse, ProgramName, prefixes);
  fprintf (file, " */\n\n");
}

