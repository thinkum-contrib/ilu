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
$Id: sysdcl.c,v 1.14 1999/08/03 01:50:10 janssen Exp $
*/

#include <stdlib.h>

#include "lisp.h"

static boolean MatchStrings (string s1, string s2)
{
  return (strcmp(s1, s2) == 0);
}

static void AddNeededSystemE (Exception e, list l)
{
  if (e->importInterfaceName != NULL && list_find (l, (FindProc) MatchStrings, e->importInterfaceName) == NULL)
    list_insert (l, e->importInterfaceName);
}

static void AddNeededSystemC (Constant c, list l)
{
  if (c->importInterfaceName != NULL && list_find (l, (FindProc) MatchStrings, c->importInterfaceName) == NULL)
    list_insert (l, c->importInterfaceName);
}

static void AddNeededSystemT (Type t, list l)
{
  if (t->importInterfaceName != NULL && list_find (l, (FindProc) MatchStrings, t->importInterfaceName) == NULL)
    list_insert (l, t->importInterfaceName);
}

static void AddNeededSystem (string s, Context context)
{
  fprintf (context->file, " :%s", lisp_string(s));
}

static void AddNeededSystems (Context context)
{
  list l = (list) new_list();

  list_insert(l, "ilu");

  list_enumerate (context->interface->exceptions, (EnumProc) AddNeededSystemE, l);
  list_enumerate (context->interface->types, (EnumProc) AddNeededSystemT, l);
  list_enumerate (context->interface->constants, (EnumProc) AddNeededSystemC, l);

  list_enumerate (l, (EnumProc) AddNeededSystem, context);
}

void GenerateCompileDriver (Interface parse, char *output_dir)
{
  FILE *file;
  char filename[1000];
  string lin = (string) lisp_interface_name(parse);
  struct context_s context;

  sprintf (filename, "%s/.compile-files.lisp", output_dir);
  if ((file = fopen (filename, "w")) == NULL)
    {
      fprintf (stderr, "Couldn't open output file %s.\n", filename);
      exit (1);
    }
  printf ("compile driver for interface %s to %s...\n", interface_name(parse), filename);

  context.file = file;
  context.interface = parse;

  fprintf (file, ";;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-\n\n");
  fprintf (file, "(in-package :user)\n\n");
  fprintf (file, "(load \"%s/pdefsys.lisp\")\n", getenv("ILUHOME"));
  fprintf (file, "(compile-file \"%s/%s-sysdcl.lisp\")\n", output_dir, lin);
  fprintf (file, "(load \"%s/%s-sysdcl\")\n", output_dir, lin);
  fprintf (file, "(pdefsys:compile-system :%s)\n", lin);
  fprintf (file, "(pdefsys:compile-system :%s-server)\n", lin);
  fprintf (file, "(exit)\n");

  fclose (file);
}

void GenerateSystemDeclaration (Interface parse, char *output_dir)
{
  FILE *file;
  char filename[1000];
  string lin = (string) lisp_interface_name(parse);
  struct context_s context;

  sprintf (filename, "%s/%s-sysdcl.lisp", output_dir, lin);
  if ((file = fopen (filename, "w")) == NULL)
    {
      fprintf (stderr, "Couldn't open output file %s.\n", filename);
      exit (1);
    }
  printf ("sysdcl for interface %s to %s...\n", interface_name(parse), filename);

  context.file = file;
  context.interface = parse;

  fprintf (file, ";;;-*- Package: USER; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-\n\n");
  fprintf (file, "(in-package :user)\n\n");

  fprintf (file, "(eval-when (load eval)\n  (pdefsys:load-system :ilu))\n\n");

  fprintf (file, "(pdefsys:defsystem :%s\n", lin);
  fprintf (file, "\t(:load-before-compile (");
  AddNeededSystems (&context);
  fprintf (file, ")\n\t:needed-systems (");
  AddNeededSystems (&context);
  fprintf (file, "))\n");

  fprintf (file, "\t(\"%s-basics\" :load-before-compile t))\n\n", lin);

  fprintf (file, "(pdefsys:defsystem :%s-server\n", lin);
  fprintf (file, "\t(:load-before-compile (:%s)\n\t:needed-systems (:%s))\n", lin, lin);
  fprintf (file, "\t(\"%s-server-procs\"))\n", lin);

  fclose (file);
}
