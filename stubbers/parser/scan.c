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
$Id: scan.c,v 1.34 1999/08/03 01:50:13 janssen Exp $
*/

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO) */

#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>

#include <stdlib.h>

#define TRUE  1
#define FALSE 0
typedef int Boolean;

#define AND  &&
#define OR   ||
#define NOT  !

#include "iluptype.h"

static boolean FirstInterface = TRUE;
static int count = 0;

/* list of directories to be searched for ISL files */
static list g_s_list_Includes = NULL;

/* holds the name of this program */
static char* s_g_pc_ProgramName = NULL;


static void PrintRef (LineNumber r)
{
  printf (" %ld", r);
}

static void OutputDocString (string s, int indent)
{
  char *p1;
  char *p2 = s;
  int n;

  while (1)
    {
      p1 = strchr(p2, '\n');
      n = (p1 == NULL) ? strlen(p2) : (p1 - p2);
      printf ("%*.*s%s%*.*s", indent, indent, "",
          (p2 == s) ? "\"" : " ", n, n, p2);
      if (p1 == NULL)
    break;
      printf ("\n");
      p2 = p1 + 1 + strspn(p1 + 1, "\t ");
    }      
  printf ("\"\n");
}

static void SprintfType (Type t, char *buffer)
{
  sprintf (buffer+strlen(buffer), "%s%s%s",
       (t->importInterfaceName == NULL) ? "" : t->importInterfaceName,
       (t->importInterfaceName == NULL) ? "" : ".",
       type_name(t));
}

static void SprintfException (Exception e, char *buffer)
{
  if (count > 0)
    sprintf (buffer+strlen(buffer), ", ");
  count += 1;
  sprintf (buffer+strlen(buffer), "%s%s%s",
       (e->importInterfaceName == NULL) ? "" : e->importInterfaceName,
       (e->importInterfaceName == NULL) ? "" : ".",
       exception_name(e));
}

static void SprintfArgument (Argument a, char *buffer)
{
  if (count > 0)
    sprintf (buffer+strlen(buffer), ", ");
  count += 1;
  sprintf (buffer + strlen(buffer), "%s : %s", name_base_name(a->name),
       a->sibling ? "SIBLING " : "");
  SprintfType (a->type, buffer);
}

static void PrintState (Argument a, Interface i)
{
  char buffer[2000];
  int pad;

  sprintf (buffer, "      %s%s : ",
	   a->sibling ? "PRIVATE " : "", argument_name(a));
  SprintfType (a->type, buffer + strlen(buffer));
  sprintf (buffer+strlen(buffer), ";");
  pad = 60 - strlen(buffer);
  if (pad < 0)
    pad = 0;
  printf ("%s%*.*s{defined %ld}\n", buffer, pad, pad, "", a->def);
}

static void PrintMethod (Procedure p, Interface s)
{
  char buffer[2000];
  int pad;

  sprintf (buffer, "      %s%s%s (", p->asynch ? "ASYNCHRONOUS " : "", p->functional ? "FUNCTIONAL " : "",
       name_base_name(p->name));
  if (list_size(p->arguments) > 0)
    {
      count = 0;
      list_enumerate (p->arguments, (void (*)(refany, refany)) SprintfArgument, buffer);
    }
  sprintf (buffer + strlen(buffer), ")");
  if (p->returnType != NULL)
    {
      sprintf(buffer + strlen(buffer), " : ");
      SprintfType (p->returnType, buffer+strlen(buffer));
    }
  if (list_size(p->exceptions) > 0)
    {
      sprintf (buffer+strlen(buffer), " {");
      count = 0;
      list_enumerate(p->exceptions, (void (*) (refany, refany)) SprintfException, buffer);
      sprintf (buffer+strlen(buffer), "}");
    }
  sprintf (buffer+strlen(buffer), ";");
  pad = 60 - strlen(buffer);
  if (pad < 0)
    pad = 0;
  printf ("%s%*.*s{defined %ld, id %d", buffer, pad, pad, "", p->def, p->id);
  if (p->authentication_type != NULL)
    printf (", auth \"%s\"", p->authentication_type);
  printf ("}\n");
  if (p->doc_string != NULL)
    OutputDocString (p->doc_string, 8);
}

static void PrintClassName (Type t, boolean comma)
{
  if (comma)
    printf (", ");
  if (t->importInterfaceName != NULL)
    printf ("%s.", t->importInterfaceName);
  printf ("%s", type_name(t));
}

static void PrintClass (Type t)
{
  int pad = 25 - strlen(type_name(t));
  Class od = class_object(t);

  printf ("  %s  %*.*s{", type_name(t), pad, pad, "");
  if (od->brand != NULL)
    printf ("brand \"%s\", ", od->brand);
  if (t->def != 0)
    printf ("defined on line %ld", t->def);
  else if (t->importInterfaceName != NULL)
    printf ("from interface \"%s\"", t->importInterfaceName);
  else
    printf ("**** no definition ****");
  if (list_size(t->refs) > 0)
    {
      printf (", refs ");
      list_enumerate (t->refs, (void (*)(refany, refany)) PrintRef, 0);
    }
  if (od->collectible)
    printf (", collectible");
  if (od->optional)
    printf (", optional");
  if (od->singleton)
    printf (", singleton");
#ifdef ILU_HTTPNG_OBJECTS
  if (od->sealed)
    printf (", sealed");
  if (od->local)
    printf (", local");
#endif
  if (od->authentication != NULL)
    printf (", authentication \"%s\"", od->authentication);
  printf ("}\n");
  if (list_size(od->superclasses) > 0)
      {
    printf ("    superclass%s:  ",
        (list_size(od->superclasses) > 1) ? "es" : "");
    PrintClassName(list_car(od->superclasses), FALSE);
    list_enumerate (list_cdr(od->superclasses), (void (*)(refany, refany)) PrintClassName, (refany) TRUE);
    printf ("\n");
      }
#ifdef ILU_HTTPNG_OBJECTS
  if (od->local && list_size(od->state) > 0) {
    printf ("    state:\n");
    list_enumerate(od->state, (iluparser_EnumProc) PrintState, t->interface);
  }
#endif
  if (od->methods != NULL && list_size(od->methods) > 0)
    {
      printf ("    methods:\n");
      list_enumerate (od->methods, (void (*)(refany, refany)) PrintMethod, t->interface);
    }
  if (od->doc_string != NULL)
    {
      printf ("    documentation:\n");
      OutputDocString (od->doc_string, 6);
    }
  printf ("    unique id:  %s\n\n", t->uid);
}

static void PrintType (Type t)
{
  if (type_basic_type (t) != object_Type)
    {
      int pad;

      pad = 25 - strlen(type_name(t));
      printf ("  %s  %*.*s{\"%s\", ", type_name(t), pad, pad, "", type_uid(t));
      if (t->builtIn)
    printf ("<built-in>");
      else if (t->def != 0)
    printf ("defined on line %ld", t->def);
      else if (t->importInterfaceName != NULL)
    printf ("from interface \"%s\"", t->importInterfaceName);
      else
    printf ("**** no definition ****");
      if (list_size(t->refs) > 0)
    {
      printf (", referenced on %d", (int) list_car(t->refs));
      list_enumerate (list_cdr(t->refs), (void (*)(refany, refany)) PrintRef, 0);
    }
      printf ("}\n");
    }
}

static void MaybePrintType (Type t)
{
  if (list_size(t->refs) > 0 || t->def > 0)
    PrintType (t);
}

static void PrintException (Exception e)
{
  int pad = 35 - (strlen(exception_name(e)) + ((e->type == NULL) ? 0 : strlen(type_name(e->type))));

  printf ("  %s%s%s  %*.*s{\"%s\", ", exception_name(e),
      (e->type == NULL) ? "   " : " : ",
      (e->type == NULL) ? "" : type_name(e->type),
      pad, pad, "", (e->corba_rep_id != NULL) ? e->corba_rep_id : "");
  if (e->builtIn)
    printf ("<built-in>");
  else if (e->def != 0)
    printf ("defined on line %ld", e->def);
  else if (e->importInterfaceName != NULL)
    printf ("from interface \"%s\"", e->importInterfaceName);
  else
    printf ("**** no definition ****");
  if (list_size(e->refs) > 0)
    {
      printf (", refs %d", (int) list_car(e->refs));
      list_enumerate (list_cdr(e->refs), (void (*)(refany, refany)) PrintRef, 0);
    }
  if (list_size(e->refs) < 1)
    printf (", not used");
  printf ("}\n");
}

static void PrintConstant (Constant c)
{
  TypeKind ctk, vtk;
  char *ctypename = name_base_name(c->name);

  ctk = type_ur_kind(c->type);
  vtk = c->value->type;

  printf ("  %s (%s) = ", name_base_name(c->name), type_name(c->type));
  if (c->value == NULL)
    printf ("(undefined)\n");
  else
    {
      switch (ctk)
    {
    case shortinteger_Type:
    case integer_Type:
    case longinteger_Type:
      if (vtk != integer_Type)
        fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated integer value.\n", ctypename);
      else
        printf ("%s%lu\n", (c->value->val.i.sign < 0) ? "-" : "", c->value->val.i.value);
      break;

    case byte_Type:
    case shortcardinal_Type:
    case cardinal_Type:
    case longcardinal_Type:
      if (vtk != integer_Type)
        fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated integer value.\n", ctypename);
      else if (c->value->val.i.sign < 0)
        fprintf (stderr, "\n*** Constants of type \"%s\" may not be negative.\n", ctypename);
      else
        printf ("%lu\n", c->value->val.i.value);
      break;

    case real_Type:
    case shortreal_Type:
    case longreal_Type:
      if (vtk != real_Type && vtk != integer_Type)
        fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated real or integer value.\n", ctypename);
      else if (vtk == integer_Type)
        printf ("%lu.0e0\n", c->value->val.i.value);
      else if (c->value->val.r.fraction == NULL)
        printf ("%s%se%lu.0e0\n",
            (c->value->val.i.sign < 0) ? "-" : "", c->value->val.r.value, c->value->val.r.exponent);
      else if (vtk == real_Type)
        printf ("%s%s.%se%lu\n", (c->value->val.i.sign < 0) ? "-" : "", c->value->val.r.value,
             c->value->val.r.fraction, c->value->val.r.exponent);
      break;

    case boolean_Type:
      if (vtk != boolean_Type)
        fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated boolean value.\n", ctypename);
      else
        printf ("%s\n", c->value->val.b ? "True" : "False");
      break;

    case sequence_Type:
      if (vtk != shortcharacter_Type)
        fprintf (stderr, "\n*** Constants of type \"%s\" must have an associated string value.\n", ctypename);
      else
        printf ("\"%s\"\n", c->value->val.s);
      break;

    case enumeration_Type:
      if (vtk != shortcharacter_Type)
        fprintf (stderr, "*** Constants of type \"%s\" must have an associated string value.\n", ctypename);
      else
        printf ("%s\n", c->value->val.s);
      break;

    default:
      fprintf (stderr, "\n*** Constants of type \"%s\" not allowed.\n", ctypename);
      break;
    }
    }
}


static void PrintImport (Imported s)
{
  printf (", \"%s\"", s->name);
}

static char *ModTime (char *path)
{
  static char timebuf[30];
  struct stat statbuf;

#ifdef MACOS
    return "Jan 15, 1592";
#else
  stat (path, &statbuf);
  strcpy (timebuf, ctime(&statbuf.st_mtime));
  timebuf[24] = '\0';
  return (timebuf);
#endif /* MACOS */
}

static void PrintParse (Interface s)
{
  printf ("Interface \"%s\"%s%s%s", interface_name(s),
      (s->brand == NULL) ? "" : " (",
      (s->brand == NULL) ? "" : s->brand,
      (s->brand == NULL) ? "" : ")");
  if (list_size(s->imports) > 0)
    {
      printf (", imports \"%s\"", ((Imported)list_car(s->imports))->name);
      list_enumerate (list_cdr(s->imports), (void (*)(refany, refany)) PrintImport, 0);
    }
  printf ("     {defined on line %ld of file %s (%s)}\n", s->def, s->filename, ModTime(s->filename));
  if (list_size(s->types) > 0)
    {
      printf ("\nTypes:\n");
      list_enumerate (s->types, (void (*)(refany, refany)) MaybePrintType, s);
    }
  if (list_size(s->classes) > 0)
    {
      printf ("\nClasses:\n");
      list_enumerate (s->classes, (void (*)(refany, refany)) PrintClass, s);
    }
  if (list_size(s->exceptions) > 0)
    {
      printf ("Exceptions:\n");
      list_enumerate (s->exceptions, (void (*)(refany, refany)) PrintException, s);
    }
  if (list_size(s->constants) > 0)
    {
      printf ("\nConstants:\n");
      list_enumerate (s->constants, (void (*)(refany, refany)) PrintConstant, s);
    }
}

static void PrintInterfaces (Interface i)
{
  if (! FirstInterface)
    printf ("\n\n");
  PrintParse(i);
  FirstInterface = FALSE;
}


static int usage()
{
  fprintf(stderr, "Usage: %s [-I ISLDIRECTORY] ISLFILE\n", s_g_pc_ProgramName);
  return 1;
}



#ifdef WIN16
    int scan_main (int ac, char **av, char **envp)
#else
    int main (int ac, char **av, char **envp)
#endif

{
  list s;
  char* pc_filename;
  s_g_pc_ProgramName = *av;
    
  if (ac < 2)
    return usage();
    
  /* initialize to empty list of directories to search for ISL files */
  g_s_list_Includes = new_list();  

  /* skip over program name, and process the remaining args */
  av++; 
  while(*av != NULL && *av[0] == '-') {
    if (strcmp(*av, "-I") == 0) {
      if (*++av != NULL)
        list_insert(g_s_list_Includes, *av);
      else
        return usage();
    }
    else {
      usage();
      fprintf(stderr, "%s: Invalid switch \"%s\".\n", s_g_pc_ProgramName, *av);
      return 1;
    }
    av++;
  }
  pc_filename = *av;
  
  if (*(av + 1) != NULL) {
      usage();
      fprintf(stderr, "%s: Invalid extra args after filename \"%s\".\n", s_g_pc_ProgramName, *(av+1));
      return 1;
  }

  /* tell the parser about the directories it should search for ISL files in */
  iluparser_RegisterInterfaceDirectories(g_s_list_Includes);

  /* run the parser */
  s = ParseFile (pc_filename);
  FirstInterface = TRUE;
  
  /* processes the interfaces */
  if (s != NULL) {
    list_enumerate (s, (void (*)(refany, refany)) PrintInterfaces, NULL);
    return 0;
  }
    
  return 1;

}
