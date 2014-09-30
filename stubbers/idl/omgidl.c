#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "iluidl.h"

extern int idlparse();

static list search_list;

static void
usage()
{
  fprintf (stderr, "Usage: omgidl [-a] [-h] filename\n");
  fprintf (stderr, "  -a       Analyse compliance with AB style guide.\n");
  fprintf (stderr, "  -I<path> Add path to include path.\n");
  fprintf (stderr, "  -h       Print this help.\n");
  exit(1);
}

int main(int argc, char* argv[])
{
  char *filename;
  extern FILE* idlin;
  extern int idl_flex_debug;
  list defs;
  list toplevel_prefix; /* Current prefix for repository ID. */
  int i;

  search_list = iluparser_new_list ();

  for (i = 1; i < argc; i++){
    if (argv[i][0] != '-')
      break;
    switch (argv[i][1]){
    case 'a':
      idl_subset |= IDL_STYLE_GUIDE;
      break;
    case 'I':
      if (argv[i][2])
	list_insert (search_list, argv[i]+2);
      else
	list_insert (search_list, argv[++i]);
      break;
    case 'h':
    default:
      usage();
      break;
    }
  }
  if (i+1 != argc)
    usage();
  filename = argv[i];
	
  /* Open top-level IDL file. */
  init_types();
  idl_flex_debug=0;
  if ((idlin = fopen (filename, "r")) == NULL)
    {
      perror(argv[1]);
      exit(1);
    }

  /* Syntactical analysis */
  idlsetinitialfile(filename);
  if(idlparse())
    return 1;
  defs = the_result;

  /* Semantical analysis */

  /* join modules for re-opening */
  defs = reopen_modules (defs);
  /* backlink, toplevel has no parent */
  list_enumerate (defs, definition_backlink,0);
  /* resolve all names */
  list_enumerate (defs, definition_resolvenames, defs);
  /* perform consistency checks, compute constants */
  list_enumerate (defs, definition_check, defs);
  /* assign repository IDs */
  toplevel_prefix = iluparser_new_list ();
  list_push (toplevel_prefix, "");
  list_enumerate (defs, definition_setuid, toplevel_prefix);
  /* Test conformance with AB style guide */
  list_enumerate (defs, ab_style, 0);

  /* Drop all results :-) */
  return 0;
}

/********** Copies from ilu.bison *****************************/

list iluparser_new_list (void)
{
  list ptr;

  ptr = (list) iluparser_Malloc (sizeof(struct list_s));
  ptr->head = NULL;
  ptr->tail = NULL;
  ptr->count = 0;
  return (ptr);
}

void list_insert (list l, refany element)
{
  listElement *new;

  if (l == NULL)
    return;

  new = (listElement *) iluparser_Malloc (sizeof(listElement));

  new->data = element;
  new->next = NULL;
  if (l->tail != NULL)
    l->tail->next = new;
  l->tail = new;
  if (l->head == NULL)
    l->head = new;
  l->count += 1;
}

void list_push (list l, refany element)
{
  listElement *new;

  if (l == NULL)
    return;

  new = (listElement *) iluparser_Malloc (sizeof(listElement));

  new->data = element;
  new->next = l->head;
  l->head = new;
  if (l->tail == NULL)
    l->tail = new;
  l->count += 1;
}

void list_clear (list l, boolean freeElements)
{
  listElement *p, *last;

  for (p = l->head, last = NULL;  p != NULL;  p = last)
    {
      last = p->next;
      if (freeElements && p->data != NULL)
	iluparser_Free(p->data);
      p->data = NULL;
      p->next = NULL;
      iluparser_Free(p);
    }
  l->head = NULL;
  l->tail = NULL;
  l->count = 0;
}

void list_enumerate (list l, iluparser_EnumProc proc, refany rock)
{
  listElement *ptr;

  if (l == NULL || l->count < 1)
    return;

  for (ptr = l->head;  ptr != NULL;  ptr = ptr->next)
    (*proc)(ptr->data, rock);
}

refany list_find (list l, iluparser_FindProc proc, refany rock)
{
  listElement *ptr;

  if (l == NULL)
    return (NULL);

  for (ptr = l->head;  ptr != NULL;  ptr = ptr->next)
    if ((*proc)(ptr->data, rock))
      return (ptr->data);
  return (NULL);
}

void *iluparser_Malloc (unsigned long size)
{
  char *p;

  if ((p = (char *) malloc(size)) == ((char *) 0))
    {
      fprintf (stderr, "Malloc:  Can't malloc %lu bytes.\n", size);
      *((char *) 0) = 20;
      return (NULL);
    }
  else
    return (p);
}
     
void iluparser_Free (void *ptr)
{
  if (ptr != ((char *) 0))
    free (ptr);
  else
    {
      fprintf (stderr, "Free:  Null pointer passed for freeing!\n");
      *((char *) 0) = 20;
    }
}

char *ilu_strdup(char *src)
{
  char *dst=(char *) iluparser_Malloc(strlen(src)+1);

  strcpy(dst,src);
  return dst;
}

/* a strcasecmp(), since we don't have one with ANSI */
int ilu_strcasecmp (char *a, char *b)
{
  register unsigned char *p1 = ( unsigned char *) a;
  register unsigned char *p2 = ( unsigned char *) b;
  unsigned char c1, c2;

  if (p1 == p2)
    return 0;

  do
    {
      c1 = tolower (*p1++);
      c2 = tolower (*p2++);
      if (c1 == '\0')
        break;
    }
  while (c1 == c2);

  return c1 - c2;
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

string iluparser_FindFileInDir (char *dir, char *name)
{
  char buf[1000];
  char *canonical_name;
  boolean status;

#if (defined(WIN32) || defined(WIN16))
  sprintf (buf, "%s\\%s", dir, name);
#elif defined( macintosh )
  sprintf (buf, "%s%s", dir, name);
#else
  sprintf (buf, "%s/%s", dir, name);
#endif /* (defined(WIN32) || defined(WIN16)) */

  canonical_name = iluparser_CanonicalPathname (buf);

#ifdef _IS_POSIX
  status = (access (canonical_name, R_OK) == 0);
#else
  {
    FILE *f = fopen(canonical_name, "r");
    if (f != NULL)
      fclose(f);
    status = (f != NULL);
  }
#endif
  if (status)
    return canonical_name;
  else {
    iluparser_Free(canonical_name);
    return NULL;
  }
}

static boolean
  FindFileInDir (string dir, string name)
{
  string b = iluparser_FindFileInDir(dir, name);
  if (b == NULL)
    return FALSE;
  else {
    iluparser_Free(b);
    return TRUE;
  }    
}

static list
GetSearchList (void)
{
  return search_list;
}

static string FigureFilename (string name, string extension)
{
  char buf[1000];
  char nbuf[1000];
  char *testname;
  char *dir;
  char *ext;
  
  if ((ext = strrchr(name, '.')) == NULL)
    {
      sprintf (nbuf, "%s%s", name, (extension == NULL) ? ".isl" : extension);
      testname = nbuf;
    }
  else
    testname = name;

#if (defined(WIN32) || defined(WIN16))
  if (*testname == '.' || *testname == '\\')
#elif defined( macintosh )
  /*
     For the Mac implementation, we'll do search list (non-canonical) processing
     only if:
       The file name is simple (no paths, no leading colon).
  */
  if ( (*testname == ':') || ( strchr( testname, ':' ) != 0 ) )
#else
  if (*testname == '.' || *testname == '/')
#endif /* (defined(WIN32) || defined(WIN16)) */
    return (iluparser_CanonicalPathname(testname));

  if ((dir = list_find(GetSearchList(), (iluparser_FindProc) FindFileInDir, testname)) != NULL)
    {  
#if (defined(WIN32) || defined(WIN16))
      sprintf (buf, "%s\\%s", dir, testname);
#elif defined( macintosh )
      sprintf (buf, "%s%s", dir, testname);
#else
      sprintf (buf, "%s/%s", dir, testname);
#endif /* (defined(WIN32) || defined(WIN16)) */
      return (iluparser_CanonicalPathname(buf));
    }
  else
    return NULL;
}

string iluparser_FindFileInIncludes (string filename)
{
  return FigureFilename(filename, "");
}

