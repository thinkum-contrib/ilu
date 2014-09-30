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
/* Last edited by Mike Spreitzer September 15, 1995 5:06 pm PDT */
 

#include <stdlib.h>
#include <stdarg.h>
#include "cstubber.h"

#define IndentIncrement		4

static char		*b = "                                                 ";
static char		*dent;
static FILE		*f;
static char 		**mList = NULL;
static int		nm = 0;

void addMethodToList (char *m)
{
  int i = nm++;

  if ( mList )
    mList = CRealloc(mList, nm * sizeof(char *), char * );
  else
    mList = CAlloc( nm * sizeof( char * ), char * );
  mList[ i ] = m;
}

void clearMethodList (void)
{
  if ( mList )
    iluparser_Free( mList );
  mList = NULL;
  nm = 0;
}

boolean methodInList( char *m )
{
  int i;

  if ( nm <= 0 )
    return( False );
  if ( !mList )
    return( False );
  for( i = 0; i < nm; i++ ) {
    if ( strcmp( m, mList[ i ]) == 0 )
      return( True );
  }
  return( False );
}

void dedent( )
{
    dent += IndentIncrement;
}

void emit( char	*fmt, ... )
{
    va_list	args;

    fprintf( f, "%s", dent );
    va_start( args, fmt );
    vfprintf( f, fmt, args );
    va_end( args );
}

void error( char *fmt, ... )
{
    va_list     args;
 
    fprintf( stderr, "Error: " );
    va_start( args, fmt );
    vfprintf( stderr, fmt, args );
    va_end( args );
}
 
void fatal( char *fmt, ... )
{
#if (defined(WIN32) && defined(_WINIO))
	extern void winio_end();
#endif
    va_list     args;
 
    fprintf( stderr, "Fatal: " );
    va_start( args, fmt );
    vfprintf( stderr, fmt, args );
    va_end( args );
#if (defined(WIN32) && defined(_WINIO))
	printf("\nwcstub complete\n");
    winio_end(); // let user look at output till user closes
	exit( 1 );
#else
    exit( 1 );
#endif
}

void indent( )
{
    dent -= IndentIncrement;
}

void setFile( FILE	*file )
{
    f = file;
    dent = b + strlen( b );
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

 

