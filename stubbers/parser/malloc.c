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
$Id: malloc.c,v 1.18 1999/08/03 01:50:14 janssen Exp $
*/

#include <stdlib.h>
#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include <ilumac.h>
#else
#include <iluconf.h>
#endif

#include <stdio.h>

#if ((defined(WIN32) || defined(WIN16)) && defined(_WINIO))
#include <winiodef.h>
#endif /* ((defined(WIN32) || defined(WIN16)) && defined(_WINIO)) */

extern void *iluparser_Malloc (unsigned long size);
extern void *iluparser_Realloc (void *, unsigned long size);
extern void  iluparser_Free (void *);

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

void *iluparser_Realloc (void *ptr, unsigned long size)
{
  char *p;

  if (ptr == ((char *) 0))
    {
      fprintf (stderr, "Realloc:  Null pointer passed!\n");
      *((char *) 0) = 20;
    }
  else if ((p = (char *) realloc(ptr, size)) == ((char *) 0))
    {
      fprintf (stderr, "Realloc:  Can't malloc %lu bytes.\n", size);
      *((char *) 0) = 20;
    }
  else
    return (p);
  return (NULL);
}

#if !(defined(__GNUC__) || defined(HAVE_ALLOCA) || defined(WIN32) || defined(WIN16))

/*
	alloca -- (mostly) portable public-domain implementation -- D A Gwyn

	last edit:	93/09/20	janssen
	   No, use Malloc(), and don't include config.h.

	last edit:	86/05/30	rms
	   include config.h, since on VMS it renames some symbols.
	   Use xmalloc instead of malloc.

	This implementation of the PWB library alloca() function,
	which is used to allocate space off the run-time stack so
	that it is automatically reclaimed upon procedure exit, 
	was inspired by discussions with J. Q. Johnson of Cornell.

	It should work under any C implementation that uses an
	actual procedure stack (as opposed to a linked list of
	frames).  There are some preprocessor constants that can
	be defined when compiling for your specific system, for
	improved efficiency; however, the defaults should be okay.

	The general concept of this implementation is to keep
	track of all alloca()-allocated blocks, and reclaim any
	that are found to be deeper in the stack than the current
	invocation.  This heuristic does not reclaim storage as
	soon as it becomes invalid, but it will do so eventually.

	As a special case, alloca(0) reclaims storage without
	allocating any.  It is a good idea to use alloca(0) in
	your main control loop, etc. to force garbage collection.
*/

/*
	Define STACK_DIRECTION if you know the direction of stack
	growth for your system; otherwise it will be automatically
	deduced at run-time.

	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
*/

#ifndef STACK_DIRECTION
#define	STACK_DIRECTION	0		/* direction unknown */
#endif

#if STACK_DIRECTION != 0

#define	STACK_DIR	STACK_DIRECTION	/* known at compile-time */

#else	/* STACK_DIRECTION == 0; need run-time code */

static int	stack_dir;		/* 1 or -1 once known */
#define	STACK_DIR	stack_dir

static void
find_stack_direction (void)
{
  static char	*addr = NULL;	/* address of first
				   `dummy', once known */
  auto char	dummy;		/* to get stack address */

  if (addr == NULL)
    {				/* initial entry */
      addr = &dummy;

      find_stack_direction ();	/* recurse once */
    }
  else				/* second entry */
    if (&dummy > addr)
      stack_dir = 1;		/* stack grew upward */
    else
      stack_dir = -1;		/* stack grew downward */
}

#endif	/* STACK_DIRECTION == 0 */

/*
	An "alloca header" is used to:
	(a) chain together all alloca()ed blocks;
	(b) keep track of stack depth.

	It is very important that sizeof(header) agree with malloc()
	alignment chunk size.  The following default should work okay.
*/

#ifndef	ALIGN_SIZE
#define	ALIGN_SIZE	sizeof(double)
#endif

typedef union hdr
{
  char	align[ALIGN_SIZE];	/* to force sizeof(header) */
  struct
    {
      union hdr *next;		/* for chaining headers */
      char *deep;		/* for stack depth measure */
    } h;
} header;

/*
	alloca( size ) returns a pointer to at least `size' bytes of
	storage which will be automatically reclaimed upon exit from
	the procedure that called alloca().  Originally, this space
	was supposed to be taken from the current stack frame of the
	caller, but that method cannot be made to work for some
	implementations of C, for example under Gould's UTX/32.
*/

static header *last_alloca_header = NULL; /* -> last alloca header */

void *
alloca (SIZE_T size)			/* returns pointer to storage */
{
  auto char	probe;		/* probes stack depth: */
  register char	*depth = &probe;

#if STACK_DIRECTION == 0
  if (STACK_DIR == 0)		/* unknown growth direction */
    find_stack_direction ();
#endif

				/* Reclaim garbage, defined as all alloca()ed storage that
				   was allocated from deeper in the stack than currently. */

  {
    register header	*hp;	/* traverses linked list */

    for (hp = last_alloca_header; hp != NULL;)
      if ((STACK_DIR > 0 && hp->h.deep > depth)
	  || (STACK_DIR < 0 && hp->h.deep < depth))
	{
	  register header	*np = hp->h.next;

	  iluparser_Free ((void *) hp);	/* collect garbage */

	  hp = np;		/* -> next header */
	}
      else
	break;			/* rest are not deeper */

    last_alloca_header = hp;	/* -> last valid storage */
  }

  if (size == 0)
    return NULL;		/* no allocation required */

  /* Allocate combined header + user data storage. */

  {
    register void *	new = iluparser_Malloc (sizeof (header) + size);
    /* address of header */

    ((header *)new)->h.next = last_alloca_header;
    ((header *)new)->h.deep = depth;

    last_alloca_header = (header *)new;

    /* User storage begins just after header. */

    return (void *)((char *)new + sizeof(header));
  }
}

#endif /* !(defined(__GNUC__) || defined(HAVE_ALLOCA) || defined(WIN32) || defined(WIN16)) */
