/** $Id: server.c,v 1.4 1999/08/03 01:59:14 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:17 am PDT */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>	/* for gethostname */
#include <string.h>

#include <iluhash.h>

#include "reference_test.h"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

static reference_test_Node1 *
  copy_node (reference_test_Node1 *node, ilu_HashTable nodes)
{
  reference_test_Node1 *newnode;
  ilu_HashTable ht = nodes;

  if (ht == ILU_NIL)
    ht = ilu_hash_MakeNewTable(23, ilu_hash_HashPointer, ilu_hash_PointerCompare);
  newnode = (reference_test_Node1 *) malloc(sizeof(*newnode));
  ilu_hash_AddToTable (ht, node, newnode);
  newnode->name = ILU_C_Strdup(node->name);
  if (node->left == ILU_NIL)
    newnode->left = ILU_NIL;
  else {
    if ((newnode->left = (reference_test_Node1*) ilu_hash_FindInTable (ht, node->left)) == ILU_NIL)
      newnode->left = copy_node(node->left, ht);
  }
  if (node->right == ILU_NIL)
    newnode->right = ILU_NIL;
  else {
    if ((newnode->right = (reference_test_Node1*) ilu_hash_FindInTable (ht, node->right)) == ILU_NIL)
      newnode->right = copy_node(node->right, ht);
  }
  if (nodes == ILU_NIL)
    ilu_hash_FreeHashTable (ht, 0, 0);
  return newnode;
}

static void
  print_node (reference_test_Node1 *node, int level)
{
  int i;
  for (i = level;  i > 0;  i--)
    printf ("  ");
  printf ("%p <%s>\n", node, (node != ILU_NIL) ? node->name : "");
  if (node != ILU_NIL) {
    print_node (node->left, level + 1);
    print_node (node->right, level + 1);
  }
}

extern void
  _ilu_debug_DumpPacket_Offset(ilu_byte * packet, ilu_cardinal length,
			       ilu_cardinal offset, ilu_string direction);

reference_test_Node1Ref1
  server_reference_test_O_m1 (reference_test_O self,
			      reference_test_Node1* p,
			      ILU_C_ENVIRONMENT *status)
{
  reference_test_Node1Ref1 retval;

  printf("Received node is:\n");
  print_node(p, 1);

#if 0
  /* quick and dirty way to copy an arbitrarily complicated value:
     create a Pickle, then pull the value out of it */
  {
    ilu_any pickle;
    reference_test_Node1Ref1 *n;

    ILU_C_Pickle_Init (&pickle, TC_reference_test_Node1Ref1, p, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
    n = ILU_C_Pickle_Value(&pickle, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
    retval = *n;
    printf("Pickle-copied node is:\n");
    print_node(retval, 1);
    ILU_C_Pickle_FreeStorage(&pickle, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
  }
#endif

  retval = copy_node (p, ILU_NIL);
  printf("Copied node is:\n");
  print_node(retval, 1);

  return retval;
}

reference_test_Node1Ref3
  server_reference_test_O_m2 (reference_test_O self,
			      reference_test_Node1Ref2 p1,
			      reference_test_Node1Ref2 p2,
			      reference_test_Node1Ref1 p3,
			      reference_test_Node1Ref1 p4,
			      ILU_C_ENVIRONMENT *status)
{
  reference_test_Node1Ref3 retval = ILU_NIL;

  printf("Received node p1 is:\n");
  print_node(p1, 1);
  if (p1 != p2) {
    printf("Received node p2 is different from p1!:\n");
    print_node(p2, 1);
    ILU_C_RAISE_SYSTEM(status,INTERNAL,ilu_im_check,NO);
    return retval;
  }
  if ((p3 == ILU_NIL) || (p4 != ILU_NIL)) {
    if (p3 == ILU_NIL)
      fprintf(stderr, "Received node p3 is NIL!\n");
    else if (p4 != ILU_NIL)
      fprintf(stderr, "Received node p4 is non-NIL!\n");
    ILU_C_RAISE_SYSTEM(status,INTERNAL,ilu_im_check,NO);
    return retval;
  }

  /* quick and dirty way to copy an arbitrarily complicated value:
     create a Pickle, then pull the value out of it */
  {
    ilu_any pickle;
    reference_test_Node1Ref3 *n;
    ILU_C_Pickle_Init (&pickle, TC_reference_test_Node1Ref3, p1, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
    n = (reference_test_Node1Ref3 *) ILU_C_Pickle_Value(&pickle, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
    retval = *n;
    printf("Pickle-copied node is:\n");
    print_node(retval, 1);
    ILU_C_Pickle_FreeStorage(&pickle, status);
    if (!ILU_C_SUCCESSFUL(status)) return ILU_NIL;
  }

  return retval;
}

static void usage(char *pname)
{
  fprintf(stderr, "Usage:  %s [-mt] [-p PROTOCOL] [-t TINFO...]\n");
  exit(1);
}

int main (int ac, char **av)
{
  static ILU_C_Server ks;
  reference_test_O s;
  char sid[1000];
  char *ltinfo[20];
  ilu_ProtocolInfo	pinfo = ILU_NIL;
  ilu_TransportInfo	tinfo = ILU_NIL;
  ilu_boolean	threaded = ilu_FALSE;
  int i;

  for (i = 1;  i < ac; ) {
    if (strcmp(av[i], "-p") == 0) {
      if (++i < ac)
	pinfo = av[i++];
      else
	usage(av[0]);
    } else if (strcmp(av[i], "-t") == 0) {
      int j = 0;
      ++i;
      while ((i < ac) && (av[i][0] != '-'))
	ltinfo[j++] = av[i++];
      ltinfo[j] = ILU_NIL;
      tinfo = ltinfo;
    } else if (strcmp(av[i], "-mt") == 0) {
      threaded = ilu_TRUE;
      ++i;
    } else
      usage(av[0]);
  }

  if (threaded) {
#if (defined(ILU_OS_THREADED))
    ILU_C_USE_OS_THREADS;
#else
    OUTPUT("OS-supplied thread support not configured into ILU!\n");
    exit(-1);
#endif				/* (defined(ILU_OS_THREADED)) */
  }

  reference_test__InitializeServer();

  sprintf (sid, "reference-test.%s", THIS_DOMAIN);
  ks = ILU_C_InitializeServer (sid, ILU_NIL, pinfo, tinfo, ILU_NIL, ilu_TRUE);
  s = reference_test_O__CreateTrue ( "O", ks, NULL );
  ILU_C_PublishObject (s);

  if (s != NULL)
    {
      printf ("%s\n", ILU_C_SBHOfObject(s));
      fflush(stdout);
      ILU_C_Run( );
    }
  else
    {
      printf ("couldn't create server object -- exiting\n");
      exit(1);
    }
  return 1;
}

