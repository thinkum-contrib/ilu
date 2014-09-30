/** $Id: client.c,v 1.3 1999/08/03 01:59:14 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 10:16 am PDT */

#include <string.h>
#include <stdio.h>

#include "reference_test.h"

#define STRINGIFIED_DOMAIN(x)	#x
#define STRINGIFIED_DOMAIN1(x)	STRINGIFIED_DOMAIN(x)
#define THIS_DOMAIN		STRINGIFIED_DOMAIN1(FULLY_QUALIFIED_DOMAIN_NAME)

static void
  print_node (reference_test_Node1 *node, int level)
{
  int i;
  for (i = level;  i > 0;  i--)
    printf ("  ");
  printf ("%x <%s>\n", node, (node != ILU_NIL) ? node->name : "");
  if (node != ILU_NIL) {
    print_node (node->left, level + 1);
    print_node (node->right, level + 1);
  }
}

int main (int ac, char **av)
{
  reference_test_Node1Ref0 p1, p2, p3, p4;
  reference_test_Node1Ref1 p5;
  reference_test_Node1Ref3 p6;
  reference_test_O s;
  char *sbh = ILU_NIL;
  ILU_C_ENVIRONMENT status;
  char sid[1000];

  if (ac > 1)
    sbh = av[1];
  else
    sprintf(sid, "reference-test.%s", THIS_DOMAIN);

  reference_test__Initialize();

  if (sbh) {
    s = (reference_test_O) ILU_C_SBHToObject (av[1], reference_test_O__MSType, &status);
    if ((!ILU_C_SUCCESSFUL(&status)) || (s == ILU_NIL)) {
      fprintf (stderr, "Can't create object from SBH <%s>\n", sbh);
      return 1;
    }
  } else {
    s = (reference_test_O) ILU_C_LookupObject (sid, "O", reference_test_O__MSType);
    if (s == ILU_NIL) {
      fprintf(stderr, "Can't bind to object %s/O\n", sid);
      return 1;
    }
  }

  /* Build a diamond-shaped structure */
  
  p1 = reference_test_Node1__alloc();
  p2 = reference_test_Node1__alloc();
  p3 = reference_test_Node1__alloc();
  p4 = reference_test_Node1__alloc();

  p1->name = "Node 1";
  p1->left = p2;
  p1->right = p3;
  p2->name = "Node 2";
  p2->right = p4;
  p2->left = ILU_NIL;
  p3->name = "Node 3";
  p3->left = p4;
  p3->right = ILU_NIL;
  p4->name = "Node 4";
  p4->left = ILU_NIL;
  p4->right = ILU_NIL;

  /* send it across and retrieve it */
  p5 = reference_test_O_m1 (s, p1, &status);
  if (!ILU_C_SUCCESSFUL(&status)) {
    fprintf (stderr, "reference_test_O_m1 signalled exception %s\n", CORBA_exception_id(&status));
    return 1;
  };

  /* check the result */
  if (!((strcmp(p5->name, "Node 1") == 0) &&
	(p5->left != ILU_NIL) &&
	(strcmp(p5->left->name, "Node 2") == 0) &&
	(p5->left->left == ILU_NIL) &&
	(p5->left->right != ILU_NIL) &&
	(strcmp(p5->left->right->name, "Node 4") == 0) &&
	(p5->left->right->left == ILU_NIL) &&
	(p5->left->right->right == ILU_NIL) &&
	(p5->right != ILU_NIL) &&
	(strcmp(p5->right->name, "Node 3") == 0) &&
	(p5->right->left != ILU_NIL) &&
	(strcmp(p5->right->left->name, "Node 4") == 0) &&
	(p5->right->left->left == ILU_NIL) &&
	(p5->right->left->right == ILU_NIL) &&
	(p5->right->right == ILU_NIL) &&
	(p5->right->left == p5->left->right))) {
    fprintf (stderr, "Bad data structure returned.\n");
    print_node (p1, 0);
    return 1;
  }

  p6 = reference_test_O_m2 (s, p1, p1, p5, ILU_NIL, &status);
  if (!ILU_C_SUCCESSFUL(&status)) {
    fprintf (stderr, "reference_test_O_m1 signalled exception %s\n", CORBA_exception_id(&status));
    return 1;
  };
  printf("Returned node p6 is:\n");
  print_node (p6, 0);

  return 0;
}

