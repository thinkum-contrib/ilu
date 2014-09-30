/*
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
*/
/* $Id: sbfile.c,v 1.50 1999/08/12 02:14:22 janssen Exp $ */
/* Last edited by Mike Spreitzer September 22, 1998 11:22 pm PDT */

#include "iluntrnl.h"
#include "object.h"

#include "oscalls.h"

#if (defined(WIN32) || defined(WIN16))
static char    *directory_delimiter = "\\";
#elif defined( macintosh )
static char    *directory_delimiter = "";
#else
static char    *directory_delimiter = "/";
#endif

/*
 * The current binding approach uses a hashed hex value of the sid &
 * ih as the file name to use in the binding directory - this was
 * primarily done because of Win3.x 8.3 filename restrictions. Name
 * collisions are unlikey since the hash is based on a 32 bit CRC,
 * and the number of binding directory entriees is likely to be
 * small.
 */

char *ilu_BindingDirectory = ILU_BINDING_DIRECTORY;

/* returns the binding directory to use - if the environment
   variable ILU_BINDING_DIRECTORY is set, it is used, else
   whatever ILU_BINDING_DIRECTORY was #defined to when the
   kernel was built */
/* L1, L2 unconstrained */
static char    *
  _ilu_get_binding_directory(void)
{
  static char    *pc_environment_setting = NULL;
  static int      i_checked_environment = 0;
  if (i_checked_environment == 0) {
    pc_environment_setting = getenv("ILU_BINDING_DIRECTORY");
    i_checked_environment = 1;
  }
  if (pc_environment_setting)
    return pc_environment_setting;
  return ilu_BindingDirectory;
}

/* puts the full pathname of the binding file into buf */
static void
get_binding_file_name(char *buf, char *sid, char *ih)
{
  ilu_cardinal crc;
  static ilu_byte nullbyte = 0;

  crc = ilu_CRC32((ilu_bytes) sid, strlen(sid));
  crc = ilu_CRC32WithAccum(&nullbyte, 1, crc);
  crc = ilu_CRC32WithAccum((ilu_bytes) ih, strlen(ih), crc);
  sprintf(buf, "%s%s%8.8lx", _ilu_get_binding_directory(),
	  directory_delimiter, ((unsigned long) crc) & 0xFFFFFFFF);
}

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ILU_PASS(ILU_OPTIONAL(char *)) ilu_PublishObject (ilu_Object obj)
{
  char           *sbh;
  char            buf[1000];
  FILE           *f;
  char           *proof = NIL;

  if ((sbh = ilu_SBHOfObject(obj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_PublishObject:  object %p not exported.\n",
	   obj));
    return (NIL);
  } else {
    get_binding_file_name(buf, obj->ob_server->sr_id, obj->ob_ih);
    ILU_NOTE(BINDING_DEBUG,
	     ("ilu_PublishObject:  binding file name is %s\n", buf));
    if (OS_ACCESS(buf, OS_ACCESS_R_OK) == 0
	&& OS_ACCESS(buf, OS_ACCESS_W_OK) != 0) {
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_PublishObject:  obj \"%s\" \"%s\" (%s) already published.\n",
	     obj->ob_server->sr_id, obj->ob_ih, buf));
    } else if ((f = fopen(buf, "w+")) == NIL) {
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_PublishObject:  Can't open file %s for obj \"%s\" \"%s\".\n",
	     buf, obj->ob_server->sr_id, obj->ob_ih));
    } else {
      proof = ilu_InventID();
      fprintf(f, "%s\n%s\n", proof, sbh);
      fclose(f);
      OS_CHMOD(buf,
	       (OS_CHMOD_S_IWUSR | OS_CHMOD_S_IRUSR
		| OS_CHMOD_S_IWGRP | OS_CHMOD_S_IRGRP
		| OS_CHMOD_S_IWOTH | OS_CHMOD_S_IROTH));
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_PublishObject:  Published \"%s\" \"%s\" on %s, proof %s.\n",
	     obj->ob_server->sr_id, obj->ob_ih, buf, proof));
    }
  }
  ilu_ExitServer(object_server(obj), object_class(obj));
  return (proof);
}

/*before: Inside(s, cl)
  after:				 L1 disjoint {cmu, s};
  after: cl collectible		      => L1  not >=  {gcmu};
  after: cl collectible & s surrogate => Main Invariant holds;
  where s = obj's server and cl = obj's type.
  (We don't really need to hold cmu for surrogate or non-collectible
   objects, but this is convenient because ilu_Enter/ExitServer can
   be used.)*/
ilu_boolean
ilu_WithdrawObject(ilu_Object obj, ILU_PASS(char *) ownership_proof)
{
  char           *sbh;
  char            buf[1000];
  FILE           *f;
  ilu_boolean     result = ilu_FALSE;

  if ((sbh = ilu_SBHOfObject(obj)) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_WithdrawObject:  object %p not exported.\n", obj));
  } else {
    get_binding_file_name(buf, obj->ob_server->sr_id, obj->ob_ih);
    ILU_NOTE(BINDING_DEBUG,
	     ("ilu_WithdrawObject:  binding file name is %s\n", buf));
    if (OS_ACCESS(buf, OS_ACCESS_R_OK) == 0
	&& OS_ACCESS(buf, OS_ACCESS_W_OK) != 0) {
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_WithdrawObject:  Can't access registry file %s.\n",
	     buf));
    } else if ((f = fopen(buf, "r+")) == NIL) {
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_WithdrawObject:  Can't open registry file %s.\n",
	     buf));
    } else {
      char            buf2[1000];

      if (fgets(buf2, sizeof(buf2), f) == NIL) {
	ILU_NOTE(BINDING_DEBUG,
	      ("ilu_WithdrawObject:  Bad registry file %s.\n",
	       buf));
	fclose(f);
      } else if (strncmp(ownership_proof, buf2,
			 strlen(ownership_proof)) != 0) {
	ILU_NOTE(BINDING_DEBUG,
	      ("ilu_WithdrawObject:  no match on ownership proofs with proof %s.\n",
	       ownership_proof));
	fclose(f);
      } else {
	fclose(f);
	OS_UNLINK(buf);
	result = ilu_TRUE;
      }
    }
  }
  ilu_free(ownership_proof);
  ilu_ExitServer(object_server(obj), object_class(obj));
  return result;
}

/*before: L1 = {};
  after:  result!=NIL => Inside(result's server, pclass);
  after:  result==NIL => L1 = {};
  forall conn: (L2 >= {conn.iomu}) => (L2 >= {conn.callmu});
  Main otherwise unconstrained*/
ILU_OPTIONAL(ilu_Object) ilu_LookupObject(char *sid, char *ih,
				      ilu_Class pclass)
{
  ilu_Object      ans;
  ilu_ReLookupObject(sid, ih, pclass, &ans);
  return ans;
}

/*before: L1 = {};
  after:  *po != NIL => Inside(result's server, pclass);
  after:  *po == NIL => L1 = {};
  Main Remnant holds*/
ilu_boolean
ilu_ReLookupObject(char *sid, char *ih,
		   ilu_Class pclass,
		   ILU_OPTIONAL(ilu_Object) * po)
{
  FILE           *f;
  char            buf[1000], proof[1000], sbh[1000];
  ilu_Object      obj;
  ilu_ConsiderSbhResult csr;
  ILU_ERRS((BadProtocolInfo, no_memory, inv_objref, internal)) lerr;
  ilu_boolean     ans;
  *po = NIL;
  get_binding_file_name(buf, sid, ih);
  ILU_NOTE(BINDING_DEBUG,
	   ("ilu_ReLookupObject:  binding file name is %s\n", buf));
  if (OS_ACCESS(buf, OS_ACCESS_R_OK) != 0) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject:  No such object %s (\"%s\" \"%s\").\n",
	   buf, sid, ih));
    return (ilu_FALSE);
  }
  if ((f = fopen(buf, "r")) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject:  Can't open registry file %s.\n",
	   buf));
    return (ilu_FALSE);
  }
  if (fgets(proof, sizeof(proof), f) == NIL
      || fgets(sbh, sizeof(sbh), f) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject:  Bad registry file %s.\n",
	   buf));
    fclose(f);
    return (ilu_FALSE);
  }
  fclose(f);

  proof[strlen(proof) - 1] = '\0';
  sbh[strlen(sbh) - 1] = '\0';

  ILU_NOTE(BINDING_DEBUG,
	("ilu_LookupObject:  found SBH=<%s>, pclass=<%s>, binding file=%s\n",
	 sbh, pclass->cl_name, buf));

  csr = ilu_ConsiderSBH(sbh, &lerr);
  switch (csr) {
  case ilucsr_err:
    ILU_HANDLED(lerr);
  case ilucsr_noProblem:
  case ilucsr_isTrue:
  case ilucsr_noNews:
    ans = ilu_FALSE;
    break;
  case ilucsr_notReified:
  case ilucsr_changed:
    ans = ilu_TRUE;
    break;
  default:
    _ilu_Assert(0, "Lookup: unexpected csr");
  }

  if ((obj = ilu_ObjectOfSBH(sbh, pclass, &lerr)) == NIL) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject(%s/%s):  ObjectOfSBH(%s) raises %s"
	   " from %s:%d\n",
	   sid, ih, sbh, ILU_ERR_NAME(lerr),
	   ilu_ErrorFile(&lerr), ilu_ErrorLine(&lerr)));
    ILU_HANDLED(lerr);
    return (ilu_FALSE);
  }
  if (!ilu_IsSubObjectType(obj->ob_class, pclass)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject(%s/%s):  actual type %s does not include putative type %s\n",
	   sid, ih, obj->ob_class->cl_name, pclass->cl_name));
    ilu_ExitServer (object_server(obj), object_class(obj));
    return (ilu_FALSE);
  }
  *po = obj;
  if (ilu_TrueInstanceP(obj)) {
    ILU_NOTE(BINDING_DEBUG,
	  ("ilu_LookupObject:  Local object.\n"));
    return (ans);
  } else if (getenv("ILU_SB_LOOKUP_NO_PING")) {
    return ans;
  } else {
    ilu_boolean     status;
    ilu_Server      s = object_server(obj);
    ILU_ERRS((GcRegFailed, bad_locks, broken_locks, internal)) lerr2;
    lerr2 = ilu_DeltaHolds(obj, 1);
    ILU_ERR_SWITCH(lerr2) {
      ILU_SUCCESS_CASE {
	ilu_Connection newconn;
	ilu_ExitServer(s, pclass);
	status = ilu_PingObject(obj, &newconn);
	if (newconn != NIL)
	  _ilu_HandOffNewConnection(newconn, &lerr2);
	ILU_MUST_BE_SUCCESS(lerr2);
	ilu_EnterServer(s, pclass);
      }
      ILU_ERR_CASE(GcRegFailed, v)
	status = ilu_FALSE;
      ILU_ERR_CASE3(internal,
		    bad_locks, broken_locks) {
	ilu_ExitServer(s, pclass);
	*po = NIL;
	return (ans);
      }
    } ILU_ERR_ENDSWITCH;
    if (status) {
      ilu_DHolds(obj, -1);
      return (ans);
    } else {
      int             wasonly;
      ilu_cardinal    nobj, nconn;
      ilu_cardinal    nL = 0;
      /* wasonly: 1 hash pair, no lspo. */
      wasonly = ilu_hash_PairsInTable(s->sr_objs) == 1;
      if (object_lspos(obj) != NIL) /* check every language */
	for (nL = 0 ; wasonly && (nL < _ilu_NLanguages) ; nL++)
	  wasonly = wasonly && object_lspo(obj,nL) == NIL;
      ILU_NOTE(BINDING_DEBUG,
	    ("ilu_LookupObject:  Bad ping of object %s\n",
	     sbh));
      (void) ilu_DeltaHolds(obj, -1);	/* BUG! should pass some errs */
      if (wasonly) {
	nobj = ilu_NumObjsInServer(s);
	nconn = ilu_NumIoingConnsOfServer(s);
	if (nobj == 0 && nconn == 0)
	  ilu_InnerBankServer(s);
      }
      ilu_ExitServer(s, pclass);
      *po = NIL;
      return (ans);
    }
  }
}
