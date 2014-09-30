#include <stdio.h>
#include <string.h>
#include <iluxport.h>

#define CORBA_NATIVE_OBJECT_IH_PREFIX		"ilu--corba-native-object:"
#define SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX	(sizeof(CORBA_NATIVE_OBJECT_IH_PREFIX)-1)

extern ilu_boolean _ilu_IIOP_ParseIOR (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *,
				       ilu_cardinal *, ilu_boolean *, ilu_Error *);

extern ilu_boolean _ilu_IIOP_ParseIOR2 (ilu_string, ilu_string *, ilu_string *, ilu_string *, ilu_string *,
					ilu_cardinal *, ilu_boolean *, ilu_Error *);

extern ilu_bytes	/* result, NUL-terminated malloced buffer, PASS */
  _ilu_DecodeBuffer (ilu_string,		/* buffer, RETAIN */
		     ilu_cardinal,	/* size of buffer */
		     ilu_cardinal *,	/* number of chars in output */
					/*  (not counting terminal NUL) */
		     ILU_ERRS((inv_objref, no_memory, internal)) *);

static char hexdigit[] = "0123456789ABCDEF";

#define PMCINFO					((ilu_bytes) "\0PMC\0")
#define SIZEOF_PMCINFO				5

#define ORBPLUSINFO				((ilu_bytes) "HP:")
#define SIZEOF_ORBPLUSINFO			3

#define OMNIORBINFO				((ilu_bytes) "OB/")
#define SIZEOF_OMNIORBINFO			3

/* #define JAVAORBINFO				((ilu_bytes) "\0257\0253\0312\0376") */
#define JAVAORBINFO				((ilu_bytes) "\xaf\xab\xca\xfe")
#define SIZEOF_JAVAORBINFO			4

static ilu_string encode (ilu_bytes key, ilu_cardinal keylen)
{
  int i;
  ilu_string copy;
  char *p;
  ilu_byte *q;

  copy = ilu_must_malloc(3 * keylen);
  for (p = copy, q = key;  (q - key) < keylen;  q++) {
    if ((*q < 0x20) || (*q > 0x7E)) {
      *p++ = '#';
      *p++ = hexdigit[((*q)>>4)&0xF];
      *p++ = hexdigit[((*q)&0xF)];
    } else if (*q == '#') {
      *p++ = '#';
      *p++ = '#';
    } else {
      *p++ = (char) *q;
    };
  }
  *p = '\0';
  return copy;
}

int main (int ac, char **av)
{
  ilu_string ih = ILU_NIL, sid = ILU_NIL, mstid = ILU_NIL, cinfo = ILU_NIL;
  ilu_cardinal cinfolen = 0, i = 1;
  ilu_Error err;
  ilu_boolean stat;
  ilu_boolean verbose = ilu_FALSE;
  ilu_boolean passcinfo = ilu_FALSE;

  while (i < ac) {
    if (strcmp(av[i], "-v") == 0) {
      verbose = ilu_TRUE;
      i++;
    } else if (av[i][0] == '-') {
      fprintf (stderr, "Usage:  %s [-v] STRINGIFIED-IOR\n", av[0]);
      return 1;
    } else break;
  }
  if ((i >= ac) ||
      ((strncmp(av[i], "IOR:", 4) != 0) &&
       (strncmp(av[i], "IOR:", 4) != 0) &&
       (strncmp(av[i], "IOR2:", 5) != 0))) {
    fprintf (stderr, "Usage:  %s [-v] STRINGIFIED-IOR\n", av[0]);
    return 1;
  } else {
    if (verbose)
      ilu_SetDebugLevelViaString("iiop");
    if (strncmp(av[i], "IOR2:", 5) == 0)
      stat = _ilu_IIOP_ParseIOR2 (av[i], &ih, &sid, &mstid, &cinfo, &cinfolen, &passcinfo, &err);
    else
      stat = _ilu_IIOP_ParseIOR (av[i], &ih, &sid, &mstid, &cinfo, &cinfolen, &passcinfo, &err);
    if (!stat) {
      ILU_ERR_SWITCH(err) {
	ILU_SUCCESS_CASE
	  ;
	ILU_ERR_CASE(marshal,e1) {
	  printf ("Your IOR was improperly marshalled, and the unmarshalling code raised\n"
		  "a MARSHAL error, with ILU minor code %d.  This code can be mapped to\n"
		  "an appropriate minor error differentiator by inspecting the enumeration\n"
		  "ilu_marshal_Minor, in the file ILUSRC/runtime/kernel/iluerrs.h.\n",
		  (int) e1->minor);
	}
	ILU_ERR_CASE(no_memory,e1) {
	  printf ("There was an attempt to allocate %lu bytes of memory, which couldn't\n"
		  "be done.  This typically indicates a malformed IOR.\n",
		  e1->nbytes);
	}
	ILU_ERR_ELSE {
	  printf ("Error parsing IOR:  %s\n", ILU_ERR_NAME(err));
	}
      } ILU_ERR_ENDSWITCH;	
      return 1;
    }
    if ((ih == ILU_NIL) && (sid == ILU_NIL)) {
      printf ("The IOR indicates the NIL object.\n");
    } else if (strncmp(ih, CORBA_NATIVE_OBJECT_IH_PREFIX, SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX) == 0)
      {
	ilu_Error lerr;
	ilu_cardinal objkeylen;
	ilu_bytes objkey;
	ilu_string printable_objkey;
	unsigned long minor_version, major_version, mapping_version, port;
	char hostname[1000];

	objkey = _ilu_DecodeBuffer(ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX,
				   strlen(ih + SIZEOF_CORBA_NATIVE_OBJECT_IH_PREFIX),
				   &objkeylen, &lerr);
	if (ILU_ERRNOK(lerr)) {
	  printf ("Error parsing IOR:  %s\n", ILU_ERR_NAME(err));
	  ILU_HANDLED(err);
	  return 1;
	};
	printable_objkey = encode(objkey, objkeylen);
	printf ("object key is <%s>;\n", printable_objkey);
	if (mstid == ILU_NIL)
	  printf (" no trustworthy most-specific-type info;");
	else
	  printf (" most specific type repository ID is \"%s\";", mstid);
	if (memcmp((const void *) objkey, (const void *) PMCINFO, SIZEOF_PMCINFO) == 0) {
	  printf (" probably a Visigenic ORB;\n");
	} else if (memcmp((const void *) objkey, (const void *) ORBPLUSINFO, SIZEOF_ORBPLUSINFO) == 0) {
	  printf (" probably a HP OrbPlus ORB;\n");
	} else if (memcmp((const void *) objkey, (const void *) OMNIORBINFO, SIZEOF_OMNIORBINFO) == 0) {
	  printf (" probably an OmniBroker ORB;\n");
	} else if (memcmp((const void *) objkey, (const void *) JAVAORBINFO, SIZEOF_JAVAORBINFO) == 0) {
	  printf (" probably a JDK Java ORB;\n");
	} else if (objkey[0] == ((ilu_byte) ':')) {
	  printf (" possibly an Iona ORB;\n");
	} else {
	  printf (" unrecognized ORB type;\n");
	};
	if (sscanf (cinfo, "iiop_%lu_%lu_%lu_%*[^@]@tcp_%[^_]_%lu",
		    &major_version, &minor_version, &mapping_version,
		    hostname, &port) != 5) {
	  fprintf (stderr, "Error parsing address information <%s>\n", cinfo);
	  return 1;
	} else {
	  printf (" reachable with IIOP %u.%u at host \"%s\", port %u\n", major_version, minor_version, hostname, port);
	}
	return 0;
      }
    else
      printf ("ILU object <%s/%s> (type <%s>)\n    at %*.*s (ILU ORB)\n", sid, ih,
	      (mstid == ILU_NIL) ? "--unknown--" : mstid,
	      cinfolen, cinfolen, cinfo);
    return 0;
  }
}
