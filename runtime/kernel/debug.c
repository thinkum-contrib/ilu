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

/* $Id: debug.c,v 1.113 1999/09/03 02:12:48 spreitze Exp $ */
/* Last edited by Mike Spreitzer September 13, 1998 8:55 pm PDT */
/* Chris Jacobi, June 10, 1998 11:28 am PDT */


#define _POSIX_SOURCE

#if defined(WIN32)
#include <windows.h>
#include <stdio.h>
#endif

#include <ctype.h>
#include <stdarg.h>

#include <patchlevel.ic>	/* defines _ilu_patchlevel */

#include "iluntrnl.h"
#include "oscalls.h"	/* for OS_SLEEP */

/*L1, L2, Main unconstrained*/

ilu_cardinal ilu_DebugLevel = 0;

ilu_boolean  ilu_DebuggingInitialized = ilu_FALSE;

#if defined(WIN32)
static ilu_boolean Win32DebugHandlerInitialized = ilu_FALSE;
static ilu_boolean CurrentProgramIsNotConsoleApp = ilu_FALSE;
static HANDLE OutputHandle;
static char *OutputBuffer;
#endif

extern ilu_Mutex ilu_debugmu;

static void 
printErrorRaises(ilu_ErrorType et,
		 const char *filename,
		 int line)
{
  ILU_ERRPRINTF("**** ILU:  Error <%s> raised in %s, line %d\n",
		ilu_GetErrorTypeDetails((int) et)->name, filename, line);
  return;
}
			      
#define CHAR_UNSIGNED 0

#define STRINGIFY(x)	#x

#ifdef _IS_BSD
#define IS_BSD_VAL "is BSD"
#else
#define IS_BSD_VAL "not BSD"
#endif

#ifdef _IS_POSIX
#define IS_POSIX_VAL "is POSIX"
#else
#define IS_POSIX_VAL "not POSIX"
#endif

#if   defined(ILU_SOLARIS2_THREADS)
#define THREAD_VAL "Solaris 2 threads"
#elif defined(ILU_POSIX_THREADS)
#define THREAD_VAL "POSIX threads"
#elif defined(ILU_WIN32_THREADS)
#define THREAD_VAL "Win32 threads"
#elif defined(ILU_DCE_THREADS)
#define THREAD_VAL "DCE threads"
#else
#define THREAD_VAL "no threads"
#endif

#ifdef SUNRPC_PROTOCOL
#define SUNRPC_PVALUE " sunrpc"
#else
#define SUNRPC_PVALUE ""
#endif

#ifdef COURIER_PROTOCOL
#define COURIER_PVALUE " courier"
#else
#define COURIER_PVALUE ""
#endif

#ifdef IIOP_PROTOCOL
#define IIOP_PVALUE " iiop"
#else
#define IIOP_PVALUE ""
#endif

#ifdef HTTP_PROTOCOL
#define HTTP_PVALUE " http"
#else
#define HTTP_PVALUE ""
#endif

#ifdef W3NG_PROTOCOL
#define W3NG_PVALUE " w3ng"
#else
#define W3NG_PVALUE ""
#endif

#ifdef JAVARMI_PROTOCOL
#define JAVARMI_PVALUE " javarmi"
#else
#define JAVARMI_PVALUE ""
#endif

#ifdef TCPIP_TRANSPORT
#define TCPIP_PVALUE " tcp"
#else
#define TCPIP_PVALUE ""
#endif

#ifdef UDPSOCKET_TRANSPORT
#define UDPSOCKET_PVALUE " udp"
#else
#define UDPSOCKET_PVALUE ""
#endif

#ifdef SUNRPCRM_TRANSPORT
#define SUNRPCRM_PVALUE " sunrpcrm"
#else
#define SUNRPCRM_PVALUE ""
#endif

#ifdef W3MUX_TRANSPORT
#define W3MUX_PVALUE " w3mux"
#else
#define W3MUX_PVALUE ""
#endif

#ifdef BATCHING_TRANSPORT
#define BATCHING_PVALUE " batching"
#else
#define BATCHING_PVALUE ""
#endif

#ifdef SECURE_TRANSPORT
#define SECURE_PVALUE " secure"
#else
#define SECURE_PVALUE ""
#endif

#ifdef __CHAR_UNSIGNED__
#define CHAR_SIGN_PVALUE "u"
#else
#define CHAR_SIGN_PVALUE "s"
#endif

#ifdef SIZE_T
#define expand_one(x)	#x
#define expand_two(x)	expand_one(x)
#define SIZE_T_PVALUE	expand_two(SIZE_T)
#else
#define SIZE_T_PVALUE	"size_t"
#endif

#ifdef ILU_TYPEUID_V2
#define TYPEUID_VERSION	2
#else
#define TYPEUID_VERSION 1
#endif

#ifdef ADD_VARIANT_SUPPORT
#define VAR_PVALUE ""
#else
#define VAR_PVALUE "no "
#endif /* ADD_VARIANT_SUPPORT */

#if defined(WIN32)
void Win32MessageHandler(const char *format, va_list args)
{
  int numWrote;

  (void) vsprintf(OutputBuffer, format, args);
  WriteConsole(OutputHandle, OutputBuffer, strlen(OutputBuffer), &numWrote, NULL);
}

void InitializeWin32ErrorHandler(ilu_boolean FullInitialize)
{
	/* Initialize the handling of ILU_DEBUG output for WIN32 */
	/* If ilu_SetDebugLevel() gets called, and the user hasn't hooked in an error trapping */
	/* function at this point, then this code will provide an output medium for the error */
	/* messages iff the current application is NOT a console app.  If it is a console app, */
	/* then the regular debug routine will work just fine. */
	/* Special case exists if Debug Level is never set, and a message comes from the kernel. */

	Win32DebugHandlerInitialized = ilu_TRUE;

	/* Attempt to create a console for this process */
	if (AllocConsole() == ilu_FALSE)
		return;		/* Console exists for this process; nothing more needs to be done */

	CurrentProgramIsNotConsoleApp = ilu_TRUE;	// Means that our program is not a console-app

	if ((OutputHandle = GetStdHandle(STD_OUTPUT_HANDLE)) == INVALID_HANDLE_VALUE)
	{
		LPVOID lpMsgBuf;
 
		FormatMessage( 
    		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
    		NULL,
    		GetLastError(),
    		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    		(LPTSTR) &lpMsgBuf,
    		0,
    		NULL 
		);

		MessageBox(NULL, lpMsgBuf, "Unable to obtain console handle", MB_OK | MB_ICONINFORMATION);
		LocalFree(lpMsgBuf);
	}

	SetConsoleTitle("ILU_DEBUG output console");

	OutputBuffer = (char *) LocalAlloc(LPTR, 1024);
	if (OutputBuffer == NULL)
	{
		MessageBox(NULL, "Unable to allocate 1K for output formatting buffer!", "Out of memory", MB_OK | MB_ICONINFORMATION);
	}
	if (!FullInitialize)
		return;

	ilu_SetDebugMessageHandler(Win32MessageHandler);
}
#endif

ilu_cardinal ilu_SetDebugLevel(ilu_cardinal level)
{
  ilu_cardinal old_level = ilu_DebugLevel;

  if (level != 0 || ilu_DebugLevel != 0) {

#if defined(WIN32)
	if (Win32DebugHandlerInitialized == ilu_FALSE)
		InitializeWin32ErrorHandler(ilu_TRUE);
#endif

    ILU_ERRPRINTF(
      "ILU version %s.  Copyright 1990-1998 Xerox Corporation.\n",
	    ilu_GetILUVersion());
    ILU_ERRPRINTF(
     "------------------------------------------------------------\n");
    ILU_ERRPRINTF("Configuration info: %s-endian, %s, %s, %s, %svariant, size_t=%s,\n",
#ifdef WORDS_BIGENDIAN
"big",
#else
"little",
#endif
		  IS_BSD_VAL, IS_POSIX_VAL, THREAD_VAL, VAR_PVALUE, SIZE_T_PVALUE);
	ILU_ERRPRINTF("  char=%u%s, short=%u, int=%u, long=%u, void *=%u, fnptr=%u,",
		      (unsigned) SIZEOF_CHAR, CHAR_SIGN_PVALUE,
		      (unsigned) SIZEOF_SHORT, (unsigned) SIZEOF_INT, (unsigned) SIZEOF_LONG,
		      (unsigned) SIZEOF_VOID_P, (unsigned) SIZEOF_FN_P);
	ILU_ERRPRINTF(" long long=%u, long double=%u, enum=%u,\n",
		      (unsigned) SIZEOF_LONG_LONG, (unsigned) SIZEOF_LONG_DOUBLE, (unsigned) SIZEOF_ENUM);
	ILU_ERRPRINTF("  arch=%s, compiler=\"%s\",\n  ANSI C lib=\"%s\", sys aux libs=\"%s\",\n",
		      ILU_MACHINE_TYPE, ILU_COMPILE_COMMAND, ILU_ANSI_C_LIBRARY, ILU_SYSAUX_LIBRARIES);
	ILU_ERRPRINTF("  protocols =%s, transports =%s,\n",
		      SUNRPC_PVALUE COURIER_PVALUE IIOP_PVALUE HTTP_PVALUE W3NG_PVALUE JAVARMI_PVALUE,
		      " inmem" TCPIP_PVALUE UDPSOCKET_PVALUE SUNRPCRM_PVALUE SECURE_PVALUE W3MUX_PVALUE BATCHING_PVALUE);
	ILU_ERRPRINTF("  type-uid-version=%d,", TYPEUID_VERSION);
#ifdef ILU_BINDING_DIRECTORY
    {
      char *binding_dir;
      if ((binding_dir = getenv("ILU_BINDING_DIRECTORY")) == NIL)
	binding_dir = ILU_BINDING_DIRECTORY;
      ILU_ERRPRINTF(" binding via shared files in %s\n", binding_dir);
    }
#elif (defined(ILU_BINDING_HOST) && defined(ILU_BINDING_PORT))
    ILU_ERRPRINTF(" binding via ILU service on %s:%u\n",
		  ILU_BINDING_HOST, ILU_BINDING_PORT);
#elif (defined(ILU_BINDING_MCASTADDR))
    ILU_ERRPRINTF(" binding via multicast to %s\n",
		  ILU_BINDING_MCASTADDR);
#endif
    ILU_ERRPRINTF(
     "------------------------------------------------------------\n");

	{
	  char **p;
	  if (_ilu_patchlevel[0] != ILU_NIL) {
	    ILU_ERRPRINTF("Patches:\n");
	    for (p = _ilu_patchlevel; *p != ILU_NIL;  p += 1)
	      ILU_ERRPRINTF("%s\n", *p);
	    ILU_ERRPRINTF("------------------------------------------------------------\n");
	  }
	}

    ILU_ERRPRINTF(
     "ilu_SetDebugLevel:  setting debug mask from 0x%x to 0x%lx\n",
	    ilu_DebugLevel, level);
  }

  ilu_DebugLevel = level;

#ifdef ENABLE_DEBUGGING

  if ((ilu_DebugLevel & ERROR_DEBUG) != 0)
    {
      ILU_ERRPRINTF("ilu_SetDebugLevel:  noting error raises via <debug.c:printErrorRaises>\n");
      ilu_SetRaiseDebugHook (printErrorRaises);
    }

#else

  if (ilu_DebugLevel != 0)
    ILU_ERRPRINTF("ilu_SetDebugLevel:  ILU kernel was compiled without debugging.  No debugging messages available.\n");

#endif

  ilu_DebuggingInitialized = ilu_TRUE;

  return old_level;
}

struct debug_entry {
  ilu_string name;
  ilu_cardinal value;
};

static struct debug_entry debugs[] = {  ILU_DEBUG_LIST /* defined in iludebug.h */ };

ilu_integer _ilu_atoi (ilu_string p, ilu_string *success)
{
 ilu_integer sign = 1;
  ilu_cardinal base = 10;
  ilu_string s = p;
  ilu_string last;

  if (*s == '-')
    {
      s++;
      sign = -1;
    }
  else if (*s == '+')
    {
      s++;
    }

  if (*s == '0')
    {
      switch (*++s)
	{
	case 'b':
	case 'B':
	  ++s;
	  base = 2;
	  break;

	case 'x':
	case 'X':
	  ++s;
	  base = 16;
	  break;

	case 'd':
	case 'D':
	  ++s;
	  base = 10;
	  break;

	case 'o':
	case 'O':
	  ++s;
	  base = 8;
	  break;

	default:
	  --s;
	}
    }

  base = strtol(s, &last, (int) base);
  if (base == 0 && last == s && success != NIL)
    *success = p;
  else if (last > s && success != NIL)
    *success = last;
  return (base * sign);
}  

int _ilu_casefree_cmp (const ilu_string s1, const ilu_string s2)
     /* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */
{
  register ilu_string p1 = s1;
  register ilu_string p2 = s2;
  register char c1;
  register char c2;

  do
    {
      c1 = tolower((int) *p1);
      c2 = tolower((int) *p2);

      if (c1 < c2)
	return (-1);
      else if (c1 > c2)
	return (1);
      else if (*p1 == (char) 0)
	return (0);
      p1++; p2++;
    }
  while (*p1 != (char) 0);
  return ((*p2 == (char) 0) ? 0 : -1);
}

int
_ilu_casefree_ncmp(const ilu_string s1, const ilu_string s2,
		   ilu_cardinal n)
/* returns 0 if s1 == s2, -1 if s1 < s2, 1 if s1 > s2 */
{
  ilu_string      p1 = s1, p2 = s2;
  char            c1, c2;
  while (n > 0) {
    c1 = tolower((int) *p1);
    c2 = tolower((int) *p2);
    if (c1 < c2)
      return (-1);
    else if (c1 > c2)
      return (1);
    else if (*p1 == (char) 0)
      return (0);
    p1++;
    p2++;
    n--;
  }
  return 0;
}

ilu_cardinal ilu_SetDebugLevelViaString (ilu_string s)
{
  if (s != NIL) {
    char            buf[2000];
    ilu_string      p = NIL;
    ilu_cardinal    debug = 0, i, debugcount;

    if (((debug = _ilu_atoi(s, &p)) == 0 && p == s) || *p != '\0') {
      strcpy(buf, s);
      for (debug = 0, p = buf, s = strchr(buf, ':'),
	debugcount = (sizeof(debugs) / sizeof(struct debug_entry));
	   p != NIL;
	   p = s + 1, s = strchr(s + 1, ':')) {
	int             negate = (*p == '-');
	if (s != NIL)
	  *s = '\0';
	p += negate;
	for (i = 0; i < debugcount; i += 1)
	  if (_ilu_casefree_cmp(debugs[i].name, p) == 0) {
	    if (negate)
	      debug &= ~debugs[i].value;
	    else
	      debug |= debugs[i].value;
	    break;
	  }
	if (i >= debugcount) {
	  ILU_ERRPRINTF("ilu_SetDebugLevelViaString:  Bad debug option"
			" \"%s\" specified.  Valid flags are:  ",
			p);
	  for (i = 0; i < debugcount; i++)
	    ILU_ERRPRINTF(" %s", debugs[i].name);
	  ILU_ERRPRINTF("\n");
	}
	if (s == NIL)
	  break;
      }
    }
    return (ilu_SetDebugLevel(debug));
  }
  return ilu_DebugLevel;
}

static void DebugPrint (const char *formatSpec, va_list ap)
{
#if defined(WIN32)
  if (!Win32DebugHandlerInitialized)
  {
    InitializeWin32ErrorHandler(ilu_FALSE);		/* initialize enough to determine program type */
  }
  if (CurrentProgramIsNotConsoleApp)
  {
    /* Send the message out to the error console */
	Win32MessageHandler(formatSpec, ap);
	return;
  }
#endif

  /* vfprintf is ANSI C, section 4.9.6.7 */
  (void) vfprintf (stderr, formatSpec, ap);
}

static ilu_DebugMessageHandler debugMessageRoutine = DebugPrint;
static ilu_ThreadPrinter debugThread = NULLFN;

/* L1 unconstrained */
void ilu_DebugPrintfCont(const char *formatSpec, ...)
{
  va_list ap;
  va_start (ap, formatSpec);
  if (debugMessageRoutine != NULLFN) 
    (*debugMessageRoutine) (formatSpec, ap);
  va_end(ap);
}

static void ilu_PrintIntro(ilu_boolean showThread)
{
  if (ilu_DebugLevel & TIMING_DEBUG) {
    ilu_FineTime    now = ilu_FineTime_Now();
    ilu_DebugPrintfCont("%lu:%lu ",
			(long unsigned) now.ft_s,
			(long unsigned) now.ft_t);
  }
  if (debugThread && showThread)
    (*debugThread) (ilu_DebugPrintfCont);
  return;
}

/* before: L1.sup < debugmu; after: L1 >= {debugmu} */
void 
ilu_DebugPrintfIntro(const char *formatSpec,...)
{
  va_list         ap;
  va_start(ap, formatSpec);
  ilu_AcquireMutex(ilu_debugmu);
  if (debugMessageRoutine != NULLFN) {
    ilu_PrintIntro(!!(ilu_DebugLevel & THREAD_DEBUG));
    (*debugMessageRoutine) (formatSpec, ap);
  }
  va_end(ap);
}

/* before: L1 >= {debugmu}; after: L1 disjoint {debugmu} */
void ilu_DebugPrintfFin(const char *formatSpec, ...)
{
  va_list ap;
  va_start (ap, formatSpec);
  if (debugMessageRoutine != NULLFN)
    (*debugMessageRoutine) (formatSpec, ap);
  ilu_ReleaseMutex(ilu_debugmu);
  va_end(ap);
}

static void
ilu_FullDebugPrintf(const char *formatSpec, va_list ap, ilu_boolean showThread)
{
  if (_ilu_lockTechDefined) ilu_AcquireMutex(ilu_debugmu);
  if (debugMessageRoutine != NULLFN) {
    ilu_PrintIntro(showThread);
    (*debugMessageRoutine) (formatSpec, ap);
  }
  if (_ilu_lockTechDefined) ilu_ReleaseMutex(ilu_debugmu);
  return;
}

void
ilu_DebugPrintf(const char *formatSpec,...)
{
  va_list         ap;
  va_start(ap, formatSpec);
  ilu_FullDebugPrintf(formatSpec, ap, !!(ilu_DebugLevel & THREAD_DEBUG));
  va_end(ap);
}

static void
  DebugPrintfWithThread(const char *formatSpec,...)
{
  va_list         ap;
  va_start(ap, formatSpec);
  ilu_FullDebugPrintf(formatSpec, ap, ilu_TRUE);
  va_end(ap);
}

void ilu_SetThreadPrinter(ilu_ThreadPrinter p)
{
  debugThread = p;
}

void ilu_SetDebugMessageHandler (void (*handler)(const char *,
						 va_list))
{
  if (((ilu_cardinal) handler) == 1)
    debugMessageRoutine = DebugPrint;
  else
    debugMessageRoutine = handler;

#if defined(WIN32)
  /* If the user sets up their own message hook, then make sure our error code */
  /* is never called...                                                        */

  Win32DebugHandlerInitialized = ilu_TRUE;
  CurrentProgramIsNotConsoleApp = ilu_FALSE;
#endif
}

static FILE * theDebugOutputFile = ILU_NIL;

static void defaultFileOutput (const char * formatSpec, va_list ap)
{
  FILE *f = (theDebugOutputFile == NIL) ? stderr : theDebugOutputFile;

  /* vfprintf is ANSI C, section 4.9.6.7 */
  (void) vfprintf (f, formatSpec, ap);
  fflush(f);
}

static const char *pidPat = "*PID*";

void ilu_SendDebugOutputToFile (ilu_string filename)
{
  ilu_string openName, pidStart, suffixStart;
  if ((pidStart = strstr(filename, pidPat))) {
    char buf[24];
    SIZE_T newLen, suffixLen, pidLen, prefixLen = pidStart - filename;
    suffixStart = pidStart + strlen(pidPat);
    sprintf(buf, "%ld", OS_GETPID());
    buf[23]=0;
    pidLen = strlen(buf);
    suffixLen = strlen(suffixStart);
    newLen = prefixLen + pidLen + suffixLen + 1;
    if (newLen > prefixLen && newLen > pidLen && newLen > suffixLen)
      openName = malloc(newLen);
    else
      openName = ILU_NIL;
    if (!openName) {
      ilu_DebugPrintf("Unable to allocate actual debug output file name, length=%lu, pid-length=%lu, filename pattern=\"%s\".\n",
		      newLen, pidLen, filename);
      return;
    }
    strcpy(openName, filename);
    strcpy(openName+prefixLen, buf);
    strcpy(openName+prefixLen+pidLen, suffixStart);
    openName[newLen-1] = 0;
  }
  else
    openName = filename;
  theDebugOutputFile = fopen(openName, "w");
  if (theDebugOutputFile == ILU_NIL) {
    ilu_DebugPrintf ("Can't open debugging output file \"%s\".\n",
		     openName);
  } else
    ilu_SetDebugMessageHandler (defaultFileOutput);
  return;
}

#ifdef macintosh
static char mac_debug_string[] = ILU_MAC_DEBUG_STRING;
#endif /* macintosh */

void _ilu_AutoSetDebugLevel (void)
{
  if (!ilu_DebuggingInitialized)	/* just do it once */
    {
      ilu_string s = (ilu_string) getenv ("ILU_DEBUG");
      ilu_string file = (ilu_string) getenv ("ILU_DEBUG_FILE");

      ilu_DebuggingInitialized = ilu_TRUE;

      if (file != NIL)
	ilu_SendDebugOutputToFile(file);

#ifdef macintosh
      s = &mac_debug_string[ 0 ];	
#endif

      if (s != NIL)
	(void) ilu_SetDebugLevelViaString (s);
    }
}

#define MAXDUMP		10000
static ilu_cardinal maxDump;
static ilu_boolean maxDumpSet = ilu_FALSE;

void
_ilu_debug_DumpPacket_Offset(ilu_byte * packet, ilu_cardinal length,
			 ilu_cardinal offset, ilu_string direction)
{
  ilu_cardinal    dumplength, i, j;
  ilu_cardinal    n;
  ilu_byte        c;

  if (!maxDumpSet) {
    long unsigned   md;
    char           *maxDumpStr = getenv("ILU_MAX_DUMP");
    if (maxDumpStr) {
      if (sscanf(maxDumpStr, "%lu", &md) != 1 || md == 0) {
	ilu_DebugPrintf("ILU_MAX_DUMP envar is malformed (\"%s\");"
			" using %lu.\n",
			maxDumpStr, (long unsigned) MAXDUMP);
	maxDump = MAXDUMP;
      } else
	maxDump = md;
    } else
      maxDump = MAXDUMP;
    maxDumpSet = ilu_TRUE;
  }
  if (length > maxDump) {
    dumplength = maxDump;
  } else
    dumplength = length;
  if (packet == NIL) {
    ilu_DebugPrintf("_ilu_debug_DumpPacket: Attempt to dump NIL packet.\n");
  } else {
    ilu_DebugPrintfIntro("ILU: DumpPacket of %s%spacket %p, length "
			 "%lu bytes, dumping %lu bytes,"
			 " print offset=%lu:\n",
			 (direction == NIL) ? "" : direction,
			 (direction == NIL) ? "" : " ",
			 (void *) packet,
			 (long unsigned) length,
			 (long unsigned) dumplength,
			 (long unsigned) offset);
    for (i = 0; i < dumplength; i += 16) {
      ilu_DebugPrintfCont("%6lu:  ", i+offset);
      for (j = 0; j < 16 && (i + j) < dumplength; j += 1)
	ilu_DebugPrintfCont("%02x%s ", packet[i + j],
			    ((j % 4) == 3) ? " " : "");
      n = 1;			/* padding before ASCII */
      if (j < 16)
	n += (((16 - j) * 3) + (4 - (j / 4)));
      ilu_DebugPrintfCont("%*.*s", n, n, "");
      for (j = 0; j < 16 && (i + j) < dumplength; j += 1) {
	c = packet[i + j];
	ilu_DebugPrintfCont("%c", ((c >= ' ') && (c <= '~')) ? (char) c
			    : '.');
      }
      ilu_DebugPrintfCont("\n");
    }
    ilu_DebugPrintfFin("");
  }
  return;
}

/* added for use in debugging interpreted programs */
static _ilu_FailureHandler theAFC = {_ilu_ConsumeByLoop, ilu_TRUE};

void ilu_SetAssertionFailureAction (int afa)
{
  ILU_NOTE(ERROR_DEBUG,
	("ilu_SetAssertionFailureAction: to %d.\n",
	 afa));
  theAFC = _ilu_FailureActionToConsumer(afa, 1);
  return;
}

void ilu_SetAssertionFailConsumer (ilu_FailureConsumer afc)
{
  _ilu_Assert(afc != NULLFN, "SetAssertionFailConsumer(NIL)");
  ILU_NOTE(ERROR_DEBUG,
	("ilu_SetAssertionFailConsumer: to %p.\n",
	 afc));
  theAFC.fc = afc;
  theAFC.printMsg = ilu_FALSE;
  return;
}

void 
_ilu_FullAssert(int t, const char *id,
		const char *file, int line)
{
  if (t)
    return;
  if (theAFC.printMsg) {
    ilu_DebugPrintf ("\nILU %s (pid %ld):  "
		     "old-style runtime kernel consistency check failure,"
		     " at line %d in file %s; clue: %s\n"
		     "For information on how to debug or report this,"
		     " see the Debugging section of the ILU manual.\n",
		     ilu_GetILUVersion(), OS_GETPID(), line, file, id);
  }
  (*theAFC.fc) (file, line);
  ilu_DebugPrintf("ilu_FailureConsumer %p returned!"
		  "going into sleep loop!\n", theAFC);
  _ilu_ConsumeByLoop(__FILE__, __LINE__);
  return;
}

static _ilu_FailureHandler theCFC = {_ilu_ConsumeByLoop, ilu_TRUE};

void ilu_SetCheckFailureAction (int cfa)
{
  ILU_NOTE(ERROR_DEBUG,
	("ilu_SetCheckFailureAction: to %d.\n",
	 cfa));
  theCFC = _ilu_FailureActionToConsumer(cfa, 2);
  return;
}

void ilu_SetCheckFailureConsumer (ilu_CheckFailureConsumer cfc)
{
  _ilu_Assert(cfc != NULLFN, "SetCheckFailureConsumer(NIL)");
  ILU_NOTE(ERROR_DEBUG,
	("ilu_SetCheckFailureConsumer: to %p.\n",
	 cfc));
  theCFC.fc = cfc;
  theCFC.printMsg = ilu_FALSE;
  return;
}

ilu_boolean
ilu_FullCheckFailed(ILU_ERRS((internal)) * err,
		     const char *file, int line)
{
  if (theCFC.printMsg) {
    ILU_ERRPRINTF("\nILU %s (pid %ld):  "
		  "new-style runtime kernel consistency check failure,"
		  " at line %d in file %s.\n"
		  "For information on how to debug or report this,"
		  " see the Debugging section of the ILU manual.\n",
		  ilu_GetILUVersion(), OS_GETPID(), line, file);
  }
  (*theCFC.fc) (file, line);
  (void) ILU_ERR_FULLCONS1(internal, err, minor, ilu_im_check, 6,
			   file, line);
  return ilu_FALSE;
}

#ifdef WIN16
extern void _far _pascal Yield(void);
#endif

void     _ilu_ConsumeByLoop(const char *f, int l)
{
  DebugPrintfWithThread("Entering endless sleep loop (at line %d of %s)\n",
			__LINE__ + 4, __FILE__);

  while (1)
#ifdef WIN16
    Yield();
#else
    OS_SLEEP(10);
#endif
}

static void ConsumeByDump(const char*f, int l)
{
  (*(int *) NIL) = 1;		/* This had better terminate the
				 * prog */
  exit(32767);			/* ... just in case it doesn't */
  return;
}

static void ConsumeByRaise(const char *f, int l)
{
  return;
}

static int exits[3];

static void ConsumeByExit0(const char*f, int l) { exit(exits[0]); }
static void ConsumeByExit1(const char*f, int l) { exit(exits[1]); }
static void ConsumeByExit2(const char*f, int l) { exit(exits[2]); }

static ilu_FailureConsumer consumeByExit[3] = {ConsumeByExit0, ConsumeByExit1, ConsumeByExit2};

_ilu_FailureHandler
_ilu_FailureActionToConsumer(int fa, int which)
{
  _ilu_FailureHandler ans = {0, ilu_TRUE};
  _ilu_Assert(0 <= which && which <= 2, "FaultActionToConsumer which");
  if (fa > 0) {
    exits[which] = fa;
    ans.fc = consumeByExit[which];
    return ans;
  }
  _ilu_Assert(fa >= ((which == 2) ? -3 : -2),
	      "_ilu_FaultActionToConsumer(bogon)");
  if (fa == -1)
    ans.fc = _ilu_ConsumeByLoop;
  else if (fa == -2)
    ans.fc = ConsumeByDump;
  else {
    ans.fc = ConsumeByRaise;
    ans.printMsg = ilu_FALSE;
  }
  return ans;
}
