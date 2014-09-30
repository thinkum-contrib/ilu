/* util.c */
/* Chris Jacobi, January 4, 1999 11:49 am PST */

/*
 * Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
 * 
 * Unlimited use, reproduction, modification, and distribution of this
 * software and modified versions thereof is permitted.  Permission is
 * granted to make derivative works from this software or a modified
 * version thereof.  Any copy of this software, a modified version
 * thereof, or a derivative work must include both the above copyright
 * notice of Xerox Corporation and this paragraph.  Any distribution of
 * this software, a modified version thereof, or a derivative work must
 * comply with all applicable United States export control laws.  This
 * software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
 * WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
 * LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
 * EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
 * NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGES.
 */

/* $Id: util.c,v 1.71 1999/08/03 01:51:06 janssen Exp $ */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "name.h"
#include "util.h"
#include "context.h"

#ifdef WIN32
/* for mkdir */
#include <direct.h>
#endif

#ifdef macintosh
#include <Errors.h>
#include <Files.h>
#endif

/*public*/
char *programName;

/*public*/
boolean generatingSkeleton;


PUBLIC char *
booleanImage(boolean value)
{
    return value ? "true" : "false";
}

static list allfiles = 0;
static void reportFile(char* fn) 
{
    if (allfiles == 0) {
        allfiles = new_list();
    }
    list_insert(allfiles, fn);
}




PUBLIC TypeDescription
baseTypeDescription(Type t)
{
    Type base = t;

    for (;;) {
	if (base->description)
	    return base->description;
	base = base->supertype;
    }
}

PRIVATE void
printBanner(const char *part, Interface ifc, IHandle ih)
{
    static char *prefixes[2] = {"//", "//"};

    printf("// %s for \"%s\"\n//\n", 
        replaceBackslashes((char*)part), replaceBackslashes(packagePrefix0(ih))
        );
    if (ifc) {
        iluparser_GenerateBoilerplate(stdout, ifc, programName, prefixes);
    }
    printf("\n");
}



PUBLIC char *
typeInitializer(TypeKind tk) 
/* returns a string containing the initializer for a type */
{
  switch(tk) {
    case enumeration_Type:
	return "null";
    case byte8_Type:
    case int16_Type:
    case int32_Type:
    case card16_Type:
    case card32_Type:
	return "0";
    case int64_Type:
    case card64_Type:
	return "0L";
    case real32_Type:
        return "0F";
    case real64_Type:
        return "0D";
    case real128_Type:
	return "null";
    case char16_Type:
    case char8_Type:
	return "\'\\u0000\'";
    case boolean_Type:
	return "false";
    case object_Type:
    case union_Type:
    case sequence_Type:
    case record_Type:
    case optional_Type:
    case pickle_Type:
    case array_Type:
	return "null";
    case alias_Type:
        fatal("typeInitializer shouldn't be alias");
        return "null";
    default:
        fatal("typeInitializer unknown case");
        return "null";
    }
}

PUBLIC char*
corbaTCKind(TypeKind tk) 
/* returns the to_int value of the coirba TypeKind */
{
  char* ctk = 0;
  switch(tk) {
    case enumeration_Type:
	ctk = "tk_enum"; break;
    case byte8_Type:
    	ctk = "tk_octet"; break;
    case int16_Type:
    	ctk = "tk_short"; break;
    case int32_Type:
    	ctk = "tk_long"; break;
    case card16_Type:
    	ctk = "tk_ushort"; break;
    case card32_Type:
	ctk = "tk_ulong"; break;
    case int64_Type:
	ctk = "tk_longlong"; break;
    case card64_Type:
	ctk = "tk_ulonglong"; break;
    case real32_Type:
        /*????*/ break;
    case real64_Type:
        /*????*/ break;
    case real128_Type:
	/*????*/ break;
    case char16_Type:
	/*????*/ break;
    case char8_Type:
	ctk = "tk_char"; break;
    case boolean_Type:
	ctk = "tk_boolean"; break;
    case object_Type:
	ctk = "tk_objref"; break;
    case union_Type:
	ctk = "tk_union"; break;
    case sequence_Type:
	ctk = "tk_sequence"; break;
    case record_Type:
	ctk = "tk_struct"; break;
    case optional_Type:
	/*????*/ break;
    case array_Type:
	ctk = "tk_array"; break;
    case alias_Type:
        ctk = "tk_alias"; 
        break;
    default:
        /*????*/ break;
    }
    if (ctk==0) {
        ctk = "tk_null";
    }
    return cat3(orgDotOmg, ".CORBA.TCKind.", ctk);
}


PRIVATE void
PrintFileHeader(const char * path, Interface interface, IHandle ih)
{
    char * pp = packagePrefixJ(ih);
    printf("// "); printf(replaceBackslashes((char*)path)); printf("\n");
    printBanner("Stubs", interface, ih);
    printf("\n\n");
    if (pp) {
        printf("package %s;\n\n", pp);
    }
}

#ifdef macintosh
static char * separator = ":";
#else
static char * separator = "/";
#endif


PRIVATE void
assertExist(const char * dirname)
{
 
#ifdef WIN32
   /* xxx note we just make specific declarations and defines for now
    * since including <windows.h> causes all kind of grief right now
    */
__declspec(dllimport)  unsigned long __stdcall GetFileAttributesA (const char*);
#define FILE_ATTRIBUTE_DIRECTORY        0x00000010
	if ((GetFileAttributesA(dirname) & FILE_ATTRIBUTE_DIRECTORY))
		return;
#elif defined( macintosh )
    FILE* f;
    char	buf[ 500 + 1 ];
    strcpy( buf, dirname );
    if ( buf[ strlen( buf ) - 1 ] != ':' ) {
        strcat( buf, ":" );
    }
    strcat( buf, "_DirExists.tmp" );
    f = fopen(dirname, "rb");
    if (f) {
        fclose(f);
        remove( buf );
        return;
    }
#else
    FILE* f;
    f = fopen(dirname, "rb");
    if (f) {
        fclose(f);
        return;
    }
#endif
    fatalError("Directory [%s] does not exist.\n", dirname);
}


PRIVATE char*
leftOfX(const char * s, const char * seps)
{
    char * p;
    p = strpbrk(s, seps); /* returns pointer to xx if found */
    if (p) {
        /* a xx is found */
        int idx = p-s;
        if (idx==0) {    
            /* leftmost character */
            return 0;
        } else {
            /* normal case */
            char * c = iluparser_Malloc(idx+1);
            strncpy(c, s, idx);
            c[idx] = 0;
            return c;
        } 
    } else {
        /* no xx found */
        return copy(s);
    }
}


PRIVATE char*
rightOfX(const char* s, const char* seps)
{    
    char * p;
    int len = stringlength(s);
    if (len<=0) return 0;
    p = strpbrk(s, seps); /* returns pointer to xx if found */
    if (p) {
        /* a xx is found */
        int ridx = p-s+1; /*index of character to the right */
        if (ridx>=len) {
            /* rightmost character */
            return 0;
        } else {
            /* normal case */
            char * c = iluparser_Malloc(len-ridx+1);
            strncpy(c, s+ridx, len-ridx);
            c[len-ridx] = 0;
            return c;
        }
    } else {
        /* no xx found */
        return 0;
    }
}

PRIVATE void
createSubdir(const char * dirname)
{
    int retval;
#if defined( WIN32 )
    retval = _mkdir(dirname);
#elif defined( macintosh )
    retval = mkdir(dirname, 0 );
#else
    retval = mkdir(dirname, (S_IRWXU | S_IRWXG));
#endif
    if (retval == 0) return; /*new directory has been created*/
#ifndef macintosh
    if (errno == EEXIST) return; /*directory already exists*/ 
#else
     if (errno == dupFNErr) return; /*directory already exists*/
#endif
    fatalError("Couldn't create directory [%s].\n", dirname);
}


PRIVATE char *
createDirectoryPath(char * existingDir, const char * newpath)
{
    char* xseps = "./\\";
    while (newpath) {
        char * thisStep = leftOfX(newpath, xseps);
        if (existingDir) {
            existingDir = cat3(existingDir, separator, thisStep);
        } else {
            existingDir = thisStep;
        }
        createSubdir(existingDir);
        newpath = rightOfX(newpath, xseps);
    }
    return existingDir;
}


PUBLIC void
LoadJavaClass(const char * className) {
    IHandle ih = currentIH;
    if (! list_find(ih->i.stuffToLoad, (iluparser_FindProc) matchString, (refany) className)) {
        list_insert(ih->i.stuffToLoad, copy(className));
    }
}


PUBLIC void
NewJavaFile(Interface interface, IHandle ih, const char * className) {
    char * dirname = ih->p.dirName;
    char * filename = dotCat(className, "java");
    char * prefix = packagePrefixJ(ih);
    setCurrentIH(ih);
    setCurrentIfc(interface);
    if (dirname==0) {
        fatalError("Must specify a directory");
    }
    if (strcmp(dirname, "javastubs")==0) {
        /* default directory is silently created if not existing */
        createDirectoryPath(0, dirname);
    } else {
        /* directory is not defaulted: it MUST exist as a security measure */
        assertExist(dirname);
    }
    if (! ih->p.flatDir) {
        dirname = createDirectoryPath(dirname, prefix);
    }
    if (dirname) {
        filename = cat3(dirname, separator, filename);
    }
    if (freopen(filename, "w", stdout) == 0 ) {
        fatalError("Couldn't create file %s.\n", filename);
    }
    fprintf(stderr, "writing file %s\n", filename);
    PrintFileHeader(filename, interface, ih);
    reportFile(filename);
}


PUBLIC void
ReportFiles(const char* name)
{
    list thefiles = allfiles;
    if (name == 0) {
        name = "jstubber.files";
    }
    if (freopen(name, "w", stdout) == 0 ) {
        fatalError("Couldn't create file %s.\n", name);
    }
    fprintf(stderr, "writing file %s\n", name);
    LOOP_BEGIN(thefiles, char*, fileName, temp)
        printf("%s\n", fileName);
    LOOP_END()
    allfiles = 0;
}


PUBLIC Type myUrType(Type t)
{
    /* ur_type reats redefinitions as well as aliases */
    return ur_type(t);
}


PUBLIC void
printLoopIndent(const unsigned long level)
{
    unsigned long i;
    for (i = 0; i < level; i++) {
	printf("    ");
    }
}


static const char* gCommentIndent = "";
static boolean gCommentNewLine = TRUE;

PUBLIC void printOpenDocComment(const char* indent)
{
    gCommentIndent = indent;
    gCommentNewLine = TRUE;
    printf("\n%s/** ", indent);
} /* printOpenDocComment */


PUBLIC void printCloseComment()
{
    printf("\n%s */\n", gCommentIndent);
    gCommentNewLine = FALSE;
} /* printCloseComment */


PUBLIC void
printCommentLines(const char* s)
{
    if (s) {
        boolean watchout = 0;
        char *p;
        for (p = (char *) s; *p != '\0'; p++) {
            if (gCommentNewLine) {
                printf("\n%s * ", gCommentIndent);
                gCommentNewLine = FALSE;
                watchout = 0;
            }
            if (*p == '*') {
                putchar('*');
                watchout = 1;
            } else if (*p == '\n') {
                gCommentNewLine = TRUE;
                watchout = TRUE;
            } else if (*p == '/') {
                if (watchout) {
                    putchar('.');
                } else {
                    putchar('/');
                }
                watchout = FALSE;
            } else {
                putchar(*p);
                watchout = 0;
            }
        }
        gCommentNewLine = TRUE;
    }
} /* printCommentLines */


PUBLIC void
printDocString(const char* indent, const char* s)
{
    if (s) {
        printOpenDocComment(indent);
        printCommentLines(s);
        printCloseComment();
    }
} /* printDocString */


PUBLIC char*
formatLongUnsigned(const unsigned long l)
{
    string buff;
    buff = iluparser_Malloc(50);
    buff[0] = 0;
    sprintf(buff, "%lu", l);
    return buff;
}


static char
hexdigit (int i)
{
    i = i % 16;
    if (i>10) {
        return 'A'+(i-10);
    } else {
        return '0'+i;
    }
}

static char*
handleString(const char* s, boolean quotes)
{
    string buff;
    char c;
    int i = 0;
    int offset = 1; /* terminator */
    int len;
    if (quotes) offset = offset+2;
    for (i = 0; s[i] != 0; i++) {  
        c = s[i];
        switch (c) {
            case '\r': case '\n': case '\\': case '\"': case '\'':
               offset++; 
               break;
            default: 
               if (c>=127 || c<32) {offset = offset+5;}
               break;
        }
    }
    len = i+offset;
    buff = iluparser_Malloc(len);
    i = 0;
    offset = 0;
    if (quotes) {buff[i+offset] = '\"'; offset++;}
    for (i = 0; s[i] != 0; i++) {  
        c = s[i];
        switch (c) {
            case '\r': 
                buff[i+offset] = '\\'; offset++; buff[i+offset] = 'r'; 
                break;
            case '\n': 
                buff[i+offset] = '\\'; offset++; buff[i+offset] = 'n'; 
                break;
            case '\\': 
                buff[i+offset] = '\\'; offset++; buff[i+offset] = '\"'; 
                break;
            case '\"': 
                buff[i+offset] = '\\'; offset++; buff[i+offset] = '\\'; 
                break;
            case '\'': 
                buff[i+offset] = '\\'; offset++; buff[i+offset] = '\''; 
                break;
            default: 
               if (c>=127 || c<32) {
                   buff[i+offset] = '\\'; offset++; 
                   buff[i+offset] = 'u'; offset++; 
                   buff[i+offset] = '0'; offset++; 
                   buff[i+offset] = '0'; offset++; 
                   buff[i+offset] = hexdigit((c / 16) % 16); offset++; 
                   buff[i+offset] = hexdigit(c % 16);
               } else {
                   buff[i+offset] = c; 
               }
               break;
        }
    }
    if (quotes) {buff[i+offset] = '\"'; offset++;}
    buff[i+offset] = 0; offset++;
    if (len != i+offset) fatal("failed in quoteString");
    return buff;
}


PUBLIC char*
quoteString(const char* s)
{
    return handleString(s, TRUE);
}


PUBLIC char*
cleanString(const char* s)
{
    return handleString(s, FALSE);
}


PUBLIC char*
qoString(const char* s)
{
    if (s) {
        return handleString(s, TRUE);
    } else {
        return ilu_strdup("null");
    }
}


PUBLIC char *
setDimension(const char *declarator, const char *idx)
{
    boolean watchout = 0;
    char* p;
    char* buff;
    int i;
    int dl = strlen(declarator);
    int il = strlen(idx);
    buff = (char *) iluparser_Malloc(dl + il + 1);
    p = buff;
    for (;;) {
        if (*declarator == 0) fatal("not an array");
        if (watchout & (*declarator == ']')) {
            for (i = 0; i<il; i++) {
                *p = idx[i];
                p++; 
            }
            while (*declarator) {
                *p = *declarator;
                p++; declarator++;
            }
            *p = 0;
            return buff;
        }
        watchout = (*declarator=='[');
        *p = *declarator;
        p++; declarator++;
    }
}


PUBLIC char*
comma0(list l)
/* Returns a comma if l not empty; an empty string otherwise. */
{
    if (list_size(l)) {
        return ", ";
    } else {
        return "";
    }
}


PUBLIC char*
  KernelTypeKindName (Type type)
{
  switch (type_kind(type))
    {
      case byte_Type: return("xerox.ilu.IluTypeKind.byte_tk");
      case boolean_Type: return("xerox.ilu.IluTypeKind.boolean_tk");
      case character_Type: return("xerox.ilu.IluTypeKind.character_tk");
      case shortcharacter_Type: return("xerox.ilu.IluTypeKind.shortcharacter_tk");
      case shortinteger_Type: return("xerox.ilu.IluTypeKind.shortinteger_tk");
      case integer_Type: return("xerox.ilu.IluTypeKind.integer_tk");
      case longinteger_Type: return("xerox.ilu.IluTypeKind.longinteger_tk");
      case shortcardinal_Type: return("xerox.ilu.IluTypeKind.shortcardinal_tk");
      case cardinal_Type: return("xerox.ilu.IluTypeKind.cardinal_tk");
      case longcardinal_Type: return("xerox.ilu.IluTypeKind.longcardinal_tk");
      case real_Type: return("xerox.ilu.IluTypeKind.real_tk");
      case shortreal_Type: return("xerox.ilu.IluTypeKind.shortreal_tk");
      case longreal_Type: return("xerox.ilu.IluTypeKind.longreal_tk");
      case object_Type: return("xerox.ilu.IluTypeKind.object_tk");
      case optional_Type: return("xerox.ilu.IluTypeKind.optional_tk");
      case alias_Type: return("xerox.ilu.IluTypeKind.alias_tk");
      case pickle_Type: return("xerox.ilu.IluTypeKind.pickle_tk");
      case union_Type: return("xerox.ilu.IluTypeKind.union_tk");
      case sequence_Type: return("xerox.ilu.IluTypeKind.sequence_tk");
      case record_Type: return("xerox.ilu.IluTypeKind.record_tk");
      case array_Type: return("xerox.ilu.IluTypeKind.array_tk");
      case enumeration_Type: return("xerox.ilu.IluTypeKind.enumeration_tk");
      case pipe_Type: return("xerox.ilu.IluTypeKind.enumeration_tk");
      default: 
          fatal("bad use of KernelTypeKindName");
          return NULL;
    }
}

PUBLIC char*
  stubConsistencyCheck ()
{
    return cat3(
        "xerox.ilu.IluPreLoad.checkStubConsistency13(\"", 
        ILU_TYPEUID_VERSION_STRING, 
        "\");"
        );  
}

/* end */
