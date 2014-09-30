/* isl2java.c */
/* Chris Jacobi, January 18, 1999 1:31 pm PST  */
/* Last edited by Mike Spreitzer September 17, 1998 10:36 pm PDT */

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

 
/* $Id: isl2java.c,v 1.72 1999/08/03 01:51:05 janssen Exp $ */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "stubops.h"
#include "name.h"
#include "util.h"
#include "genstub.h"
#include "context.h"
#include "genobj.h"
#include "gencust.h"


#ifdef _IS_POSIX
#include <unistd.h>
#include <errno.h>
#endif				/* _IS_POSIX */

static void genLoadOneClass(char * fullName)
/* Generates code to load class.  
 * No short name: uses a string
 */
{
    printf("        xerox.basics.Environment.loadClasses(\"%s\");\n", fullName);
}


PUBLIC void printLoadClasses(IHandle ih, boolean alreadyInStatic) 
{
    char* classShortName = ih->p.loaderClass_Name;
    if (! classShortName) return;
    if (!alreadyInStatic) printf("    static {\n");
    /*genLoadOneClass(dotCat(packagePrefixJ(ih), classShortName));*/
    printf("        %s.load();\n", dotCat(packagePrefixJ(ih), classShortName));
    if (!alreadyInStatic) printf("    }\n");
}


static void
generateLoadClassOneLevel(IHandle ih) {
    Interface ifc = 0;
    char* classShortName = ih->p.loaderClass_Name;
    if (! classShortName) return;
    if (list_size(ih->i.stuffToLoad) == 0) return;
    NewJavaFile(ifc, ih, classShortName);
    printf("import xerox.basics.Environment;\n\n");
    printf("/**\n");
    printf(" * Loads all java classes used for the stub (client) \n");
    printf(" * side of the ilu interface which might not be loaded  \n");
    printf(" * automatically when needed.\n");
    printf(" */\n");
    printf("public class %s {\n",
        classShortName
        );
    printf("    public static void load(){}\n");
    printf("    static {\n");
    LOOP_BEGIN(ih->i.stuffToLoad, char*, className, t1)
        genLoadOneClass(dotCat(packagePrefixJ(ih), className));
    LOOP_END()
    printf("    }\n");
    printf("}//%s\n\n", classShortName);
}


static void
generateLoadClass(IHandle ih) {
    generateLoadClassOneLevel(ih);
    LOOP_BEGIN(ih->i.children, IHandle, cih, t1)
        generateLoadClass(cih);
    LOOP_END()
}


static boolean
matchImportedWithString(void *p1, void *p2)
{
    return strcmp(((Imported) p1)->name, (char *) p2) == 0;
}


typedef struct {
    list sortedTypes;
} PtRock;


static void
processType(Type t, PtRock * r)
{
    static list pendingTypes;
    if (t->importInterfaceName != 0)
	return;
    if (list_find(r->sortedTypes, matchPointer, t) != 0) {
	return;
    }
    if (pendingTypes == 0) {
	pendingTypes = new_list();
    }
    if (list_find(pendingTypes, matchPointer, t) != 0) {
	fatalError("internal error: circularity in types");
    }
    list_insert(pendingTypes, t);
    if (t->description) {
	switch (t->description->type) {
            case object_Type:
                {
		    Class c = t->description->structuredDes.object;
		    list_enumerate(c->superclasses, (EnumProc) processType, r);
		}
		break;
            default:
		/* null */
		break;
	}
    } else {
	/* alias */
	processType(t->supertype, r);
    }
    list_remove(pendingTypes, t);
    list_insert(r->sortedTypes, t);
}


static list
normalizeInterface(Interface ifc)
{
    PtRock rock;
    rock.sortedTypes = new_list();
    list_enumerate(ifc->types, (EnumProc) processType, &rock);
    return rock.sortedTypes;
}


static void
recognizeIDLInterfaces(Interface ifc, list localtypes)
{
    LOOP_BEGIN(localtypes, Type, t, temp)
        if (t->description) {
            switch (t->description->type) {
                case record_Type: 
                case object_Type:
                    {
                        char* baseName = easyShortTypeNameCleaned(t);
                        IHandle ih = getContextT(t);
                        IHandle dummySelfIH = getChildContext(ih, baseName, 1);
                        dummySelfIH->i.isIdlInterface = 1;
                    }
                    break;
                default:
                    break;
            }
        }
    LOOP_END()
} /* recognizeIDLInterfaces */


typedef enum {
    OptSpecial,
    OptDebug, 
    OptNoReport, 
    OptReportFile, 
    CustomMap, 
    OptDir, OptDir1, 
    OptExSuffix, OptExSuffix1, 
    OptXrx, OptXrx1, 
    OptXrxSuffix, OptXrxSuffix1, 
    OptXrxHolder, OptXrxHolder1, 
    OptOmg, OptOmg1, 
    OptOmgSuffix, OptOmgSuffix1, 
    OptOmgHolder, OptOmgHolder1, 
    OptFlat, OptFlat1, 
    OptPrefix, OptPrefix1, 
    OptNoPrefix, OptNoPrefix1,
    OptHlp, OptHlp1, 
    OptNoHlp, OptNoHlp1,
    OptNoServ, OptNoServ1, 
    OptPort, OptPort1, 
    OptNoPort, OptNoPort1, 
    OptDel, OptDel1, 
    OptMethNameWithIf, OptMethNameWithIf1, /* full method names */
    OptMethNameWithPa, OptMethNameWithPa1, /* full method names */
    OptVisiVar, OptVisiVar1, 
    OptVisiMode, OptVisiMode1, 
    OptNoSkeleton, OptNoSkeleton1, 
    OptVisiSkeleton, OptVisiSkeleton1, 
    OptOmgSkeleton, OptOmgSkeleton1, 
    OptInclude,
    OptNull
} Option;


typedef struct {
    char *optName;
    Option opt;
    boolean ifaceArg;
    int otherArgs;
    char* help;
} OptTuple;


static OptTuple optionTable[] =
{
    {"null", OptNull, 0, 0, 0}, /* must be first */
    {"special", OptSpecial, 0, 0, "for ilu implementors only (PIDL mode)"},
    {"debug", OptDebug, 0, 0, "used when debugging the stubber"},
    {"noreport", OptNoReport, 0, 0, "don't report list of generated java files"},
    {"reportfile", OptReportFile, 0, 1, "specify file for list of generated java files"},
    {"custom", CustomMap, 0, 1, "specify file for custom mapping definitions"},
    {"dir", OptDir, 0, 1, 0},
    {"dir1", OptDir1, 1, 1, "specify java stubs directory"},
    {"flat", OptFlat, 0, 0, 0},
    {"flat", OptFlat1, 1, 0, "do not generate hierarchical directories"},
    {"prefix", OptPrefix, 0, 1, 0},
    {"prefix1", OptPrefix1, 1, 1, "specify prefix package for stubs"},
    {"noprefix", OptNoPrefix, 0, 0, 0},
    {"noprefix1", OptNoPrefix1, 1, 0, "no prefix package for stubs"},
    {"excsuffix", OptExSuffix, 0, 1, 0},
    {"excsuffix1", OptExSuffix1, 1, 1, "specify suffix for exceptions"},
    {"hlp", OptHlp, 0, 0, 0},
    {"hlp1", OptHlp1, 1, 0, "do generate helper classes"},
    {"nohlp", OptNoHlp, 0, 0, 0},
    {"nohlp1", OptNoHlp1, 1, 0, "don't generate helper classes"},
    {"noserv", OptNoServ, 0, 0, 0},
    {"noserv1", OptNoServ1, 1, 0, "don't generate server skeletons"},
    {"port", OptPort, 0, 0, 0},
    {"port1", OptPort1, 1, 0, "operations ifs. etc. (according Portability orbos/98-03-10)"},
    {"noport", OptNoPort, 0, 0, 0},
    {"noport1", OptNoPort1, 1, 0, "don't generate operations ifs. etc."},
    {"delegation", OptDel, 0, 0, 0},
    {"delegation1", OptDel1, 1, 0, "experiment: generate delegation classes"},
    {"xrx", OptXrx, 0, 0, 0},
    {"xrx1", OptXrx1, 1, 0, "generate ilu-style stubs"},
    {"xrxsuffix", OptXrxSuffix, 0, 0, 0},
    {"xrxsuffix1", OptXrxSuffix1, 1, 0, "generate ilu-style classname suffixes"},
    {"xrxholder", OptXrxHolder, 0, 0, 0},
    {"xrxholder1", OptXrxHolder1, 1, 0, "don't generate holder classes; (ilu-style: use arrays)"},
    {"noskel", OptNoSkeleton, 0, 0, 0},
    {"noskel1", OptNoSkeleton1, 1, 0, "don't generate servant base classes (ilu-style: interfaces only)"},
    {"omg", OptOmg, 0, 0, 0},
    {"omg1", OptOmg1, 1, 0, "generate omg-style stubs"},
    {"omgsuffix", OptOmgSuffix, 0, 0, 0},
    {"omgsuffix1", OptOmgSuffix1, 1, 0, "generate omg-style classname suffixes"},
    {"omgholder", OptOmgHolder, 0, 0, 0},
    {"omgholder1", OptOmgHolder1, 1, 0, "generate omg-style holder classes"},
    {"omgskel", OptOmgSkeleton, 0, 0, 0},
    {"omgskel1", OptOmgSkeleton1, 1, 0, "generate omg-style servant base classes"},
    {"methodif", OptMethNameWithIf, 0, 0, 0},
    {"methodif1", OptMethNameWithIf1, 1, 0, "method names include interface"},
    {"methodfull", OptMethNameWithPa, 0, 0, 0},
    {"methodfull1", OptMethNameWithPa1, 1, 0, "method names include package and interface"},
{"visimode", OptVisiMode, 0, 0, 0},
    {"visimode1", OptVisiMode1, 1, 0, "generate visigenic-style stubs"},
    {"visivar", OptVisiVar, 0, 0, 0},
    {"visivar1", OptVisiVar1, 1, 0, "generate visigenic-style var classes"},
    {"visiskel", OptVisiSkeleton, 0, 0, 0},
    {"visiskel1", OptVisiSkeleton1, 1, 0, "generate visigenic-style servant base classes"},
    {"I", OptInclude, 0, 1, "specify include directory"},

};


static void
printOptions()
{
    boolean hadHelp = 0;
    int i;
    for (i = 1; i < sizeof optionTable / sizeof optionTable[0]; i++) {
	if (! optionTable[i].ifaceArg || hadHelp) fprintf(stderr, "\n    ");
	fprintf(stderr, "[-%s", optionTable[i].optName);
	if (optionTable[i].ifaceArg) {
	    fprintf(stderr, " ifc");
	}
	if (optionTable[i].otherArgs) {
	    fprintf(stderr, " arg");
	}
	fprintf(stderr, "] ");
	if (optionTable[i].help) {
	    fprintf(stderr, " %s", optionTable[i].help);
	    hadHelp = 1;
	} else {
	    hadHelp = 0;
	}
    }
    fprintf(stderr, "\n    ");
}


static int
getOptionTabIndex(char *name)
{
    int i;
    for (i = 1; i < sizeof optionTable / sizeof optionTable[0]; i++) {
	if (strcmp(name, optionTable[i].optName) == 0) return i;
    }
    return 0;
}

static Option
optionFromIndex(int i)
{
    return optionTable[i].opt;
}


static boolean
needsInterfaceFromIndex(int i)
{
    return optionTable[i].ifaceArg;
}


static void
usage(void)
{
    fprintf(stderr, "usage: %s ", programName);
    printOptions();
    fprintf(stderr, "files\n");
    exit(0);
}

static void contextAdd_NoSkeleton(IHandle ih)
{
    ih->p.genImpl = 0;
    ih->p.implSuffix = 0;
    ih->p.implPrefix = 0;
}

static void contextAdd_VisiSkeleton(IHandle ih)
{
    ih->p.genImpl = 1;
    ih->p.implSuffix = 0;
    ih->p.implPrefix = "_sk_";
}

static void contextAdd_OmgSkeleton(IHandle ih)
{
    ih->p.genImpl = 1;
    ih->p.implSuffix = "ImplBase";
    ih->p.implPrefix = "_";
}

static void contextAdd_OmgAttributes(IHandle ih)
{
    ih->p.genOmgAttr = 1;
}

static void contextAdd_xrxholder(IHandle ih)
{
    ih->p.genHldCls = 0;
}

static void contextAdd_xrxsuffix(IHandle ih)
{
    ih->p.stubSuffix = "_stub";
    ih->p.refSuffix = "";
    ih->p.holderSuffix = "_var";
    ih->p.helperSuffix = "_help";
    ih->p.delClassSuffix = "_delegation";
}

static void contextAdd_xrx(IHandle ih)
{
    ih->p.genHlp = 1;
    contextAdd_xrxholder(ih);
    contextAdd_xrxsuffix(ih);
}

static void contextAdd_visiVar(IHandle ih)
{
    ih->p.genVisiVar = 1;
}

static void contextAdd_Omgsuffix(IHandle ih)
{
    ih->p.stubSuffix = "Stub";
    ih->p.refSuffix = "";
    ih->p.holderSuffix = "Holder";
    ih->p.helperSuffix = "Helper";
    ih->p.delClassSuffix = "_delegation";
}

static void contextAdd_Omgholder(IHandle ih)
{
    ih->p.genHldCls = 1;
}

static void contextAdd_Omg(IHandle ih)
{
    ih->p.genHlp = 1;
    contextAdd_Omgsuffix(ih);
    contextAdd_Omgholder(ih);
    contextAdd_OmgSkeleton(ih);
    contextAdd_OmgAttributes(ih);
}


static void contextAdd_flatness(IHandle ih, boolean flatness)
{
    ih->p.flatDir = flatness;
}


static void contextAdd_prefix(IHandle ih, char * prefix)
{
    if (stringlength(prefix)==0) prefix = 0;
    ih->p.prefix = prefix;
}


static boolean mySeparator(char c) {
    if (c == 0) return TRUE;
    if (c == ' ') return TRUE;
    if (c == '\n') return TRUE;
    if (c == '\r') return TRUE;
    if (c == '\t') return TRUE;
    return FALSE;
}


static char* skipSeparators(char* ptr) 
/* While ptr points to a separator, increase it. 
 * But doesn't skip a terminating 0
 */
{
    while (1) {
        if (ptr == 0) {
            return 0;
        } else if (*ptr == 0) {
            return ptr;
        } else if (mySeparator(*ptr)) {
            ptr++; 
        } else {
            return ptr;
        }
    }
}


static char* skipNonSeparators(char* ptr) 
/* While ptr points to a non separator, increase it. 
 * But doesn't skip a terminating 0
 */
{
    while (1) {
        if (ptr == 0) {
            return 0;
        } else if (*ptr == 0) {
            return ptr;
        } else if (mySeparator(*ptr)) {
            return ptr; 
        } else {
            ptr++;
        }
    }
}


static char* copyNextSegment(char* ptr) 
/* Copies from ptr[0] up to but not including the separator. 
 * Returns 0 if no segment available or segment empty.
 */
{
    char* result = 0;
    char* startP = 0;
    char* stopP = 0;
    if (ptr == 0) {return 0;}
    startP = skipSeparators(ptr);
    stopP = skipNonSeparators(startP);
    if (stopP-startP) {
        result = iluparser_Malloc(stopP-startP+1);
        strncpy(result, startP, stopP-startP);
        result[stopP-startP] = 0;
        return result;
    } else {
        return 0;
    }
}

static char* advance(char* ptr) 
/* advances pointer past what "copyNextSegment" would return 
 * and past white space
 */
{
    char* startP = 0;
    char* stopP = 0;
    if (ptr == 0) {return 0;}
    startP = skipSeparators(ptr);
    stopP = skipNonSeparators(startP);
    return skipSeparators(stopP);
}


PRIVATE CustomMappingSpec* allocCustomSpec()
{
    CustomMappingSpec* cms = iluparser_Malloc(
        sizeof(struct customMappingSpec_s)
        );
    cms->iluTypeName = 0;
    cms->javaTypeName = 0;
    cms->holderClassName = 0;
    cms->loadThisClassName = 0;
    return cms;
}


static void remoteFromIDoc(Interface ifc, list ll)
{
    boolean isFirst = 1;
    IHandle ih;
    char* ifname = ifc->name->base_name;
    if (stringlength(ifname) == 0) {
        fprintf(stderr, "Remote-request error (empty interface)\n");
        fatalError("Remote-request error");
    }
    ih = getContextC(ifname);
    LOOP_BEGIN(ll, char*, iluTypeName, temp)
        if (isFirst) {
            isFirst = 0;
        } else {
            if (strcmp(iluTypeName, "*")==0) {
                ih->p.iJavaRemote = 1;
            } else {
                TypeSpec ts = getTypeSpec(ih, iluTypeName);
                ts->javaRemote = 1;
            }
        }
    LOOP_END()
} /* remoteFromIDoc */


static void noCorbaFromIDoc(Interface ifc, list ll)
{
    boolean isFirst = 1;
    IHandle ih;
    char* ifname = ifc->name->base_name;
    if (stringlength(ifname) == 0) {
        fprintf(stderr, "NoCorbaObject-request error (empty interface)\n");
        fatalError("NoCorbaObject-request error");
    }
    ih = getContextC(ifname);
    LOOP_BEGIN(ll, char*, iluTypeName, temp)
        if (isFirst) {
            isFirst = 0;
        } else {
            if (strcmp(iluTypeName, "*")==0) {
                ih->p.iNoIluObject = 1;
            } else {
                TypeSpec ts = getTypeSpec(ih, iluTypeName);
                ts->noIluObject = 1;
            }
        }
    LOOP_END()
} /* noCorbaFromIDoc */


static void customMappingFromIDoc(Interface ifc, list ll)
{
    IHandle ih;
    char* ifname = ifc->name->base_name;
    int sz = list_size(ll);
    CustomMappingSpec* cms = allocCustomSpec();
    if (stringlength(ifname) == 0) {
        fprintf(stderr, "Custom-map error (empty interface)\n");
        fatalError("Custom-map error");
    }
    if (sz < 3) {
        fprintf(stderr, "Custom map: not enough arguments in %s\n", ifname);
        fatalError("Custom-map error");
    }
    
    cms->iluTypeName = (string) list_ref(ll, 1);
    if (stringlength(cms->iluTypeName) == 0) {
        fprintf(stderr, "Custom-map: empty isl type name in %s\n", ifname);
        fatalError("Custom-map error");
    }
    
    cms->javaTypeName = (string) list_ref(ll, 2);
    if (stringlength(cms->javaTypeName) == 0) {
        fprintf(stderr, "Custom map: empty java type name in %s.%s\n", 
            ifname, cms->iluTypeName);
        fatalError("Custom-map error");
    }
    
    if (sz >= 4) {
        cms->holderClassName = (string) list_ref(ll, 3);
        if (stringlength(cms->holderClassName) == 0) {
            cms->holderClassName = 0;
        } else if (cms->holderClassName[0] == '*') {
            cms->holderClassName = 0;
        }
    }
        
    if (sz >= 5) {
        cms->loadThisClassName = (string) list_ref(ll, 4);
        if (stringlength(cms->loadThisClassName) == 0) {
            cms->loadThisClassName = 0;
        } else if (cms->loadThisClassName[0] == '*') {
            cms->loadThisClassName = 0;
        }
    }
        
    ih = getContextC(ifname);
    if (ih->i.customMappings == 0) {
        ih->i.customMappings = new_list();
    }
    ih->p.genHlp = TRUE;
    list_insert(ih->i.customMappings, cms);
} /* customMappingFromIDoc */


static void customMappingFromFile(char * arg)
{
    FILE* f;
    char buffer[1000];
    CustomMappingSpec* cms;
    IHandle ih;
    char* ifname;
    char* cp;
    int lineNumber = 0;
    arg = iluparser_FindFileInIncludes(arg);
    f = fopen(arg, "r");
    if (f == 0) {
        fatal("custom mapping file \"%s\" not found ", arg);
    }
    while (1) {
        lineNumber = lineNumber+1;
        cp = fgets(buffer, 999, f);
        if (cp == 0) break; /* end of file */
        cp = skipSeparators(cp); /*skips over white space */
        if (cp == 0) continue; /*skip empty line*/
        if (*cp == 0) continue; /*skip empty line*/
        if (*cp == '#') continue; /*skip comment line*/
        if (*cp == '!') continue; /*skip comment line*/
        
        ifname = copyNextSegment(cp); cp = advance(cp);
        if (stringlength(ifname)==0) {
            fprintf(stderr, "Custom-map error (iface): line %d\n", lineNumber);
            fprintf(stderr, "\"%s\"\n", buffer);
            fatalError("Custom-map error");
        }
        
        ih = getContextC(ifname);
        if (ih->i.customMappings == 0) {
            ih->i.customMappings = new_list();
        }
        ih->p.genHlp = TRUE;
        cms = allocCustomSpec();
        list_insert(ih->i.customMappings, cms);
        
        cms->iluTypeName = copyNextSegment(cp); cp = advance(cp);
        if (stringlength(cms->iluTypeName) == 0) {
            fprintf(stderr, "Custom map error (w-type): line %d\n", lineNumber);
            fprintf(stderr, "\"%s\"\n", buffer);
            fatalError("Custom-map error");
        }

        cms->javaTypeName = copyNextSegment(cp); cp = advance(cp);
        if (stringlength(cms->javaTypeName) == 0) {
            fprintf(stderr, "Custom map error (j-type): line %d\n", lineNumber);
            fprintf(stderr, "\"%s\"\n", buffer);
            fatalError("Custom-map error");
        }
        
        cms->holderClassName = copyNextSegment(cp); cp = advance(cp);
        if (stringlength(cms->holderClassName) == 0) {
            cms->holderClassName = 0;
        } else if (cms->holderClassName[0] == '*') {
            cms->holderClassName = 0;
        }
        
        cms->loadThisClassName = copyNextSegment(cp);
        if (stringlength(cms->loadThisClassName) == 0) {
            cms->loadThisClassName = 0;
        } else if (cms->loadThisClassName[0] == '*') {
            cms->loadThisClassName = 0;
        }
        
        /* for debugging  
            if (cms->iluTypeName==0) cms->iluTypeName = "";
            if (cms->javaTypeName==0) cms->javaTypeName = "";
            if (cms->holderClassName==0) cms->holderClassName = "";
            if (cms->loadThisClassName==0) cms->loadThisClassName = "";
            fprintf(stderr, "CUSTOM MAPPING line %d\n", lineNumber);
            fprintf(stderr, "  if: %s\n", ifname);
            fprintf(stderr, "  iluType: %s\n", cms->iluTypeName);
            fprintf(stderr, "  javaType: %s\n", cms->javaTypeName);
            fprintf(stderr, "  holderClass: %s\n", cms->holderClassName);
            fprintf(stderr, "  loadThisClass: %s\n", cms->loadThisClassName);
        */

    }
    fclose(f);
}


static void contextAdd_dirName(IHandle ih, char * dirName)
{
    if (stringlength(dirName)==0) dirName = 0;
    ih->p.dirName = dirName;
}

static void contextAdd_mni(IHandle ih)
{
    ih->p.methodNamesWithInterface = 1;
    ih->p.methodNameSeparator = "_";
}

static void contextAdd_mnp(IHandle ih)
{
    ih->p.methodNamesWithInterface = 1;
    ih->p.methodNamesWithPackage = 1;
    ih->p.methodNameSeparator = "_";
}


static void contextAdd_visiMode(IHandle ih)
{
    contextAdd_Omgholder(ih);
    contextAdd_Omgsuffix(ih);
    contextAdd_visiVar(ih);
    contextAdd_VisiSkeleton(ih);
    contextAdd_prefix(ih, 0);
}


static void contextAdd_ExSuffix(IHandle ih, char * suffix)
{
    if (stringlength(suffix)==0) suffix = 0;
    ih->p.exceptSuffix = suffix;
}

static void defineDefaultContext() {
    IHandle ctx = getContextRoot();
    contextAdd_Omg(ctx);
    ctx->p.prefix = 0;
    ctx->p.dirName = "javastubs";
    ctx->p.loaderClass_Name = "_allJavaStubs";
    ctx->p.genFactory = 0;
    ctx->p.extraFiles = 1;
    ctx->p.extraSuffix = "_exh_";
}

static void defineIluContext() {
    IHandle ctx = getContextC("ilu");
    contextAdd_Omg(ctx);
    ctx->p.forbidden = 1;
    contextAdd_prefix(ctx, "xerox");
    init_genobj();
}


/* list of directories to be searched for ISL files */
static list includesPathList = NULL;

static void AppendIncludePath(char* path)
{
    list_insert(includesPathList, path);
} /*AppendIncludePath*/


static list checkedInterfaces = 0;


extern void ilujava_stubber_checkImportsPragmas(Imported imp, refany rock);
 /* not public but only a forward declaration */ 

static void
checkInterfacePragmas(Interface ifc, refany rock)
{
    IHandle ctx;
    if (! list_insert_onceonly(&checkedInterfaces, ifc)) {
	return;
    }
    setCurrentIH(0);
    if (ifc->imports) {
        list_enumerate(ifc->imports, (EnumProc) ilujava_stubber_checkImportsPragmas, NULL);
    }
    if (ifc->idirectives) {
        LOOP_BEGIN(ifc->idirectives, list, thisDoc, t1)
            if (thisDoc) {
                int sz = list_size(thisDoc);
                string key = (string) list_ref(thisDoc, 0);
                /*
                 * JAVA-PREFIX
                 */
                if (strcmp(key, "JAVA-PREFIX") == 0) {
                    string val = 0;
                    ctx = getContextC(ifc->name->base_name);
                    if (sz >= 2) {
                        val = (string) list_ref(thisDoc, 1);
                    }
                    /*fprintf(stderr, "DIRECTIVE JAVA-PREFIX: %s\n", val);*/
                    contextAdd_prefix(ctx, val);
                }
                /*
                 * JAVA-CUSTOMFILE
                 */
                if (strcmp(key, "JAVA-CUSTOMFILE") == 0) {
                    string val = 0;
                    if (sz >= 2) {
                        val = (string) list_ref(thisDoc, 1);
                        customMappingFromFile(val);
                    }
                    /*fprintf(stderr, "DIRECTIVE JAVA-CUSTOMFILE: %s\n", val);*/
                }
                /*
                 * JAVA-CUSTOM
                 */
                if (strcmp(key, "JAVA-CUSTOM") == 0) {
                    customMappingFromIDoc(ifc, thisDoc);
                    /*fprintf(stderr, "DIRECTIVE JAVA-CUSTOM\n");*/
                }
                /*
                 * JAVA-REMOTE
                 */
                if (strcmp(key, "JAVA-REMOTE") == 0) {
                    remoteFromIDoc(ifc, thisDoc);
                    /*fprintf(stderr, "DIRECTIVE JAVA-REMOTE\n");*/
                }
                /*
                 * JAVA-REMOTE
                 */
                if (strcmp(key, "JAVA-NOCORBAOBJECT") == 0) {
                    noCorbaFromIDoc(ifc, thisDoc);
                    /*fprintf(stderr, "DIRECTIVE JAVA-NOCORBAOBJECT\n");*/
                }
            }
        LOOP_END()
    }
} /* checkInterfacePragmas */


void
ilujava_stubber_checkImportsPragmas(Imported imp, refany rock)
{
    Interface impIfc;
    impIfc = GetInterface(imp->name, imp->filename);
    if (impIfc == 0) {
	fprintf(stderr, "Can't find imported interface <%s>\n", imp->name);
    	return;
    }
    checkInterfacePragmas(impIfc, rock);
} /* checkImportsPragmas */


static void
generate_1_ifc(Interface ifc, refany rock)
{
    list localtypes;
    setCurrentIH(0);
    localtypes = normalizeInterface(ifc);
    recognizeIDLInterfaces(ifc, localtypes);
    generateStub(ifc, localtypes);
} /* generate_1_ifc */


int main(int argc, char **argv)
{
    boolean reportFlag = TRUE;
    char * reportFilename = 0;
    int optind = 1;
    char * ifname;
    IHandle ctx;
    includesPathList = new_list(); /* directories to search for ISL files */
    initName();
    defineDefaultContext();
    defineIluContext();
    if ((programName = iluparser_GetProgramName(argv[0])) == NULL) {
      programName = argv[0];
    };
    while (optind < argc && argv[optind][0] == '-') {
	char *opt = (argv[optind++])+1;
	int tabIndex = getOptionTabIndex(opt);
	ifname = 0;
	if (needsInterfaceFromIndex(tabIndex)) {
            if (optind>=argc) usage();
            ifname = argv[optind++]; 
            ctx = getContextC(ifname); 
	} else {
	    ctx = getContextRoot();
	}
	switch (optionFromIndex(tabIndex)) {
            case OptDebug:
                debugFlag = TRUE;
                break;
            case OptNoReport:
                reportFlag = FALSE;
                break;
            case OptReportFile:
                reportFlag = TRUE;
                if (optind >= argc) usage();
                reportFilename = argv[optind++];
                reportFlag = (stringlength(reportFilename) > 0);
                break;
            case OptSpecial:
                {   
                    IHandle ctx = getContextC("ilu");
                    ctx->p.forbidden = 0;
                }
                break;
            case CustomMap:
		customMappingFromFile(argv[optind++]);
		break;
            case OptFlat:
            case OptFlat1:
		contextAdd_flatness(ctx, 1);
		break;
            case OptNoPrefix:
            case OptNoPrefix1:
		contextAdd_prefix(ctx, 0);
		break;
            case OptPrefix:
            case OptPrefix1:
		if (optind>=argc) usage();
		contextAdd_prefix(ctx, argv[optind++]);
		break;
            case OptHlp:
            case OptHlp1:
		ctx->p.genHlp = TRUE;
		break;
            case OptNoHlp:
            case OptNoHlp1:
		ctx->p.genHlp = FALSE;
		break;
            case OptDir:
            case OptDir1:
		if (optind>=argc) usage();
		contextAdd_dirName(ctx, argv[optind++]);
		break;
            case OptInclude:
		if (optind>=argc) usage();
		AppendIncludePath(argv[optind++]);
		break;
            case OptExSuffix:
            case OptExSuffix1:
		if (optind>=argc) usage();
		contextAdd_ExSuffix(ctx, argv[optind++]);
		break;
            case OptOmg:
            case OptOmg1:
		contextAdd_Omg(ctx);
		break;
            case OptOmgSuffix:
            case OptOmgSuffix1:
		contextAdd_Omgsuffix(ctx);
		break;
            case OptOmgHolder:
            case OptOmgHolder1:
		contextAdd_Omgholder(ctx);
		break;
            case OptXrx:
            case OptXrx1:
		contextAdd_xrx(ctx);
		break;
            case OptXrxSuffix:
            case OptXrxSuffix1:
		contextAdd_xrxsuffix(ctx);
		break;
            case OptXrxHolder:
            case OptXrxHolder1:
		contextAdd_xrxholder(ctx);
		break;
            case OptNoSkeleton:
            case OptNoSkeleton1:
		contextAdd_NoSkeleton(ctx);
		break;
            case OptVisiSkeleton:
            case OptVisiSkeleton1:
		contextAdd_VisiSkeleton(ctx);
		break;
            case OptOmgSkeleton:
            case OptOmgSkeleton1:
		contextAdd_OmgSkeleton(ctx);
		break;
            case OptNoServ:
            case OptNoServ1:
		ctx->p.noServer = 1;
		break;
            case OptPort:
            case OptPort1:
		ctx->p.genPortability = 1;
		ctx->p.operationsSuffix = "Operations";
		break;
            case OptNoPort:
            case OptNoPort1:
		ctx->p.genPortability = 0;
		break;
            case OptDel:
            case OptDel1:
		ctx->p.genDel = 1;
		break;
            case OptVisiVar:
            case OptVisiVar1:
		contextAdd_visiVar(ctx);
		break;
            case OptVisiMode:
            case OptVisiMode1:
		contextAdd_visiMode(ctx);
		break;
            case OptMethNameWithIf:
            case OptMethNameWithIf1:
		contextAdd_mni(ctx);
		break;
            case OptMethNameWithPa:
            case OptMethNameWithPa1:
		contextAdd_mnp(ctx);
		break;
            default:
		usage();
	}
    }
    if (optind >= argc) {
        usage();
        return 0;
    }
    
    /* tell parser about the directories it should search for ISL files in */
    iluparser_RegisterInterfaceDirectories(includesPathList);

    while (optind < argc) {
	char *fileName = argv[optind++];
	list s;
	if ((s = ParseFile(fileName)) == 0) {
            fatalError("Couldn't find or parse %s.\n", fileName);
	}
	fprintf(stderr, "done with parsing\n");
	list_enumerate(s, (EnumProc) checkInterfacePragmas, NULL);
	list_enumerate(s, (EnumProc) generate_1_ifc, NULL);
    }
    generateLoadClass(getContextRoot());
    if (reportFlag) {
        ReportFiles(reportFilename);
    }
    return 0;
}


/* end */

