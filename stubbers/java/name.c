/* name.c */
/* Chris Jacobi, December 1, 1998 6:22 pm PST */
 
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
  
/* $Id: name.c,v 1.83 1999/08/03 01:51:04 janssen Exp $ */


/*
 * contains code that sets and gets all types of names (type, exceptions etc.).
 */

#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "shared.h"
#include "name.h"
#include "util.h"
#include "stubops.h"
#include "context.h"
#include "genopt.h"
#include "genrecord.h"
#include "genunion.h"
#include "genarr.h"
#include "genseq.h"
#include "genobj.h"
#include "genenum.h"
#include "genpick.h"
#include "gencust.h"

PRIVATE char simpleKey[] = "java-simple-58764"; /*supposedly unique*/
PRIVATE char mangledKey[] = "-java-mangeld-3ert"; /*supposedly unique*/
PRIVATE char procNameMangledKey[] = "=java-mangeld-7834"; /*supposedly unique*/


PRIVATE char* 
translateBoth(const char* s)
/* Returns new string with dots and dashes converted to underscores.
 */
{
    int l = strlen(s);
    const char* p;
    char* out;
    char* q;
    out = (char* ) iluparser_Malloc(l + 1);
    for (p = s, q = out; *p != 0; p++) {
        if (*p == '-' || *p == '.')
            *q++ = '_';
        else
            *q++ = *p;
    }
    *q = 0;
    return (out);
}


PRIVATE char*
translateDots(const char* s)
/* Returns new string with dots converted to underscores.
 */
{
    int l = strlen(s);
    const char* p;
    char* out;
    char* q;
    out = (char* ) iluparser_Malloc(l + 1);
    for (p = s, q = out; *p != 0; p++) {
        if (*p == '.')
            *q++ = '_';
        else
            *q++ = *p;
    }
    *q = 0;
    return (out);
}

PRIVATE char*
translateDashes(const char* s)
/* Returns new string with dashes converted to underscores.
 */
{
    int l = strlen(s);
    const char* p;
    char* out;
    char* q;
    out = (char* ) iluparser_Malloc(l + 1);
    for (p = s, q = out; *p != 0; p++) {
        if (*p == '-')
            *q++ = '_';
        else
            *q++ = *p;
    }
    *q = 0;
    return (out);
}


#define STANDARDHOLDERPREFIX "org.omg.CORBA."
#define alias_holders_open 1

PRIVATE char*  sunStandardHolderName(Type t)
/* Returns full name of holder class if class is predefined.
 * Strictly returns 0 if holderclass is stub generated.
 * Important as used for decision whether to prefix interface names or not
 */
{
    Type ut = myUrType(t);
    TypeKind k = type_kind(ut);
    switch (k) {
        case alias_Type:
            return 0;
        case enumeration_Type:
            return 0;
        case byte_Type: 
              return STANDARDHOLDERPREFIX "ByteHolder";
        case boolean_Type: 
              return STANDARDHOLDERPREFIX "BooleanHolder";
        case char8_Type:
        case char16_Type: 
              return STANDARDHOLDERPREFIX "CharHolder";
        case int16_Type:
        case card16_Type: 
              return STANDARDHOLDERPREFIX "ShortHolder";
        case card32_Type: 
        case int32_Type: 
              return STANDARDHOLDERPREFIX "IntHolder";
        case card64_Type: 
        case int64_Type: 
              return STANDARDHOLDERPREFIX "LongHolder";
        case real32_Type: 
              return STANDARDHOLDERPREFIX "FloatHolder";
        case real64_Type: 
              return STANDARDHOLDERPREFIX "DoubleHolder";
        case real128_Type: 
              return STANDARDHOLDERPREFIX "Float128Holder";
        case pickle_Type: 
              return STANDARDHOLDERPREFIX "AnyHolder";
        case sequence_Type: 
              if (sq_isJString(t)) {
                  return STANDARDHOLDERPREFIX "StringHolder";
              } else {
                  return 0;
              }
        default: 
            return 0;
    }
}


PRIVATE char* 
declaratorsForStandardTypes(Type t)
/* and 0 otherwise because used on trial basis */
{
    TypeDescription d = baseTypeDescription(t);
    switch (d->type) {
        case enumeration_Type:
        case int32_Type:
        case card32_Type:
            return "int";
        case boolean_Type:
            return "boolean";
        case byte8_Type:
            return "byte";
        case char8_Type:
        case char16_Type:
            return "char";
        case card64_Type:
        case int64_Type:
            return "long";
        case card16_Type:
        case int16_Type:
            return "short";
        case real32_Type:
            return "float";
        case real64_Type:
            return "double";
        case real128_Type:
            fatal("real128 not yet supported");
            return "bad-bad-bad";
        default:
            return 0;
    }
}

static int bugWithPredefined = FALSE;
    /* JDK-1.0.2 forbids classes with names matching predefined classes */ 
    /* JDK-1.1 has this fixed */ 

static list keywords = 0;
static list predefined = 0;
static list not_for_enums = 0;

PRIVATE boolean occurs(const char* s, list lst)
{
    LOOP_BEGIN(lst, char*, str, t1)
        if (strcmp((char*) s, str)==0) return TRUE;
    LOOP_END()
    return FALSE;
}


PUBLIC void
initName()
{
    not_for_enums = iluparser_new_list();
    list_insert(not_for_enums, "value");
    list_insert(not_for_enums, "from_int");
    
    keywords = iluparser_new_list();
    /*real java keywords*/
    list_insert(keywords, "abstract");
    list_insert(keywords, "boolean");
    list_insert(keywords, "break");
    list_insert(keywords, "byte");
    list_insert(keywords, "case");
    list_insert(keywords, "catch");
    list_insert(keywords, "char");
    list_insert(keywords, "class");
    list_insert(keywords, "const");
    list_insert(keywords, "continue");
    list_insert(keywords, "default");
    list_insert(keywords, "do");
    list_insert(keywords, "double");
    list_insert(keywords, "else");
    list_insert(keywords, "extends");
    list_insert(keywords, "final");
    list_insert(keywords, "finally");
    list_insert(keywords, "float");
    list_insert(keywords, "for");
    list_insert(keywords, "goto");
    list_insert(keywords, "if");
    list_insert(keywords, "implements");
    list_insert(keywords, "import");
    list_insert(keywords, "instanceof");
    list_insert(keywords, "int");
    list_insert(keywords, "interface");
    list_insert(keywords, "long");
    list_insert(keywords, "native");
    list_insert(keywords, "new");
    list_insert(keywords, "package");
    list_insert(keywords, "private");
    list_insert(keywords, "protected");
    list_insert(keywords, "public");
    list_insert(keywords, "return");
    list_insert(keywords, "short");
    list_insert(keywords, "static");
    list_insert(keywords, "super");
    list_insert(keywords, "switch");
    list_insert(keywords, "synchronized");
    list_insert(keywords, "this");
    list_insert(keywords, "throw");
    list_insert(keywords, "throws");
    list_insert(keywords, "transient");
    list_insert(keywords, "try");
    list_insert(keywords, "void");
    list_insert(keywords, "volatile");
    list_insert(keywords, "while");
    
    /*non-keywords which cause too much problems to be permitted*/
    list_insert(keywords, "true");
    list_insert(keywords, "false");
    list_insert(keywords, "null");

    /*methods of class java.lang.Object which may cause problems*/
    list_insert(keywords, "getClass");
    list_insert(keywords, "toString");
    list_insert(keywords, "equals");
    list_insert(keywords, "clone");
    list_insert(keywords, "wait");
    list_insert(keywords, "notify");
    list_insert(keywords, "notifyAll");
    list_insert(keywords, "finalize");
    
    predefined = iluparser_new_list();
    list_insert(predefined, "Cloneable");
    list_insert(predefined, "Runnable"); 

    list_insert(predefined, "Void");
    list_insert(predefined, "Byte");
    list_insert(predefined, "Short");
    list_insert(predefined, "Boolean");
    list_insert(predefined, "Character");
    list_insert(predefined, "Class");
    list_insert(predefined, "ClassLoader");
    list_insert(predefined, "Compiler");
    list_insert(predefined, "Double");
    list_insert(predefined, "Float");
    list_insert(predefined, "Integer");
    list_insert(predefined, "Long");
    list_insert(predefined, "Math");
    list_insert(predefined, "Number");
    list_insert(predefined, "Object");
    list_insert(predefined, "Process");
    list_insert(predefined, "Runtime");
    list_insert(predefined, "SecurityManager");
    list_insert(predefined, "String");
    list_insert(predefined, "StringBuffer");
    list_insert(predefined, "System");
    list_insert(predefined, "Thread");
    list_insert(predefined, "ThreadGroup");
    list_insert(predefined, "Throwable"); 

    list_insert(predefined, "ArithmeticException");
    list_insert(predefined, "ArrayIndexOutOfBoundsException");
    list_insert(predefined, "ArrayStoreException");
    list_insert(predefined, "ClassCastException");
    list_insert(predefined, "ClassNotFoundException");
    list_insert(predefined, "CloneNotSupportedException");
    list_insert(predefined, "Exception");
    list_insert(predefined, "IllegalAccessException");
    list_insert(predefined, "IllegalArgumentException");
    list_insert(predefined, "IllegalMonitorStateException");
    list_insert(predefined, "IllegalThreadStateException");
    list_insert(predefined, "IndexOutOfBoundsException");
    list_insert(predefined, "InstantiationException");
    list_insert(predefined, "InterruptedException");
    list_insert(predefined, "NegativeArraySizeException");
    list_insert(predefined, "NoSuchMethodException");
    list_insert(predefined, "NullPointerException");
    list_insert(predefined, "NumberFormatException");
    list_insert(predefined, "RuntimeException");
    list_insert(predefined, "SecurityException");
    list_insert(predefined, "StringIndexOutOfBoundsException"); 

    list_insert(predefined, "AbstractMethodError");
    list_insert(predefined, "ClassCircularityError");
    list_insert(predefined, "ClassFormatError");
    list_insert(predefined, "Error");
    list_insert(predefined, "IllegalAccessError");
    list_insert(predefined, "IncompatibleClassChangeError");
    list_insert(predefined, "InstantiationError");
    list_insert(predefined, "InternalError");
    list_insert(predefined, "LinkageError");
    list_insert(predefined, "NoClassDefFoundError");
    list_insert(predefined, "NoSuchFieldError");
    list_insert(predefined, "NoSuchMethodError");
    list_insert(predefined, "OutOfMemoryError");
    list_insert(predefined, "StackOverflowError");
    list_insert(predefined, "ThreadDeath");
    list_insert(predefined, "UnknownError");
    list_insert(predefined, "UnsatisfiedLinkError");
    list_insert(predefined, "VerifyError");
    list_insert(predefined, "VirtualMachineError");     
}


PUBLIC char*
javaizeIdentSeq(char* s)
{
    s = translateDashes(s);
    if (occurs(s, keywords)) {
        s = cat2("_", s);
    } else if (bugWithPredefined && occurs(s, predefined)) {
        s = cat2("_", s); 
    }
    return s;
}


PUBLIC char*
javaizeIdent(char* s)
{
    s = translateBoth(s);
    if (occurs(s, keywords)) {
        s = cat2("_", s);
    } else if (bugWithPredefined && occurs(s, predefined)) {
        s = cat2("_", s); 
    }
    return s;
}


PRIVATE char* 
unscopedJavaizedName(Name n)
/* syntacticly javaizes name */
{
    string s;
    if (n==0) return 0;
    s = name_lang_name(n, simpleKey);
    if (s==0) {
        s = name_base_name(n);
        if (s) {
            s = javaizeIdent(s);
            name_set_lang_name(n, simpleKey, s);
        }
    }
    return s;
}

PRIVATE refany list_last (list l)
{
    if (l) {
        if (l->tail) {
            return l->tail->data;
        }
    }
    return 0;
}


PRIVATE char* 
scopedIslName(Name n, list scoping)
/* 
 * Scoping if present has as last element the name
 */
{
    string s;
    if (scoping) {
        s = (char *) list_last(scoping);
        if (s) {return s;} 
    } 
    s = name_base_name(n);
    return s;
}


PRIVATE char* 
scopedJavaizedName(Name n, list scoping)
/* Also syntacticly javaizes name.
 * Scoping if present has as last element the name
 */
{
    char* s;
    s = javaizeIdent(scopedIslName(n, scoping));
    return s;
}


PUBLIC char* 
argumentName(Argument a) 
{
    return unscopedJavaizedName(a->name);
}



PUBLIC char* 
constantNameRJ(Constant c) 
{
    if (c==0) fatal("name of null constant");
    while (c->import) {
        c = c->import;
    }
    if (c->name==0) fatal("constant name");
    return scopedJavaizedName(c->name, c->scoping);
}


PUBLIC char* 
enumFieldName(EnumField e)
{
    char* s = javaizeIdent(e->name);
    if (occurs(s, not_for_enums)) {
        /* 
         * At least at a certain date conflicts were
         * resolved by changing the access functions instead
         * the enum field.
         * Loosers! Not only more work but surprises for the programmer.
         */
        s = cat2("__", s);
    }
    return s;
}

 
PUBLIC char* 
exceptionShortName(Exception e)
{
    char* s;
    IHandle ih;
    while (e->import) {
        e = e->import;
    }
    s = scopedJavaizedName(e->name, e->scoping);
    ih = getContext(e->interface, e->scoping);
    if (ih->p.exceptSuffix) {
        s = cat2(s, ih->p.exceptSuffix);
    }
    return s;
}


PUBLIC char* 
exceptionJName(Exception e) 
{
    string name;
    IHandle ih;
    while (e->import) {
        e = e->import;
    }
    name = exceptionShortName(e);
    ih = getContext(e->interface, e->scoping);
    if (ih != currentIH) {
        name = dotCat(packagePrefixJ(ih), name);
    }
    return name; 
}


PUBLIC char* 
packageName(Interface i, list scoping)
{
    IHandle ih = getContext(i, scoping);
    return packagePrefixJ(ih); 
}


PUBLIC char* 
packageNameOptional(Interface i, list scoping) 
{
    IHandle ih = getContext(i, scoping);
    if (ih == currentIH) return 0;
    return packagePrefixJ(ih);
}


PUBLIC char* 
packageDotStringJ(IHandle ih, char* name) 
{
    if (ih == currentIH) return name;
    return dotCat(packagePrefixJ(ih), name);
}


PUBLIC char* 
methodNameBase(Procedure m) 
{
    string s;
    s = name_lang_name(m->name, procNameMangledKey);
    if (s==0) {
        s = unscopedJavaizedName(m->name);
        if (isPrefixOf(idlAttributePrefixCleaned, s)) {
            IHandle ih = getContextT(m->object);
            s = s + strlen(idlAttributePrefixCleaned);
            if (isPrefixOf(idlAttributeSetterPartCleaned, s)) {
                s = s + strlen(idlAttributeSetterPartCleaned);
                if (ih->p.genOmgAttr == 0) {
                    s = cat2("set_", s);
                }
            } else if (isPrefixOf(idlAttributeGetterPartCleaned, s)) {
                s = s + strlen(idlAttributeGetterPartCleaned);
                if (ih->p.genOmgAttr == 0) {
                    s = cat2("get_", s);
                }
            }
        }
        name_set_lang_name(m->name, procNameMangledKey, s);
    }
    return s;
}


PRIVATE char* 
unresolvedEasyTypeShortJavaName(Type t) 
{
    if (t->name == 0 && t->scoping == 0) return 0;
    switch(type_kind(t)) {
        case alias_Type:
        case optional_Type:
        case object_Type:
        case union_Type:
        case array_Type:
        case sequence_Type:
        case record_Type: 
        case enumeration_Type:
            return scopedJavaizedName(t->name, t->scoping);
        default:
            return 0;
    }
}




PRIVATE char* 
classNameCleaning(IHandle ih, char* name)
/* takes a name (syntacticly correct java) and mangles
 * it if its name would be bad in the package from ih.
 * Apply this after easy names...
 */
{
    if (ih->i.stepJ) {
        /* case dependant; we avoid conflict in java's name space */
        if (strcmp(ih->i.stepJ, name)==0) {
            name = cat2("_", name);
        }
    }
    /* isPostfixOf case IN-dependant to also avoid conflicting
     * file names on window based machines
     */
    if (ih->p.genHldCls && stringlength(ih->p.holderSuffix)) {
        if (isPostfixOf(ih->p.holderSuffix, name)){
            name = cat2(name, "_");
        }
    }
    if (ih->p.genHlp && stringlength(ih->p.helperSuffix)) {
        if (isPostfixOf(ih->p.helperSuffix, name)){
            name = cat2(name, "_");
        }
    }
    if (ih->p.genPortability && stringlength(ih->p.operationsSuffix)) {
        if (isPostfixOf(ih->p.operationsSuffix, name)){
            name = cat2(name, "_");
        }
    }
    if (isPostfixOf(ih->p.stubSuffix, name)){
        name = cat2(name, "_");
    }
    if (stringlength(ih->p.refSuffix)){
        if (isPostfixOf(ih->p.refSuffix, name)){
            name = cat2(name, "_");
        }
    }
    return name;
}


PUBLIC char* 
easyShortTypeNameCleaned(Type t) 
{
    char* s;
    if (t->name == 0) fatal("bad name");
    s = name_lang_name(t->name, mangledKey);
    if (s == 0) {
        s = unresolvedEasyTypeShortJavaName(t);
        if (s == 0) {
            Type t1 = t->supertype;
            /* Don't take ur_type all the way
             * (we promised not to take urtype)
             */
            if (t1 && (t1 != t)) {
                s = easyShortTypeNameCleaned(t1);
            }
        }
        if (s == 0) {
            fatal("bad name");
        }
        s = javaizeIdent(s);
        s = classNameCleaning(getContextT(t), s);
        if (rec_is_a(t)) {
            if (isPrefixOf(idlExceptionPrefixCleaned, s)) {
                s = s + strlen(idlExceptionPrefixCleaned);
                s = cat2(s, idlExceptionSuffix);
            }
        }
        name_set_lang_name(t->name, mangledKey, s);
    }
    return s;
}


PRIVATE char* 
easyTypeNameJ(Type t)
{
    IHandle ih;
    string name;
    t = myUrType(t);
    name = easyShortTypeNameCleaned(t);
    ih = getContextT(t);
    if (ih != currentIH) {
        name = dotCat(packagePrefixJ(ih), name); 
    }
    return name;
}   


PRIVATE char* 
sunsWayHolderName(Type t) 
{
    char* s;
    t = myUrType(t);
    s = sunStandardHolderName(t);
    if (s==0) {
        if (isCorbaDotObject(t)) {
            return  copy("org.omg.CORBA.ObjectHolder");
        } else {
            IHandle ih = getContextT(t);
            char * easyName = easyShortTypeNameCleaned(t);
            if (easyName==0) fatal("failed naming holder");
            easyName = cat2(easyName, ih->p.holderSuffix);
            s = packageDotStringJ(ih, easyName);
        }
    }
    return s;
}

PUBLIC char* unresolvedIslTypeName(Type t)
{
     IHandle ih;
     char*  name;
     ih = getContextT(t);
     name = scopedIslName(t->name, t->scoping); 
     name = dotCat(packagePrefixMinus1(ih), name);
     return name;
}  /*unresolvedIslTypeName*/ 


PUBLIC char* 
typeNameUnresolvedButClean(Type t)
{
    /* Intent is type name, but unlike other places
     * 1) should not resolve aliases
     * 2) replaces IDL import dots by underscores because it is part
     *    of an identifier.
     * 3) idl name, not java name
     *
     * (USED FOR UNION CASE NAMES)
     */
     char*  x;
     x = unresolvedEasyTypeShortJavaName(t);  
     if (x==0) {
         x = scopedJavaizedName(t->name, t->scoping);
     }
     x = packageDotStringJ(getContextT(t), x);
     return translateDots(x);
}   


PUBLIC char* 
typeDeclarator(Type t)
{
    char*  buff = 0;
    t = myUrType(t);
    if (custom_is_a(t)) {
        return custom_typeDeclarator(t);
    }
    switch (type_kind(t)) {
        case enumeration_Type:
            return enm_typeDeclarator(t);
        case sequence_Type:
            return sq_typeDeclarator(t);
        case array_Type:
            return ar_typeDeclarator(t);
        case object_Type:
            return obj_typeDeclarator(t);
        case record_Type:
            return rec_typeDeclarator(t);
        case optional_Type:
            return opt_typeDeclarator(t);
        case union_Type:
            return un_typeDeclarator(t);
        case pickle_Type:
            return pick_typeDeclarator();
        default: 
            buff = declaratorsForStandardTypes(t);
            if (buff) return buff;
            return easyTypeNameJ(t);  
    }    
}   


PUBLIC char* 
holderTypeDeclarator(Type t) 
{
    IHandle ih = getContextT(t);
    if (custom_is_a(t)) {
        char* decl = custom_holderTypeDeclarator(t);
        if (decl) return decl;
    }
    if (ih->p.genHldCls) {
        return sunsWayHolderName(t); 
    } else {
        return cat2(typeDeclarator(t), "[]");
    }
}


/* end */


