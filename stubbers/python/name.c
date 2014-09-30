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
$Id: name.c,v 1.16 1999/08/30 23:49:51 janssen Exp $
*/

#include <stdio.h>
#include <string.h>
#include "iluptype.h"
#include "manifest.h"
#include "name.h"
#include "util.h"

static void
translateNameString(char *dst, const char *src)
{
 	char *name = dst;
	while (*src)
	{
		*dst++ = *src == '-' ? '_' : *src;
		src++;
	}
	*dst = 0;
#ifdef ILU_CORBA_PYTHON_MAPPING
	/* ``IDL names that are also Python identifiers are suffixed
	   with an underscore ('_')'' */
#else
 	/* Python keywords that are not also reserved keywords in IDL
 	 * must be renamed by pre-pending an underscore to avoid trouble
 	 */
#endif
 	if (strcmp(name, "access") == 0
	    || strcmp(name, "and") == 0
	    || strcmp(name, "assert") == 0
 	    || strcmp(name, "break") == 0
 	    || strcmp(name, "class") == 0
 	    || strcmp(name, "continue") == 0
 	    || strcmp(name, "def") == 0
 	    || strcmp(name, "del") == 0
 	    || strcmp(name, "elif") == 0
 	    || strcmp(name, "else") == 0
 	    || strcmp(name, "except") == 0
 	    || strcmp(name, "finally") == 0
 	    || strcmp(name, "for") == 0
 	    || strcmp(name, "from") == 0
 	    || strcmp(name, "global") == 0
 	    || strcmp(name, "if") == 0
 	    || strcmp(name, "import") == 0
 	    || strcmp(name, "in") == 0
 	    || strcmp(name, "is") == 0
 	    || strcmp(name, "lambda") == 0
 	    || strcmp(name, "not") == 0
 	    || strcmp(name, "or") == 0
 	    || strcmp(name, "pass") == 0
 	    || strcmp(name, "print") == 0
 	    || strcmp(name, "raise") == 0
 	    || strcmp(name, "return") == 0
 	    || strcmp(name, "try") == 0
 	    || strcmp(name, "while") == 0) {
 	   /* somewhat dangerous because we cannot check for overflow! */
 	   memmove(name + 1, name, strlen(name) + 1);
#ifdef ILU_CORBA_PYTHON_MAPPING
	   {
	     int i;
	     i = strlen(name);
	     name[i] = '_';
	     name[i+1] = 0;
	   }
#else
 	   name[0] = '_';
#endif /* def ILU_CORBA_PYTHON_MAPPING */
 	}
}

static char	language[] = "python";
static char	corba_innermost[] = "corba_innermost";
static char	corba_scoped[] = "corba_scoped";
static char	simple_exception[] = "simple_exception";

static char *
setItemName(Name n, char *importInterface)
{
	char	iname[1024];
	char	buffer[1024];
	char *	dst = buffer;

	if (importInterface)
	{
		translateNameString(iname, importInterface);
		sprintf (dst, "%s['%s'].", nameVarImports, iname);
		dst += strlen(dst);
	}
	translateNameString(dst, n->base_name);
	name_set_lang_name(n, language, buffer);
	return name_lang_name(n, language);
}

static char *
getItemName(Name n, char *importInterface)
{
	char *	result	= name_lang_name(n, language);

	if (result == 0)
		result = setItemName(n, importInterface);
	return result;
}

/************************************************************************/

char *
getArgumentName(Argument a)
{
	return getItemName(a->name, NULL);
}

char *
getConstantName(Constant c)
{
	return getItemName(c->name, c->importInterfaceName);
}

char *
getEnumFieldName(EnumField e, char *buffer)
{
	translateNameString(buffer, e->name);
	return buffer;
}

char *
getExceptionName(Exception e)
{
	return getItemName(e->name, e->importInterfaceName);
}

char *
getSimpleExceptionName(Exception e)
{
#ifdef ILU_CORBA_PYTHON_MAPPING
  char *	result;
  char    buffer[1024];
  int	idlish = 0;

  result = name_lang_name(e->name, corba_innermost);
  if (result == 0) {
    char *basename = list_ref(e->scoping, list_size(e->scoping) - 1);
/*
    fprintf (stderr, "basename for %s.%s is %s\n",
	     interface_name(e->interface), exception_name(e), basename);
*/
    translateNameString(buffer, basename);
    name_set_lang_name(e->name, corba_innermost, buffer);
    result = name_lang_name(e->name, corba_innermost);
  };
  return result;
#else
  char *	result;
  char    buffer[1024];

  result = name_lang_name(e->name, simple_exception);
  if (result == 0) {
    char *basename = name_base_name(e->name);
/*
    fprintf (stderr, "basename for %s.%s is %s\n",
	     interface_name(e->interface), exception_name(e), basename);
*/
    translateNameString(buffer, basename);
    name_set_lang_name(e->name, simple_exception, buffer);
    result = name_lang_name(e->name, simple_exception);
  };
  return result;
#endif
}

char *
getImportName(const char *name, char *buffer)
{
	translateNameString(buffer, name);
	return buffer;
}

char *
getInterfaceName(Interface i)
{
  char buffer[1024];
  char newname[1024];
  char *result	= name_lang_name(i->name, "imported_interface");

  if (result == 0)
    {
      translateNameString(buffer, name_base_name(i->name));
      sprintf (newname, "%s['%s']", nameVarImports, buffer);
      name_set_lang_name(i->name, "imported_interface", newname);
      result = name_lang_name(i->name, "imported_interface");
    }
  return result;
}

char *
getSimpleInterfaceName (Interface i)
{
  return getItemName(i->name, NULL);
}

char *
getProcedureName(Procedure p)
{
	char *	result	= name_lang_name(p->name, language);

	if (result == 0)
	{
		char	buffer[1024];
		char *	src	= p->name->base_name;

		if (isPrefixOf(prefixIdlAttribute, src))
			src += strlen(prefixIdlAttribute);

		translateNameString(buffer, src);
		name_set_lang_name(p->name, language, buffer);
		result = name_lang_name(p->name, language);
	}
	return result;
}

char *
getTypeName(Type t)
{
	char *	result;
	char    buffer[1024];
	int	idlish = 0;

	result = name_lang_name(t->name, language);
	if (result == 0) {
	  char *basename = name_base_name(t->name);
	  if (isPrefixOf(prefixIdlExcType, basename)) {
	    basename += strlen(prefixIdlExcType);
	    idlish = 1;
	  };
	  translateNameString(buffer, basename);
	  if (idlish)
	    strcat(buffer, suffixIdlExcType);
	  name_set_lang_name(t->name, language, buffer);
	  result = name_lang_name(t->name, language);
	};
	if (t->importInterfaceName)
		fatal("getTypeName: type is external (%s)", result);
	return result;
}

char *
getSimpleTypeName(Type t)
{
#ifdef ILU_CORBA_PYTHON_MAPPING
  char *	result;
  char    buffer[1024];
  int	idlish = 0;

  result = name_lang_name(t->name, corba_innermost);
  if (result == 0) {
    char *basename = list_ref(t->scoping, list_size(t->scoping) - 1);
    if (isPrefixOf(prefixIdlExcType, basename))
      basename += strlen(prefixIdlExcType);
    /*
    fprintf (stderr, "basename for %s.%s is %s\n",
	     interface_name(type_interface(t)), type_name(t), basename);
	     */
    translateNameString(buffer, basename);
    name_set_lang_name(t->name, corba_innermost, buffer);
    result = name_lang_name(t->name, corba_innermost);
  };
  if (t->importInterfaceName)
    fatal("getSimpleTypeName: type is external (%s)", result);
  return result;
#else
  return getTypeName(t);
#endif
}

char *
getScopedTypeName(Type t)
{
#ifdef ILU_CORBA_PYTHON_MAPPING
  char *	result;
  char *	basename;
  char    buffer[1024], buffer2[1024];
  int i, lim, idlish;

  if (isPrefixOf(prefixIdlExcType, type_name(t)))
    return getTypeName(t);

  result = name_lang_name(t->name, corba_scoped);
  if (result == 0) {
    buffer[0] = 0;
    for (i = 1, lim = list_size(t->scoping);  i < lim;  i++) {
      if (i > 1)
	strcat (buffer, ".");
      basename = (char *) list_ref(t->scoping, i);
      strcat (buffer, basename);
    }
    /*
    fprintf (stderr, "scoped name for %s.%s is %s\n",
	     interface_name(type_interface(t)), type_name(t), buffer);
    */
    translateNameString(buffer2, buffer);
    name_set_lang_name(t->name, corba_scoped, buffer2);
    result = name_lang_name(t->name, corba_scoped);
  };
  if (t->importInterfaceName)
    fatal("getScopedTypeName: type is external (%s)", result);
  return result;
#else
  return getTypeName(t);
#endif
}

char *
getTypeUID(Type t)
{
	char *	result;

	return (type_uid(t));
}

char *
getIslProcedureName(Procedure p)
{
	char *	result	= procedure_name(p);

	if (isPrefixOf(prefixIdlAttribute, result))
		result += strlen(prefixIdlAttribute);
	return result;
}
