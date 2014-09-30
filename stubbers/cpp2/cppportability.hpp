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
$Id: cppportability.hpp,v 1.10 1999/09/14 10:24:11 pnewman Exp $
*/

/* Macros for portability across C++ compilers */

#ifndef __cppportability_hpp_
#define __cppportability_hpp_ 1

#ifndef WIN32
    #include <iluconf.h>
#else
    #include <iluwin.h>
#endif

/*

Namespaces are a relatively new feature in C++, and are poorly implemented in current compilers.

In some cases (SunPro4.1, G++2.7.1), "namespace" is not implemented at all, but can be faked with "class". Use BEGIN_NAMESPACE and END_NAMESPACE to begin/end a namespace definition.  Members of the class can be accessed as className::memberName, so the net effect is similar to a real namespace (but to get real portability, use access macros, as described below). Of course, one cannot apply "using" clauses to classes, nor can a class be "re-opened" the way that a real namespace can.

For compilers that support neither namespaces nor nested classes, hyphenated compound names of the form namespaceName_memberName are constructed (the MSVC4.1 implementation is buggy, and we use this workaround for it). NAME_INSIDE_SCOPE and NAME_OUTSIDE_SCOPE are used for this purpose (you are in the scope of a namespace when within it's declaration, or in some further nested namespace, class method, etc). These two macros are typically used indirectly, through macros defined specifically for a given namespace. Consider the following code, written in ANSI-compliant C++:


  // Define a namespace

  namespace MyNamespace {
    struct someStruct {
      ...
    };
    class SomeClass {
      public:
        void someMethod (someStruct * sc);
    };
  };  // MyNamespace


  // Use namespace members

  MyNamespace::someStruct ss;
  MyNamespace::SomeClass someObject;
  ...
  someObject.someMethod(&ss);
  ...

  // Define member function of class in namespace

  void
  MyNamespace::SomeClass::someMethod (MyNamespace::someStruct * ss) {
    someStruct copyStruct;
    copyStruct = *ss;
    ...
  };



Using the portability namespace macros, the preceeding becomes:


  // Define a namespace

  #define MyNamespace_(name) NAME_INSIDE_SCOPE(MyNamespace, name)
  #define MyNamespace(name) NAME_OUTSIDE_SCOPE(MyNamespace, name)

  BEGIN_NAMESPACE(MyNamespace)
    struct someStruct {
      ...
    };
    class MyNamespace_(SomeClass) {
      public:
        void someMethod (MyNamespace_(someStruct) * sc);
    };
  END_NAMESPACE;  // MyNamespace

  // Use namespace members

  MyNamespace(someStruct) ss;
  MyNamespace(SomeClass) someObject;
  ...
  someObject.someMethod(&ss);
  ...

  // Define member function of class in namespace

  void
  MyNamespace(SomeClass)::someMethod (MyNamespace(someStruct) * ss) {
    MyNamespace_(someStruct) copyStruct;
    copyStruct = *ss;
    ...
  };


*/


    #ifdef CPLUSPLUSMAPPING_NAMESPACES
        #define BEGIN_NAMESPACE(nsName) namespace nsName {
        #define END_NAMESPACE }
        #define NAME_INSIDE_SCOPE(prefix, name) name
        #define NAME_OUTSIDE_SCOPE(prefix, name) prefix ## :: ## name
        #define NS_EXTERN extern
    #elif defined(CPLUSPLUSMAPPING_NESTEDCLASSES)
		#ifdef WIN32
		#define BEGIN_NAMESPACE(nsName) ILU_RUNTIME_PUBLIC_CLASS nsName { public:
		#else
        #define BEGIN_NAMESPACE(nsName) class nsName { public:
		#endif
        #define END_NAMESPACE }
        #define NAME_INSIDE_SCOPE(prefix, name) name
        #define NAME_OUTSIDE_SCOPE(prefix, name) prefix ## :: ## name
        #define NS_EXTERN static
    #else
        #define BEGIN_NAMESPACE(nsName)
        #define END_NAMESPACE
        #define NAME_INSIDE_SCOPE(prefix, name) prefix ## _ ## name
        #define NAME_OUTSIDE_SCOPE(prefix, name) prefix ## _ ## name
        #define NS_EXTERN extern
    #endif  // CPLUSPLUSMAPPING_NAMESPACES



/*
SunPro4.1 doesn't like use of the "static" storage specifier for template functions (""static" is not allowed and is being ignored").

*/


    #ifdef __SUNPRO_CC
        #define TEMPLATE_STATIC
    #else
        #define TEMPLATE_STATIC static
    #endif  // (defined(__SUNPRO_CC) || defined(_MSC_VER))






/*
Not all compilers support the bool type.

SunPro4.1 does not support bool.

MSVC4.1 treats bool, true, and false as reserved words, but does not yet implement the bool type.

For MSVC 6, _MSC_VER is 1200
*/


    #if ((defined(__SUNPRO_CC) && (__SUNPRO_CC < 0x500)) || (defined(_MSC_VER) && (_MSC_VER < 1200)))
        typedef int ILUCPP_BOOL;
        #define ILUCPP_TRUE 1
        #define ILUCPP_FALSE 0
    #else
        typedef bool ILUCPP_BOOL;
        #define ILUCPP_TRUE true
        #define ILUCPP_FALSE false
    #endif  // (defined(__SUNPRO_CC) || defined(_MSC_VER))



/*

Casting.

SunPro4.1 does not support static_cast; G++2.7.1 only supports static_cast when RTTI is enabled.

*/
/* Used in stubs */
#if ( defined(__GNUC__) && __GNUC__ <= 2 && __GNUC_MINOR__ < 95)
  #define OLDGNUC_WORKAROUND 1
#endif

  #if (defined(__GNUC__) || (defined(__SUNPRO_CC) && (__SUNPRO_CC < 0x500)))
    #define STATIC_CAST(typeid, expression) ((typeid)(expression))
    #define CONST_CAST(typeid, expression) ((typeid)(expression))
    #define REINTERPRET_CAST(typeid, expression) ((typeid)(expression))
  #else
    #define STATIC_CAST(typeid, expression) static_cast<typeid>(expression)
    #define CONST_CAST(typeid, expression) const_cast<typeid>(expression)
    #define REINTERPRET_CAST(typeid, expression) reinterpret_cast<typeid>(expression)
  #endif  // (defined(__GNUC__) || defined(__SUNPRO_CC))


/* NULL pointers */
  #if (defined(__SUNPRO_CC) && (__SUNPRO_CC >= 0x500))
     #include <inttypes.h>
  #endif
  #if (defined(__SUNPRO_CC) && (__SUNPRO_CC >= 0x500))
     #define ILUCPP_NULL_PTR  uintptr_t
     #define ILUCPP_NULL     ((uintptr_t) 0) 
  #else
     #define ILUCPP_NULL_PTR  int 
     #define ILUCPP_NULL      0 
  #endif


#endif // __cppportability_hpp_
