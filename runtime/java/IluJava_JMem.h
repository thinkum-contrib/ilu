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
/* IluJava_JMem.h */
/* Chris Jacobi, November 18, 1998 2:01 pm PST */

/*
 */
 
/* $Id: IluJava_JMem.h,v 1.9 1999/08/03 01:54:20 janssen Exp $ */

/*
 * Macros to make it possible to switch the implementation
 * of java system memory allocator without disrupting the  
 * actual usage code in ilu.
 *
 * The ilu language support is conceptionally dealing with
 * 3 heaps without any knowledge whether they are shared or not.
 *   a) the collected java object heap
 *   b) the uncollected, untraced C heap used by the java VM
 *   c) the uncollected, untraced C heap used by the ilu kernel
 *
 * This include file provides the b) case above.
 */

#ifndef _ILUJAVA_JMEM_
#define _ILUJAVA_JMEM_

#include "IluJava_Includes.h"

/* java_sysMalloc 
 * Used this to allocate memory into what java thinks
 * is the C heap. (which may or may not be the same
 * heap as used by ilu...)
 *
 * java_free 
 * Use this to free an object which has been allocated on the
 * C heap by the java interpreter.
 */

#if (defined(RNI))

    #define java_sysMalloc(sz) (malloc( sz ))
    #define java_free(ptr) (free( ptr ))

#elif (ILUJAVA_H_MINORVERSION < 1)

    #define java_sysMalloc(sz) sysMalloc((size_t) sz )
    #define java_free(ptr) sysFree( ptr );

#else

    #define java_sysMalloc(sz) (malloc( sz ))
    #define java_free(ptr) (free( ptr ))

#endif


#endif /* _ILUJAVA_JMEM_ */
