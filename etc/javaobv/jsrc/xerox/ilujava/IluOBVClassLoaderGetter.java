/* $Id: IluOBVClassLoaderGetter.java,v 1.2 1999/08/03 01:56:22 janssen Exp $
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
/* Chris Jacobi, December 26, 1998 9:05 pm PST */

package xerox.ilujava;

/**
 * Interface by which a class loader might notify its presence to
 * IluOBV.   An object which exports this interface is a
 * factory for class loaders which can be used when internalizing
 * objects in IluOBV. <p>
 *
 * Typical usages: <br>
 *     - Static registration of loader mechanisms <br>
 *     - Caching loader on data types, e.g. custom surrogates <p> 
 *
 * Consider this ILU engine room and not used for generic clients. <p>
 */
public interface IluOBVClassLoaderGetter {
    
    /** A classloader factory */
    public java.lang.ClassLoader IluOBVGetClassLoader(
        xerox.ilujava.MarshalledObjectBase m) 
            throws java.lang.ClassNotFoundException;
    
} //IluOBVClassLoaderGetter 
