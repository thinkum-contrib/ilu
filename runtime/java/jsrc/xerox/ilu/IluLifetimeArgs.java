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
/* IluLifetimeArgs.java */
/* Chris Jacobi, December 23, 1998 12:09 pm PST */
/* 
 * $Id: IluLifetimeArgs.java,v 1.8 1999/08/03 01:54:01 janssen Exp $ 
 */
 
package xerox.ilu;

/** 
 * Literals for specifying the lifetime argument of ilu objects.<p>
 *
 * @see Ilu#registerTrueObject 
 * @see IluRT0#registerTrueObject 
 * @see IluObjectTable#returnTrueObject 
 * @see IluLifetimeForget
 * @see IluLifetimeRemember
 */
public interface IluLifetimeArgs {

    /** 
     * Requires ilu to set a value depending on other criteria. 
     */
    public static final int iluLifetimeUnspec = 0;
    
    /** 
     * Object will be remembered by the server and therefore 
     * not found by the garbage collector. <p>
     *
     * Using this specifier is better then using a global 
     * hashtable because this specifier prevents java garbage 
     * collection only while the IluServer exporting the object
     * stays alive. <p>
     *
     * Do NOT USE for ilu collectable objects as this will prevent
     * collection.
     */
    public static final int iluLifetimeRemember = 1;

    /** 
     * Object will not be remembered by ilu, but left for the 
     * garbage collector. 
     */
    public static final int iluLifetimeForget = 2;

} //IluLifetimeArgs

