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
/* IluSimpleBinding.java */
/* Chris Jacobi, January 6, 1999 5:15 pm PST */
/* $Id: IluSimpleBinding.java,v 1.12 1999/08/03 01:54:05 janssen Exp $ */

 
package xerox.ilu;

/**
 * Simple binding for ILU.<p>
 * @see Ilu
 */
public final class IluSimpleBinding {
        
    /* Don't create instances */
    private IluSimpleBinding(){
    } //constructor
    
    
    /** 
     * Importing an object 
     */
    public static java.lang.Object 
    lookup(
            java.lang.String sid, 
            java.lang.String ih, 
            IluClassRep iluClass
            ) throws org.omg.CORBA.SystemException {
        return IluRT0.lookupObject(sid, ih, iluClass);
    } //lookup
    
    
    /** 
     * Publishes the SBH of the object in the local object domain.
     * Do not use on surrogates.
     */
    public static void
    publish(java.lang.Object tobj) throws org.omg.CORBA.SystemException {
        IluRT0.publishTrueObject(tobj);
    } //publish


    /** 
     * "withdraw" is undo of publish.
     * Do not use on surrogates.
     */
    public static void
    withdraw(java.lang.Object tobj) throws org.omg.CORBA.SystemException {
        IluRT0.withdrawObject(tobj);
    } //withdraw

  
    static {
       IluInit.init();
    } //static
    
} // IluSimpleBinding

