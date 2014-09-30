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
/* IluExceptionRep.java */
/* Chris Jacobi, December 23, 1998 12:04 pm PST */
/* $Id: IluExceptionRep.java,v 1.15 1999/08/03 01:53:50 janssen Exp $ */
 
/*
 * See IluJava_IluExceptionRep.c
 */

package xerox.ilu;

/**
 * IluExceptionRep represents an Ilu exception.<p>
 *
 * Stub visible; not likely to be used by applications. <p>
 * This is NOT an exception class but something to 
 * represent an exception class; IluExceptionRep's are not
 * raised and caught but passed around. <br>
 * (Unlike real java exceptions, IluExceptionRep's are 
 * created without capturing a stack frame). <p>
 *
 * @see IluMethodRep
 */
public final class IluExceptionRep extends IluWPBase {
    private long yIluException = 0; //native defined
    /*friendly*/ java.lang.Class exClass;
    private java.lang.String i;	//native used
    private java.lang.String e;	//native used
    private java.lang.String jtuuid;	//native used
    
    private static java.util.Hashtable tab = 
       new java.util.Hashtable(); //no native use
    
    /** 
     * Private to prevent creation of bogus instances.
     * Use defineException to create instances.
     */
    private IluExceptionRep(
            java.lang.Class exClass, 
            java.lang.String i, java.lang.String e, java.lang.String tuuid) {
        super();
        this.exClass = exClass;
        this.i = i; 
        this.e = e;
        this.jtuuid = tuuid;
    } //IluExceptionRep
    
    /** 
     * Stub accessible. <br>
     * Fetches or defines an IluExceptionRep and its ilu_Exception.
     *
     * @param i  ilu interface name
     * @param e  ilu exception name
     */
    public static IluExceptionRep 
    defineException (
            java.lang.String className, 
            java.lang.String i, java.lang.String e, java.lang.String tuuid) {
       IluExceptionRep exRep = null;
       java.lang.String key = (i==null) ? ("#" + e) : (i + "#" + e);
       java.lang.Object val = tab.get(key);
       if (val!=null) {
           return ((IluExceptionRep) val);
       }
       synchronized (tab) {
           val = tab.get(key);
           if (val!=null) {
               exRep = ((IluExceptionRep) val);
           } else {
               java.lang.Class exClass = null;
               try {
                   exClass = java.lang.Class.forName(className);
               } catch (ClassNotFoundException cnfex) {
                   throw new IluSystemExceptionBase("couldn't define exception");
               }
               exRep = new IluExceptionRep(exClass, i, e, tuuid);
               exRep.registerException();
               tab.put(key, exRep);
           }
       } 
       return exRep;
    } //defineException
    
    /* called by defineException; called within monitor lock */
    private native void registerException();
    
    public java.lang.String toString() {
       if (i==null) {
           return super.toString() + ":#" + this.e;
       } else {
           return super.toString() + ":" + this.i + "#" + this.e;
       }
    } //toString
    
    /*friendly*/ IluUserException
    allocInstance() throws org.omg.CORBA.SystemException {
        try {
            IluUserException ex = (IluUserException) exClass.newInstance();
            return ex;
        } catch (java.lang.Exception e) {
            throw new IluSystemExceptionBase("couldn't allocate exception");
        }
    } //allocInstance
    
    static {
        IluInit.init();
    }
     
} //IluExceptionRep
