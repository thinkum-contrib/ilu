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
/* IluClassAccess.java */
/* Chris Jacobi, December 23, 1998 11:52 am PST */
/* $Id: IluClassAccess.java,v 1.18 1999/08/03 01:53:58 janssen Exp $ */
 
package xerox.ilu;
    
/**
 * Ilu internal class helping to find java classes for ilu classes..
 *
 * This allows registering java classes as implementing
 * ilu classes (and retrieving the ilu class from the java class). <p>
 * Unlikely to be useful to applications.
 */
public class IluClassAccess {
    
    private static java.util.Hashtable tab = new java.util.Hashtable();
        //mapping from name of java class to ilu class
    
    /** 
     * Get the ilu class implemented by objects of a java class.<p>
     * You might loose if a java class implements multiple ilu classes 
     * or if a java class is not registered to implement any ilu
     * class.
     */
    public static IluClassRep iluClassFromJavaClass(java.lang.Class cls) 
            throws org.omg.CORBA.SystemException {
        if (cls == null) throw new java.lang.NullPointerException();
        IluClassRep x = (IluClassRep) tab.get(cls);
        if (x == null) {
            x = searchTheWorld(cls);
            if (x == null) {
                throw new org.omg.CORBA.BAD_PARAM(
                    "java class not known to implement an ilu class");
            }
            synchronized (tab) {
                IluClassRep y = (IluClassRep) tab.get(cls);
                if (y != null) {
                    x = y;
                } else {
                    tab.put(cls, x);
                }
            }
        }
        return x;
    } //iluClassFromJavaClass
    

    /** 
     * Returns whether a java class implements an ilu class directly.
     * cls is the java class.  <p>
     * iluName is the name of the ilu class.
     * It does recognize a superclass of cls doing the exporting.
     * It does not recognize if only a sublass of iluName is supported.
     * (Not a lousy implementation; ON PURPOSE).
     */
    private static boolean 
    javaClassSupportsIluClass(java.lang.Class cls, java.lang.String iluName) {
        java.lang.Class [] interfaces = cls.getInterfaces();
        for (int i = 0; i<interfaces.length; i++) {
            java.lang.String ifName = interfaces[i].getName();
            if (ifName.equals(iluName)) return true;
        }
        java.lang.Class sClass = cls.getSuperclass();
        if (sClass != null) {
            return javaClassSupportsIluClass(sClass, iluName);
        }
        return false;
    } //javaClassSupportsIluClass
    
        
    /** 
     * Registers a java class as implementor of an ilu class.<p>
     * If a java class implements multiple ilu classes your out of luck.
     * Use on most specific ilu class only.
     */
    public static void registerIluClassImplementor(
        java.lang.Class cls, 
        IluClassRep iluClass) 
            throws org.omg.CORBA.SystemException {
        if (cls == null) {throw new java.lang.NullPointerException();}
        java.lang.String iluName = iluClass.referenceIfName();
        //it is necessary to do a check as errors would be ugly
        if (javaClassSupportsIluClass(cls, iluName)) {
            synchronized (tab) {
                tab.put(cls, iluClass);
            }
        } else {
            throw new org.omg.CORBA.BAD_PARAM(
                "java class does not support ilu class");
        }
    } //registerIluClassImplementor
    
    
    /** 
     * Search all registered ilu classes whether it matches one of the
     * interfaces implemented by the java class cls. <p>
     * Returns any class found of maximal depth...
     */
    private static IluClassRep searchTheWorld(java.lang.Class cls) {
        int bestDepth = -1; 
        IluClassRep bestIluClass = null;
        if (cls == null) throw new java.lang.NullPointerException();
        java.util.Enumeration enum = IluClassRep.gClassTable.elements();
        while (enum.hasMoreElements()) {
            IluClassRep ic = (IluClassRep) enum.nextElement();
            int thisDepth = ic.superClassCnt();
            if (thisDepth>bestDepth) {
                java.lang.String iluName = ic.referenceIfName();
                if (javaClassSupportsIluClass(cls, iluName)) {
                    bestDepth = thisDepth;
                    bestIluClass = ic;
                }
            }
        }
        return bestIluClass;
    } //searchTheWorld
    
    
} //IluClassAccess
 


