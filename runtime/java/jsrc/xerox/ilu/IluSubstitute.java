/* IluSubstitute.java */
/* Chris Jacobi, January 18, 1999 7:35 pm PST */
/* $Id: IluSubstitute.java,v 1.5 1999/08/03 01:53:42 janssen Exp $ */
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
 
package xerox.ilu;

/**
 * Place-holder class used for serializing ILU Object types.<p>
 * Client use not expected.<br>
 * Resolves automaticly on JKD1.2 and newer only; explicitely resolved
 * on JKD1.1 in ILU's ObjectInputStreams.  This class very explicitely
 * is not an org.omg.CORBA.Object, so replaceObject will not be called
 * twice even in JKD1.2 where both mechansims might be used. 
 * @see IluResolving
 */
public class IluSubstitute implements java.io.Serializable, IluResolving {
    
    public java.lang.String sbh = null;

    /**
     * Provides IluResolving.
     * Called by deserialization
     */
    public java.lang.Object readResolve() 
            throws java.io.ObjectStreamException
    {
        java.lang.Object obj;
        obj = xerox.ilu.IluRT0.objectFromSBH(
            this.sbh, 
            xerox.ilu.IluClassRep.rootClass()
            );
            // THIS DOES NOT LOAD THE CLASS
        return obj;
    } //readResolve
    
    static final long serialVersionUID = -2748043724941557685L;
    
} //IluSubstitute

