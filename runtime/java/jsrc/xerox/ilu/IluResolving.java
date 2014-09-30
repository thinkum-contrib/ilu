/* IluResolving.java */
/* Chris Jacobi, December 23, 1998 10:03 am PST */
/* $Id: IluResolving.java,v 1.2 1999/08/03 01:54:03 janssen Exp $ */
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
 * Ilu based java objects (except object-types) may export this   
 * interface to denote special treatment on de-serialization.<p>
 * Typically IluResolving objects also implement java.io.Serializable, but
 * we require such objects to implement java.io.Serializable explicitely.<p>
 *
 * <b>Warning:</b> Resolving must be implemented idempotent.  Resolving in
 * JDK1.2 is invoked automaticly by the deserialization. However in JDK1.1
 * this is not yet implemented and Ilu calls the resolving exxplicitely.
 * As a consequence, in JDK1.2 this might be called twice. <p>
 *
 * ILU object-types must therefore NOT implement this IluResolving 
 * interface!  ILU object-types in JDK1.2 are substituted on serialization 
 * and the substitution type takes care of resolving. Implementing this
 * interface would cause double deserialization.
 *
 * @see java.io.Serializable
 * @see IluSubstitute
 */
public interface IluResolving {
    
    /**
     * Called automaticly on deserialization for JDK1.2 and newer.
     * Called explicitely from ILU based ObjectInputStreams.
     * Must be idempotent in case of being called because of each reason.
     */
    public java.lang.Object readResolve() 
            throws java.io.ObjectStreamException;

} //IluResolving

