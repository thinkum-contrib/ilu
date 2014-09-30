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
/* IluDataSupport.java */
/* Chris Jacobi, November 12, 1998 4:26 pm PST */

/*
 */
 
/* 
 * $Id: IluDataSupport.java,v 1.7 1999/08/03 01:54:09 janssen Exp $ 
 */
 
package xerox.ilu;

/**
 * An object implementation can help ILU by remembering 
 * internal data. <p>
 *
 * This is kind of a last resort measure when it is not possible to
 * extend a base class.  The easiest way for an application to help 
 * ilu (and therefore receive the benefits of garbage collection) is 
 * to extend one of xerox.ilu.IluObjectBase or 
 * org.omg.CORBA.portable.ObjectImpl<br>
 * 
 * Objects which do not provide this help may still be used 
 * but might not be exposed to garbage collection and
 * could be hold on permanently.<p>
 *
 * If this interface is exported, ILU calls the setIluData 
 * procedure and asks the object to remember the argument. <br>
 * Whenever ILU needs its internal data it will call getIluData.<br>
 *
 * (A paranoid implementation of this interface may take advantage 
 * of the fact that ILU calls setIluData at most once per object.) <p>
 *
 * ILU expects that the internal data is not garbage collected
 * before the ILU object itself is garbage. (This means that
 * ILU object's may NOT be "resurected".<br>
 *
 * ILU calls this from within and from outside its locks;
 * the safest way to avoid deadlock is to not call any locking 
 * operation which could interfear with locks held by ILU.<br>
 *
 *
 * @see IluObjectBase
 * @see org.omg.CORBA.portable.ObjectImpl
 * @see Ilu
 */
public interface IluDataSupport {
    
    /** Ilu calls this once when the object is created */
    public void setIluData(java.lang.Object internal);
    
    /** Ilu calls this whenever it needs to retrieve internal data */
    public java.lang.Object getIluData();
    
} //IluDataSupport
