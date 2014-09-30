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
/* IluObject.java */
/* Chris Jacobi, November 12, 1998 9:29 pm PST */
 
/* $Id: IluObject.java,v 1.14 1999/08/03 01:54:09 janssen Exp $ */
 

package xerox.ilu;

/**
 * Generic, client visible marker interface. <p>
 *
 * All ilu objects need to provide this interface.
 * This interface includes non corba compatible ilu-ness.<p>
 *
 * This interface does not require Ilu to be loaded and initialized.
 * ??? NOT TRUE ANYMORE.
 *
 * @see org.omg.CORBA.Object
 */
public interface IluObject extends org.omg.CORBA.Object { 
} //IluObject
