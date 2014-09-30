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
/* IluServiceThread.java */
/* Chris Jacobi, December 23, 1998 12:05 pm PST */
/* $Id: IluServiceThread.java,v 1.13 1999/08/03 01:54:11 janssen Exp $ */
 
package xerox.ilu;

/**
 * Subclass for certain ILU threads.<p>
 * This class is used to be able to transmit extra environment or thread
 * specific information between the ILU kernel and its application.<p>
 * 
 * Threads forked by ILU for serving skeleton methods do use this thread
 * class.
 * Applications optionally may use this thread class when requiring
 * service from ILU.  Application use is optional.
 *
 * @see java.lang.Thread
 */
 
public abstract class IluServiceThread extends java.lang.Thread {
    
    public IluServiceThread() {
        super("IluServiceThread");
    }
    
    public IluServiceThread(java.lang.String s) {
        super(s);
    } 
    
    
    /** 
     * Skeleton-features are set by ILU and queried 
     * by the implementation of skeleton methods (the callee). 
     */

    /*friendly*/ IluCall skeletonCall = null;

    /** 
     * Sets the call for a skeleton. 
     * Also cleans all stub features. 
     */
    /*friendly*/ final void setSkeleton(IluCall sc) {
        stubInitiatingPassport = null;
        stubPipeline = null;
        stubSerializationCtx = null;
        stubBatcher = null;
        skeletonCall = sc;
    }


    /** 
     * Stub-features are set by the application (the caller) and 
     * queried by ILU. 
     */

    /*friendly*/ IluPipeline stubPipeline = null;
    /*friendly*/ IluSerializationContext stubSerializationCtx = null;
    /*friendly*/ IluBatcher stubBatcher = null;
    private IluPassport stubInitiatingPassport = null;

    /** 
     * Friendly: Thou shall not get the passport of a
     * thread which isn't yours. 
     */
    /*friendly*/ final IluPassport getInitiatingPassport() {
        return stubInitiatingPassport;
    } //getInitiatingPassport


    /** 
     * Gets the IluPipeline, if one is specified.
     */
    public final IluPipeline getPipeline() {
        return stubPipeline;
    } //getPipeline


    /** 
     * Gets the IluBatcher, if one is specified.
     */
    public final IluBatcher getBatcher() {
        return stubBatcher;
    } //getBatcher


    /** 
     * Throws the appropriate exception.
     */
    public static final void badThread() {
        throw new org.omg.CORBA.BAD_CONTEXT("Called from a non-ILU thread");
    } //badThread


    /** 
     * Set the passport used when initiating ILU calls 
     * from a surrogate. This method has been made static
     * for security so that a thread can only set its own 
     * stubInitiatingPassport
     * @see IluPassport
     */
    public static final void setInitiatingPassport(IluPassport p) {
        java.lang.Thread thread = java.lang.Thread.currentThread();
        if (thread instanceof IluServiceThread) {
            ((IluServiceThread)thread).stubInitiatingPassport = p;
        } else if (p != null) {
            badThread();
        }
    } //setInitiatingPassport

    
    /** 
     * Sets the IluPipeline used when doing ILU calls
     * from this thread. <p> 
     * @see IluPipeline
     */
    public static final void setCurrentPipeline(IluPipeline p) {
        java.lang.Thread thread = java.lang.Thread.currentThread();
        if (thread instanceof IluServiceThread) {
            ((IluServiceThread)thread).stubPipeline = p;
        } else if (p != null) {
            badThread();
        }
    } //setCurrentPipeline


    /** 
     * Sets the IluBatcher context used when doing ILU calls
     * from this thread. <p> 
     * @see IluBatcher
     */
    public static final void setCurrentBatcher(IluBatcher b) {
        java.lang.Thread thread = java.lang.Thread.currentThread();
        if (thread instanceof IluServiceThread) {
            ((IluServiceThread)thread).stubBatcher = b;
        } else if (b != null) {
            badThread();
        }
    } //setCurrentBatcher


    /** 
     * Sets the IluSerializationContext used when doing ILU calls
     * from this thread. <p> 
     * @see IluSerializationContext
     */
    public static final 
    void setCurrentSerializationContext(IluSerializationContext s) {
        java.lang.Thread thread = java.lang.Thread.currentThread();
        if (thread instanceof IluServiceThread) {
            ((IluServiceThread)thread).stubSerializationCtx = s;
        } else if (s != null) {
            badThread();
        }
    } //setCurrentSerializationContext


    /** 
     * Set the passport of an explicit thread.
     * Don't increase priviledges of another thread.
     * Not worried about simple denial of service.
     */
    public final void setStubPassport(IluPassport p) {
        //Everybody may reduce priviledges; 
        //test on increases only
        if (p != null) {
            if (java.lang.Thread.currentThread() != this) {
                throw new java.lang.SecurityException();
            }
        }
    } //setStubPassport


    /** 
     * Reset all stub features. <p>
     * 
     * Clears initiating Passport, Pipeline and SerializationContext, <p>
     * 
     * Security concern: This can be used from any thread,
     * as the risk is only a denial of service attack.<p>
     *
     * @see IluPassport#setInitiatingPassport
     * @see IluPipeline#setCurrentPipeline
     * @see IluSerializationContext#setCurrentSerializationContext
     */
    public final void cleanAllStubFeatures() {
        stubInitiatingPassport = null;
        stubPipeline = null;
        stubSerializationCtx = null;
        stubBatcher = null;
    } //cleanAllStubFeatures
    

    /**
     * This construct prevents subclasses from implementing cloning.<p>
     * @see     java.lang.Cloneable
     */
    protected final java.lang.Object clone() 
        throws java.lang.CloneNotSupportedException
    {
        throw new java.lang.CloneNotSupportedException();
    } //clone
    

} //IluServiceThread



