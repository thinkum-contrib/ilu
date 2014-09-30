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
/* IluPipeline.java */
/* Chris Jacobi, December 23, 1998 12:15 pm PST */
/* 
 * $Id: IluPipeline.java,v 1.7 1999/08/03 01:53:40 janssen Exp $ 
 */


/* 
 * Representation for ilu_IluPipeline. <p>
 * Native code in IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * A client uses an IluPipeline to let ILU know it can
 * safely pipeline certain calls down a serial (i.e.,
 * non-concurrent) connection.  Clients can attach the 
 * IluPipeline to the thread (if it is a IluServiceThread) 
 * for calls that can be pipelined.  <p> 
 * A given serial connection can have multiple calls outstanding only
 * if they are all associated with the same (non-null) IluPipeline
 * (remember that absent pipelining, ILU will do concurrent calls
 * over a serial protocol by opening multiple connections).  Multiple
 * connections, even of different servers, can have outstanding
 * calls associated with the same IluPipeline.<p>
 * 
 * @see IluSerializationContext
 */
public final class IluPipeline {

    /*friendly*/ long yIluPipeline = 0;
    private native void nInitPipeline();
    private native void nFinalizePipeline();
    
    public IluPipeline() {
        this.nInitPipeline();
    } //constructor
    
    /** 
     * Allocates an IluPipeline.
     */
    public static IluPipeline alloc() {
        return new IluPipeline();
    } //alloc
    
    /**
     * Not available to the general public.
     * (protected final to make sure)
     */
    protected final void finalize () throws java.lang.Throwable {
        this.nFinalizePipeline(); 
    } //finalize
    

    /** 
     * Sets the IluPipeline used for doing ILU calls
     * from this thread. <p>
     *
     * Use null IluPipeline to clear. <br>
     * Throws an exception if the current thread is not an IluServiceThread.
     *
     * @see IluServiceThread
     * @see IluServiceThread#cleanAllStubFeatures
     */
    public static void setCurrentPipeline(IluPipeline pipeline) {
        IluServiceThread.setCurrentPipeline(pipeline);
    } //setCurrentPipeline
    

} //IluPipeline
