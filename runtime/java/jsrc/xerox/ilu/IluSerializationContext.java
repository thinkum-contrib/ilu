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
/* IluSerializationContext.java */
/* Chris Jacobi, January 6, 1999 5:11 pm PST */
/* 
 * $Id: IluSerializationContext.java,v 1.10 1999/08/03 01:53:40 janssen Exp $ 
 */


/* 
 * Representation for ilu_IluSerializationContext. <p>
 * Native code in IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * An IluSerializationContext represents an instance of the 
 * serialization guarantee.  An instance is with respect to a 
 * particular server and set of calls.  The guarantee is that 
 * the server application code receives calls in the same order 
 * as the client application code makes them, except that calls  
 * that return after a barrier call may have started service  
 * before calls that return before the same barrier call.  A 
 * barrier call is one that raises a barrier exception. Remember 
 * that ASYNCHRONOUS calls do return, they just do so particularly 
 * quickly. <br>
 * Two calls are considered to have been issued concurrently if  
 * each one's ilu_FullStartCall is initiated before the other's 
 * ilu_FinishCall returns. <br>
 * The client may issue concurrent calls with the same 
 * IluSerializationContext, and ILU will put them in some serial 
 * order. <br>
 *
 * Note that for two concurrently issued calls, either:
 * (a) the one put first is ASYNCHRONOUS, (b) they both are in the 
 * same IluPipeline, or (c) the one put second is delayed until the  
 * one put first returns. <p>
 *
 * The way for a client to associate an IluSerializationContext is
 * to attach the IluSerializationContext to the thread.  This works 
 * only from IluServiceThread-s. <p> 
 * 
 * @see IluPipeline
 * @see IluServiceThread
 */
public final class IluSerializationContext {
    
    private long ySerializationContext = 0;
    private long ykServer = 0;
    private long yClass = 0;
    private IluServer server = null;
    private native void nInitSerializationContext(IluServer server, IluOInt oi);
    private native void nFinalizeSerializationContext();
    
    private IluSerializationContext() {
    } //constructor

    /**
     * Not available to the general public.
     * (protected final to make sure)
     */
    protected final void finalize() throws java.lang.Throwable {
        this.nFinalizeSerializationContext(); 
    } //finalize

    /** 
     * Constructs an IluSerializationContext as used for shortcut.<p>
     */
    private IluSerializationContext(IluOInt oi) {
        this.server = null;
        this.nInitSerializationContext(null, oi);
    } //constructor


    /** 
     * Constructs an IluSerializationContext.<p>
     * IluServer must be surrogate server.
     * In the current ilu release these are hard to get...
     */
    public IluSerializationContext(IluServer server) {
        if (server == null || ! server.isSurrogateServer()){
            throw new xerox.ilu.IluSystemExceptionBase(
                "surrogate servers only"
                );
        }
        this.server = server;
        this.nInitSerializationContext(server, null);
    } //constructor


    /** 
     * Allocates an IluSerializationContext.<p>
     * IluServer must be surrogate server.
     * In the current ilu release these are hard to get...
     */
    public static IluSerializationContext alloc(IluServer server) {
        IluSerializationContext serializationCtx = 
            new IluSerializationContext(server);
        return serializationCtx;
    } //alloc
    
    /** 
     * Allocates an IluSerializationContext.<p>
     *
     * Shortcut for allocating a IluSerializationContext using
     * the server of a surrogate object.  <p>
     *
     * This is more efficient then re-ifying the IluServer for a 
     * surrogate object  (Which at the current release is
     * not yet supported anyway).<p>
     *
     * @see IluRT0#objectsAreSiblings
     */
    public static IluSerializationContext alloc(java.lang.Object obj) {
        if (! IluRT0.isSurrogate(obj)) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "surrogate objects only"
                );
        }
        IluOInt oi = IluOInt.peek(obj);
        if (oi == null) {
            throw new xerox.ilu.IluSystemExceptionBase(
                "not a working ilu object"
                );
        }
        IluSerializationContext serializationCtx = 
            new IluSerializationContext(oi);
        return serializationCtx;
    } //alloc
    
    
    /** 
     * Sets the IluSerializationContext used for doing ILU calls
     * from this thread. <p>
     *
     * Use null IluSerializationContext to clear. <br>
     * Throws an exception if the current thread is not an IluServiceThread.<br>
     *
     * @see IluServiceThread
     * @see IluServiceThread#cleanAllStubFeatures
     */
    public static void setCurrentSerializationContext(
            IluSerializationContext serializationCtx
            ) {
        IluServiceThread.setCurrentSerializationContext(serializationCtx);
    } //setCurrentSerializationContext
        
} //IluSerializationContext
