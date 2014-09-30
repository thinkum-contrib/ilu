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
/* IluBatcher.java */
/* Chris Jacobi, December 23, 1998 11:50 am PST */

/* 
 * $Id: IluBatcher.java,v 1.7 1999/08/03 01:54:08 janssen Exp $ 
 */

/* 
 * Implements ilu_Batcher. <p>
 * Native code will be in IluJava_IluPort.c
 */



package xerox.ilu;


/**
 * An IluBatcher represents a batching scope.  Calls made within a
 * batching scope are batched by the ILU runtime.  The `pushBatcher' 
 * operation affects all the calls in a given batching scope.
 */
public final class IluBatcher {
    
    private long yBatcher = 0;
    private boolean pushable;
    private int millis;

    private native void nInitBatcher(int millis, boolean pushable);
    private native void nFinalizeBatcher();
    private native void nPushBatcher();
    

    /** 
     * Private; Use alloc to create 
     */  
    private IluBatcher(int millis, boolean pushable) {
        this.pushable = pushable;
        this.millis = millis;
        this.nInitBatcher(millis, pushable);
    } //constructor


    /** 
     * Accessor
     */  
    public int getTimeout() {
        return this.millis;
    } //getTimeout
    

    /** 
     * Accessor
     */  
    public boolean getPushable() {
        return this.pushable;
    } //getPushable


    /** 
     * Creates a new batching scope.<p>  
     * (timeout > 0) or (pushable) should be true..
     */
    public static IluBatcher alloc(int millis, boolean pushable) {
        if (millis < 0) {
            throw new org.omg.CORBA.BAD_PARAM("bad timeout for batcher");
        }
        if (!pushable && millis==0) {
            throw new org.omg.CORBA.BAD_PARAM("needs timeout or pushable");
        }
        return new IluBatcher(millis, pushable);
    } //alloc
    
    
    /** 
     * Sets the batching scope used for doing ILU calls
     * from the current thread. <p>
     *
     * Use null IluBatcher to clear. <br>
     * Throws an exception if the current thread is not an IluServiceThread.<br>
     *
     * @see IluServiceThread
     * @see IluServiceThread#cleanAllStubFeatures
     */
    public static void setCurrentBatcher(
            IluBatcher batcher
            ) {
        IluServiceThread.setCurrentBatcher(batcher);
    } //setCurrentBatcher

    
    /** 
     * Ensures that the requests of all associated calls since
     * the last call on pushBatcher will eventually be completely
     * transmitted to their servers (barring various failures), if
     * the IluBatcher is pushable.
     * There is no guarantee of how much progress the transmissions 
     * have made by the time this procedure returns.
     * No-op if not pushable, or, no calls have been made.
     */
    public void pushBatcher() {
        if (this.pushable) {this.nPushBatcher();}
    } //pushBatcher
        
        
    /**
     * Not available to the general public by means of "protected final".
     */
    protected final void finalize() throws java.lang.Throwable {
        this.nFinalizeBatcher(); 
    } //finalize
    
        
} //IluBatcher
