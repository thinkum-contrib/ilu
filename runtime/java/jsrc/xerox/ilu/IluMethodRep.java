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
/* IluMethodRep.java */
/* Chris Jacobi, December 23, 1998 12:11 pm PST */
/* $Id: IluMethodRep.java,v 1.18 1999/08/03 01:53:53 janssen Exp $ */
 
/*
 * The registration of the method in the kernel is made
 * in IluJava_IluClassRep.c
 */

package xerox.ilu;

/**
 * Definition of a method.<p>
 * Used by stubs; unlikely to be useful for applications.<p>
 * @see IluClassRep
 * @see IluExceptionRep
 */
public final class IluMethodRep  extends IluWPBase {
    private long yIluMethod = 0;
    /*friends*/ xerox.ilu.IluClassRep jjClassRep;
    public int methodIdx; //accessed by stub
    /*friends*/ java.lang.String jjIluMethodName;
    /*friends*/ int id;
    /*friends*/ boolean cacheable;
    /*friends*/ boolean asynchronous;
    /*friends*/ xerox.ilu.IluExceptionRep[] jjExceptions;
    /*friends*/ xerox.ilu.IluMethodArgRep[] jjArgs;
    /*friends*/ int argCnt;
    /*friends*/ java.lang.String jjRetValUid;
    /*friends*/ xerox.ilu.IluSkeleton skeleton;
    	/* Implementation trick: IluMethodRep is stored
    	 * in the ilu_StubProc field of the _ilu_Method_s.
    	 * Requirement: IluMethodRep must not be garbage collected
    	 * met by stuffing the  IluMethodRep into the IluClassRep
    	 */
    
    /** 
     * Disable unspecified methods (reduces creation of bogus instances) 
     */
    private IluMethodRep() {
        super();
    }

    /** 
     * Stub visible creation of an IluMethodRep, except it doesn't yet 
     * perform any ilu kernel call 
     */
    public static IluMethodRep registerMethod (
            xerox.ilu.IluClassRep classRep,
            int methodIdx, 		//Internal indexing
            java.lang.String iluMethodName,
            int id, 			//This is passed through the wire
            boolean cacheable,
            boolean asynchronous,
            xerox.ilu.IluExceptionRep[] exceptions,
            int argCnt,
            java.lang.String retValUid,
            xerox.ilu.IluSkeleton skelet
            ) {
        if (skelet==null || classRep==null) {
            throw new java.lang.RuntimeException("missing arguments");
        }
        if (classRep.jjMethods[methodIdx]!=null) {
            throw new java.lang.RuntimeException("ilu method redefined");
        }
        IluMethodRep m = new IluMethodRep();
        m.jjClassRep = classRep;
        m.methodIdx = methodIdx;
        m.jjIluMethodName = iluMethodName;
        m.id = id;
        m.cacheable = cacheable;
        m.asynchronous = asynchronous;
        //canonicalize exceptions (used by native code) 
        if (exceptions!=null) {
            if (exceptions.length==0) exceptions = null;
        }
        m.jjExceptions = exceptions;
        m.argCnt = argCnt;
        m.jjArgs = new IluMethodArgRep[argCnt];
        m.jjRetValUid = retValUid;
        m.skeleton = skelet;
        classRep.jjMethods[methodIdx] = m;
        return m;
    } //registerMethod
    
    /** 
     * Stub visible definition of method arguments.  
     * All ilu kernel actions are delayed 
     */
    public void defineArg(
            int argIdx,
            java.lang.String argName,
            boolean sibling, 
            int argDirection, 
            java.lang.String typeUid
            ) {
        if (yIluMethod!=0) {
           throw new java.lang.RuntimeException("method is already immutable");
        }
        IluMethodArgRep a = new IluMethodArgRep();
        a.argName = argName;
        a.sibling = sibling;
        a.argDirection = argDirection;
        a.typeUid = typeUid;
        this.jjArgs[argIdx] = a;
    } //defineArg
    
    
    /** useful for debugging */
    public java.lang.String toString() {
       return super.toString() + ":" + this.jjIluMethodName;
    } //toString
    
} //IluMethodRep
