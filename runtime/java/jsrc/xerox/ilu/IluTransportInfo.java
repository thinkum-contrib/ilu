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
/* IluTransportInfo.java */
/* Chris Jacobi, December 23, 1998 12:22 pm PST */
/* $Id: IluTransportInfo.java,v 1.15 1999/08/03 01:54:00 janssen Exp $ */
 
/* 
 * Native side together with IluPort 
 * See IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * Transport information is used to describe servers. <p>
 * @see IluServer
 * @see IluPort
 */
final public class IluTransportInfo {
    private long yIluTransportInfo = 0; 
    /*friendly*/ java.lang.String[] tinfo = null;
    private static java.lang.String[] defaultTInfo = null;
    
    /* Accessors to build the ilu-defaults */
    /*friendly*/ static native int nCountDefaultTransportInfo();
    /*friendly*/ static native java.lang.String nDefaultTInfoComponent(int n);

    /* Build corresponding native resources */
    private native void nativeInitTransportInfo(java.lang.String[] tinfo);

    /* Reclaim native resources */
    private native void nativeFinalizeTransportInfo();
    

    /** Copy an array of strings.
     * Internally used; public for conveniance.
     */
    public static java.lang.String[] copyStringArray(java.lang.String[] x) {
        java.lang.String[] cpy = new java.lang.String[x.length];
        for (int i = 0; i < cpy.length; i++) {
            cpy[i] = x[i];
        }
        return cpy;
    } //copyStringArray
   
   
    /** 
     * Create a transport tinfo.
     * null tinfo array for default. 
     */
    public IluTransportInfo(java.lang.String[] tinfo) {
        /*$ idb */  if (IluDebug.traceDefs() > 0) {
        /*$ idb */      IluDebug.log.println("! IluTransportInfo: created");
        /*$ idb */  }
        if (tinfo == null) {
            tinfo = defaultTransports();
            //Assembling the default on java side may be slower but will be 
            //more robust compared to passing a default marker. 
        } else {
            tinfo = copyStringArray(tinfo); //copy guarantees immutability
        }
        this.tinfo = tinfo;
        this.nativeInitTransportInfo(tinfo);
    } //constructor
    
    
    /* Returns new mutable copy of the default transport info */
    public static java.lang.String[] defaultTransports() {
        if (defaultTInfo == null) {
            int n = nCountDefaultTransportInfo();
            java.lang.String[] def = new java.lang.String[n];
            for (int i = 0; i < n; i++) {
                def[i] = nDefaultTInfoComponent(i);
            }
            defaultTInfo = def;
        }
        return copyStringArray(defaultTInfo); //copy guarantees immutability
    } //defaultTransports

    
    /** Accessors function */
    public java.lang.String[] getTransports() {
        return copyStringArray(tinfo);
    } //getTransports
   
   
    /**
     * Need to reclaim native resources.
     * Notice that this is protected final to prevent missuse.
     */
    protected final void finalize() throws java.lang.Throwable {
        nativeFinalizeTransportInfo();
    } //finalize
    
} //IluTransportInfo
