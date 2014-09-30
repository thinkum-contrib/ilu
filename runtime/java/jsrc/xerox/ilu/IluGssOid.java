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
/* IluGssOid.java */
/* Chris Jacobi, December 23, 1998 12:07 pm PST */
/* 
 * $Id: IluGssOid.java,v 1.8 1999/08/03 01:53:50 janssen Exp $ 
 */


package xerox.ilu;

/**
 * Representation for gssapi::gss_OID
 *
 * Sort of heavy-weight: OID's are cached in the assumption that only
 * very few actual values will ever be used. <p>
 *
 * WARNING: This class is only implemented if ilu is configured
 * enabling secure transports. Check using xerox.ilu.IluRT0.hasSecurity().
 */
public final class IluGssOid extends IluWPBase {
    long yIluGssOid = 0;
    private static java.util.Hashtable gtable = new java.util.Hashtable();
    static {
        IluInit.init();
    }

    /**
     * Disable arbitrary creations because object is NOT VALID unless
     * native initialization is performed.
     */
    /*friendly*/ IluGssOid() {
    } //constructor
    
    
    /*friendly*/ native void nIluGssOidInit(java.lang.String s);
    
    /**
     * Parse a string to compute a gss_OID.
     */
    public static IluGssOid fromString(java.lang.String s) {
        IluGssOid oid = null;
        synchronized (gtable) {
            oid = fromString_or_null(s);
            if (oid != null) {return oid;}
            oid = new IluGssOid();
            oid.nIluGssOidInit(s);
            if (oid.yIluGssOid == 0) {
                //Not likely as it should rather throw an exception
                //then not allocate a value
                throw new org.omg.CORBA.IMP_LIMIT("bad oid");
            } else {
                //And it obviously didn't throw an exception either...
                gtable.put(s, oid);
            }
        }
        return oid;
    } //fromString
    
    
    /**
     * Parse a string to get a gss_OID if it is already known
     */
    public static IluGssOid fromString_or_null(java.lang.String s) {
        IluGssOid oid = (IluGssOid) gtable.get(s);
        return oid;
    } //fromString_or_null
    
    
    static {
        xerox.basics.VMExtras.makeGCRoot(gtable);
    }
    
} //IluGssOid

