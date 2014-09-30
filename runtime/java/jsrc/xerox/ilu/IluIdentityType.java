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
/* IluIdentityType.java */
/* Chris Jacobi, December 23, 1998 12:07 pm PST */
/* 
 * $Id: IluIdentityType.java,v 1.10 1999/08/03 01:53:52 janssen Exp $ 
 */


/* 
 * Representation for ilu_IdentityType <p>
 * See IluJava_IluPort.c
 */


package xerox.ilu;

/**
 * An IluIdentityType represents a "kind" or "class" of identities.<p> 
 * 
 * There are a few standard ilu IluIdentityType and we might define   
 * a meta interface to generate IluIdentityType's from java. At the
 * current time however new IluIdentityType's can not be created in
 * java.<p>
 *
 * This class deals with the creation of IluIdentity, but the mechanism
 * is still somewhat in flux.
 *
 * @see IluIdentity
 * @see IluPassport
 * @see IluGssCred
 */
public class IluIdentityType extends IluWPBase {
    private static java.util.Hashtable gtable = new java.util.Hashtable();
    /*friendly*/ long yIluIdentityType = 0;
    /*friendly*/ java.lang.String name = null;
    
    /**
     * Disables unauthorized creation or subclassing
     */
    /*friendly*/ IluIdentityType() {
    } //constructor
    
    /**
     * Returns the name of the IluIdentityType
     */
    public final java.lang.String getName() {
        return name; 
    } //getName
    
    /**
     * Finds the IluIdentityType with a given name
     */
    public final static IluIdentityType fromName(java.lang.String name) {
        java.lang.Object x = gtable.get(name);
        if (x==null) {
                throw new org.omg.CORBA.NO_PERMISSION("not found");
        } 
        return (IluIdentityType) x;
    } //fromName
    
    /**
     * Step in conversion of IluIdentity to String<p>
     * This can be subclassed, but the subclasses are likely
     * to need to call super.identityToStringPart...
     */
    protected java.lang.String identityToStringPart(IluIdentity ii) {
        if (ii.it != this) {
            throw new org.omg.CORBA.NO_PERMISSION("bad IluIdentity type");
        }
        return ii.nId2String();
    } //identityToStringPart

     
    /*friendly*/ native int nRegStdIdentityType(int key);
    
    /**
     * Initialization for a few standard IluIdentityType-s
     */
    /*friendly*/ static IluIdentityType createStandardIdtType(
            java.lang.String name, int key
            )
    {
        IluIdentityType it = new IluIdentityType();
        it.name = name;
        synchronized (gtable) {
            if (gtable.get(name) != null) {
                throw new org.omg.CORBA.UNKNOWN("duplicate");
            }
            if (it.nRegStdIdentityType(key)<0) {
                throw new org.omg.CORBA.UNKNOWN(
                    "bad standardIluIdentityType"
                    );
            }
            gtable.put(name, it);
        }
        return it;
    } //createStandardIdtType;
    
    
    /**
     * Create an IluIdentity of this type, if implemented.
     * We expect this to be sub-classed 
     */
    public IluIdentity createIluIdentity()
    {
        throw new org.omg.CORBA.NO_IMPLEMENT("not impl");
    } //createIluIdentity;
   
    
    private static IluIdentityType connectionIdentity_Type = null;
    private static IluIdentityType noIdentity_Type = null;
    private static IluIdentityType gssIdentity_Type = null;
    private static IluIdentityType sunRPCAuthUnix_Type = null;
    
    /**
     * Returns the IluIdentityType used for connectionIdentity<p>
     */
    public static IluIdentityType getConnectionIdentityType() {
        return connectionIdentity_Type;
    } //getConnectionIdentityType

    /**
     * Returns the IluIdentityType used for gssIdentity<p>
     */
    public static IluIdentityType getGSSIdentityType() {
        return gssIdentity_Type;
    } //getNoIdentityType
    
    
    /**
     * Returns the IluIdentityType used for noIdentity<p>
     */
    public static IluIdentityType getNoIdentityType() {
        return noIdentity_Type;
    } //getNoIdentityType
    
    
    /**
     * Returns the IluIdentityType used for sunRPCAuthUnix<p>
     */
    public static IluIdentityType getSunRPCAuthUnixIdentityType() {
        return sunRPCAuthUnix_Type;
    } //getNoIdentityType
    
    
    static {
        IluInit.init();
        xerox.basics.VMExtras.makeGCRoot(gtable);
    } //static
    
    public static void init() {
    } //init
    
    public static void initPhase3() {
        if (noIdentity_Type == null) {
            noIdentity_Type = 
                createStandardIdtType("NoIdentity", 0);
            connectionIdentity_Type = 
                createStandardIdtType("ConnectionIdentity", 1);
            gssIdentity_Type = 
                createStandardIdtType("GSSIdentity", 2);
            sunRPCAuthUnix_Type = 
                createStandardIdtType("UNIXIdentity", 3);
        }
    } //initPhase3
    
    
} //IluIdentityType


