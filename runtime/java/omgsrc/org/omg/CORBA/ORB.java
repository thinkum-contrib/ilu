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
/* ORB.java */
/* Chris Jacobi, November 16, 1998 6:34 pm PST */
/* $Id: ORB.java,v 1.19 1999/08/03 01:55:02 janssen Exp $ */

 
package org.omg.CORBA;

/**
 * Makes Ilu look like it were a CORBA ORB<p>
 * The purpose of this module is to be CORBA compatible; it will change
 * whenever necessary.
 *
 * It does not yet have the exact type required by CORBA.
 */
public abstract class ORB {
    
    private static ORB getIluOrb() {
        try {
            String className = "xerox.ilu.IluORB";
            java.lang.Class orbClz = Class.forName(className);
            return (ORB) orbClz.newInstance();
        } catch (java.lang.Exception ex) {
            System.err.println("can't instantiate Ilu ORB");
            ex.printStackTrace(System.err);
            throw new org.omg.CORBA.INITIALIZE(
                "can't instantiate Ilu ORB"
                );
        }
    } //getIluOrb

    
    public static ORB init() {
        return getIluOrb();
    } //init

    /** 
     * Standard, application initialization 
     */
    public static ORB init(
        java.lang.String[] args, java.util.Properties props)
    {
        ORB orb = getIluOrb();
        orb.set_parameters(args, props);
        return orb;
    } //init
    
    
    /** 
     * Standard, applet initialization; 
     * Applet support in ilu is limited, but not checked
     * at this point. 
     */
    public static ORB init(
        java.applet.Applet app, java.util.Properties props)
    {
        ORB orb = getIluOrb();
        orb.set_parameters(app, props);
        return orb;
    } //init
    
    abstract protected void set_parameters(String[] args, java.util.Properties props);
    
    abstract protected void set_parameters(java.applet.Applet app, java.util.Properties props);

    abstract public org.omg.CORBA.Object string_to_object(java.lang.String s);

    abstract public java.lang.String object_to_string(org.omg.CORBA.Object ob);
    
    public BOA BOA_init() {
        return new BOA(this);
    } //BOA_init
    
    public BOA BOA_init(java.lang.String boaType, java.util.Properties properties) {
        return new BOA(this);
    } //BOA_init
    
    
    /**
     * Resolve an initial reference (bootstrap the naming service).
     * Argument is the name of an initial service.  (Well it is 
     * a property key). This key will be used for a property look up 
     * and the found string is used as an sbh used for the initial service
     * object which then will be looked up using ilu.
     *
     * If the found "sbh" starts with "file:" an additional
     * look up using the file system is performed.  This is usefull
     * to deal with foreign name services which can write the "root" 
     * name service object into a file. 
     *
     * @param serviceKey The name of an initial service
     * @return The ilu object refered to by the initial service.
     * Missing declaration of corba exceptions... 
     */
    public abstract org.omg.CORBA.Object     
        resolve_initial_references(java.lang.String name) 
        throws org.omg.CORBA.ORBPackage.InvalidName;
    
    abstract public void connect(org.omg.CORBA.Object obj);    
    abstract public void disconnect(org.omg.CORBA.Object obj);
    
} // ORB

