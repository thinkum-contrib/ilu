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
/* IluEnvironment.java */
/* Chris Jacobi, December 23, 1998 12:03 pm PST */
/* $Id: IluEnvironment.java,v 1.10 1999/08/12 06:08:26 janssen Exp $ */

package xerox.ilu;

/**
 * Access of values set up by the environment. <p>
 */
public final class IluEnvironment {
    
/*===== Generic property access =====*/
    
    /**
     * Accessing environment variable;  Returns null if not
     * set or caller and key are not privileged to access 
     * environment variable. <br>
     *
     * This is NOT used for features which need to be accessed
     * before the native ilu library has been initialized. <br>
     * Standard ilu search order proposes that caller gives precedence
     * to properties found with getStringProp0 and uses getenv
     * only if getStringProp0 did return null.
     */
    public static java.lang.String getenv(java.lang.String key) {
        java.lang.String val = null;
        if (xerox.ilu.IluEnvironment.isPriviledged(2)) {
            val = xerox.ilu.IluDebug.ngetenv(key);
        }
        return val;
    } //getenv

    /**
     * Setting environment variable;  Returns False if 
     * caller and key are not privileged to access 
     * environment variable. <br>
     *
     * This is NOT used for features which need to be accessed
     * before the native ilu library has been initialized. <br>
     * Standard ilu search order proposes that caller gives precedence
     * to properties found with getStringProp0 and uses getenv
     * only if getStringProp0 did return null.
     */
    public static boolean putenv(java.lang.String key, java.lang.String val) {
        if (xerox.ilu.IluEnvironment.isPriviledged(2)) {
            return (xerox.ilu.IluDebug.nputenv(key, val) == 1);
        } else {
	    return false;
	}
    } //putenv

    /** 
     * Accesses a string from the system properties.
     * null if not defined. <br>
     *
     * (The "0" in the name forces users to think whether
     * to use getStringPropX or getStringProp0) 
     */
    public static String getStringProp0(java.lang.String key) {
        return xerox.basics.Environment.getStringProp(key);
    } //getStringProp0
    
    /** 
     * Accesses a string from the system properties or environment.
     * null if not defined. <br>
     *
     * This is NOT used for features which need to be accessed
     * before the native ilu library has been initialized.
     */
    public static String getStringPropX(java.lang.String key) {
        java.lang.String val = xerox.basics.Environment.getStringProp(key);
        if (val==null) {
            val = getenv(key);
        }
        return val;
    } //getStringPropX
    
    /** 
     * Accesses an integer from the system properties.
     * If not defined, returns defaultvalue. <br>
     * (Does not use environment variables because many clients 
     * use getIntProp in early initialization code) 
     */
    public static int getIntProp(java.lang.String key, int defaultvalue) {
        return xerox.basics.Environment.getIntProp(key, defaultvalue);
    } //getIntProp
    
    
/*===== Specific properties =====*/

    private static int threadPrio = 0;
    
    /** 
     * Priority used for most ilu threads. 
     */
    public static int threadPriority() {
        //Not inquiring environment variables because threads might 
        //be forked before the native ilu library is loaded.
        if (threadPrio<=0) {
            threadPrio = getIntProp(
                "ilu.priority", java.lang.Thread.MAX_PRIORITY-3);
        }
        return threadPrio;
    } //threadPriority
    
    
    /** 
     * Returns whether the caller "frames" down
     * is priviledged to modify ilu features.
     * 1 frame down is the direct caller of isPriviledged)
     * Priviledged means there is no class loader, or, the
     * same class loader which was used for ilu itself.
     */
    public static boolean isPriviledged(int frames) {
        //we don't do real applets yet...
        return true;
    } //isPriviledged

} // IluEnvironment
