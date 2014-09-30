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
/* Environment.java */
/* Chris Jacobi, June 22, 1998 5:54 pm PDT */

/*
 */
 
/* $Id: Environment.java,v 1.8 1999/08/03 01:53:36 janssen Exp $ */

package xerox.basics;
import java.lang.System;
import java.lang.Boolean;
import java.lang.Integer;
import java.lang.NumberFormatException;

/**
 * Access of values set up through the environment. <p>
 */
public class Environment {

/*===== Overloading mechanism (usefull for applets) =====*/

    private static Environment theEnvironment = null;
    protected static void setEnvironment(Environment e) {
        if (theEnvironment==null) theEnvironment = e;
    }
    
    /** 
     * Default implementation for getStringProp.
     */
    protected static String systemGetStringProp(String key) {
        String s;
        try {
            s = System.getProperty(key);
        } catch (SecurityException se) {
            s = null;
        }
        return s;
    }
    
    /** 
     * To re-implement getStringProp, overload this method and
     * register by calling setEnvironment.
     */
    protected String theStringProp(String key) {
        return systemGetStringProp(key);
    }
    
    static {
        try { 
            Class self = Class.forName("xerox.basics.Environment");
            if (self.getClassLoader()==null) {
                Class.forName("xerox.basics.BaseEnvironment");
            } else {
                Class.forName("xerox.basics.OtherEnvironment");
            }
        } catch (Exception e) {
            //its ok; for most applications this class nneds not  
            //been overloaded. (but when doing applets it might) 
        }
    }
    
/*===== Generic property access =====*/
    
    /** 
     * Accesses a string from some system properties.
     * null if not defined
     */
    public static String getStringProp(String key) {
        if (theEnvironment==null) {
            return systemGetStringProp(key);
        } else {
            return theEnvironment.theStringProp(key);
        }
    } //getStringProp
    
    /** 
     * Accesses an integer from some system properties.
     * If not defined, returns defaultvalue
     */
    public static int getIntProp(String key, int defaultvalue) {
        int i = defaultvalue;
        String s = getStringProp(key);
        if (s != null) {
            try {
                i = Integer.valueOf(s).intValue();
            } catch (NumberFormatException se) {
                i = defaultvalue;
            }
        }
        return i;
    } //getIntProp
    
    /** 
     * Accesses a boolean from some system properties.
     * If not defined, returns defaultvalue
     */
    public static boolean getBooleanProp(String key, boolean defaultvalue) {
        boolean b = defaultvalue;
        String s = getStringProp(key);
        if (s != null) {
            b = Boolean.valueOf(s).booleanValue();
        }
        return b;
    } //getBooleanProp

    
/*===== Loading extra classes =====*/
    
    /** 
     * Returns substring to the left of the first occurrence of the separator.
     * (Returns the input string if there is no separator)
     */
    public static String leftOfSeparator(String s, char separator) {
        if (s.length()<=0) return null;
        int sepIdx = s.indexOf(separator);
        if (sepIdx >= 0) {
            return s.substring(0, sepIdx);
        } else {
            return s;
        }
    } //leftOfSeparator
    
    
    /** 
     * Returns substring to the right of the first occurrence of the separator.
     * (Returns null if there is no separator)
     */
    public static String rightOfSeparator(String s, char separator) {
        int len = s.length();
        if (len <= 0) return null;
        int sepIdx = s.indexOf(separator);
        if (sepIdx >= 0) {
            if ((sepIdx+1) < len) {
                return s.substring(sepIdx+1);
            } else {
                return null;
            }
        } else {
            return null;
        }
    } //rightOfSeparator
    
    
    /* Used to prevent garbage collection of classes */
    private static java.util.Hashtable classTable = new java.util.Hashtable();
    
    /** 
     * Loads extra classes...
     * Classes to load are specified as colon separated list.
     */
     public static void loadClasses(String rest) {
        while (rest != null) {
            String piece = leftOfSeparator(rest, ':');
            // Load one class denoted by "piece"
            if (piece != null & piece.length() > 0) {
                try {
                    Class c = Class.forName(piece);
                    synchronized (classTable) {
                        classTable.put(c, c);
                    }
                } catch (java.lang.ClassNotFoundException e) {
                    //Convert the exception into an error, as this
                    //method is not supposed to raise exceptions.
                    //This isn't exactly a NoClassDefFoundError but that
                    //is a surprisingly good match.
                    System.err.println("failed: ");
                    e.printStackTrace(System.err);
                    throw new java.lang.NoClassDefFoundError(
                        "Couldn't load " + piece + " " + e
                        );
                }
            }
            rest = rightOfSeparator(rest, ':');
        }
    } //loadClasses

    /* Classes listed on the xerox.basics.Environment.load property
     * are loaded at startup.
     * WARNING: Initialization order can be problematic; you might
     * rather use an application specific version of this.
     */
    static {
        loadClasses(getStringProp("xerox.basics.Environment.load"));
        VMExtras.makeGCRoot(classTable);
    } //static
    
} // Environment

