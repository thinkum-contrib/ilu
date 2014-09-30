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
/* japp.java  */
/* Chris Jacobi, November 27, 1998 7:32 am PST */
/* $Id: japp.java,v 1.9 1999/08/03 01:54:14 janssen Exp $ */

package xerox.ilutools;


/** 
 * simple JAva Pre-Processor <p>
 *
 * The need for a pre processor emerges only in programming 
 * languages not powerful enough to express what needs
 * to be expressed... that means Java<p>
 *
 * This preprocessor is only used for providing simple 
 * if-defs.  We want to be able to provide source variants for 
 * say jdk-1.1 versus jdk-1.0 environments.  We might also use 
 * this for changing what package is imported to accomodate
 * other environmental changes we can not express in
 * Java. For example, want to accomodate different environments 
 * for netscape versus for jdk applications.  <p>
 *
 * The usage should be easy and even casual users should   
 * be able to understand how to fix "make" or "Imake"
 * files.  To make usage robust, we must guarantee that
 * users will keep all source information and won't loose
 * the source by editing a wrong file.  
 * This could be accomplished by prominent documentation   
 * of derived files as not being the source. But japp
 * uses a more robust mechanism.<p>
 *
 * The way chosen for japp is simple:  It makes the 
 * source and the output "similar".  This means, the 
 * output of the operation contains all the information 
 * and could be used to replace the source.  <br>
 * 1) All the pre-processing directives need to be still 
 * present in the generated java, so we can go back. <br>
 * 2) All removed or innactive source code needs to be
 * kept available (though, innactive) in the generated java.<p>
 *
 * japp doesn't change line numbers: debugging stays possible
 * even if different pre-processor options were used.<p>
 *
 *
 * Directives<br>
 *     "Meta comment"
 *     To avoiding terminating this documentation-comments these
 *     instructions use a "+" sign to denote actual use of "*"
 *     in real applications. <p>
 * Directives (in the source file) look like <pre> 
 * /+$ key +/  
 * /+$ key +/// 
 * </pre> and they guide the two following characters to be either
 * two spaces or two slashes depending on last used arguments. <p>
 *
 * <pre>
 * Use is
 * java xerox.ilutools.japp {-option key} {path}*  
 * where the "-option" is
 * -v   	make remainder of key'ed lines visible
 * -c   	make remainder of key'ed lines comment
 * -kill	remove complete key'ed lines from file
 * -perm 	remove key'ed directives (but keep key'ed lines)
 * </pre>
 */
 
public class japp {
    
    public static void usage(String s) {
        if (s !=null) {
            System.err.println(s);
        }
        System.err.print("use: java xerox.ilutools.japp ");
        System.err.println("{[-v arg][-c arg][-kill arg][-perm arg]} path");
        System.exit(1);
    } //usage
        
    static final Integer makeVisible = new Integer(1);
    static final Integer makeComment = new Integer(2);
    static final Integer makeDisappear = new Integer(3);
    static final Integer makePermanent = new Integer(4);
    
    static final String startSpecial = "/" + "*$";
    static final String stopSpecial = "*" + "/";
    static final int startLength = startSpecial.length();
    static final int stopLength = stopSpecial.length();
    
    static String transformLine(String s, java.util.Hashtable h, int lineNo) {
        int initPos = s.indexOf(startSpecial);
        if (initPos < 0) {return s;}
        int stopStart = s.indexOf(stopSpecial, startLength);
        if (stopStart < 0) {return s;} //comment doesn't finish on this line
        if (stopStart+startLength<=initPos) {return s;} //key is empty
        String key = s.substring(initPos + startLength, stopStart).trim();
        int switchIdx = stopStart+stopLength; //first pos after special comment
        if ((switchIdx+2) >= s.length()) {
            //no space for switching characters in line
            System.err.println("bad special marker on line "+lineNo);
            return s;
        }
        char c1 = s.charAt(switchIdx);
        char c2 = s.charAt(switchIdx+1);
        if ((c1 != '/') && (c1 != ' ')) {
            //first switch character is not as expected
            System.err.println("bad special marker on line "+lineNo);
            return s;
        }
        if ((c2 != '/') && (c2 != ' ')) {
            //second switch character is not as expected
            System.err.println("bad special marker on line "+lineNo);
            return s;
        }
        Object op = h.get(key);
        if (op == null) {
            //no action for this key
            return s;
        }
        if (op == makeVisible) {
            if ((c1 != ' ') || (c2 != ' ')) {
                s = s.substring(0, switchIdx) 
                       + "  " 
                       + s.substring(switchIdx+2);
            }
            return s;
        }
        if (op == makeComment) {
            if ((c1 != '/') || (c2 != '/')) {
                s = s.substring(0, switchIdx) 
                       + "//" 
                       + s.substring(switchIdx+2);
            }
            return s;
        }
        if (op == makeDisappear) {
            //the whole line disappears
            return null;
        }
        if (op == makePermanent) {
            //only the right of the special marker stays
            s = s.substring(switchIdx+2);
        }
        throw new RuntimeException("bad operation");
    } //transformLine
    
    static boolean transformVector(java.util.Vector v, java.util.Hashtable h) {
        boolean change = false;
        int sz = v.size();
        int position = 0;
        for (int i = 0; i < sz; i++) {
            String line = (String) v.elementAt(position);
            String newline = transformLine(line, h, i+1);
            if (newline == line) {
                position++;
            } else {
                change = true;
                if (newline == null) {
                    v.removeElementAt(position);
                } else {
                    v.setElementAt(newline, position);
                    position++;
                }
            }
        }
        return change;
    } //transformVector
    
    static java.util.Vector readLineVector(String s) {
        java.util.Vector v = new java.util.Vector();
        try {
            /*$ 1.1 *///java.io.FileReader freader = new java.io.FileReader(s);
            /*$ 1.1 *///java.io.BufferedReader br  
            /*$ 1.1 *///    = new java.io.BufferedReader(freader);
            /*$ 1.0 */  java.io.FileInputStream in 
            /*$ 1.0 */      = new java.io.FileInputStream(s);
            /*$ 1.0 */  java.io.DataInputStream br 
            /*$ 1.0 */     = new java.io.DataInputStream(in);
            String line;
            while ((line = br.readLine()) != null) {
                v.addElement(line);
            }
            br.close();
        } catch (Exception e) {
            System.err.println("Exception: " + e);
            System.exit(1);
        }
        return v;
    } //readLineVector
    
    static void writeLineVector(String s, java.util.Vector v) {
        try {
            int sz = v.size();
            java.io.FileOutputStream out = new java.io.FileOutputStream(s);
            java.io.DataOutputStream ds = new java.io.DataOutputStream(out);
            for (int i = 0; i < sz; i++) {
                String line = (String) v.elementAt(i);
                ds.writeBytes(line);
                ds.writeBytes("\n");
            }
            ds.flush();
            ds.close();
        } catch (Exception e) {
            System.err.println("Exception: " + e);
            System.exit(1);
        }
    }  //writeLineVector
    
    static void transformOneFile(String s, java.util.Hashtable h) {
        java.util.Vector v = readLineVector(s);
        boolean change = transformVector(v, h);
        if (change) {
            try {
                java.io.File f = new java.io.File(s);
                java.io.File back = new java.io.File(s + "~");
                f.renameTo(back);
            } catch (Exception e) {
                System.err.println("Couldn't rename file");
            }
            writeLineVector(s, v);
        }
    } //transformOneFile
    
    static void acceptOption(japp_Args args, java.util.Hashtable h, Integer i) {
        String arg = null;
        try {
            arg = args.next1();
        } catch (japp_ArgsException ae) {
            usage("No key");
        }
        Object old = h.put(arg, i);
        if (old != null) {
            usage("Option redefined");
        }
    } //acceptOption
    
    public static void main(String[] argv) {
        java.util.Hashtable h = new java.util.Hashtable();
        japp_Args args = new japp_Args(argv);
        try {
            String s = args.next1();
            while (s.startsWith("-")) {
                if (s.equals("-v")) {
                    acceptOption(args, h, makeVisible);
                } else if (s.equals("-c")) {
                    acceptOption(args, h, makeComment);
                } else if (s.equals("-kill")) {
                    acceptOption(args, h, makeDisappear);
                } else if (s.equals("-perm")) {
                    acceptOption(args, h, makePermanent);
                } else {
                    usage("Unknown options");
                }
                s = args.next1();
            }
            if (h.size() <= 0) {
                usage("No option");
            }
            while (s != null) {
                transformOneFile(s, h);
                s = args.next0();
            }
        } catch (japp_ArgsException ae) {
            usage("Bad arguments");
        } 
    } //main
        
} //japp


/** Exception raised when arguments are required but not available */
class japp_ArgsException extends Exception {
    public japp_ArgsException() {
        super();
    }
} //japp_ArgsException


/** 
 * Argument handling utility.
 * Single threaded use expected. 
 */
class japp_Args {
    int idx = 0;
    String[] argv;
    /** 
     * Construct an japp_Args object from argv data and position
     * it at the beginning.
     */
    public japp_Args(String[] argv) {
        this.argv = argv;
    }
    /** returns the next argument or raise an exception if none available */
    public String next1() throws japp_ArgsException {
        if (idx >= argv.length) {
            throw new japp_ArgsException();
        }
        String s = argv[idx];
        idx++;
        return s;
    }
    /** returns the next argument or null if none available */
    public String next0() {
        if (idx >= argv.length) {
            return null;
        }
        String s = argv[idx];
        idx++;
        return s;
    }
    /** returns whether more arguments are available */
    public boolean more() {
       return (idx < argv.length);
    }
} //japp_Args
