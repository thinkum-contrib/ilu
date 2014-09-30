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
/* gnh.java */
/* Chris Jacobi, December 7, 1998 1:26 pm PST */
/* $Id: gnh.java,v 1.24 1999/08/03 01:54:13 janssen Exp $ */

/*
    javac -d classes gnh.java
    java xerox.ilutools.gnh -jni gnhtest.in
    java xerox.ilutools.gnh -oni ilunative.in
*/

package xerox.ilutools;

/**
 * Generate native header files so that the actual
 * native code is independent of the architecture
 * for native headers. <p>
 *
 * This handles only a subset of all possibilities for
 * native access.  Portable programs will either stick
 * to this subset or will extend this tool. This is not
 * a tool for the general public but for programmers of
 * of native ..methods. Besides of restrictions on the
 * supported cases it also expects correct input.<p>
 *
 * ONI is my own acronym for Sun's "OLD native interface"<br>
 * JNI Sun's "java native interface" 
 *            which used to be underpowered befor jdk1.2<br>
 * RNI Microsofts "raw native interface"<br>
 * JRI is Netscape's "Java Runtime Interface" <p>
 *
 * <pre>
 * Currently understands oni and rni.
 * Currently understands dynamic fields.
 *    (Static fields are not important to ilu)
 *
 * Input is line oriented
 * Lines starting with // are comment
 * Lines starting with CLASS set the current class
 * Lines starting with FIELD make a field in the current class accessible
 *
 * CLASS lines syntax:
 * class fully-qualified-name-of-class 
 *
 * FILED lines syntax:
 * field name-of-field type-signature-of-field
 * Currently supports only simple types
 * Special type-signature: type-signature's starting with 0
 *    are long fields in java used to store native pointers;
 *    following the 0 is the c type for the c pointer.
 *
 * </pre>
 */
 
public class gnh {

    /* This is used at build time and I couldn't care
     * less about efficiency
     */

    public static final int modeONI = 1;
    public static final int modeJNI = 2;
    public static final int modeRNI = 3;

    public static boolean debug = false; //debugging at gnh time
    public static boolean genDebug = true; //generate debugging at run time

    public static String shortMyPackagePrefix = "xerox_ilu_";

    public static String quoted(String s) {
        return "\"" + s + "\"";
    } //quoted
    
    public static void usage(String s) {
        if (s !=null) {
            System.err.println(s);
        }
        System.err.print("use: java gnh ");
        System.err.println("(-jni | -oni | -rni) file");
        System.exit(1);
    } //usage
    
    static java.util.Hashtable jni_piece_tab = new java.util.Hashtable();
    static {
        jni_piece_tab.put("Z", "Boolean");
        jni_piece_tab.put("B", "Byte");
        jni_piece_tab.put("C", "Char");
        jni_piece_tab.put("S", "Short");
        jni_piece_tab.put("I", "Int");
        jni_piece_tab.put("J", "Long");
        jni_piece_tab.put("F", "Float");
        jni_piece_tab.put("D", "Double");
        jni_piece_tab.put("V", "Void");
    }

    public static String get_jni_piece(String sig){
        Object ob = jni_piece_tab.get(sig);
        if (ob==null) {return "Object";}
        return (String) ob;
    }
    
    
    /* read a file as a vector of lines */
    public static java.util.Vector readLineVector(String fName) {
        java.util.Vector v = new java.util.Vector();
        try {
        /*$ 1.1 */  java.io.FileReader freader = new java.io.FileReader(fName);
        /*$ 1.1 */  java.io.BufferedReader br  
        /*$ 1.1 */      = new java.io.BufferedReader(freader);
        /*$ 1.0 *///java.io.FileInputStream in 
        /*$ 1.0 *///    = new java.io.FileInputStream(fName);
        /*$ 1.0 *///java.io.DataInputStream br 
        /*$ 1.0 *///   = new java.io.DataInputStream(in);
        String line;
        while ((line = br.readLine()) != null) {
            v.addElement(line);
        }
        br.close();
    } catch (Exception e) {
        System.err.println("Exception: " + e);
        e.printStackTrace(System.err);
        System.exit(1);
    }
        return v;
    } //readLineVector
    
    
    /** write a vector of lines as a file */
    public static void writeLineVector(String s, java.util.Vector v) {
        int sz = v.size();
        try {
            java.io.File outFile = new java.io.File(s);
            if (outFile.exists()) {
                if (! outFile.canWrite()) {
                    System.err.println("Output file " + s + " not writable");
                    System.exit(1);
                }
            }
            java.io.FileOutputStream outStream = 
                new java.io.FileOutputStream(outFile);
            java.io.DataOutputStream ds = 
                new java.io.DataOutputStream(outStream);
            for (int i = 0; i < sz; i++) {
                String line = (String) v.elementAt(i);
                ds.writeBytes(line);
                ds.writeBytes("\n");
            }
            ds.flush();
            ds.close();
        } catch (Exception e) {
            System.err.println("Exception: " + e);
            e.printStackTrace(System.err);
            System.exit(1);
        }
        //write log of generated files because casual users might get
        //confused by the inflexibility of this application
        System.out.println(s + " written");
    }  //writeLineVector
    

    /** Copy a vector */
    public static java.util.Vector shallowCopyVector(java.util.Vector v) {
        java.util.Vector c = (java.util.Vector) v.clone();
        return c;
    } //shallowCopyVector
     
     
    /** Create new vector with "v2" inserted in "v1" at "pos" */
    public static java.util.Vector insertVector(
        java.util.Vector v1, int pos,
        java.util.Vector v2)
    {
        int i;
        int s1 = v1.size();
        int s2 = v2.size();
        java.util.Vector v = new java.util.Vector(s1 + s2);
        if (pos>s1) pos = s1; 
        for (i = 0; i < pos; i++) {
            v.addElement(v1.elementAt(i));
            }
        for (i = 0; i < s2; i++) {
            v.addElement(v2.elementAt(i));
        }
        for (i = pos; i < s2; i++) {
            v.addElement(v1.elementAt(i));
        }
        return v;
    } //insertVector

    /** Create appends vector v2 to v */
    public static void appendVector(
        java.util.Vector v,
        java.util.Vector v2)
    {
        int s2 = v2.size();
        for (int i = 0; i < s2; i++) {
            v.addElement(v2.elementAt(i));
        }
    } //appendVector
    

    /** Create new vector with v2 inserted in v1; SLOW  */
    public static void insertVector0(
        java.util.Vector v1, int pos,
        java.util.Vector v2)
    {
        int i;
        int s1 = v1.size();
        int s2 = v2.size();
        if (pos>s1) pos = s1; 
        for (i = 0; i < s2; i++) {
            v1.insertElementAt(v2.elementAt(i), pos);
            pos = pos + 1;
        }
    } //insertVector0
    

    /** remove extension from a file name */
    public static String stripExt(String s) {
        int dotPos =  s.lastIndexOf('.');
        if (dotPos>0) {
            s = s.substring(0, dotPos);
        }
        return s;
    } //stripExt
    
    
    public static void main(String argv[]) {
        int mode = 0;
        Args args = new Args(argv);
        String nameBase = null;
        try {
            String s = args.next1();
            while (s.startsWith("-")) {
                if (s.equals("-shorten")) { 
                    shortMyPackagePrefix = args.next1();
                } else if (s.equals("-debug")) { 
                    debug = true;
                } else if (s.equals("-genDebug")) { 
                    genDebug = true;
                } else if (s.equals("-name")) { 
                    nameBase = args.next1();
                } else if (s.equals("-rni")) { mode = modeRNI;
                } else if (s.equals("-oni")) { mode = modeONI;
                } else if (s.equals("-jni")) { mode = modeJNI;
                } else {
                    usage("Unknown options");
                }
                s = args.next1();
            }
            if (mode == 0) {
                usage("No mode option set");
            }
            while (s != null) {
                handleOneFile(s, nameBase, mode);
                s = args.next0();
                if (s != null) {
                    usage("Additional files ignored");
                }
            }
        } catch (ArgsException ae) {
            usage("Bad arguments");
        } 
    } //main
    
    public static void appendLine(java.util.Vector v, String line) {
        if (line!=null) v.addElement(line);
    }
    
    public static void handleOneFile(String fName, String nameBase, int mode) {
        gnhState state;
        if (nameBase==null) {
            nameBase = stripExt(fName);
        }
        state = new gnhState();
        state.mode = mode;

        // read all the lines of the file into an internal vector of lines
        state.inV = readLineVector(fName);
        
        // write our standard file beginnings
        appendLine(state.hdrV, "/* " + nameBase + ".h */");
        appendLine(state.hdrV, "/* This file is machine generated */");
        appendLine(state.hdrV, "#ifndef _ILUJAVA_" + nameBase + "_H_" );
        appendLine(state.hdrV, "#define _ILUJAVA_" + nameBase + "_H_\n" );
        if (state.mode == modeJNI) {
            appendLine(state.hdrV, "#include <jni.h>");
        }
        if (state.mode == modeJNI) {
            appendLine(state.hdrV, "extern void JinitNativeHeaders(JNIEnv* j_env); \n");
        } else {
            appendLine(state.hdrV, "extern void JinitNativeHeaders(); \n");
        }

        appendLine(state.implV, "/*  " + nameBase + ".c  */");
        appendLine(state.implV, "/* This file is machine generated */\n");
        appendLine(state.implV, "#include " + quoted( "IluJava_Includes.h" ));
        appendLine(state.implV, "#include " + quoted( nameBase + ".h" ));
        
        if (genDebug) {
            appendLine(state.implV, "#include <stdio.h>");
            appendLine(state.implV, " ");
            appendLine(state.implV, "static void bug(char* s)");
            appendLine(state.implV, "{");
            appendLine(state.implV, "    printf(\"****ERROR: %s\\n\", s);");
            appendLine(state.implV, "}");
            appendLine(state.implV, " ");
        }

        // generate the actual code for the lines
        handleVector(state);

        // write our standard file endings
        appendLine(state.hdrV, "\n#endif");

        // write out our internal vectors of the headers and code into files
        writeLineVector(nameBase + ".h",  state.hdrV);
        writeLineVector(nameBase + ".c",  state.implV);

    } //handleOneFile
    

    // generate the actual code for the descriptive input lines
    public static void handleVector(gnhState state) {

        if (debug) {
            System.out.println("generating section comments");
        }
        
        // initialize the various sections with a comment 
        // indicating their purpose 
        appendLine(state.clsInitsV, "\n    /* class initializations... */");
        appendLine(state.fldInitsV, "\n    /* field & method definitions ... */");
        appendLine(state.globalInitsV, "\n    /* global declarations ... */");
		
        // process all the lines of the input file
        int inputLines = state.inV.size();
        for (int i = 0; i < inputLines; i++) {
            String line = (String) state.inV.elementAt(i);
            handleLine(state, line, i);
        }

        // write out the global declarations section
        appendVector(state.implV, state.globalInitsV);
        appendLine(state.implV, "");
        appendLine(state.implV, "");

        // write out the initialization function
        if (state.mode == modeJNI) {
            appendLine(state.implV, "void JinitNativeHeaders(JNIEnv* j_env) {\n");
        } else {
            appendLine(state.implV, "void JinitNativeHeaders() {\n");
        }
        appendVector(state.implV, state.clsInitsV); 
                // put out any class initializations produced by handleLine...
        appendVector(state.implV, state.fldInitsV); 
                // put out any field initializations produced by handleLine...
        appendLine(state.implV, "}");

    } //handleVector
    

    /**
     *  process one line from the input file
     */
    public static void handleLine(gnhState state, String line, int lineno) {
        if (debug) {
            System.out.println("line: " + line);
        }
        java.util.StringTokenizer st = new java.util.StringTokenizer(line);
        try {
            if (st.hasMoreTokens()) {
                String key = st.nextToken();
                // call the appropriate processing function based on the 
                // first token of the line
                if (key.startsWith("/")) {
                    //comment line
                } else if (key.equalsIgnoreCase("field")) {
                   doField(state, st);
                } else if (key.equalsIgnoreCase("class")) {
                   doClass(state, st);
                } else if (key.equalsIgnoreCase("method")) {
                   doMethod(state, st);
                } else {
                    System.err.println("line: " + lineno + " unrecognized");
                    System.exit(1);
                }
            }
        } catch (java.util.NoSuchElementException e) {
            System.err.println("line: " + lineno + " too short");
            System.exit(1);
        }
    } //handleLine


    public static void doField(gnhState state, java.util.StringTokenizer st) {
        String getFieldLine, putFieldLine;
        String hdrLn = null;
        String defDeclLn = null;
        String initLn = null;
        String fieldName = st.nextToken();
        String fieldSig = st.nextToken();
        String castSig = st.nextToken();
        String fieldNick = state.clsNick + "_" + fieldName;
        String getMacroStart = 
        "#define GET_" + fieldNick + "(jh_ob) ";
        String putMacroStart = 
        "#define PUT_" + fieldNick + "(jh_ob, value) ";

        if (state.mode == modeONI) {
            getFieldLine = "(unhand( jh_ob )->" + fieldName + ")";
            putFieldLine = "(unhand( jh_ob )->" + fieldName + ") = "; 
            if (fieldSig.startsWith("0")) {
                String fieldType = fieldSig.substring(1);
                getFieldLine = "* (" + fieldType + "*) &" + getFieldLine;
                putFieldLine = "* (int**) &" + putFieldLine + "(int*)";
            }
            appendLine(state.hdrV, getMacroStart + "\\");
            appendLine(state.hdrV, "    " + getFieldLine);
            appendLine(state.hdrV, putMacroStart + "\\");
            appendLine(state.hdrV, "    " + putFieldLine + "value;");
        }

        if (state.mode == modeRNI) {  // Microsoft Raw Native Interface

            // build up the lines that define the accessor macros

            getFieldLine = "(" + castSig 
                 + ") ((H" + state.clsName + "*)jh_ob)->" + fieldName;

            putFieldLine = "*((" + castSig + "*)(&(((H" + state.clsName + "*)jh_ob)->"
				+ fieldName + "))) = (" + castSig + ")value";
					
            // write out the accessor macros
            appendLine(state.hdrV, getMacroStart + "\\");		
            appendLine(state.hdrV, "\t" + getFieldLine);
            appendLine(state.hdrV, putMacroStart + "\\");
            appendLine(state.hdrV, "\t" + putFieldLine);
            appendLine(state.hdrV, "");

        } // end RNI

        if (state.mode == modeJNI) {
            String fromjnicast = "";
            String tojnicast = "";
            if (fieldSig.startsWith("0")) {
                tojnicast = "(jlong) ";
                fromjnicast = "(" + fieldSig.substring(1) + ") ";
                fieldSig = "J";
            }
            String jniTypePiece = get_jni_piece(fieldSig);
            String fieldIDName = "FIELDID_" + fieldNick;
            hdrLn = "extern jfieldID " 
                + fieldIDName 
                + ";";
            appendLine(state.hdrV, hdrLn); 
            defDeclLn = "jfieldID " 
                + fieldIDName 
                + ";";
            appendLine(state.implV, defDeclLn); 
            initLn = fieldIDName 
                + " = (*j_env)->GetFieldID(j_env, " 
                + state.clsID + ", " 
                + quoted(fieldName) + ", " 
                + quoted(fieldSig) 
                + ");";  
            appendLine(state.fldInitsV, "    " + initLn);
            getFieldLine = fromjnicast
               + "(*j_env)->Get" + jniTypePiece
               + "Field(j_env, jh_ob, " + fieldIDName
               + ")";
            appendLine(state.hdrV, getMacroStart + "\\");
            appendLine(state.hdrV, "    " + getFieldLine);
            putFieldLine = 
                 "(*j_env)->Set" + jniTypePiece
               + "Field(j_env, jh_ob, " + fieldIDName
               + ", " + tojnicast + "value )"; 
            appendLine(state.hdrV, putMacroStart + "\\");
            appendLine(state.hdrV, "    " + putFieldLine);
            if (genDebug) {
                appendLine(state.fldInitsV, "    if (" 
                    + fieldIDName 
                    + "==0) {bug(\""
                    + fieldIDName
                    + "\");}");
            }
        }
    } //doField
    
    
    /** 
     * Invoking methods. 
     * No 64 bit arguments or returns ... 
     * No static methods ... 
     */
    public static void doMethod(gnhState state, java.util.StringTokenizer st) {
        String name = st.nextToken();
        String signature = st.nextToken();
        AnalizeCallSignature as = new AnalizeCallSignature(signature);
        String methNick = state.clsNick + "_" + name;
        String callArgs = "";
        for (int i = 0;  i < as.argCnt; i++) {
            callArgs = callArgs + ", arg" + i;
        }
        String callMacroStart 
            = "#define JCALL_" + methNick + "(jh_ob"
            + callArgs + ") \\";
        if (state.mode == modeJNI) {
            //not yet finished
            String jniMethodID = "METHODID_" + state.clsNick + "_" + name;
            String jniCallRest;
            String jniTypeOfRet = get_jni_piece(as.retType);
            String hdrLn = "extern jmethodID " + jniMethodID + ";";
            String defDeclLn = "jmethodID " + jniMethodID + ";"; 
            String initLn = jniMethodID + " = (*j_env)->GetMethodID(j_env, "
                + state.clsID + ", "
                + quoted(name) + ", "
                + quoted(signature) + ");";
            appendLine(state.implV, defDeclLn); 
            appendLine(state.hdrV, hdrLn); 
            appendLine(state.hdrV, callMacroStart);
            appendLine(state.fldInitsV, "    " + initLn);
            jniCallRest = "    (*j_env)->Call" + jniTypeOfRet  
                + "Method(j_env, jh_ob, "
                + jniMethodID + callArgs + ")"; 
            appendLine(state.hdrV, jniCallRest);
            if (genDebug) {
                appendLine(state.fldInitsV, "    if (" 
                    + jniMethodID 
                    + "==0) {bug(\""
                    + jniMethodID
                    + "\");}");
            }
        } else if (state.mode == modeONI || state.mode == modeRNI) {
            String callMacroRest;
            appendLine(state.hdrV, callMacroStart);
            callMacroRest = "    execute_java_dynamic_method(EE(), \\";
            appendLine(state.hdrV, callMacroRest);
            callMacroRest = "    (Hjava_lang_Object *) jh_ob, \\";
            appendLine(state.hdrV, callMacroRest);
            callMacroRest = "    " + quoted(name) + ",\\";
            appendLine(state.hdrV, callMacroRest);
            callMacroRest = "    " + quoted(signature) + callArgs + ")";
            appendLine(state.hdrV, callMacroRest);
        }
    } //doMethod
    
    
    /** Give nickname by which we address class */
    public static String nickFromName(String name) {
        if (name.startsWith(shortMyPackagePrefix)) {
            int skip = shortMyPackagePrefix.length();
            name = name.substring(skip);
        }
        return name;
    } //nickFromName


    /** changes dots etcetera by underscores */
    public static String canonicalize(String name) {
        name = name.replace('.', '_');
        name = name.replace('/', '_');
        return name;
    } //canonicalize
    
    
    public static void doClass(gnhState state, java.util.StringTokenizer st) {
        state.clsName = canonicalize(st.nextToken());
        state.clsNick = nickFromName(state.clsName);
        state.clsID = "CLASSID_" + state.clsNick;
        if (state.mode == modeJNI) {
            String hdrDeclLn = null;
            String defDeclLn = null;
            String initLn = null;
            hdrDeclLn = "extern jclass " + state.clsID + ";";
            appendLine(state.hdrV, hdrDeclLn);
            defDeclLn = "jclass " + state.clsID + ";";
            appendLine(state.implV, defDeclLn);
            initLn = state.clsID + " = (*j_env)->FindClass(j_env, "  
                + quoted(state.clsName.replace('_', '/'))
                + ");";
            appendLine(state.clsInitsV, "   " + initLn);
            if (genDebug) {
                appendLine(state.clsInitsV, "   if (" 
                    + state.clsID 
                    + "==0) {bug(\""
                    + state.clsID
                    + "\");}");
            }
        } else if (state.mode == modeONI) {
            //nothing
        } else if (state.mode == modeRNI) {
            //nothing
        }
    } //doClass
    
} //gnh
    

class gnhState {
    //
    //input
    int mode = 0; // relects native interface use e.g. ONI, RNI, ...
    java.util.Vector inV = null;   // vector holding lines of the input file
    //
    //output
    java.util.Vector hdrV = null; 	//contents of the .h include file
    java.util.Vector implV = null;	//contents of the .c file
    //
    //internals
    String clsID = null;   //name of variable representing class
    String clsName = null; //full name of current class (underscore syntax)
    String clsNick = null; //shortened name of current class (underscore syntax)
    java.util.Vector globalInitsV = null; // global declarations, etc.
    java.util.Vector clsInitsV = null; //initializations done first
    java.util.Vector fldInitsV = null; //initializations done second
    
    public gnhState() {
        clsID = "ERROR NO CLASS DEFINED";
        clsName = "ERROR NO CLASS DEFINED";
        clsNick = "ERROR NO CLASS DEFINED";
        hdrV = new java.util.Vector();
        implV = new java.util.Vector();
        globalInitsV = new java.util.Vector();
        clsInitsV = new java.util.Vector();
        fldInitsV = new java.util.Vector();
    } //constructor
} //gnhState


/** 
 * Argument handling utility.
 * Single threaded use expected. 
 */
class Args {
    int idx = 0;
    String[] argv;
    /** 
     * Construct an Args object from argv data and position
     * it at the beginning.
     */
    public Args(String[] argv) {
        this.argv = argv;
    } //constructor
    /** returns the next argument or raise an exception if none available */
    public String next1() throws ArgsException {
        if (idx >= argv.length) {
            throw new ArgsException();
        }
        String s = argv[idx];
        idx++;
        return s;
    } //next1
    /** returns the next argument or null if none available */
    public String next0() {
        if (idx >= argv.length) {
            return null;
        }
        String s = argv[idx];
        idx++;
        return s;
    } //next0
    /** returns whether more arguments are available */
    public boolean more() {
       return (idx < argv.length);
    }//more
} //Args
    

class ArgsException extends java.lang.Exception {
    public ArgsException() {
	super();
    }
    public ArgsException(java.lang.String s) {
	super(s);
    }
} //ArgsException


class StringReader {
    String s = null;
    int nextPos = 0;
    public StringReader(String s) {
        this.s = s;
        this.nextPos = 0; 
    }
    public char get() {
        char ch = ' ';
        if (this.nextPos<this.s.length()) {
            ch = this.s.charAt(this.nextPos);
            this.nextPos = this.nextPos+1;
        } else {
            throw new java.util.NoSuchElementException("bad signature");
        }
        return ch;
    }
}// StringReader


class AnalizeCallSignature extends StringReader {
    int argCnt = 0;
    String retType = null;
    public AnalizeCallSignature(String s) {
        super(s);
        char ch = super.get();
        if (ch != '(') {
            throw new java.util.NoSuchElementException("bad signature"); 
        }
        ch = super.get();
        while (ch != ')') {
            argCnt = argCnt+1;
            while (ch == '[') {ch = super.get();}
            if (ch == 'L') {
                while (ch != ';') {ch = super.get();}
            }
            ch = super.get();
        }
        retType = super.s.substring(super.nextPos);
    } //AnalizeCallSignature
    
}// AnalizeCallsignature
