/* GenIluTypeIds.java  */
/* Chris Jacobi, November 27, 1998 9:29 am PST */
/* $Id: GenIluTypeIds.java,v 1.1 1998/11/27 20:39:21 jacobi Exp $ */


package xerox.ilutools;


/** 
 * Simple utility to generate the list of type ids
 * used by ilu's java runtime. We can not envision 
 * any further use then building Ilu.<p>
 * do  first
 * ../../stubbers/parser/islscan build.isl >build/typeids.scanned 
 */
 
public class GenIluTypeIds {

    public static void usage(String s) {
        if (s != null) {
            System.err.println(s);
        }
        System.err.print("use: java xerox.ilutools.GenIluTypeIds ");
        System.exit(1);
    } //usage
    
    
    /*returns occurrence of type id including doublequotes*/
    public static String scanid (String s) {
        int startPos;
        int stopPos;
        startPos = s.indexOf('"');
        stopPos = s.indexOf('"', startPos+1);
        return s.substring(startPos, stopPos+1);
    } //scanid
    
    
    public static void main(String[] argv) {
        String inputFileName = "build/typeids.scanned";
        String outputFileName = "build/IluTypeIdProps.java";
        String idChar = null;
        String idShortChar = null;
        String idSString = null;
        String idWString = null;
        try {
            java.io.FileReader freader 
                   = new java.io.FileReader(inputFileName);
            java.io.BufferedReader br  
                   = new java.io.BufferedReader(freader);
            String line;
            while ((line = br.readLine()) != null) {
                //System.out.println("READ: " + line);
                line = line.trim();
                if (line.startsWith("char")) {
                    idChar = scanid(line);
                }
                if (line.startsWith("shortcharacter")) {
                    idShortChar = scanid(line);
                }
                if (line.startsWith("string")) {
                    idSString = scanid(line);
                }
                if (line.startsWith("wstring")) {
                    idWString = scanid(line);
                }
            }
            br.close();
        } catch (Exception e) {
            System.err.println("Exception while processing input: " + e);
            System.exit(1);
        }
        try {
            java.io.Writer wr 
                = new java.io.BufferedWriter(
                       new java.io.FileWriter(outputFileName));
            wr.write("/* This file is machine generated */\n");
            wr.write("package xerox.ilu;\n");
            wr.write("public class IluTypeIdProps extends java.util.Hashtable {\n");
            wr.write("    public IluTypeIdProps() {\n");
            wr.write("        this.put(\"character\", " + idChar + ");\n");
            wr.write("        this.put(\"shortcharacter\", " + idShortChar + ");\n");
            wr.write("        this.put(\"string\", " + idSString + ");\n");
            wr.write("        this.put(\"wstring\", " + idWString + ");\n");
            wr.write("    }\n");
            wr.write("}\n");
            wr.flush();
            wr.close();
        } catch (Exception e) {
            System.err.println("Exception while generating output: " + e);
            System.exit(1);
        }
        System.out.println(outputFileName + " generated");
    }// main

} //GenIluTypeIds
