/* $Id: simple4.java,v 1.16 1999/08/03 01:57:24 janssen Exp $
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
/* Rick Yardumian 16Dec96 */
/* Chris Jacobi, November 13, 1998 2:43 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:53 pm PDT */
 
/*
 * A simple client program that finds the TapeCalculator-Factory,
 * creates a calculator and executes the users input.
 */ 

/*
 * Run this like
 * java Tutorial2.simple4 serverId
 */ 

package Tutorial2;

public class simple4 {
    
    /* We define a new routine, "Get_Tutorial_Calculator", which 
     * finds the tutorial factory, then creates a new TapeCalculator
     * object for us.
     */
    public static Tutorial2.TapeCalculator 
    GetTutorialTapeCalculator(String serverId, String factoryId) {
        Tutorial2.Factory factory = null;
        Tutorial2.TapeCalculator calc = null;
        System.out.println("Looking up factory");
        try {
            /* We have to call lookup with the object ID of
             * the factory object, and the ``type'' of the object we're looking
             * for.
             */
            factory = (Tutorial2.Factory) 
                            xerox.ilu.IluSimpleBinding.lookup(
                    serverId, 
                    factoryId, 
                    Tutorial2.FactoryStub.iluClass()
                    );
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Failed to get factory: " + e);
            System.exit(1);
        }
        if (factory==null) {
            System.err.println("Got null factory");
            System.exit(1);
        } 
        System.out.println("Got factory " + factory);
        System.out.println("Looking up TapeCalculator");
        try {
            calc = factory.CreateTapeCalculator();
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Failed to get TapeCalculator: " + e);
            System.exit(1);
        }
        if (calc==null) {
            System.err.println("Got null TapeCalculator");
            System.exit(1);
        }
        System.out.println("Got TapeCalculator " + calc);
        return calc;
    } //GetTutorialTapeCalculator
     
     
    static String opNames[]
        = {"SetValue", "Add", "Subtract", "Multiply", "Divide"};
    
    static double argToDouble(String inputLine) {
        return Double.valueOf(inputLine.substring(1)).doubleValue();
    } //argToDouble


    public static void main(String argv[]) {
        Tutorial2.TapeCalculator calc;
        Tutorial2.Operation[] tape;
        boolean quitFlag = false;
        double value = 0.0;
        String line;
        java.io.InputStreamReader ir = new java.io.InputStreamReader(System.in);
        java.io.BufferedReader br = new java.io.BufferedReader(ir);

        if (argv.length < 1) {
            System.err.println("USAGE: java Tutorial2.simple4 serverid");
            System.exit(1);
        }
        //Find a calculator
        String serverId = argv[0];
        calc = GetTutorialTapeCalculator(serverId, "theFactory");
        if (calc==null) {
            System.err.println("Didn't get a calculator");
            System.exit(1);
        }
        System.out.println("Got tape calculator");
        try {
            //Clear the calculator before using it
            calc.SetValue(0.0);
            //Loop over user inputs and perform the requested operation
            while (!quitFlag) {
                value = calc.GetValue();
                System.out.print(value + "\n> "); System.out.flush();
                line = br.readLine();
                if (line == null) line = "q";   
                switch (line.charAt(0)) {
                    case '\n':
                        break;
                    case '+':
                        value = argToDouble(line);
                        calc.Add(value);
                        break;
                    case '-':
                        value = argToDouble(line);
                        calc.Subtract(value);
                        break;
                    case '*':
                        value = argToDouble(line);
                        calc.Multiply(value);
                        break;
                    case '/':
                        try {
                            value = argToDouble(line);
                            calc.Divide(value);
                        } catch (Tutorial.DivideByZero e) {
                            System.out.println("** division by zero " + e);
                        }
                        break;
                    case 'q':
                        quitFlag = true;
                        break;
                    case 't':
                        tape = calc.GetTape();
                        for (int i = 0; i <  tape.length; i++) {
                           System.out.println("  " 
                                   + opNames[tape[i].op.value()]
                                   + "(" + tape[i].value + ") => "
                                   + tape[i].accumulator
                                   );
                        }
                        break;
                    case 'c':
                        calc.SetValue(0.0);
                        break;
                    default:
                        System.out.println("Invalid Operation <" + line + ">");
                        System.out.println("Valid ops are +, -, *, /, "
                            + "t (for tape),"
                            + "c (for clear),"
                            + "q (for quit),"
                            );
                }
            }
        } catch (java.io.IOException e) {
            System.err.println("Example raised IOException: " + e);
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Example raised SystemException: " + e);
        }
    } //main
    
} //simple4
