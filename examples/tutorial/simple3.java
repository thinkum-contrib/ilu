/* $Id: simple3.java,v 1.13 1999/08/03 01:57:22 janssen Exp $
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
/* Chris Jacobi, January 6, 1999 10:50 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:52 pm PDT */

/*
 * A simple client program that finds the Calculator-Factory,
 * creates a calculator, and adds up its arguments.
 */ 

/*
 * Run this like
 * java Tutorial.simple3 servername number [number...]
 * after making sure a server is running.
 */ 

package Tutorial;

public class simple3 {
    
    /* We define a new routine, "Get_Tutorial_Calculator", which 
     * finds the tutorial factory, then creates a new Calculator
     * object for us.
     */
    public static Tutorial.Calculator 
    GetTutorialCalculator(String serverId, String factoryId) {
        Tutorial.Factory factory = null;
        Tutorial.Calculator calc = null;
        System.out.println("Looking up factory");
        try {
            /* We have to call lookup with the object ID of
             * the factory object, and the ``type'' of the object 
             * we're looking for.
             */
            factory = (Tutorial.Factory) 
                            xerox.ilu.IluSimpleBinding.lookup(
                    serverId, 
                    factoryId, 
                    Tutorial.FactoryStub.iluClass()
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
        System.out.println("Looking up Calculator");
        try {
            calc = factory.CreateCalculator();
        } catch (org.omg.CORBA.SystemException e) {
            System.err.println("Failed to get Calculator: " + e);
            System.exit(1);
        }
        if (calc==null) {
            System.err.println("Got null Calculator");
            System.exit(1);
        }
        System.out.println("Got Calculator " + calc);
        return calc;
    } //GetTutorialCalculator
    
    
    public static void main(String argv[]) {
        Tutorial.Calculator calc;
        if (argv.length < 2) {
            System.err.println("usage: java Tutorial.simple3 number*");
            System.exit(1);
        }
        //Find a calculator
        String serverId = argv[0];
        calc = GetTutorialCalculator(serverId, "theFactory");
        if (calc==null) {
            System.out.println("Null calculator");
            System.exit(1);
        }
        try {
            //clear the calculator before using it
            calc.SetValue(0.0);
            //now loop over the arguments, adding each in turn
            int i = 1;
            while (i<argv.length) {
                Double v = Double.valueOf(argv[i]);//exceptions possible
                calc.Add(v.doubleValue());
                i = i+1;
            } 
            //and print the result
            System.out.println("The sum is " + calc.GetValue());
            
/* This is awfull but Java does not let you catch exceptions which
 * are not raised.  What an idiotic feature!
 *      } catch (Tutorial.DivideByZero e) {
 *          System.err.println("Division by zero: " + e);
 */

        } catch (Exception e) {
            System.err.println("Some Exception: " + e);
            e.printStackTrace(System.err);
        }
    } //main
    
} //simple3

