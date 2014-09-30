/* $Id: simple2.java,v 1.12 1999/08/03 01:57:22 janssen Exp $
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
/* Chris Jacobi, January 6, 1999 10:54 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:52 pm PDT */

/*
 * A simple client program that demonstrates the use of the
 * Calculator stub module with a local implementation.
 */ 
 
/*
 * Run this like
 * java Tutorial.simple2 number [number...]
 */ 

package Tutorial;

public class simple2 {
    
    public static void main(String argv[]) {
        Tutorial.Calculator calc;  //Interface from stubbing...
        try {
            //create the calculator
            calc = new Tutorial.CalculatorImpl();
            if (calc==null) {
                 System.err.println("Couldn't create calculator");
                 System.exit(1);
            }
            //clear the calculator before using it
            if (argv.length<1) {
                calc.SetValue(0.0);
            } else {
                Double v = Double.valueOf(argv[0]);
                calc.SetValue(v.doubleValue());
            }
            //now loop over the arguments, adding each in turn
            int i = 1;
            while (i<argv.length) {
                Double v = Double.valueOf(argv[i]); //don't bother...
                calc.Divide(v.doubleValue());
                i = i+1;
            } 
            //and print the result
            System.out.println("the 'sum' is " + calc.GetValue());
        } catch (Tutorial.DivideByZero e) {
            System.err.println("raised DivideByZero exception: " + e);
        } catch (Exception e) {
            System.err.println("raised exception: " + e);
            e.printStackTrace(System.err);
        }
    } //main
    
} //simple2
