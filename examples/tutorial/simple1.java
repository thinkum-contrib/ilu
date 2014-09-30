/* $Id: simple1.java,v 1.7 1999/08/03 01:57:21 janssen Exp $
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
/* Chris Jacobi, January 23, 1997 2:46 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:54 pm PDT */

/*
 * A simple client program that demonstrates the use of the
 * Calculator module as a library.
 */ 
 
/*
 * Run this like
 * java Tutorial.simple1 number [number...]
 */ 

package Tutorial;

public class simple1 {
    
    public static void main(String argv[]) {
        CalculatorImpl calc;
        //create the calculator
        calc = new CalculatorImpl();
        if (calc==null) {
            System.err.println("Got null TapeCalculator");
            System.exit(1);
        }
        //clear the calculator before using it
        calc.SetValue(0.0);
        //now loop over the arguments, adding each in turn
        int i = 0;
        while (i<argv.length) {
            Double v = Double.valueOf(argv[i]); //don't bother about exceptions
            calc.Add(v.doubleValue());
            i = i+1; 
        } 
        //and print the result
        System.out.println("The sum is " + calc.GetValue());
    } //main
    
} //simple1
