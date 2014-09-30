/* $Id: TapeCalculatorImpl.java,v 1.7 1999/08/03 01:57:23 janssen Exp $
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
/* Rick Yardumian, December 13, 1996 2:59 pm PDT */
/* Chris Jacobi, August 20, 1997 4:31 pm PDT */
/* Last edited by Mike Spreitzer October 9, 1998 1:46 pm PDT */

package Tutorial2;

import java.util.Vector;
import Tutorial2.OpType;
import Tutorial2.Operation;

/*
 * While this class complies to the Tutorial2.isl specification
 * it is a local implementation.  Its instances need to be
 * registered with Ilu before they are publicly accessible.
 */
  
public class TapeCalculatorImpl 
        extends xerox.ilu.IluObjectBase
        implements Tutorial2.TapeCalculator {
    double value;
    java.util.Vector tape;
    public TapeCalculatorImpl() {
        value = 0.0;
        tape = new java.util.Vector();
    }
    public void SetValue(double v) {
        value = v;
        Operation op = new Operation(OpType.SetValue, v, value);
        tape.addElement(op);
    }
    public double GetValue() {
        return value;
    }
    public void Add(double v) {
        value = value + v;
        Operation op = new Operation(OpType.Add, v, value);
        tape.addElement(op);
    }
    public void Subtract(double v) {
        value = value - v;
        Operation op = new Operation(OpType.Subtract, v, value);
        tape.addElement(op);
    }
    public void Multiply(double v) {
        value = value * v;
        Operation op = new Operation(OpType.Multiply, v, value);
        tape.addElement(op);
    }
    public void Divide(double v) throws Tutorial.DivideByZero {
        if (v==0.0) throw new Tutorial.DivideByZero();
        if (v==1.0) tape = null; //raise an unexpected exception for debugging
        value = value / v;
        Operation op = new Operation(OpType.Divide, v, value);
        tape.addElement(op);
    }
    public Operation[] GetTape() {
        Operation retVal[];
        //We protect structural integrity even if we don't care
        //about the value in case of a conflict.
        synchronized (tape) { 
            retVal = new Operation[tape.size()];
            tape.copyInto(retVal);
        }
        return retVal;
    } 
} //TapeCalculatorImpl

