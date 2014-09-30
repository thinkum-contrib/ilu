/* $Id: FooWhizBangFuzzler.java,v 1.2 1999/08/03 01:59:09 janssen Exp $
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
/* Chris Jacobi, December 16, 1998 5:21 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:31 pm PDT */

package test_sender; 

/**
 * Useless class used to demonstrate shipping behaviour.
 * The main point is to make sure that this class exist
 * on the server side of the example but not on the client
 * side.  Successfully running the example will load this
 * class on the client side through the ilu connection.
 */
public class FooWhizBangFuzzler implements java.io.Serializable  {
   
   java.lang.Object theChild = null;
   
   public FooWhizBangFuzzler(java.lang.Object ob) {
       this.theChild = ob;
   } //constructor
   
   public java.lang.String toString() {
       return "FooWhizBangFuzzler[" + theChild + "]";
   } //toString
   
   protected java.lang.Object readResolve() throws java.io.ObjectStreamException {
        System.out.println("readResolve called");
        if (theChild instanceof String) {
            theChild = "resolved..." + theChild;
        }
        return this;
    } //readResolve
    
    static {
      System.out.println("The FooWhizBangFuzzler class static initialization;  check how often this is called to count class loaders.");
   }
   
} //FooWhizBangFuzzler

