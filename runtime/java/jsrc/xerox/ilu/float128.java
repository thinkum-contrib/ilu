/* float128.java */
/* Chris Jacobi, January 18, 1999 7:32 pm PST */
/* $Id: float128.java,v 1.6 1999/08/03 01:53:48 janssen Exp $ */

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
 
 
package xerox.ilu;

/**
 * A class for 128 bit floats.
 * We expect this class to be redesigned when 128 bit floats
 * get used for real...
 *
 * Whats the byte order?  It goes in the same way it comes out.
 */
public class float128 implements java.io.Serializable {
    protected byte[] b16;
    
    protected float128() {
    } //constructor
    
    protected static float128 alloc () {
        return new float128();
    }
    
    //not really public, but needed by ILU
    public static float128 fromByte16(byte[] b16){
        if (b16.length!=16) {
            throw new IllegalArgumentException("float128 needs 16 bytes");
        }
        float128 f = alloc();
        f.b16 = b16;
        return f;
    }
    
    //not really public, but needed by ILU
    public byte[] toByte16() {
        return b16;
    }
    
    static final long serialVersionUID = 3196044364790516757L;
    
} //float128
