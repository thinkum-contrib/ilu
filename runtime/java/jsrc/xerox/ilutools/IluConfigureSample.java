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
/* IluConfigureSample.java */
/* Chris Jacobi, December 23, 1998 11:53 am PST */
/* $Id: IluConfigureSample.java,v 1.2 1999/08/03 01:54:14 janssen Exp $ */


/**
 * Run the native stubber on this to see how to configure
 * IluJava_JTypes.h <p><b>
 *
 * THIS IS NOT PART OF A RUNNING ILU
 * BUT ONLY USED FOR CONFIGURATION </b><p>
 *
 * Use 
 * javac IluConfigureSample.java
 * javah IluConfigureSample
 * then look at IluConfigureSample.h <br>
 *
 * A type is inconsistent if it doesn't map the same way
 * for input parameters, output parameters, fields and array elements. <br>
 *
 * On sparc, using jdk1.1.4 with ONI => short, byte and char are inconsistent
 * We should never use fields of inconsistent types from native code, however 
 * we use input parameters, output parameters and array elements.
 *
 */
class IluConfigureSample {
    
    int  	thisIsJint;
    short	thisIsJshort; //INCONSISTENT: AVOID FIELDS
    boolean	thisIsJboolean;
    byte	thisIsJbyte; //INCONSISTENT: AVOID FIELDS
    long	thisIsJlong;
    float	thisIsJfloat;
    double	thisIsJdouble;
    char	thisIsJchar; //INCONSISTENT: AVOID FIELDS
    Object	thisIsJObject;
    
    native int argsAreJint(int anInt);
    native short argsAreJshort(short aShort);
    native boolean argsAreJboolean(boolean aBoolean);
    native byte argsAreJbyte(byte aByte);
    native long argsAreJlong(long aLong);
    native float argsAreJfloat(float aFloat);
    native double argsAreJdouble(double aDouble);
    native char argsAreJchar(char aChar);
    native Object argsAreJObject(Object anObject);

} //IluConfigureSample

