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
/* IluCustomMapping.java */
/* Chris Jacobi, December 23, 1998 12:01 pm PST */
/*
 * $Id: IluCustomMapping.java,v 1.6 1999/08/03 01:54:08 janssen Exp $
 */

package xerox.ilu;

/**
 * An ilu type which supports custom mapping needs to register an object
 * of IluCustomMapping-type to perform the actual transformation.<p>
 *
 * Details:<br>
 * org.omg.CORBA.SystemException or IluCustomMappingException is normally 
 * thrown by ilu; however, the implementor of a custom mapping may throw 
 * these exceptions also. <br>
 * See the comprehensive "javaserialize" example of custom mapping.  
 */
public interface IluCustomMapping {

    /**
     * Convert an ilu object as received from the wire to the custom 
     * type used in this java environment. <p>
     *
     * The returned value must have the correct java type since the  
     * ilu stub will likely perform a cast. 
     */
    public java.lang.Object 
        iluCustomMapping_customFromIlu(java.lang.Object iluObject);

    /**
     * Convert a custom mapped object back to the type which is  
     * used by ilu to represent it on the wire. <p>
     *
     * The returned value must have the correct (ilu) type as the  
     * ilu stub will likely perform a cast.  <p>
     * This method may be called multiple times for a single ilu   
     * call.  Its returned values for a single ilu call must be 
     * equal as much as ilu is concerened. 
     */
    public java.lang.Object 
        iluCustomMapping_iluFromCustom(java.lang.Object customObject);
    
} //IluCustomMapping
