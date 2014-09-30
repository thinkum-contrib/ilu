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
/* IluObjectTable.java */
/* Chris Jacobi, January 6, 1999 5:05 pm PST */

/*
 */
 
/* $Id: IluObjectTable.java,v 1.27 1999/08/03 01:53:39 janssen Exp $ */
 
/* 
 * The native implementation is in IluJava_IluServer.c
 */
 
package xerox.ilu;
       
/**
 * An object table gives the application the ability to create true
 * objects upon presentation of an instance handle.  This is used
 * by the application to pass in to the creation of a true server.<p>
 *
 * This must be subclassed to be useful. <p>
 *
 * WARNING: A ilu server monitor lock is hold while calls to the
 * abstract procedures; don't do random ilu calls. <p>
 * Restriction: An IluObjectTable can serve at most one server. <p>
 *
 * @see IluServer
 * @see IluLifetimeArgs
 */

public class IluObjectTable {
   
    private static int traceObjects = IluDebug.traceObjects();
    private xerox.ilu.IluServer jjServer = null; //native access
    
    /** 
     * Constructor client-accessible because IluObjectTable's 
     * must be subclassed to be usefull.
     */
    protected IluObjectTable() {
    } //constructor
   
    /** 
     * Call-back to return resulting true object.
     * Do not use except from within "createTrueObject".
     * It is expected that tobj is newly created and not yet an
     * ilu object.  Otherwise ilu might detected a conflict
     * or synchronization problem and might raise an exception.<p>
     *
     * @see IluLifetimeArgs
     */
    protected void returnTrueObject(
    		java.lang.Object tobj, 
    		xerox.ilu.IluClassRep iluClass, 
    		int lifetime //See IluLifetimeArgs; 0 gives default behaviour
    		)
    {
    	theTrueObject = tobj;
    	theClassRep = iluClass;
    	theLifetime = lifetime;
    } //returnTrueObject
        
    /**
     * Procedure called to require creation of the object associated  
     * with the given instance handle (or deny creation if desired). <p>  
     *
     * Called by server for unknown true objects. <br>
     * Use returnTrueObject to actually return the created object. <p>
     *
     * Called with a server monitor lock held. <p>
     * 
     * An object may be "made true" only once. <br>
     * "Abstract": called by LSR, implemented in application.
     * 
     */ 
    protected /*abstract*/ void createTrueObject(java.lang.String ih) {
    }
   
    /** 
     * Notification that the server using this object table is 
     * being closed. Called with a server monitor lock hold.  <p>
     *
     * Most subclasses ignore this.
     */
    protected /*abstract*/ void objectTableFreed() {
    }
   
    /**  Accessor function */
    public final IluServer serving() {
        return this.jjServer;
    } //serving


    private java.lang.Object theTrueObject = null;
    private xerox.ilu.IluClassRep theClassRep = null;
    private int theLifetime = 0;
       
   /* 
    * Calling mechanism for the createTrueObject method. <p>
    * Called from native code. <br> 
    * Caller will register object with server.
    */
    private final xerox.ilu.IluOInt doCreateTrueObject(java.lang.String ih) {
        try {
            IluOInt oi = null;
            /*$ idb */  if (traceObjects > 0) {
            /*$ idb */      IluDebug.log.println("! IluObjectTable.doCreateTrueObject requests:" + ih);
            /*$ idb */  }
            this.theTrueObject = null;
            this.theClassRep = null;
            createTrueObject(ih);
            java.lang.Object ob = this.theTrueObject;
            /*$ idb */  if (traceObjects > 0) {
            /*$ idb */      IluDebug.log.println("! IluObjectTable.doCreateTrueObject got:" + ob);
            /*$ idb */  }
            if (ob == null) {
                //application didn't create an object: this is ok
                return null;
            }
            xerox.ilu.IluClassRep classRep = this.theClassRep;
            if (classRep==null) {
                IluDebug.clientPreError("** IluObjectTable class not specified " 
                    + ih + " " + ob);
                return null;
            }
            oi = IluOInt.newOI(ob, classRep, this.jjServer); 
            	//raises exception if object already exists
            oi.setLifetime(theLifetime);
            /*$ idb */  if (traceObjects > 0) {
            /*$ idb */      IluDebug.log.println("! IluObjectTable.doCreateTrueObject oi: " + oi);
            /*$ idb */  }
            return oi;
        } catch (java.lang.Exception e) {     
            //Don't propagate exceptions: this is called by the ilu
            //kernel which wouldn't know what to do.
            IluDebug.clientPreError("** IluObjectTable client failure " + e);
            e.printStackTrace();
            return null;
        }
    } //doCreateTrueObject
   
   /* 
    * Calling mechanism for the objectTableFreed method.
    * Called from native code
    */
    private void doObjectTableFreed(){
        try {
            this.jjServer.retainTable.clear();
            objectTableFreed();
        } catch (java.lang.Exception e) {     
            //Don't propagate exceptions: this is called by the ilu
            //server which wouldn't know what to do.
            IluDebug.clientPreError("** xerox.ilu.IluObjectTable failure " + e);
            e.printStackTrace();
        }
    } //doObjectTableFreed
    
           
    /*  
     * Used by server creation code to tie IluObjectTable to server.
     */
    /*friendly*/ synchronized void 
    setServer(xerox.ilu.IluServer server) {
        if (this.jjServer != null && this.jjServer != server) {
            IluDebug.clientPreError("Object table can serve only 1 server");
            throw new org.omg.CORBA.IMP_LIMIT(
            	"Object table can serve only 1 server"
            	);
        }
        this.jjServer = server;
    } //setServer       


    /**
     * This construct prevents subclasses from implementing cloning.<p>
     * @see     java.lang.Cloneable
     */
    protected final java.lang.Object clone() 
        throws java.lang.CloneNotSupportedException
    {
        throw new java.lang.CloneNotSupportedException();
    } //clone

   
} //IluObjectTable


