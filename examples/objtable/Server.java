/* $Id: Server.java,v 1.4 1999/08/03 01:58:03 janssen Exp $
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
/* Chris Jacobi, February 19, 1998 7:40 pm PST */
/* Last edited by Mike Spreitzer October 9, 1998 1:34 pm PDT */
 
/*
 * Run this like
 * java objtable.Server
 */ 
 
package objtable;


/*friendly*/
class fileImpl extends org.omg.CORBA.ObjectImpl implements file {

    java.lang.String theName;
    public fileImpl(java.lang.String n) {
        this.theName = n;
    } //constructor
    
    public java.lang.String name() {
        return theName;
    } //name

} //fileImpl


/*friendly*/
class serverImpl extends org.omg.CORBA.ObjectImpl implements server {
    
    public serverImpl() {
    } //constructor
    
    public file find_file(java.lang.String name) throws not_found {
        System.out.println("find_file called: " + name);
        file f = new fileImpl(name);
        return f;
    } //find_file
    
}//serverImpl


/*friendly*/
class MyIluObjectTable extends xerox.ilu.IluObjectTable {
    
    public void createTrueObject(java.lang.String ih) {
        System.out.println("object table invoked: " + ih);
        file f = new fileImpl(ih);
        super.returnTrueObject(f, fileStub.iluClass(), 0);
    } //createTrueObject
    
} //MyIluObjectTable


public 
class Server {
    static xerox.ilu.IluServer trueServer;
    static serverImpl serverObject;
    
    public static void main(String argv[]) {
        java.lang.String sid = null;
        System.out.println("Create the IluServer");
        try {
            sid = xerox.ilu.Ilu.inventID();
            trueServer = xerox.ilu.IluServer.createServer(sid);
            trueServer.setObjectTable(new MyIluObjectTable());
        } catch (xerox.ilu.IluSystemException e) {
            System.err.println("Failed creating IluServer: " + e);
            System.exit(1);
        }
        System.out.println("Create the server IluObject");
        try {
            serverObject = new serverImpl();
            serverStub.registerTrueObject( 
                "----", 
                serverObject, 
                trueServer
                );
            xerox.ilu.IluSimpleBinding.publish(serverObject);
        } catch (xerox.ilu.IluSystemException e) {
            System.err.println("Failed creating server object: " + e);
            System.exit(1);
        }
        System.out.println("server object published");
        System.out.println("SID is " + sid );
    } //main 
    
} //Server

