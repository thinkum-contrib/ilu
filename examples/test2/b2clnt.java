/* $Id: b2clnt.java,v 1.5 1999/08/03 01:57:38 janssen Exp $
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
/* Chris Jacobi, May 4, 1998 5:39 pm PDT */
/* Last edited by Mike Spreitzer October 9, 1998 1:36 pm PDT */

/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */
/* THIS HAS NOT BEEN DEBUGGED */

package Test;
 
/** 
 * Main class uses same name as c program important because 
 * I have no idea what it is supposed to do and therefore the
 * association to its origin is necessary to figure out whether
 * this program does what it should...
 *
 * Don't use this as example java code.
 */ 
public class b2clnt {
    
    static void usage() {
        System.err.println(
            "Usage: java Test.b2clnt pCall bTimeout pPush len nCalls [-r] [-v]"
            );
        System.err.println("   times in milliseconds");
        System.exit(1);
    } //usage
    

    static void usage(Throwable t) {
        if (t!=null) {
            System.err.println(t);
            t.printStackTrace(System.err);
        }
        usage();
    } //usage
    
    
    public static void main(String argv[]) {
        long pCallTime = 0;
        long bTimeoutTime = 0;
        long pPushTime = 0;
        int len = 0;
        int nCalls = 1000000000;
        boolean verbose = false;
        boolean retry = false;
        System.out.println("starting b2clnt.main");
        //
        //read in command line arguments
        try {
            if (argv.length<5) {
                usage();
            }
            pCallTime = (new Long(argv[0])).intValue();
            bTimeoutTime = (new Long(argv[1])).intValue();
            pPushTime = (new Long(argv[2])).intValue();
            len = (new Integer(argv[3])).intValue();
            nCalls = (new Integer(argv[4])).intValue();
            if (nCalls<=0) {nCalls = 1000000000;}
            for (int i = 5; i<argv.length; i++) {
                if ("-v".equalsIgnoreCase(argv[i])) {
                    verbose = true;
                } else if ("-r".equalsIgnoreCase(argv[i])) {
                    retry = true;
                } else {
                    usage();
                }
            }
        } catch (NumberFormatException ne) {
            usage(ne);
        }
        System.out.println(
           "got the input: pCallTime="  + pCallTime 
           + " bTimeoutTime=" + bTimeoutTime
           + " pPushTime=" + pPushTime
           + " len=" + len
           + " nCalls=" + nCalls
           + " verbose=" + verbose
           + " retry=" + retry);
        //
        //invoke operations on proper thread
        BThread bt = new BThread(
            pCallTime, bTimeoutTime, pPushTime, len, nCalls, verbose, retry
            );
        bt.start();
        //
        //using join to be able to print the "done" message
        System.out.println("Waiting to join...");
        try {
            bt.join();
        } catch (java.lang.InterruptedException ie) {
            System.err.println("InterruptedException caught by main: " + ie);
            ie.printStackTrace(System.err);
            System.exit(1);
        }
        System.out.println("main is done");
    } //main
    
} //b2clnt


class CallBackImpl implements Batcher2.CallBack {
    
    public void Reply(long r, int x, long s) {
        System.out.println(
            "Reply(" + r  
            + ", " + x
            + ", " + s
            +")"
            );
            
    } //Reply

} //CallBackImpl


class PushThread extends Thread {
    
    BThread bt;
    
    PushThread(BThread bt) {
        this.bt = bt;
    } //constructor
    
    /** PushNow */
    public void run() {
        System.out.println("Started PushThread");
        while (! bt.done) {
            long now = System.currentTimeMillis();
            if (bt.verbose) {
                System.out.println("PushNow(" + now + ")");
            }
            bt.doPush(bt.batcher);
            try {
                Thread.sleep(bt.pPushTime);
            } catch (java.lang.InterruptedException ie) {
                System.out.println("InterruptedException " + ie);
            }
        }
    } //run
    
} //PushThread


/**
 * I can't decipher the c source whether this should be batching or not..
 */
class CallThread extends xerox.ilu.IluServiceThread {
    
    BThread bt;
    
    CallThread(BThread bt) {
        this.bt = bt;
    } //constructor
    
    
    public void run() {
        System.out.println("Started CallThread");
        xerox.ilu.IluBatcher.setCurrentBatcher(bt.batcher);
        xerox.ilu.IluPipeline.setCurrentPipeline(bt.pipeline);
        for (int i = 0;  i<bt.nCalls; i++) {
            long now = System.currentTimeMillis();
            if (bt.done) {return;}
            try {
                if (bt.verbose){System.out.println("do 1 request");}
                bt.svr.Request(now, i, bt.phil, bt.replyTo);
                if (bt.verbose){System.out.println("did 1 request");}
            } catch (xerox.ilu.IluSystemException e) {
                System.err.println("Push() => " + e);
                if (! bt.retry) {
                    e.printStackTrace(System.err);
                    System.exit(1);
                }
            }
            try {
                Thread.sleep(bt.pCallTime);
            } catch (java.lang.InterruptedException ie) {
                System.out.println("InterruptedException " + ie);
            }
        }
        bt.done = true;
    } //run
    
} //CallThread


/** Running as a thread to enable hidden call data */ 
class BThread extends xerox.ilu.IluServiceThread {

    long pCallTime;
    long bTimeoutTime;
    long pPushTime;
    int len;
    int nCalls;
    boolean verbose = false;
    boolean retry = false;
    
    public BThread(
            long pCallTime, 
            long bTimeoutTime, 
            long pPushTime,
            int len,
            int nCalls,
            boolean verbose,
            boolean retry) {
        this.pCallTime = pCallTime;
        this.bTimeoutTime = bTimeoutTime;
        this.pPushTime = pPushTime;
        this.len = len;
        this.nCalls = nCalls;
        this.verbose = verbose;
        this.retry = retry;
    }//constructor
    
    Batcher2.CallBack replyTo;
    Batcher2.Server svr; //a surrogate object
    byte[] phil;
    xerox.ilu.IluPipeline pipeline;
    xerox.ilu.IluBatcher batcher;
    boolean done = false;
    
    /** called as main */ 
    public void run() {
        System.out.println("Starting BThread");
        xerox.ilu.IluServer s = null;
        try {
            s = xerox.ilu.IluServer.createServer(null);
            System.out.println("Created IluServer");
            replyTo = new CallBackImpl();
            Batcher2.CallBackStub.registerTrueObject( 
                null, 
                replyTo, 
                s
                );
            System.out.println("registered replyTo");
            phil = new byte[this.len];
            svr = (Batcher2.Server) xerox.ilu.IluSimpleBinding.lookup(
                    "Batcher2-Server", 
                    "it", 
                    Batcher2.ServerStub.iluClass()
                    );
            System.out.println("got svr");
            batcher = xerox.ilu.IluBatcher.alloc(
                (int) bTimeoutTime, (pPushTime > 0L)
                );
            xerox.ilu.IluBatcher.setCurrentBatcher(batcher);
            System.out.println("setCurrentBatcher");
            pipeline = xerox.ilu.IluPipeline.alloc();
            xerox.ilu.IluPipeline.setCurrentPipeline(pipeline);
            System.out.println("setCurrentPipeline");
            //
            if (pPushTime > 0) {
                PushThread pt = new PushThread(this);
                pt.start();
            }
            //
            CallThread ct = new CallThread(this);
            ct.start();
            //
            xerox.ilu.IluBatcher.setCurrentBatcher(null);
            xerox.ilu.IluPipeline.setCurrentPipeline(null);
            //
            if (pPushTime > 0) {
                doPush(batcher);
            }
            //
            //wait some time
            long dt = 0;
            if (bTimeoutTime>0) {
                dt = pPushTime;
            } else if (pPushTime>0) {
                dt = bTimeoutTime;
            } else if (pPushTime < bTimeoutTime) {
                dt = pPushTime; //looks funny to me
            } else {
                dt = bTimeoutTime; //looks funny to me
            }
            dt = dt+10000;
            Thread.sleep(dt);
        } catch (java.lang.InterruptedException i) {
            System.err.println("InterruptedException: " + 1);
            i.printStackTrace(System.err);
            System.exit(1);
        } catch (xerox.ilu.IluSystemException e) {
            System.err.println("exception: " + e);
            e.printStackTrace(System.err);
            System.exit(1);
        }
    } //run
    
    
    void doPush(xerox.ilu.IluBatcher batcher) {
        try {
            batcher.pushBatcher();
        } catch (xerox.ilu.IluSystemException e) {
            System.err.println("Push() => " + e);
            if (!retry) {
               e.printStackTrace(System.err);
               System.exit(1); 
            }
        }
    } //doPush
        
} //BThread
