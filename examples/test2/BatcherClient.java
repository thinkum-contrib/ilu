/* $Id: BatcherClient.java,v 1.7 1999/08/03 01:57:41 janssen Exp $
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
/* Chris Jacobi, May 4, 1998 5:43 pm PDT */
/* Last edited by Mike Spreitzer October 9, 1998 1:36 pm PDT */

/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */
/* THIS HAS NOT BEEN TESTED */

/*
 * Run this like
 * java Batcher.BatcherClient 3 3 v
 */ 

package Batcher;

public class BatcherClient {
    
    static boolean verbose = false;
    
    public static void usage() {
        System.err.println(
                "usage: java Batcher.BatcherClient number number [v]");
        System.exit(1);
    }
    
    public static void main(String argv[]) {
        if (argv.length < 2) {
            usage();
        }
        if (argv.length > 2) {
            if (argv[2].indexOf('v')>=0) verbose = true;
            if (argv[2].indexOf('V')>=0) verbose = true;
        }
        int i;
        int j;
        int k;
        long now;
        Batcher.T t;
        Batcher.TimeRec[] ans;
        String serverId = "Batcher-Server";
        String ih = "it";
        String nsendsString = argv[0];
        String nsyncsString = argv[1];
        int nsends = Integer.valueOf(nsendsString).intValue();
        int nsyncs = Integer.valueOf(nsyncsString).intValue();
        System.out.println("Starting BatcherClient");
        Batcher._allJavaStubs.load();
        try {
            t = (T) xerox.ilu.IluSimpleBinding.lookup(
                serverId, ih, Batcher.TStub.iluClass()
                );
            for (i = 0; i < nsyncs; i++) {
                for (j = 0; j < nsyncs; j++) {
                    now = System.currentTimeMillis();
                    if (verbose) {
                        System.out.println("Sending " + now);
                        }
                    t.Send(now);
                    try {
                        Thread.sleep(1000);
                    } catch(java.lang.InterruptedException ie) {
                    }
                } //for-j
                now = System.currentTimeMillis();
                if (verbose) {
                    System.out.println("Syncing " + now);
                }
                ans = t.Sync(now);
                System.out.println("Ans(" + i + ") = " + ans.length);
                for (k = 0; k < ans.length; k++) {
                    System.out.println(" " + ans[k].s + "->" + ans[k].r);
                } //for-k
                System.out.println();
            } //for-i
        } catch (xerox.ilu.IluSystemException e) {
            System.err.println("IluSystemException caught by main: " + e);
        }
        System.out.println("done");
    } //main
    
} //BatcherClient

