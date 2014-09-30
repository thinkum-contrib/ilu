/* IluAlarmTech.java */
/* Chris Jacobi, December 23, 1998 11:49 am PST */
/* 
 * $Id: IluAlarmTech.java,v 1.22 1999/08/03 01:53:48 janssen Exp $
 * See also IluJava_IluAlarmTech.c
 */
 
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

/*friendly*/ 

/**
 * Class to implement the Java side of ILU alarms... <br>
 * No Java clients anticipated.
 */
 
final
class IluAlarmTech extends java.lang.Thread {
    
    private static int priority = 
        IluEnvironment.getIntProp(
            "ilu.alarmpriority", java.lang.Thread.MAX_PRIORITY-1);
    private static long count = 0;
    private static java.util.Hashtable noGC = new java.util.Hashtable();
    private static int traceAlarms = 999;
    
    private long wakeupAt = 0;
    private int x1; private int x2; //x1 .. x4 represent the proc and the rock 
    private int x3; private int x4;
    private boolean keepAlive;
    
   
      
    /** Native code needs an instance of an IluAlarmTech
     *  which will not be garbage collected.
     */
    private native void giveInstance();
    private static IluAlarmTech prototype = null; //must not be gc'ed
    
    /*friendly*/ static void initPhase2() {
        traceAlarms = IluDebug.traceAlarms();
        prototype = new IluAlarmTech();
        prototype.giveInstance();
        xerox.basics.VMExtras.makeGCRoot(prototype);
    }
   
    /* Loads the code and all of ilu if necessary */
    public static void init() {
        IluInit.init();
    }

    /* returns and increments the classes object count */
    private static final synchronized long incrementCount() {
        return count++;
    }

 
    /* Constructor; private because real creation is done in createAlarm */
    private IluAlarmTech() {
        super("ILU_Alarm " + incrementCount());
        keepAlive = true;
        noGC.put(this, this);
    } //constructor
   
    /** 
     * Real creator; (called from C) to create alarm <p>
     * If you wonder why this isn't static: I couldn't get 
     * invoking static methods to work from the native side,
     * so I made this dynamic.
     */
    IluAlarmTech createAlarm() {
        IluAlarmTech at = new IluAlarmTech();
        at.setDaemon(true);
        at.setPriority(priority);
        at.wakeupAt = 0;
        at.start();
        return at;
    } //createAlarm
   
    /* (called from C side) when implementing ml_set_alarm */
    void setAlarm (int sec, int milli, int x1, int x2, int x3, int x4) {
        synchronized (this) {
            this.x1 = x1; this.x2 = x2;
            this.x3 = x3; this.x4 = x4;
            long time = ((long) sec) * 1000 + milli;
            /*$ idb */  if (traceAlarms > 1) {
            /*$ idb */      IluDebug.log.println("! IluAlarmTech setAlarm "  
            /*$ idb */          + time + " " + this);
            /*$ idb */  }
            wakeupAt = time + java.lang.System.currentTimeMillis();
            if (wakeupAt == 0) {wakeupAt = 1;}
            this.notifyAll();
        }
    } //setAlarm
   
    /* (called from C side) when implementing ml_unset_alarm */
    void unsetAlarm () {
        synchronized (this) {
            /*$ idb */  if (traceAlarms > 1) {
            /*$ idb */      IluDebug.log.println("! IluAlarmTech unsetAlarm " 
            /*$ idb */          + this);
            /*$ idb */  }
            wakeupAt = 0;
            this.x1 = 0; this.x2 = 0;
            this.x3 = 0; this.x4 = 0;
            this.notifyAll();
        }
    } //unsetAlarm
   
    /* (called from C side) when implementing ml_destroy_alarm */
    void destroyAlarm () {
        keepAlive = false;
        unsetAlarm();
        noGC.remove(this);
    } //destroyAlarm
   

    /* For java to call C and finally execute the C procedure variable */
    private native void wakeIlu(int x1, int x2, int x3, int x4);
   
    /* The thread which invokes the alarms */
    public void run () {
        boolean call = false;
        int localx1 = 0; int localx2 = 0;
        int localx3 = 0; int localx4 = 0;
        /*$ idb */  if (traceAlarms > 0) {
        /*$ idb */      IluDebug.log.println("! IluAlarmTech create " + this);
        /*$ idb */  }
        while (keepAlive) {
            synchronized (this) {
                while (wakeupAt==0 && keepAlive) {
                    //we are not required to wake up at a certain time
                    try {
                        this.wait();
                    } catch (java.lang.InterruptedException e) {
                    }
                } 
                long time = java.lang.System.currentTimeMillis();
                long difference = wakeupAt - time;
                if (difference>0) {
                    try {
                        this.wait(difference);
                        //Go to the top of the loop to check whether
                        //we need to wait more...
                    } catch (java.lang.InterruptedException e) {
                    }
                } else if (wakeupAt>0) {
                    localx1 = x1; localx2 = x2;
                    localx3 = x3; localx4 = x4;
                    call = true;
                }
            }
            if (call) {
                /*$ idb */  if (traceAlarms > 1) {
                /*$ idb */      IluDebug.log.println("! IluAlarmTech wake it " 
                /*$ idb */          + this);
                /*$ idb */  }
                wakeIlu(localx1, localx2, localx3, localx4);
                call = false;
                this.yield();
            } 
        }
    } //run
   
} // IluAlarmTech

