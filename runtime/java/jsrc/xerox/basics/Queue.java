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
/* Queue.java  */
/* Chris Jacobi, July 29, 1996 6:10 pm PDT */

/*
 */
 
/* $Id: Queue.java,v 1.3 1999/08/03 01:53:37 janssen Exp $ */

package xerox.basics;

/**
 * A simple queue class.   <p>
 * Synchronization is up to clients.  (The class is not synchronized). <br>
 *
 * @author Chris Jacobi
 */

public final class Queue {
   //Index-numbers grow from left to right.
   //Asymmetric so that right >left means there are right-left elements in queue
   //and left=right means empty (buffer is never filled up all the way).
   private int left; //left-most element IN queue
   private int right; //right-most element NOT in queue
   private Object [] a;

   /** constructor for new, empty Queue */
   public Queue() {
       a = new Object[8]; right = 0; left = 0;
   }

   /** returns whether queue is empty */
   public final boolean empty() {
       return (right==left);
   }

   /** returns the number of elements in queue */
   public final int size() {
       return (right+a.length-left) % a.length;
   }

   /** returns the left most element of queue without removing it */ 
   public final Object peekLeft() {
      if (right==left) return null; 
      return a[left];
   }

   /** returns the right most element of queue without removing it */ 
   public final Object peekRigth() {
      if (right==left) return null; 
      int r = (right+a.length-1) % a.length;
      return a[r];
   }

   /** inserts (enqueues) new item at the left end of queue */
   public final void insertLeft(Object item) {
       int nxt = (right+1) % a.length;
       if (nxt==left) { //full
           assertBuffer(a.length + a.length/2); 
       }
       left = (left+a.length-1) % a.length;
       a[left] = item;
   }

   /** insert (enqueues) new item at the right end of queue */
   public final void insertRight(Object item) {
       int nxt = (right+1) % a.length;
       if (nxt==left) { //full
           assertBuffer(a.length + a.length/2); nxt = (right+1) % a.length;
       }
       a[right] = item;
       right = nxt;
   }
   
   /** removes and returns the left-most item from queue */
   public final Object removeLeft() {
       if (right==left) return null; //empty...
       Object item = a[left]; a[left] = null;
       left = (left+1)%a.length;
       return item;
   }

   /** removes and returns the right-most item from queue */
   public final Object removeRight() {
       if (right==left) return null; //empty...
       right = (right+a.length-1)%a.length;
       Object item = a[right]; a[right] = null;
       return item;
   }

   /** 
    * Replaces contents with contents of "from".<br>
    * Results in sharing the elements but leaves the queues completely
    * independent. 
    */
   public final void shallowCopy(Queue from) {
      int sz = from.size();
      this.a = new Object[(sz<4)? 4 : sz];
      this.left = 0;
      this.right = sz;
      from.myCopy(this.a);
   }
   
   /* error if array "into" is not large enough */
   private final void myCopy(Object [] into) {
      int sz = size();
      if (right>left) { //middle is good stuff  
          System.arraycopy(this.a, left, into, 0, sz);
      }
      if (right<left) { //middle is gap
          System.arraycopy(this.a, left, into, 0, a.length-left);
          if (right>0) System.arraycopy(this.a, 0, into, a.length-left, right);
      }
   }

   /** 
    * Allocates enough room in internal buffer to insert n more entries 
    * without the need of further memory allocations. <br>
    * Returns itself for conveniance.
    */
   public final Queue assertBuffer(int n) {
       int sz = size();
       if ((sz+n) >= a.length) {
           if (n<256 && n<sz/2) n = sz/2; //prevent too smallish increases
           Object [] aa = new Object[sz+n];
           myCopy(aa);
           left = 0; right = sz; a = aa;
       }
       return this;
   }

} //Queue 

