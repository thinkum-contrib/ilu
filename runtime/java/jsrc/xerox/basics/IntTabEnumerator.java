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
/* IntTabEnumerator.java  */
/* Chris Jacobi, September 3, 1997 4:05 pm PDT */

/*
 */

/* $Id: IntTabEnumerator.java,v 1.7 1999/08/03 01:53:35 janssen Exp $ */

package xerox.basics;

/**
 * An IntTabEnumerator.... <p>
 *
 * The enumeration is not synchronized with 
 * update (insert, or, delete) operations.  If
 * the IntTab is changed while an emumerations
 * is going on, the changed elements might or  
 * might not seen or might not by an enumeration.  <p>
 *
 * (That means that elements are removed or added
 *  to the chained elements, but that the chains
 *  are neither re-utilized or sorted, so that
 *  at least all other elements are stable)<p>
 *
 * Each enumeration is independent.  One particular
 * enumeration must be accessed only sequentially. <p>
 * 
 *
 * @see IntTab
 */
 
public final class IntTabEnumerator {
    private IntTab boundTo = null;
    private IntTabNode currNode = null;
    private int currIdx = -1;
    
    /** Creates an enumerator which isn't yet bound to a particular IntTab */
    public IntTabEnumerator() {
    }
    
    
    /** 
     * Constructor for a new enumeration.<p>
     * The new enumeration is not yet enabled: the client
     * need to advance before accessing the first element.
     */
    public IntTabEnumerator(IntTab tab) {
        if (tab != null) setTable(tab);
    } //IntTabEnumerator
    
    
    /** 
     * Sets this enumerator onto a new IntTab. <p>
     * An old enumeration is implicitly disabled.
     * The new enumeration is not yet enabled: the client
     * need to advance before accessing the first element.
     */
    public void setTable(IntTab tab) {
        done();
        if (tab != null) {
            synchronized (tab) {
                if (currIdx < -1) {
                    //Already finalized.
                    //This can actually occur if the finalization
                    //is part of a circular structure.  However
                    //we prefer IntTabEnumerator not to work
                    //anymore once it is finalized because who
                    //knows whether it will release inhibitCount's.
                    return;
                }
                tab.inhibitCount = tab.inhibitCount+1;
                boundTo = tab;
                currIdx = -1;
                currNode = null;
            }
        }
    } //setTable
    
    
    /**  
     * No more elementys can be accessed through this enumerator. <p>
     * If an enumeration is not advanced to the very last element, 
     * kindly call done so that the enumeration can be finalized
     * without stepping through all the way.  
     */
    public void done() {
        IntTab tab = boundTo;
        if (tab != null) {
            synchronized (tab) {
                boundTo = null;
                if (currIdx >= 0) currIdx = -1;
                currNode = null;
                tab.inhibitCount = tab.inhibitCount-1;
                if (tab.inhibitCount == 0) {
                    if (tab.size > tab.limit) tab.rehash();
                }
            }
        }
    } //done
    

    /** 
     * Sets up the next element in the enumeration. <p>
     * Returns true if there was an element, false otherwise.
     */
    public boolean advance() {
        IntTab tab = boundTo;
        IntTabNode oldNode = currNode;
        IntTabNode[] data;
        if (tab == null) return false;
        data = tab.data;
        while (true) {
            //
            // oldNode == null here
            //    we are done with the current bucket
            // oldNode != null
            //    there might or might not be more in the current bucket
            //    depending on oldNode.next
            //
            if (oldNode == null) {
                //look for the next bucket
                int newIdx = currIdx+1;
                if (newIdx < 0) return false; //already finalized
                if (newIdx < data.length) {
                    //there is another bucket; check whether it has something
                    currIdx = newIdx;
                    oldNode = data[newIdx];
                    if (oldNode != null) {
                        //bucket has a node; handle this node
                        currNode = oldNode;
                        return true;
                    }
                    //bucket is empty; loop for next bucket
                } else {
                    //there was no more bucket to look at
                    done();
                    return false; 
                }
            } else {
                //oldNode != null and has been handled
                //check whether there is another node in bucket
                oldNode = oldNode.next;
                if (oldNode != null) {
                    //got a node; handle this node
                    currNode = oldNode;
                    return true;
                }
                //no more nodes in bucket; loop for next bucket
            }
        }
    } //advance
    
    
    /** 
     * Returns the current key of the enumeration.
     */
    public int currKey() {
        IntTabNode n = currNode;
        if (n != null) return n.key;
        return -1;
    } //currKey
    
    
    /** 
     * Returns the current element of the enumeration.
     */
    public Object currVal() {
        IntTabNode n = currNode;
        if (n != null) return n.val;
        return null; 
    } //currVal
    
    
    protected final void finalized() {
        done();
        currIdx = -100;  //not monitored but is not matter of correctness
    } //finalized
    
} //IntTabEnumerator 

