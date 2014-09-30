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
/* IntTab.java  */
/* Chris Jacobi, September 3, 1997 4:03 pm PDT */

/*
 */

/* $Id: IntTab.java,v 1.13 1999/08/03 01:53:38 janssen Exp $ */

package xerox.basics;

/**
 * An IntTab is a hash table which associates int keys with objects. <p>
 *
 * Sort of like Hashtable but with keys of type int.  Given that it
 * is kind of a variant of a standard class, there is no need for
 * total completeness. I.e. If users really need enumeration they
 * can use Hashtable with Integer's as keys. <p>
 *
 * Operations are synchronized on the IntTab's own
 * lock.  Composed operations can therefore be synchronized by using  
 * external synchronized statements.
 *
 * @see java.lang.Hashtable
 * @see IntTabEnumerator
 */
 
public final class IntTab implements Cloneable {
    int size = 0;	//Number of entries in table
    int dSize = 0;	//Modulus (or size of data)
    int limit = 0;	//Rehash if more then limit entries in table
    int inhibitCount = 0; //so enumerators can inhibit rehash...
    IntTabNode[] data;	
    Object noneFound = null;
    
    /** 
     * Creates new empty IntTab.
     * @param estimateCnt estimate for number of elements; zero is fine.
     * @param ifNoneFound sentinel value: returned when no entry is found.
     */
    public IntTab(int estimateCnt, Object ifNoneFound) {
        if (estimateCnt<1) estimateCnt = 1;
        size = 0;
        dSize = goodModulus(estimateCnt);
        limit = dSize;
        data = new IntTabNode[dSize]; 
        this.noneFound = ifNoneFound;
    } //constructor
    
    /** 
     * Creates new empty IntTab.  
     * Uses null for ifNoneFound sentinel 
     */
    public IntTab() {
        this(2, null);
    } //constructor

    /** Current number of key-value associations in IntTab */
    public int size() {
        return size;
    }
    
    /** 
     * returns the ifNoneFound sentinel used at creation of IntTab 
     */
    public Object noneFoundMarker() {
        return noneFound;
    }
    
    
    /** 
     * Like fetch but without using synchronized.<p>
     * Use this for sequential operations only.
     * If any other mutating operation is performed
     * this may fail in an unspecified way, but it does
     * never modify the IntTab.
     */
    public Object unmonitoredFetch(int key) {
        IntTabNode node = data[(key & 0x7FFFFFFF) % dSize];
        while (node != null) {
            if (key == node.key) return node.val;
            node = node.next;
        }
        return noneFound;
    } //unmonitoredFetch
    
    
    /** 
     * Fetches the object associated with key.<p>
     * If no object is found, returns the ifNoneFound sentinel value.
     */
    public synchronized Object fetch(int key) {
        return unmonitoredFetch(key);
    } //fetch
    
    
    private /*internal*/ final void 
    ins(int idx, int key, Object val) {
        IntTabNode n = new IntTabNode();
        n.key = key; n.val = val; n.next = data[idx];
        data[idx] = n;
        size = size + 1; 
        if (inhibitCount == 0 && size>limit) rehash();
    } //ins
    
    
    /** 
     * Inserts new key-value pair. <p>
     * Does not override a previous association.  Returns 
     * true if new value is inserted.
     */
    public synchronized boolean insert(int key, Object val) {
        int idx = (key & 0x7FFFFFFF) % dSize;
        IntTabNode node = data[idx];
        while (node != null) {
            if (key == node.key) return false;
            node = node.next;
        }
        ins(idx, key, val);
        return true;
    } //insert
    
    
    /** 
     * Inserts new key-value pair. <p>
     * Returns previous value for key or the ifNoneFound sentinel
     * if there was no previous value for key. 
     */
    public synchronized Object store(int key, Object val) {
        int idx = (key & 0x7FFFFFFF) % dSize;
        IntTabNode node = data[idx];
        while (node != null) {
            if (key == node.key) {
                Object old = node.val;
                node.val = val;
                return old;
            }
            node = node.next;
        }
        ins(idx, key, val);
        return noneFound;
    } //store
    
    
    /** 
     * Removes new key-value pair. <p>
     * Returns previous value for key or the ifNoneFound sentinel 
     * if there was no previous value for key.
     */
    public synchronized Object delete(int key) {
        int idx = (key & 0x7FFFFFFF) % dSize;
        IntTabNode node = data[idx];
        IntTabNode lag = null;
        while (node != null) {
            if (key == node.key) {
                Object old = node.val;
                if (lag==null) {
                    data[idx] = node.next;
                } else {
                    lag.next = node.next;
                }
                size = size - 1;
                return old;
            }
            lag = node;
            node = node.next;
        }
        return noneFound;
    } //delete


    /** 
     * Removes all key-value pairs: IntTab will become empty.
     */
    public synchronized void erase() {
        for (int i = 0; i<dSize ; i++) {
            data[i] = null;
        }
        size = 0;
    } //erase
    
    
    /*friendly*/ /*internal*/ void rehash() {
        IntTabNode[] oldData = data;
        IntTabNode[] newData;
        int oldDSize = dSize;
        int newDSize = goodModulus(size);
        if (newDSize<=oldDSize) {
            //Either maximum size reached, or, goodModulus function
            //has been changed so that it would allow fewer buckets. 
            limit = limit * 2; //can't overflow; runs out of memory before
            return;
        }
        newData = new IntTabNode[newDSize];
        for (int i = 0 ; i<oldDSize ; i++) {
            IntTabNode node = oldData[i];
            while (node != null) {
                IntTabNode next = node.next;
                int idx = (node.key & 0x7FFFFFFF) % newDSize;
                node.next = newData[idx];
                newData[idx] = node;
                node = next;
            }
        }
        limit = newDSize;
        data = newData;
        dSize = newDSize;
    } //rehash
    

    /** Returns a new shallow copy */
    public Object clone() {
        return copy();
    }
    
    
    /** 
     * Creates a copy of the IntTab.  
     * This creates a new table but shares the "values". (Shallow copy) 
     */
    public synchronized IntTab copy() {
        IntTab newTab = new IntTab(size, noneFound);
        for (int i = 0 ; i<dSize ; i++) {
            IntTabNode node = data[i];
            while (node != null) {
                int idx = (node.key & 0x7FFFFFFF) % newTab.dSize;
                Object val = node.val;
                newTab.ins(idx, node.key, val);
                node = node.next;
            }
        }
        return newTab;
    } //copy


    /** 
     * Returns a modulus which is well suited for cnt element 
     */
    static int goodModulus(int cnt) {
        //Because we are using prime numbers we are expecting
        //a rather nice distribution.  The bucket array uses  
        //the same order of magnitude of memory as the 
        //stored objects, therefore we don't want to make those 
        //arrays any smaller but rather have short overflow lists.   
        int pidx = 0;
        if (cnt>=maxPrime) return maxPrime;
        while (cnt>primeTable[pidx]) {
            pidx++;
        }
        return primeTable[pidx];
    } //goodModulus
   
    
    /* Good prime numbers for the bucket allocations */
    static private final int[] primeTable = {
        2,  5,  11,  23,  53,  113,  251,      
        509,  1019,  2039,  4079,  8179,  16369,  32749
        };
    static private final int maxPrime = 32749;
    
} //IntTab 


/*friendly*/ class IntTabNode {
    int key;
    Object val;
    IntTabNode next;
} //IntTabNode
