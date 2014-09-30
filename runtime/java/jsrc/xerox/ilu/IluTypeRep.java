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
/* IluTypeRep.java */
/* Chris Jacobi, December 23, 1998 12:33 pm PST */
/* $Id: IluTypeRep.java,v 1.19 1999/08/03 01:53:43 janssen Exp $ */

/* 
 * See also IluJava_IluPickle.c
 */

package xerox.ilu;

/**
 * Registration of types with Ilu.  Stub visible.
 * See ilutypes.h::ilu_Type.
 * @see IluTypeKind
 * @see IluTypeCode
 */
 
/* security note:  this should be final but it will extended.
 * This is countered by making all constructors either friendly or private.
 */
public class IluTypeRep {

    /*friendly*/ long yIluType = 0;
    /*friendly*/ IluTypeKind ilutk = null;
    /*friendly*/ java.lang.String name = null; 
    /*friendly*/ java.lang.String islIfName = null;
    /*friendly*/ java.lang.String islIfBrand = null;
    /*friendly*/ java.lang.String uid = null;


    private IluTypeRep() {
        super();
    } //constructor
    
    
    /*friendly*/ 
    /** 
     * Not public to prevent creation of bogus IluTypeRep's
     */
    IluTypeRep(IluTypeKind code){
        ilutk = code;
    } //constructor
    
    
    /** Not likely to be usefull to applications*/ 
    public void assertUnfinished() {
        if (yIluType != 0) {
            throw new IluSystemExceptionBase("can't redefine IluTypeRep"); 
        }
    } //assertUnfinished
    
    
    /** Not likely to be usefull to applications*/ 
    public void assertType(IluTypeKind t) {
        if (this.ilutk != t) {
            throw new IluSystemExceptionBase("buh"); 
        }
    } //assertType
    
    
    /* Used for records and enumerations and a few more types */
    /*friendly*/ native 
    void nativeRegisterSome(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        int cnt, 
        int ilutk, 
        java.lang.String base, 
        int[] dims
        );
        
    /*friendly*/ native 
    void nativeRegisterUnionBase(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String discriminatUID, 
        int armCnt, 
        int defaultArm, 
        int othersAllowed, 
        int cvkIndex
        );
        

    /** 
     * Stub only.
     */
    public static IluTypeRep registerSequenceType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String baseUID, 
        int limit  
        ) 
    {
        IluTypeRep t = new IluTypeRep(IluTypeKind.sequence_tk);
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.nativeRegisterSome(name, islIfName, islIfBrand, uid, 
            limit, t.ilutk.value(), baseUID, null);
        return t;
    } //registerSequenceType


    /** 
     * Stub only.
     */
    public static IluTypeRep registerArrayType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String baseUID, 
        int dimcnt, 
        int[] dims 
        ) 
    {
        if (dims.length != dimcnt) {
            throw new IluSystemExceptionBase("bad dimensions");
        }
        IluTypeRep t = new IluTypeRep(IluTypeKind.array_tk);
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.nativeRegisterSome(name, islIfName, islIfBrand, uid, 
            dimcnt, t.ilutk.value(), baseUID, dims);
        return t;
    } //registerArrayType


    /** 
     * Stub only.
     */
    public static IluTypeRep registerOptionalType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String baseUID  
        ) 
    {
        IluTypeRep t = new IluTypeRep(IluTypeKind.optional_tk);
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.nativeRegisterSome(name, islIfName, islIfBrand, uid, 
            0, t.ilutk.value(), baseUID, null);
        return t;
    } //registerOptionalType


    /** 
     * Stub only.
     */
    public static IluTypeRep registerAliasType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String baseUID  
        ) 
    {
        IluTypeRep t = new IluTypeRep(IluTypeKind.alias_tk);
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.nativeRegisterSome(name, islIfName, islIfBrand, uid, 
            0, t.ilutk.value(), baseUID, null);
        return t;
    } //registerAliasType


    /** 
     * Stub only.
     */
    public static IluTypeRep registerRecordType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        int cnt
        ) 
    {
        IluTypeRep_Record t = new IluTypeRep_Record();
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.cnt = cnt;
        return t;
    } //registerRecordType

    /** 
     * Stub only.
     * To be overloaded. 
     */
    public 
    void registerRecordField(int index, 
        java.lang.String fldIslName, 
        java.lang.String fldUID
        ) {
        throw new IluSystemExceptionBase("overload!");
    } //registerRecordField 

    /*friendly*/  native 
    void nativeRegisterRecordField(
        int index, 
        java.lang.String fldIslName, 
        java.lang.String fldUID
        );
    
    
    /** 
     * Stub only.
     */
    public static IluTypeRep registerEnumerationType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        int cnt
        ) 
    {
        IluTypeRep_Enum t = new IluTypeRep_Enum();
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.cnt = cnt;
        return t;
    } //registerEnumerationType
    
    /*friendly*/  native 
    void nativeRegisterEnumerationElement(
        int elNum, 
        java.lang.String elIslName, 
        int elVal
        );
    
    /** 
     * Stub only.
     * To be overloaded. 
     */
    public void registerEnumerationElement(
            int elNum, 
            java.lang.String elIslName, 
            int elVal
            ) {
        throw new IluSystemExceptionBase("overload!");
    } //registerEnumerationElement
    
    
    /** 
     * Stub only.
     */
    public static IluTypeRep registerUnionType(java.lang.String name, 
        java.lang.String islIfName, 
        java.lang.String islIfBrand, 
        java.lang.String uid, 
        java.lang.String discriminatUID, 
        int cnt, 
        int defaultArm, 
        boolean othersAllowed, 
        xerox.ilu.IluConstantValueKind cvk
        ) 
    {
        IluTypeRep_Union t = new IluTypeRep_Union();
        t.name = name;
        t.islIfName = islIfName;
        t.islIfBrand = islIfBrand;
        t.uid = uid;
        t.discriminatUID = discriminatUID;
        t.cnt = cnt;
        t.defaultArm = defaultArm;
        if (othersAllowed) t.othersAllowed = 1;
        t.cvkIndex = cvk.value();
        return t;
    } //registerUnionType
    
    
    /** 
     * Stub only.
     * To be overloaded. 
     */
    public void registerUnionArm(
            int armNum, 
            java.lang.String islArmName, 
            java.lang.String armUID,
            int valNum, 
            int[] ivals, 
            java.lang.String[] svals) {
        throw new IluSystemExceptionBase("overload!");
    } //registerUnionArm
    
    /*friendly*/  native 
    void nativeRegisterUnionArm(
        int armNum, 
        java.lang.String islArmName, 
        java.lang.String armUID, 
        int valnum, 
        int[] ivals, 
        java.lang.String[] svals, 
        int cvkIndex);


    /** 
     * Stub only.
     * To be overloaded 
     */
    public void finish() {
        /* 
         * The reason we build a data structure and later walk it
         * in the finish method is security.  This way malicious 
         * stubs might register useless types but won't get stuck 
         * with a kernel lock held... 
         */
        throw new IluSystemExceptionBase("overload!");
    } //finish


} //IluTypeRep



/*friendly*/ 
class IluTypeRep_baseWithChain extends IluTypeRep {
    IluTypeRep_ChainEl first = null;
    IluTypeRep_ChainEl last = null;
    int cnt = 0;
    
    /*friendly*/ IluTypeRep_baseWithChain(IluTypeKind code){
        super(code);
    }
    
    /* not synchronized */
    void append(IluTypeRep_ChainEl x) {
        assertUnfinished();
        x.next = null;
        if (this.last != null) {
            this.last.next = x;
        } else {
            this.first = x;
        }
        this.last = x;
    } //append
    
} //IluTypeRep_baseWithChain



/*friendly*/ 
class IluTypeRep_Record extends IluTypeRep_baseWithChain {
    
    /*friendly*/ IluTypeRep_Record(){
        super(IluTypeKind.record_tk);
    }
        
    /* not synchronized; but assign-once */
    public void registerRecordField(
            int index, 
            java.lang.String fldIslName, 
            java.lang.String fldUID) {
        IluTypeRep_RecordFld fld = new IluTypeRep_RecordFld();
        fld.index = index;
        fld.fldIslName = fldIslName;
        fld.fldUID = fldUID;
        this.append(fld);
    } //registerRecordField


    /* not synchronized; but assign-once */
    public void finish() {
        assertUnfinished();
        this.nativeRegisterSome(this.name, 
            this.islIfName, this.islIfBrand, this.uid, 
            this.cnt, this.ilutk.value(), null, null
            );
        if (this.yIluType == 0) {
            this.yIluType = -1;
        } else {
            IluTypeRep_RecordFld fld = (IluTypeRep_RecordFld) this.first;
            while (fld != null) {
                this.nativeRegisterRecordField(
                    fld.index, fld.fldIslName, fld.fldUID
                    );
                fld = (IluTypeRep_RecordFld) fld.next;
            }
            this.nativeRegisterRecordField(-1, null, null);
        }
    } //finish

} //IluTypeRep_Record



/*friendly*/ 
class IluTypeRep_Enum extends IluTypeRep_baseWithChain {
    
    /*friendly*/ IluTypeRep_Enum(){
        super(IluTypeKind.enumeration_tk);
    }
        
    /* not synchronized; but assign-once */
    public void registerEnumerationElement(
            int elNum, java.lang.String elIslName, int elVal) {
        assertUnfinished();
        IluTypeRep_EnumEl el = new IluTypeRep_EnumEl();
        el.elNum = elNum;
        el.elIslName = elIslName;
        el.elVal = elVal;
        this.append(el);
    } //registerEnumerationElement


    /* not synchronized; but assign-once */
    public void finish() {
        int checkCount = 0;
        IluTypeRep_EnumEl el;
        //check correctness
        el = (IluTypeRep_EnumEl) this.first;
        while (el != null) {
            if (el.elNum != checkCount) {
                throw new IluSystemExceptionBase("bad enumeration element");
            }
            el = (IluTypeRep_EnumEl) el.next;
            checkCount = checkCount+1;
        }
        if (checkCount != this.cnt) {
            throw new IluSystemExceptionBase("bad element count");
        }
        assertUnfinished();
        //ok, start registering
        this.nativeRegisterSome(this.name, 
            this.islIfName, this.islIfBrand, this.uid, 
            this.cnt, this.ilutk.value(), null, null
            );
        if (this.yIluType == 0) {
            this.yIluType = -1;
        } else {
            el = (IluTypeRep_EnumEl) this.first;
            while (el != null) {
                this.nativeRegisterEnumerationElement(
                    el.elNum, el.elIslName, el.elVal
                    );
                el = (IluTypeRep_EnumEl) el.next;
            }
            this.nativeRegisterEnumerationElement(-1, null, -1);
        }
    } //finish

} //IluTypeRep_Enum



/*friendly*/ 
class IluTypeRep_Union extends IluTypeRep_baseWithChain {
    /*friendly*/ int defaultArm = 0;
    /*friendly*/ int othersAllowed = 0;
    /*friendly*/ int cvkIndex = 0;
    /*friendly*/ java.lang.String discriminatUID = null;
    
    /*friendly*/ IluTypeRep_Union(){
        super(IluTypeKind.union_tk);
    }
        
    /* not synchronized; but assign-once */
    public void registerUnionArm(
            int armNum, 
            java.lang.String islArmName, 
            java.lang.String armUID, 
            int valNum, 
            int[] ivals, 
            java.lang.String[] svals) {
        assertUnfinished();
        IluTypeRep_UnionArm arm = new IluTypeRep_UnionArm();
        arm.armNum = armNum;
        arm.islArmName = islArmName;
        arm.armUID = armUID;
        arm.valNum = valNum;
        arm.ivals = ivals;
        arm.svals = svals;
        this.append(arm);
    } //registerUnionArm


    /* not synchronized; but assign-once */
    public void finish() {
        assertUnfinished();
        this.nativeRegisterUnionBase(this.name, 
            this.islIfName, this.islIfBrand, this.uid, 
            this. discriminatUID, this.cnt, this.defaultArm, 
            this.othersAllowed, this.cvkIndex
            );
        if (this.yIluType == 0) {
            this.yIluType = -1;
        } else {
            IluTypeRep_UnionArm el = (IluTypeRep_UnionArm) this.first;
            while (el != null) {
                this.nativeRegisterUnionArm(
                    el.armNum, el.islArmName, el.armUID,
                    el.valNum, el.ivals, el.svals, this.cvkIndex
                    );
                el = (IluTypeRep_UnionArm) el.next;
            }
            this.nativeRegisterUnionArm(-1, null, null, -1, null, null, -1);
        }
    } //finish

} //IluTypeRep_Union



/*friendly*/ 
class IluTypeRep_ChainEl {
    IluTypeRep_ChainEl next = null;
} //IluTypeRep_EnumEl



/*friendly*/ 
class IluTypeRep_EnumEl extends IluTypeRep_ChainEl {
    int elNum;
    java.lang.String elIslName;
    int elVal;
} //IluTypeRep_EnumEl



/*friendly*/ 
class IluTypeRep_RecordFld extends IluTypeRep_ChainEl {
    int index;
    java.lang.String fldIslName;
    java.lang.String fldUID;
} //IluTypeRep_RecordFld



/*friendly*/ 
class IluTypeRep_UnionArm extends IluTypeRep_ChainEl {
    int armNum = 0;
    java.lang.String islArmName = null;
    java.lang.String armUID = null;
    int valNum = 0;
    int[] ivals = null;
    java.lang.String[] svals = null;
} //IluTypeRep_UnionArm


/* end */

