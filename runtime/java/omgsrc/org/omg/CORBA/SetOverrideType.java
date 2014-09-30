/*
 * Class for corba compatibility
 * contents is NOT completely corba compatible *implements*
 */
/* SetOverrideType.java */
/* Chris Jacobi, November 24, 1998 9:55 am PST */
/* $Id: SetOverrideType.java,v 1.1 1998/11/24 18:04:22 jacobi Exp $ */

package org.omg.CORBA;

public class SetOverrideType /*implements org.omg.CORBA.portable.IDLEntity*/ {
    
    public static final int _SET_OVERRIDE = 0;

    public static final int _ADD_OVERRIDE = 1;

    public static final SetOverrideType SET_OVERRIDE = 
        new SetOverrideType(_SET_OVERRIDE);

    public static final SetOverrideType ADD_OVERRIDE = 
        new SetOverrideType(_ADD_OVERRIDE);

    public int value() {
        return _value;
    }

    public static SetOverrideType from_int(int i) throws org.omg.CORBA.BAD_PARAM
    {
        switch (i) {
            case _SET_OVERRIDE:
                return SET_OVERRIDE;
            case _ADD_OVERRIDE:
                return ADD_OVERRIDE;
            default:
	        throw new org.omg.CORBA.BAD_PARAM();
        }
    }

    protected SetOverrideType(int _value){
        this._value = _value;
    }

    private int _value;
    
} //SetOverrideType
