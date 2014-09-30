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
/* IluSBH.java */
/* Chris Jacobi, December 23, 1998 12:18 pm PST */
/* $Id: IluSBH.java,v 1.6 1999/08/03 01:53:42 janssen Exp $ */

 
package xerox.ilu;

/**
 * String binding handle access<p>
 *
 * Parsing or composing SBH's takes the arguments and
 * builds and IluSBH structure.  The desired information
 * then can be accessed with accessor functions. <p>
 *
 * Currently IluSBH structure are immutable, however
 * you should not rely on this immutability: future 
 * releases may or may not make this type more lightweight,  
 * mutable, and the fields unprotected.
 */
public final class IluSBH {
    
    private java.lang.String jjsbh;
    private java.lang.String jjih;
    private java.lang.String jjsid;
    private java.lang.String jjmstid;
    private java.lang.String jjcinfo;
       
    /**
     * Accessor functions.
     */
    public java.lang.String sbh() {return jjsbh;}
    public java.lang.String ih() {return jjih;}
    public java.lang.String sid() {return jjsid;}
    public java.lang.String mstid() {return jjmstid;}
    public java.lang.String cinfo() {return jjcinfo;}
    
    /*
     * Don't create uninitialized instances 
     */
    private IluSBH(){};
    
    /*
     * Native part of "parse" 
     */
    private native void nativeSetSBHO(java.lang.String sbhs) 
            throws org.omg.CORBA.SystemException;
            //sbhs not null
        
    /**
     * Parses an ILU sbh string.<p>
     * May or may not throw exceptions if input is invalid. 
     */
    public static IluSBH parse(java.lang.String sbhs) 
            throws org.omg.CORBA.SystemException
    {
        IluSBH sbho = new IluSBH();
        sbho.jjsbh = sbhs;
        sbho.nativeSetSBHO(sbhs);
        return sbho;
    } //parse  
    
    
    /**
     * Special characters used in the composition of SBH's. 
     */
    public static final char typeMarker = ';';
    public static final char cInfoMarker = ';';
    public static final char cInfoDivider = '@';
    public static final char tInfoDivider = '=';
    
    
    /* 
     * Used for encoding strings.
     */
    private static java.util.BitSet validChars = new java.util.BitSet(256);
    static {
        char c;
        for (c = 'A'; c <= 'Z'; c++) {validChars.set((int)c);}
        for (c = 'a'; c <= 'z'; c++) {validChars.set((int)c);}
        for (c = '0'; c <= '9'; c++) {validChars.set((int)c);}
        validChars.set((int)'_');
        validChars.set((int)'.');
    } //static


    /* 
     * Like sb.append(s) but encodes s first.
     */
    private static final void appEncode(
        java.lang.StringBuffer sb, 
        java.lang.String s ) throws org.omg.CORBA.SystemException
    {
        int len = s.length();
        sb.ensureCapacity(len);
        for (int i = 0; i < len; i++) {
           char c = s.charAt(i);
           if (validChars.get((int)c)) {
               sb.append(c);
           } else if (c <= '\u00FF') {
               sb.append('%');
               sb.append(Character.forDigit( ((int)c) >> 4, 16));
               sb.append(Character.forDigit( ((int)c) & 15, 16));
           } else {
               throw new IluSystemExceptionBase("non ascii character");
           }
        }
    } //appEncode
    
    
    /** 
     * Computes a contactInfo string from protocol info and 
     * transport info.
     */
    public static java.lang.String contactInfo(
            java.lang.String pinfo, // null NOT ok
            java.lang.String[] tinfo) // null NOT ok
    {
        java.lang.StringBuffer sb = new java.lang.StringBuffer();
        if (pinfo == null) {
            throw new IluSystemExceptionBase("null not an option");
            //pinfo = IluPort.defaultProtocolInfo();
        }
        appEncode(sb, pinfo);
        sb.append(cInfoDivider);
        if (tinfo == null) {
            throw new IluSystemExceptionBase("null not an option");
            //tinfo = IluTransportInfo.buildDefault();
        }
        int len = tinfo.length;
        for (int i = 0; i < len; i++) {
           if (i>0) sb.append(tInfoDivider);
           appEncode(sb, tinfo[i]);
        }
        return sb.toString();
    } //contactInfo

    
    /** 
     * Composes an IluSBH data structure from its components
     */
    public static IluSBH compose(
        java.lang.String sid, 
        java.lang.String ih, 
        java.lang.String mstid,  //use IluClassRep.iluClassId()
        java.lang.String cinfo)  //use IluSBH.contactInfo(pinfo, tinfo)
            throws org.omg.CORBA.SystemException
    {
        IluSBH sbho = new IluSBH();
        //
        //fill in easy fields
        sbho.jjsid = sid;        
        sbho.jjih = ih;
        sbho.jjmstid = mstid;        
        sbho.jjcinfo = cinfo;
        //
        //now compute sbh
        java.lang.StringBuffer sb = new java.lang.StringBuffer(20);
        sb.append("ilu:");
        appEncode(sb, sid);
        sb.append("/");
        appEncode(sb, ih);
        sb.append(";");
        appEncode(sb, mstid);
        sb.append(";");
        sb.append(cinfo);
        sbho.jjsbh = sb.toString();
        return sbho;
    } //compose
  
  
} // IluSBH

