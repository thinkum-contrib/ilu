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
/* $Id: IluSystemExceptionBase.java,v 1.6 1999/08/03 01:54:04 janssen Exp $ */
/* IluSystemExceptionBase.java */
/* Chris Jacobi, November 22, 1998 7:49 pm PST */
 
package xerox.ilu;

/**
 * IluSystemExceptionBase is a superclass of org.omg.CORBA.SystemException
 * used as base class for arbitrary additional system exceptions for Ilu
 * - sadly not in the set of predefined CORBA SystemException's
 * @author      Chris Jacobi
 */
public class IluSystemExceptionBase extends org.omg.CORBA.SystemException {
    
    public IluSystemExceptionBase() {
	super(
	    "IluSystemExceptionBase", 0, 
	    org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE
	    );
    } //constructor


    public IluSystemExceptionBase(java.lang.String s) {
	super(s, 0, org.omg.CORBA.CompletionStatus.COMPLETED_MAYBE);
    } //constructor


    /**
     * Corba level constructor from ILU level class is not really
     * for clients (no harm however).  It is here, so corba
     * level code can use inheritance.
     */
    public IluSystemExceptionBase(
            int minor, org.omg.CORBA.CompletionStatus cmpl) {
	super("IluSystemExceptionBase", minor, cmpl);
    } //constructor
    

    /**
     * Corba level constructor from ILU level class is not really
     * for clients (no harm however).  It is here, so corba
     * level code can use inheritance.
     */
    public IluSystemExceptionBase(java.lang.String s, 
            int minor, org.omg.CORBA.CompletionStatus cmpl) {
	super(s, minor, cmpl);
    } //constructor
    
    
    /** Corba mandated; not yet supported */
    public java.lang.String repositoryId() {
        return theRepositoryId;
    } //repositoryId


    /** Corba mandated; not yet supported */
    public static java.lang.String classNameOf (java.lang.String repositoryId) {
	return null;
    } //classNameOf
    
    
    private java.lang.String theRepositoryId = null;
    
    //straight from iluxport.h
    /*friendly*/ static final int protocolException_Success = 0;
    /*friendly*/ static final int protocolException_NoSuchClassAtServer = 1;
    /*friendly*/ static final int protocolException_ClassVersionMismatch = 2;
    /*friendly*/ static final int protocolException_NoSuchMethodOnClass = 3;
    /*friendly*/ static final int protocolException_GarbageArguments = 4;
    /*friendly*/ static final int protocolException_Unknown = 5;
    /*friendly*/ static final int protocolException_LostConnection = 6;
    /*friendly*/ static final int protocolException_RequestRejected = 7;
    /*friendly*/ static final int protocolException_RequestTimeout = 8;
    /*friendly*/ static final int protocolException_Not = 9;
        // non-protocol failure; see *err 
       

    /** Called from stubs; "code" argument set by runtime. */
    public static 
    org.omg.CORBA.SystemException fromIluProtocolException(int code) {
        org.omg.CORBA.SystemException se = null;
        if (code<0) {code = -code;}
        org.omg.CORBA.CompletionStatus cmpl = 
            org.omg.CORBA.CompletionStatus.from_int(code / 100000);
        switch (code % 100000) {
            case protocolException_Success: 
                se = new org.omg.CORBA.INTERNAL(
                    "BadStub", 0, cmpl
                    );
                break;
            case protocolException_NoSuchClassAtServer: 
                se = new org.omg.CORBA.INTF_REPOS(
                    "NoSuchClassAtServer", 0, cmpl
                    );
                break;
            case protocolException_ClassVersionMismatch: 
                se = new org.omg.CORBA.INTF_REPOS(
                    "ClassVersionMismatch", 0, cmpl
                    );
                break;
            case protocolException_NoSuchMethodOnClass: 
                se = new org.omg.CORBA.INTF_REPOS(
                    "NoSuchMethodOnClass", 0, cmpl
                    );
                break;
            case protocolException_GarbageArguments: 
                se = new org.omg.CORBA.DATA_CONVERSION(
                    "GarbageArguments", 0, cmpl
                    );
                break;
            case protocolException_Unknown: 
                se = new org.omg.CORBA.UNKNOWN(
                    "Unknown", 0, cmpl
                    );
                break;
            case protocolException_LostConnection: 
                se = new org.omg.CORBA.COMM_FAILURE(
                    "LostConnection", 0, cmpl
                    );
                break;
            case protocolException_RequestRejected: 
                se = new org.omg.CORBA.COMM_FAILURE(
                    "RequestRejected", 0, cmpl
                    );
                break;
            case protocolException_RequestTimeout: 
                se = new org.omg.CORBA.NO_RESPONSE(
                    "RequestTimeout", 0, cmpl
                    );
                break;
            case protocolException_Not: 
                se = new org.omg.CORBA.UNKNOWN(
                    "NotProtocolException", 0, cmpl
                    );
                break;
            default: break;
        }
        if (se == null) {
            se = new org.omg.CORBA.UNKNOWN(
                "unknown_ilu_ProtocolException_" + code, 0, cmpl
                );
        }
        return se;
    } //fromIluProtocolException


} //IluSystemExceptionBase
