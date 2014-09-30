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
/* IluJava_IluPort.c */ 
/* Chris Jacobi, December 1, 1998 6:33 pm PST */

/*
 */
 
/* $Id: IluJava_IluPort.c,v 1.67 1999/08/03 01:54:21 janssen Exp $ */
 
/*
 * C side of 
 *    IluPort.java, 
 *    IluPassport.java, 
 *    IluIdentity.java, 
 *    IluIdentityType.java and 
 *    IluTransportInfo.java
 *    IluGssOid.java
 *    IluGssCred.java
 *    IluBatcher.java
 */

#include "IluJava_Includes.h"
#include "IluJava_Common.h"
#include "IluJava_JArrays.h"


#include "IluJava_JStubs.h"
#include "xerox_ilu_IluIdentity.h"
#include "xerox_ilu_IluIdentityType.h"
#include "xerox_ilu_IluPassport.h"
#include "xerox_ilu_IluPipeline.h"
#include "xerox_ilu_IluSerializationContext.h"
#include "xerox_ilu_IluSBH.h"
#include "xerox_ilu_IluBatcher.h"
#include "xerox_ilu_IluGssOid.h"
#include "xerox_ilu_IluGssCred.h"
#include "IluJava_JGC.h"
#include "IluJava_JOps.h"
#include "IluJava_Ops.h"


#ifdef SECURE_TRANSPORT
#else
static void signalNoSecureTransport()
{
    JENV_DECLARE_INITFROMTHINAIR
    _ilujava_throwException(JENV_ACTUAL 13, "xerox/ilu/IluNotConfiguredException", "no secure transport");
}
#endif /* def SECURE_TRANSPORT */


JAVAEXPORT(IluPort_nativeInitPort, void) 
    JIluPort jh_Port
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluPort) jwp_Port = JGC_GET_WP_FROM_JAVA_OBJECT(jh_Port);
    JGC_WP_TYPE(JIluServer) jwp_Server = 
        JGC_GET_WP_FROM_JAVA_OBJECT(GET_IluPort_jjServer(jh_Port));
    JString jjProtocolInfo;
    JIluTransportInfo jjTransportInfo;
    JIluPassport jjPassport;
    ilu_string cProtInfo = 0;
    ilu_Server cServer;
    ilu_TransportInfo cTransInfo = 0;
    ilu_Passport cPassport = 0; 
    ilu_Port cPort;
    ilu_Error errs = ILU_INIT_NO_ERR;
    
    if (jwp_Server==0) {
        _ilujava_NullPointerException("null server");
        return;
    }
    cServer = GET_IluServer_yIluServer(JGC_WP_REVEAL(jwp_Server));
    
    jjTransportInfo = GET_IluPort_jjTransportInfo(JGC_WP_REVEAL(jwp_Port));
    if (jjTransportInfo==0) {
        _ilujava_NullPointerException("null transport");
        return;
    }
    cTransInfo = GET_IluTransportInfo_yIluTransportInfo(jjTransportInfo);
    
    jjPassport = GET_IluPort_jjPassport(JGC_WP_REVEAL(jwp_Port));
    /* null is ok */
    if (jjPassport) {
        cPassport = GET_IluPassport_yIluPassport(jjPassport);
    }
    
    jjProtocolInfo =  GET_IluPort_jjProtocolInfo(JGC_WP_REVEAL(jwp_Port));
    if (jjProtocolInfo==0) {
        _ilujava_NullPointerException("null protocol");
        return;
    }
    cProtInfo = IluJava_JString_toheap80(JENV_ACTUAL jjProtocolInfo);
    /* gc might have happened */
    
    JTRACE(_ilujava_pctFlag>0, 
        ("$ IluPort_nativeInitPort jprt: %x jsrv: %x csrv %x\n", 
            (int) JGC_WP_REVEAL(jwp_Port), 
            (int) JGC_WP_REVEAL(jwp_Server), (int) cServer));
    cPort = ilu_CreatePort(cServer, cProtInfo, cTransInfo, cPassport, &errs);
    if (cProtInfo) {
        java_free(cProtInfo);
    }
    if (ILU_ERRNOK(errs)) {
        CONDPRINT("$ nativeInitPort: Error creating port");
        _ilujava_IluErrorToException(&errs, "Error creating port");
        return;
    }
    if (cPort == 0) {
        CONDPRINT("$ nativeInitPort: null port");
        _ilujava_throwException(JENV_ACTUAL 21, "org/omg/CORBA/UNKNOWN", "failed creating port");
        return;
    }
    PUT_IluPort_yPort(JGC_WP_REVEAL(jwp_Port), cPort);
    JTRACE(_ilujava_pctFlag>0, ("$ Port created %x \n", (int) cPort));
} /* nativeInitPort */


JAVAEXPORT(IluPort_nativeClosePort, void) 
    JIluPort jjPort
    ENDJAVAEXPORT
{
    ilu_Port cPort = GET_IluPort_yPort(jjPort);
    if (cPort == 0) {return;}
    ilu_ClosePort(cPort);
 }  /* nativeClosePort */


JAVAEXPORT(IluPort_nativeDonePort, void) 
    JIluPort jjPort
    ENDJAVAEXPORT
{
    ILU_ERRS((bad_param, bad_locks, internal)) errs = ILU_INIT_NO_ERR;
    ilu_boolean success;
    ilu_Port cPort = GET_IluPort_yPort(jjPort);
    if (cPort == 0) return;
    PUT_IluPort_yPort(jjPort, 0);
    if (GET_IluPort_pstate(jjPort) > 3) return;
    PUT_IluPort_pstate(jjPort, 99);
    success = ilu_DoneWithPort(cPort, &errs);
    if (!success) {
        ILU_HANDLED(errs);
    }
}  /* nativeDonePort */


JAVAEXPORT(IluTransportInfo_nativeInitTransportInfo, void) 
    JIluTransportInfo jh_TransInfo,
    JArrayOfString jjInfo
    ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluTransportInfo) jwp_TransInfo = JGC_WP_MAKE(jh_TransInfo);
    char ** cObject = 0;
    if (jjInfo) {
        char * ctemp;
        char * rover; /* the next un-assigned byte */
        JString jjString;
        int totalLen = 0;
        int thisLen = 0;
        int i;
        int cnt = ARRAY_LENGTH(jjInfo);
        /* compute length */
        for (i = 0; i < cnt; i = i+1) {
            jjString = ARRAY_object_GET(jjInfo, i);
            thisLen = JSTRING_LENGTH(jjString);
            totalLen = totalLen+thisLen;
        }
        totalLen = totalLen + cnt; /* add padding per string */
        totalLen = totalLen + cnt; /* add extra padding per string; see below */
        totalLen = totalLen + (cnt+2)*sizeof(char *); 
                /* pointer array, stopper and final padding */
        /* allocate data */
        cObject = (char **) ilu_must_malloc(totalLen);
        /* copy data */
        cObject[cnt] = 0;
        rover = (char *) & (cObject[cnt+1]);
        for (i = 0; i < cnt; i = i+1) {
            cObject[i] = rover;
            jjString = ARRAY_object_GET(jjInfo, i);
            thisLen = JSTRING_LENGTH(jjString);
            /* 
             * Historicly there was some gotcha here:
             * When we were using javaString2CString we had to 
             * add 1 otherwise it did overwrites the last    
             * character. I did not understand this, but the 
             * java rt did the same thing and thats where I got
             * the idea from.
             * Using IluJava_JString_toheap80 this is no more
             * necessary. 
             */
            ctemp = IluJava_JString_toheap80(JENV_ACTUAL jjString);
            memcpy(rover, ctemp, thisLen);
            ilu_free(ctemp);
            rover = rover + thisLen; /* ... used to require +1 */
            rover[0] = '\0';
            rover = rover + 1; 
        }
    }
    PUT_IluTransportInfo_yIluTransportInfo(
        JGC_WP_REVEAL(jwp_TransInfo), cObject); 
    JGC_WP_RELEASE(jwp_TransInfo);
} /* nativeInitTransportInfo */


JAVAEXPORT(IluTransportInfo_nativeFinalizeTransportInfo, void) 
    JIluTransportInfo jjTransInfo
    ENDJAVAEXPORT
{
    ilu_TransportInfo cMem =
        GET_IluTransportInfo_yIluTransportInfo(jjTransInfo);
    PUT_IluTransportInfo_yIluTransportInfo(jjTransInfo, 0);
    if (cMem) ilu_free(cMem);
} /* nativeFinalizeTransportInfo */


static int countDefaultTransportInfo() 
{
    int cnt = 0;
    ilu_TransportInfo to = ilu_DefaultTransportInfo();
    if (to) {
        while (*to) {
            to++; cnt++;
        }
    } 
    return cnt; 
} /* countDefaultTransportInfo */


JAVAEXPORT(IluTransportInfo_nCountDefaultTransportInfo, Jint)
    JIluTransportInfo jjTransInfo
    ENDJAVAEXPORT
{
    return countDefaultTransportInfo(); 
} /* nCountDefaultTransportInfo */


JAVAEXPORT(IluPort_nDefaultProtocol, JString)
    JIluPort jjPort
    ENDJAVAEXPORT
{
    ilu_string cs = ilu_DefaultProtocolInfo();
    if (cs==0) {return 0;}
    return IluJava_JString_fromA0(JENV_ACTUAL cs); 
} /* nDefaultProtocol */


JAVAEXPORT(IluTransportInfo_nDefaultTInfoComponent, JString)
    JIluTransportInfo jjTransInfo,
    Jint n
    ENDJAVAEXPORT
{
    ilu_string cs = 0;
    int cnt = countDefaultTransportInfo(); 
    if (n<cnt) {
        ilu_TransportInfo to = ilu_DefaultTransportInfo();
        cs = to[n];
    }
    if (cs==0) {return 0;}
    return IluJava_JString_fromA0(JENV_ACTUAL cs); 
} /* nDefaultTInfoComponent */


static ilu_Passport cDefaultPassport;


EXPORTLIMITED void
_ilujava_PassportInit()
{
    ilu_Error errs = ILU_INIT_NO_ERR;
    cDefaultPassport = ilu_CreatePassport(0, &errs);
    ILU_MUST_BE_SUCCESS(errs);
}


EXPORTLIMITED ilu_Passport 
_ilujava_getIluPassport(
        JENV_FORMAL ilu_refany jh_iluPassport /* JIluPassport */ 
	)
/* Converts java passport to ilu passport.
 * Converts null IluPassport into default passport. 
 */
 {
     ilu_Passport c_pp = 0;
     JIluPassport jh_pp = (JIluPassport) jh_iluPassport;
     if (jh_pp == 0) return cDefaultPassport;
     c_pp = GET_IluPassport_yIluPassport(jh_pp);
     if (c_pp == 0) return cDefaultPassport;
     return c_pp;
 }


JAVAEXPORT(IluSBH_nativeSetSBHO, void)
	JIluSBH jh_self,
	JString jh_sbhs
	ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluSBH) jwp_self = JGC_WP_MAKE(jh_self);
    ilu_Error	errs		= ILU_INIT_NO_ERR;
    char*	cSBH;
    char*	cih		= 0;
    char*	csid		= 0;
    char*	cmstid		= 0;
    char*	ccinfo		= 0;
    ilu_cardinal len		= 0;
    ilu_boolean pcinfo		= ilu_FALSE;
    cSBH = IluJava_JString_toheap8x(JENV_ACTUAL jh_sbhs); 
    if (!ilu_ParseSBH(
            cSBH, &cih, &csid, &cmstid, &ccinfo, &len, &pcinfo, &errs)) { 
        _ilujava_IluErrorToException(&errs, "ilu_ParseSBH");
        return;
    }
    PUT_IluSBH_jjih(JGC_WP_REVEAL(jwp_self),    
        IluJava_JString_fromA0(JENV_ACTUAL cih)
        );
    PUT_IluSBH_jjsid(JGC_WP_REVEAL(jwp_self),   
        IluJava_JString_fromA0(JENV_ACTUAL csid)
        );
    PUT_IluSBH_jjmstid(JGC_WP_REVEAL(jwp_self), 
        IluJava_JString_fromA0(JENV_ACTUAL cmstid)
        );
    PUT_IluSBH_jjcinfo(JGC_WP_REVEAL(jwp_self), 
        IluJava_JString_fromAX(JENV_ACTUAL ccinfo, len)
        );
    JGC_WP_RELEASE(jwp_self);
    ilu_free(cih);
    ilu_free(csid);
    ilu_free(cmstid);
    if (pcinfo) ilu_free(ccinfo);
    java_free(cSBH);
}


JAVAEXPORT(IluPassport_nInitClientPassport, void)
	JIluPassport jh_ppSelf
	ENDJAVAEXPORT
{
    JGC_WP_TYPE(JIluPassport) jwp_pp = JGC_GET_WP_FROM_JAVA_OBJECT(jh_ppSelf);
    ilu_Passport c_pp = 0;
    ILU_ERRS((no_memory)) errs = ILU_INIT_NO_ERR;
    c_pp = ilu_CreatePassport(0, &errs);
    PUT_IluPassport_yIluPassport(JGC_WP_REVEAL(jwp_pp), c_pp);
    ILU_MUST_BE_SUCCESS(errs);
}


JAVAEXPORT(IluPassport_nFinalizePassport, void)
	JIluPassport jh_ppSelf
	ENDJAVAEXPORT
{
    ILU_ERRS((no_memory)) errs = ILU_INIT_NO_ERR;
    ilu_Passport c_pp;
    /* no locking or checking:  finalize is protected final */
    c_pp = GET_IluPassport_yIluPassport(jh_ppSelf);
    PUT_IluPassport_yIluPassport(jh_ppSelf, 0);
    if (c_pp) {
        ilu_DestroyPassport(c_pp, &errs);
        ILU_MUST_BE_SUCCESS(errs);
    }
}


JAVAEXPORT(IluIdentity_nFinalizeIdentity, void)
	JIluIdentity jh_iiSelf
	ENDJAVAEXPORT
{
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_IdentityInfo c_ii;
    /* no locking or checking:  finalize is protected final */
    c_ii = GET_IluIdentity_yIluIdentity(jh_iiSelf);
    PUT_IluIdentity_yIluIdentity(jh_iiSelf, 0);
    if (c_ii) {
        if (! c_ii->ii_owned_by_passport) {
            ilu_FreeIdentity(c_ii, &errs);
            ILU_HANDLED(errs); 
            /* Ignore errors!  Who knows whether another thread changed 
             * ii_owned_by_passport or what...
             */
        }
    }
} /*nFinalizeIdentity*/


/* jh_ppSelf is synchronized in java */
JAVAEXPORT(IluPassport_nAddIdentity, Jint)
	JIluPassport jh_ppSelf,
	JIluIdentity jh_ii
	ENDJAVAEXPORT
{
    /* of course passport needs to be mutable; tested by caller */
    ILU_ERRS((no_memory)) errs = ILU_INIT_NO_ERR;
    ilu_Passport c_pp;
    ilu_IdentityInfo c_ii;
    ilu_IdentityInfo c_ii2;
    ilu_IdentityType c_it = 0;
    ilu_boolean ret;
    c_pp = GET_IluPassport_yIluPassport(jh_ppSelf);
    c_ii = GET_IluIdentity_yIluIdentity(jh_ii);
    if (c_pp==0) {return -1;}
    if (c_ii==0) {return -2;}
    c_it = c_ii->ii_type;
    if (c_it==0) {return -3;}
    c_ii2 = ilu_FindIdentity(c_pp, c_it);
    if (c_ii2) {
        /* already has an identity of this type */
        return -4;
    }
    if (c_ii->ii_owned_by_passport) {
        /* Already owned by some passport; I am afraid that
         * adding this to another passport might cause confusion
         */
        return -5;
    }
    ret = ilu_AddIdentity(c_pp, c_ii, &errs);
    if (ret) {
        /* success */
        return 0;
    } else {
        /* failed */
        ILU_HANDLED(errs);
        return -6;
    }
} /*nAddIdentity*/


JAVAEXPORT(IluPassport_nFindIdentity, Jint)
	JIluPassport jh_ppSelf,
	JIluCall jh_call,
	JIluIdentityType jh_it,
	JIluIdentity jh_iicontainer
	ENDJAVAEXPORT
{
    Jint returnkey = 0;
    ILU_ERRS((no_memory)) errs = ILU_INIT_NO_ERR;
    JGC_WP_TYPE(JIluIdentity) jwp_iicontainer = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_iicontainer);
    ilu_IdentityInfo c_ii;
    ilu_Passport c_pp = 0;
    ilu_IdentityType c_it;
    c_pp = GET_IluPassport_yIluPassport(jh_ppSelf);
    c_it = GET_IluIdentityType_yIluIdentityType(jh_it);
    if (c_pp) {
        /* client generated passport */
    } else {
        /* passport of a call */
        ilu_Call cCall = GET_IluCall_yCall(jh_call);
        if (cCall) {c_pp = ilu_CallerPassportOfCall(cCall);}
    }
    if (c_pp) {
        c_ii = ilu_FindIdentity(c_pp, c_it);
        if (c_ii) {
            c_ii = ilu_CopyIdentity(c_ii, &errs);
            /* Must copy since we will stuff it into IluIdentity where it
             * might be finalized
             */
            if (ILU_ERRNOK(errs)) {
                _ilujava_IluErrorToException(&errs, "Error creating port");;
            } else {
                PUT_IluIdentity_yIluIdentity(
                    JGC_WP_REVEAL(jwp_iicontainer), c_ii);
                if (c_ii->ii_owned_by_passport) {
                    returnkey = 2;
                    /* Not likely since we just made a copy.
                     * But anyway: we will keep track of ownership in java
                     * to prevent early garbage collection of passport
                     */
                }
            } 
        }
        /* else it is ok for ilu_FindIdentity to return NIL */
    }
    return returnkey;
}


JAVAEXPORT(IluIdentity_nId2String, JString)
	JIluIdentity jh_iiSelf
	ENDJAVAEXPORT
{
    JString jh_string = 0;
    ILU_ERRS((no_memory)) errs = ILU_INIT_NO_ERR;
    ilu_string c_s = 0;
    ilu_IdentityInfo c_ii;
    ilu_cardinal ignore;
    c_ii = GET_IluIdentity_yIluIdentity(jh_iiSelf);
    if (c_ii==0) {goto do_return;}
    c_s = ilu_malloc(1000);
    ignore = ilu_DisplayIdentity(c_ii, c_s, 1000, &errs);
    if (ILU_ERRNOK(errs)) {
        ILU_HANDLED(errs);
        goto do_return;
    }
    if (c_s) {
        jh_string = IluJava_JString_fromA0(JENV_ACTUAL c_s);
    }
    /*fall through*/
  do_return:
    if (c_s) {
        ilu_free(c_s);
    }
    return jh_string;
}


JAVAEXPORT(IluIdentityType_nRegStdIdentityType, Jint)
	JIluIdentityType jh_itSelf,
	Jint key
	ENDJAVAEXPORT
{
    if (key==0) {
        PUT_IluIdentityType_yIluIdentityType(
            jh_itSelf, ilu_NoIdentity
            );
        return 1;
    } 
    if (key==1) {
        PUT_IluIdentityType_yIluIdentityType(
            jh_itSelf, ilu_ConnectionIdentity
            );
        return 1;
    } 
    if (key==2) {
  #ifdef SECURE_TRANSPORT
        PUT_IluIdentityType_yIluIdentityType(
            jh_itSelf, ilu_GSSIdentity
            );
  #endif /* def SECURE_TRANSPORT */
        return 1;
    } 
    if (key==3) {
  #ifdef SUNRPC_PROTOCOL
        PUT_IluIdentityType_yIluIdentityType(
            jh_itSelf, ilu_SunRPCAuthUnixIdentity
            );
  #endif /* def SUNRPC_PROTOCOL */
        return 1;
    } 
    return -1;
}


JAVAEXPORT(IluSerializationContext_nInitSerializationContext, void)
    JIluSerializationContext jh_self,
    JIluServer jh_server,
    JIluOInt jh_oi
    ENDJAVAEXPORT
{
    ILU_ERRS((no_memory, no_resources, bad_locks, broken_locks, bad_param)) 
        errs = ILU_INIT_NO_ERR;
    ilu_Server cServer = 0;
    ilu_Serializer cSerializer = 0;
    /* using frame instead weak pointer because once the serializer
     * has been setup there is no further native call which is 
     * under danger of java garbage collection
     */ 
    struct {
       JIluSerializationContext jh_serializer;
       } frame;
    JGC_FRAME_DECL(framename)  
    JGC_FRAME_PUSH(framename, &frame, sizeof(frame))
    frame.jh_serializer = jh_self;
    if (jh_server) {
        cServer = GET_IluServer_yIluServer(jh_server);
        
        /* ??? wait a second: 
        this is done outside the server lock.  This is only
        legal if holding on to the IluServer will hold on
        to the cServer...
        Not a problem yet as (September 19, 1997 1:47:22 pm PDT)
        because surrogate IluServer don't even exist.
        */
        
        cSerializer = ilu_GetSerializer(cServer, &errs);
            /* java garbage collection */
        if (ILU_ERRNOK(errs)) {
            ILU_HANDLED(errs);
            cSerializer = 0;
        } else {
            /* good */
        }
    } else if (jh_oi) {
        /* we need to prevent unlucky finalization order
         * which would retire the server before the serializer 
         * is released.  Thats why we used to use ilu_DeltaServerHolds
         */
        KInfo kinfo;
        _ilujava_getSetCIluObject(JENV_ACTUAL jh_oi, &kinfo, ilu_TRUE);
        if (kinfo.cIluObject) {
            
            /*
            Not necessary anymore since now GetSerializer does 
            DeltaServerHolds in the kernel ...            
            ilu_DeltaServerHolds(kinfo.cServer, 1, &errs);
            if (ILU_ERRNOK(errs)) {
                _ilujava_IluErrorToCallException(&errs, "zzzz");
            } else {
                PUT_IluSerializationContext_ykServer(
                    frame.jh_serializer, kinfo.cServer
                    );
                PUT_IluSerializationContext_yClass(
                    frame.jh_serializer, kinfo.cClass
                    );
                cSerializer = ilu_InnerGetSerializer(kinfo.cServer, &errs);
                if (ILU_ERRNOK(errs)) { 
                    cSerializer = 0;
                }
            }
            */
            
            cSerializer = ilu_InnerGetSerializer(kinfo.cServer, &errs);
            if (ILU_ERRNOK(errs)) { 
                ILU_HANDLED(errs);
                cSerializer = 0;
            }
            ilu_ExitServer(kinfo.cServer, kinfo.cClass);
        }
    }
    PUT_IluSerializationContext_ySerializationContext(
        frame.jh_serializer, cSerializer
        );
    JGC_FRAME_POP(framename)
}


JAVAEXPORT(IluSerializationContext_nFinalizeSerializationContext, void)
    JIluSerializationContext jh_self
    ENDJAVAEXPORT
{
    ILU_ERRS((bad_locks, broken_locks, bad_param)) errs = ILU_INIT_NO_ERR;
    ilu_Serializer cSerializer = 0;
    ilu_Class cClass = 0;
    ilu_Server cServer = 0;
    cSerializer = GET_IluSerializationContext_ySerializationContext(jh_self);
    cServer = GET_IluSerializationContext_ykServer(jh_self);
    cClass = GET_IluSerializationContext_yClass(jh_self);
    PUT_IluSerializationContext_ySerializationContext(jh_self, 0);
    PUT_IluSerializationContext_ykServer(jh_self, 0);
    PUT_IluSerializationContext_yClass(jh_self, 0);
    if (cSerializer) {
        ilu_ReleaseSerializer(cSerializer, &errs);
        if (ILU_ERRNOK(errs)) { 
            ILU_HANDLED(errs);
        }
    }
    
    /*
    DeltaServerHolds is now done in the kernel
    and doesn't need to be done by the LSR anymore
    if (cServer) {
        ilu_EnterServer(cServer, cClass);
        ILU_CLER(errs)
        ilu_DeltaServerHolds(cServer, -1, &errs);
        if (ILU_ERRNOK(errs)) { 
            ILU_HANDLED(errs);
        }
        ilu_ExitServer(cServer, cClass);
    }
    */
    
}


JAVAEXPORT(IluPipeline_nInitPipeline, void)
    JIluPipeline jh_self
    ENDJAVAEXPORT
{
    ILU_ERRS((no_memory, no_resources, bad_locks, broken_locks, bad_param)) 
        errs = ILU_INIT_NO_ERR;
    ilu_Pipeline cPipeline = 0;
    /* using frame instead weak pointer because once the pipeline
     * has been setup there is no further native call which is 
     * under danger of java garbage collection
     */ 
    struct {
       JIluPipeline jh_pipline;
       } frame;
    JGC_FRAME_DECL(framename)  
    JGC_FRAME_PUSH(framename, &frame, sizeof(frame))
    frame.jh_pipline = jh_self;
    cPipeline = ilu_GetPipeline(&errs);
        /* java garbage collection */
    if (ILU_ERRNOK(errs)) {
        ILU_HANDLED(errs);
    } else {
        PUT_IluPipeline_yIluPipeline(frame.jh_pipline, cPipeline);
    }
    JGC_FRAME_POP(framename)
}


JAVAEXPORT(IluPipeline_nFinalizePipeline, void)
    JIluPipeline jh_self
    ENDJAVAEXPORT
{
    ILU_ERRS((bad_locks, broken_locks, bad_param)) errs = ILU_INIT_NO_ERR;
    ilu_Pipeline cPipeline = 0;
    cPipeline = GET_IluPipeline_yIluPipeline(jh_self);
    PUT_IluPipeline_yIluPipeline(jh_self, 0);
    if (cPipeline) {
        ilu_ReleasePipeline(cPipeline, &errs);
        if (ILU_ERRNOK(errs)) {
            ILU_HANDLED(errs);
        }
    }
}


JAVAEXPORT(IluGssCred_nIluGssCredFree, void)
    JIluGssCred jh_self
    ENDJAVAEXPORT
{
#ifdef SECURE_TRANSPORT
    gss_cred_id_t c_cred_handle = 0;
    OM_uint32 retval = 0;
    OM_uint32 minor_status = 0;
    c_cred_handle = GET_IluGssCred_yIluGssCred(jh_self);
    PUT_IluGssCred_yIluGssCred(jh_self, 0);
    if (c_cred_handle) {
        /* 
         * Don't free gss_cred_id_t here currently:
         *
         * Kernel keeps ref count of identities and frees
         * the gss_cred_id_t when identity is freed.
         *
         * This prevents us from re-ifying gss_cred_id_t
         * and we stuff the IluGssCred into an IluIdentity
         * right awaay after creation for guaranteeing
         * correct native memory management        
         *
         * retval = gss_release_cred(&minor_status, &c_cred_handle);
         */
    }
#endif /* def SECURE_TRANSPORT */
}


JAVAEXPORT(IluGssCred_nIluGssCredAcquire, void)
    JIluGssCred jh_self,
    JString jh_principalName,
    Jint lifetimeSecs,
    JIluGssOid jh_mechanism,
    Jboolean acceptOnly
    ENDJAVAEXPORT
{
  #ifdef SECURE_TRANSPORT
    ilu_string c_name = 0;
    gss_OID c_secmech = GET_IluGssOid_yIluGssOid(jh_mechanism);
    JGC_WP_TYPE(JIluGssCred) jwp_self = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_self);
    ilu_Error err;
    gss_cred_id_t c_cred_handle = 0;
    c_name = IluJava_JString_toheap8x(JENV_ACTUAL jh_principalName);
    c_cred_handle = ilu_AcquireGSSCredForName(c_name,
        (ilu_cardinal) lifetimeSecs,
        c_secmech,
        (acceptOnly != 0),
        &err
        );
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToException(&err, "Error aquiring credentials");
    }
    if (c_cred_handle) {
        /* store it independent of error; it will be freed on
         * finalization
         */
        PUT_IluGssCred_yIluGssCred(
            JGC_WP_REVEAL(jwp_self), c_cred_handle);
    }
    if (c_name) {ilu_free(c_name);}
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
}


JAVAEXPORT(IluGssCred_nIluGssCredIdToIdentity, void)
    JIluGssCred jh_self,
    JIluIdentity jh_id
    ENDJAVAEXPORT
{
  #ifdef SECURE_TRANSPORT
    JGC_WP_TYPE(JIluIdentity) jwp_iicontainer = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_id);
    ilu_IdentityInfo c_idInfo = 0;
    ilu_Error err;
    gss_cred_id_t c_cred_handle = 0;
    ilu_IdentityInfo c_ii;
    c_cred_handle = GET_IluGssCred_yIluGssCred(jh_self);
    c_ii = ilu_AcquireGSSIdentity (c_cred_handle, &err);
    if (ILU_ERRNOK(err)) {
        _ilujava_IluErrorToException(&err, "Error aquiring credentials");
        return;
    }
    PUT_IluIdentity_yIluIdentity(JGC_WP_REVEAL(jwp_iicontainer), c_ii);
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
}

JAVAEXPORT(IluGssOid_nIluGssOidInit, void)
    JIluGssOid jh_self,
    JString jh_s
    ENDJAVAEXPORT
{
  #ifdef SECURE_TRANSPORT
    JGC_WP_TYPE(JIluGssOid) jwp_self = 
        JGC_GET_WP_FROM_JAVA_OBJECT(jh_self);
    gss_OID c_oid; 
    OM_uint32 major = 0;
    OM_uint32 minor = 0;
    ilu_string c_name = 0;
    gss_buffer_desc b;
    c_name = IluJava_JString_toheap8x(JENV_ACTUAL jh_s);
    b.value = (char *) c_name;
    b.length = strlen(c_name);
    JTRACE(_ilujava_pctFlag>0,
        ("$ IluGssOid_nIluGssOidInit %s\n", c_name) );
    major = gss_str_to_oid(&minor, &b, &c_oid);
    if (major != GSS_S_COMPLETE) {
        ilu_free(c_name);
        _ilujava_throwException(JENV_ACTUAL 21, 
            "org/omg/CORBA/UNKNOWN", 
            "failed internalizing oid");
        return; 
    }
    PUT_IluGssOid_yIluGssOid(JGC_WP_REVEAL(jwp_self), c_oid);
    ilu_free(c_name);
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
}


JAVAEXPORT(IluGssCred_decodeName, JString)
    JIluGssCred jh_self,
    JIluIdentity jh_id,
    JIluGssOid jh_mechanism
    ENDJAVAEXPORT
{
    JString jh_string = 0;
  #ifdef SECURE_TRANSPORT
    OM_uint32 major = 0;
    OM_uint32 minor = 0;
    gss_name_t c_gss_name;
    ilu_string c_name = 0;
    ilu_IdentityInfo c_ii;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean noError;
    gss_OID c_secmech = 0;
    if (jh_id == 0) {
        _ilujava_NullPointerException("null IluIdentity");
        return 0;
    }
    c_ii = GET_IluIdentity_yIluIdentity(jh_id);
    if (jh_mechanism) {
        c_secmech = GET_IluGssOid_yIluGssOid(jh_mechanism);
    }
    noError = ilu_DecodeGSSIdentity(c_ii, &c_gss_name, 
        0, /* ilu_FineTime*/
        c_secmech, 
        0, /* ilu_boolean*/
        0, /* connection flags*/
        &errs
        );
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "failed ilu_DecodeGSSIdentity");
        return 0;
    }
    ILU_CLER(errs);
    c_name = ilu_GSSNameToString(c_gss_name, &errs);
    major = gss_release_name(&minor, &c_gss_name);
    if (c_name) {
        jh_string = IluJava_JString_fromA0(JENV_ACTUAL c_name);
        ilu_free(c_name);
    }
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "failed ilu_GSSNameToString");
        return 0;
    }
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
  return jh_string;
}


JAVAEXPORT(IluGssCred_decodeIsLocal, Jboolean)
    JIluGssCred jh_self,
    JIluIdentity jh_id,
    JIluGssOid jh_mechanism
    ENDJAVAEXPORT
{
    ilu_boolean local = ilu_FALSE;
  #ifdef SECURE_TRANSPORT
    OM_uint32 major = 0;
    OM_uint32 minor = 0;
    ilu_string c_name = 0;
    ilu_IdentityInfo c_ii;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean noError;
    gss_OID c_secmech = 0;
    if (jh_id == 0) {
        _ilujava_NullPointerException("null IluIdentity");
        return 0;
    }
    c_ii = GET_IluIdentity_yIluIdentity(jh_id);
    if (jh_mechanism) {
        c_secmech = GET_IluGssOid_yIluGssOid(jh_mechanism);
    }
    noError = ilu_DecodeGSSIdentity(c_ii, 
        0, /* name */ 
        0, /* ilu_FineTime */
        c_secmech, 
        &local, /* ilu_boolean */
        0, /* connection flags */
        &errs
        );
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "failed ilu_DecodeGSSIdentity");
        local = ilu_FALSE;
    }
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
  return (Jboolean) local;
}


JAVAEXPORT(IluGssCred_decodeFlags, Jint)
    JIluGssCred jh_self,
    JIluIdentity jh_id,
    JIluGssOid jh_mechanism
    ENDJAVAEXPORT
{
    ilu_integer flags = 0;
  #ifdef SECURE_TRANSPORT
    OM_uint32 major = 0;
    OM_uint32 minor = 0;
    ilu_string c_name = 0;
    ilu_IdentityInfo c_ii;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean noError;
    gss_OID c_secmech = 0;
    if (jh_id == 0) {
        _ilujava_NullPointerException("null IluIdentity");
        return 0;
    }
    c_ii = GET_IluIdentity_yIluIdentity(jh_id);
    if (jh_mechanism) {
        c_secmech = GET_IluGssOid_yIluGssOid(jh_mechanism);
    }
    noError = ilu_DecodeGSSIdentity(c_ii, 
        0, /* name */ 
        0, /* ilu_FineTime */
        c_secmech, 
        0, /* ilu_boolean */
        (ilu_cardinal *) &flags, /* connection flags */
        &errs
        );
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "failed ilu_DecodeGSSIdentity");
    }
  #else
    signalNoSecureTransport();
  #endif /* def SECURE_TRANSPORT */
  return flags;
}


JAVAEXPORT(IluGssCred_decodeGoodTill, Jint)
    JIluGssCred jh_self,
    JIluIdentity jh_id,
    JIluGssOid jh_mechanism
    ENDJAVAEXPORT
{
  #ifdef SECURE_TRANSPORT
    ilu_FineTime t;
    OM_uint32 major = 0;
    OM_uint32 minor = 0;
    ilu_string c_name = 0;
    ilu_IdentityInfo c_ii;
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_boolean noError;
    gss_OID c_secmech = 0;
    if (jh_id == 0) {
        _ilujava_NullPointerException("null IluIdentity");
        return 0;
    }
    c_ii = GET_IluIdentity_yIluIdentity(jh_id);
    if (jh_mechanism) {
        c_secmech = GET_IluGssOid_yIluGssOid(jh_mechanism);
    }
    noError = ilu_DecodeGSSIdentity(c_ii, 
        0, /* name */ 
        &t, /* ilu_FineTime */
        c_secmech, 
        0, /* ilu_boolean */
        0, /* connection flags */
        &errs
        );
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "failed ilu_DecodeGSSIdentity");
    }
    return (Jint) t.ft_s;
  #else
    signalNoSecureTransport();
    return 0;
  #endif /* def SECURE_TRANSPORT */
}


JAVAEXPORT(IluGssCred_now, Jint)
    JIluGssCred jh_self
    ENDJAVAEXPORT
{
    return (Jint) ilu_CoarseTime_Now();
}

JAVAEXPORT(IluBatcher_nInitBatcher, void)
    JIluBatcher jh_self,
    Jint millis,
    Jboolean pushable 
    ENDJAVAEXPORT
{
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Batcher c_b;
    ilu_FineTime c_ft;
    JGC_WP_TYPE(JIluBatcher) jwp_batcher = JGC_WP_MAKE(jh_self);
    c_ft.ft_s = millis/1000;
    c_ft.ft_t =  ilu_rescale((millis % 1000), 1000, ilu_FineTimeRate);
    c_b = ilu_CreateBatcher(c_ft, (pushable != 0), &errs);
    if (ILU_ERRNOK(errs)) {
        _ilujava_IluErrorToException(&errs, "IluBatcher_nInitBatcher");
    } else {
        PUT_IluBatcher_yBatcher(JGC_WP_REVEAL(jwp_batcher), c_b);
    }
    JGC_WP_RELEASE(jwp_batcher);
} /* IluBatcher_nInitBatcher */


JAVAEXPORT(IluBatcher_nFinalizeBatcher, void)
    JIluBatcher jh_self 
    ENDJAVAEXPORT
{
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Batcher c_b;
    c_b = GET_IluBatcher_yBatcher(jh_self);
    PUT_IluBatcher_yBatcher(jh_self, 0);
    if (c_b) {
        ilu_ReleaseBatcher(c_b, &errs);
        ILU_MUST_BE_SUCCESS(errs);
    }
} /* IluBatcher_nFinalizeBatcher */


JAVAEXPORT(IluBatcher_nPushBatcher, void)
    JIluBatcher jh_self 
    ENDJAVAEXPORT
{
    ilu_Error errs = ILU_INIT_NO_ERR;
    ilu_Batcher c_b;
    c_b = GET_IluBatcher_yBatcher(jh_self);
    if (c_b) {
        /* The java IluBatcher can't have been finalized; so c_b memory exists */
        ilu_PushBatcher(c_b, &errs);
        if (ILU_ERRNOK(errs)) {
            _ilujava_IluErrorToException(&errs, "IluBatcher_nPushBatcher");
        }
    }
} /* IluBatcher_nPushBatcher */




/* end */




