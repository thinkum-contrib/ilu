/** 
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

/*
$Id: shs.h,v 1.10 1999/08/03 01:50:14 janssen Exp $
*/

#if (defined(WIN32)||defined(WIN16))
#include <iluwin.h>
#elif defined( macintosh )
#include <ilumac.h>
extern void ILUStartup( void );
extern void ILUShutdown( void );
#else
#include <iluconf.h>
#endif


#include <ilubasic.h>

typedef ilu_cardinal UINT4;

typedef struct {
    UINT4 state[5];                             /* state (ABCDE) */
    UINT4 count[2];        /* number of bits */
    unsigned char buffer[64];                   /* input buffer */
} SHS_CTX;

void SHSInit (SHS_CTX *context);

void SHSUpdate (SHS_CTX *context, unsigned char *buffer, unsigned int nbytes);
	
void SHSFinal (unsigned char hash[20], SHS_CTX *context);
