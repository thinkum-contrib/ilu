/** $Id: ngrendering_getbot_impl.h,v 1.6 1999/08/03 01:58:09 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:02 pm PDT */

#ifndef __ngrendering_getbot_impl__h__
#define __ngrendering_getbot_impl__h__

#include "NgRendering.h"

#ifdef __cplusplus
extern "C" {
#endif


typedef struct RenderingSinkObjectData_s {
	ilu_cardinal m_card_last_byte_received_index;
	NgStream_DataSource m_datasource;
	ilu_Mutex m_access_mutex;
	ilu_Condition m_done_condition;
	char* m_pc_object_id; /* object id os object whose rendering we're receiving */
	ilu_FineTime  m_time_begin;  /* When the SendRendering began */

} RenderingSinkObjectData;

/* allocates instance data for RenderingSinks 
 pc_object_id is the object id of the object whose rendering we're going to receive */
extern RenderingSinkObjectData* MallocRenderingSinkObjectData(char* pc_object_id, ilu_FineTime time_begin);


#ifdef __cplusplus
}
#endif


#endif /* __ngrendering_getbot_impl__h__ */
