/** $Id: ngstream_webserv_impl.h,v 1.5 1999/08/03 01:58:06 janssen Exp $
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
/* Last edited by Mike Spreitzer October 9, 1998 2:03 pm PDT */

#ifndef __ngstream_webserv_impl__h__
#define __ngstream_webserv_impl__h__

#ifdef __cplusplus
extern "C" {
#endif

/* the chunk size to use if one isn't suggested */
#define NGWEBSEVER_DEFAULT_CHUNK_SIZE 2048


typedef struct DataSourceObjectData_s {
	int m_h_native_content_file;
	ilu_cardinal m_card_chunking_size;
	ilu_cardinal m_card_next_unsent_byte_index;
	ilu_cardinal m_card_last_byte_to_send_index;
	ilu_integer m_i_state; /* 0 = run, 1 = paused, 2 = abort */
	ilu_Mutex m_access_mutex;
	ilu_Condition m_state_change_condition;
	char* m_pc_content_type;
	ilu_boolean m_b_like_head;
	char* m_pc_object_id;
	NgStream_DataSink m_renderSink;
	ILU_C_Pipeline m_pipeline;
	ILU_C_Serializer m_serializer;
} DataSourceObjectData;


extern DataSourceObjectData* MallocDataSourceObjectData(int h_native_content_file,
												 ilu_cardinal start, 
												 ilu_cardinal end,
												 ilu_cardinal suggestedChunkSize,
												 char* pc_content_type,
												 ilu_boolean b_like_head,
												 char* pc_object_id,
												 NgStream_DataSink renderSink);


extern void do_data_source_work (NgStream_DataSource data_source);

#ifdef __cplusplus
}
#endif


#endif /* __ngstream_webserv_impl__h__ */
