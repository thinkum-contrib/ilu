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
/*
*/
/* $Id: iiop.h,v 1.23 1999/08/03 01:52:58 janssen Exp $ */
/* Last tweaked by Mike Spreitzer May 21, 1998 9:25 pm PDT */

#include <stdio.h>

#include <sys/types.h>

#include <fcntl.h>

struct IIOP_IOR_TaggedProfile {
  ilu_cardinal	tag;
  ilu_cardinal	profileDataLen;
  ilu_bytes	profileData;
};

struct IIOP_IOR_IOR {
  ilu_string	type_id;
  ilu_cardinal	nProfiles;
  struct IIOP_IOR_TaggedProfile	Profile[1];
};  

struct IIOP_DataBlock {
  ilu_cardinal major;
  ilu_cardinal minor;
  ilu_cardinal mapping;
  ilu_bytes    key;
  ilu_cardinal keylen;
  ilu_boolean  relocate_checked;
  ilu_boolean  charsets_sent;
};

#define IIOP_TAG_INTERNET_IOP		0
#define IIOP_TAG_MULTIPLE_COMPONENT_IOP	1
#define IIOP_TAG_ILU_IOP		0x494c5500
#define IIOP_TAG_ILU_SUNRPC_IOP		0x494c5501
#define IIOP_TAG_ILU_COURIER_IOP	0x494c5502
#define IIOP_TAG_ILU_UNUSED1_IOP	0x494c5503
#define IIOP_TAG_ILU_UNUSED2_IOP	0x494c5504
#define IIOP_TAG_ILU_UNUSED3_IOP	0x494c5505
#define IIOP_TAG_ILU_UNUSED4_IOP	0x494c5506
#define IIOP_TAG_ILU_UNUSED5_IOP	0x494c5507
#define IIOP_TAG_CODE_SETS		1

#define IIOP_CHARSET_UNICODE_UCS_2	0x00010100
#define IIOP_CHARSET_UNICODE_UCS_4	0x00010104
#define IIOP_CHARSET_UNICODE_UTF_8	0x05010001
#define IIOP_CHARSET_ISO_LATIN1		0x00010001
#define IIOP_CHARSET_US_ASCII		0x00010020

#define ilu_IIOP_NATIVE_SHORT_CHAR_CODE_SET_ID	IIOP_CHARSET_ISO_LATIN1
#define ilu_IIOP_NATIVE_CHAR_CODE_SET_ID	IIOP_CHARSET_UNICODE_UCS_2

#define GIOP_PacketType_Request		0
#define GIOP_PacketType_Reply		1
#define GIOP_PacketType_CancelRequest	2
#define GIOP_PacketType_LocateRequest	3
#define GIOP_PacketType_LocateReply	4
#define GIOP_PacketType_CloseConnection	5
#define GIOP_PacketType_MessageError	6

#define GIOP_ReplyStatusType_NO_EXCEPTION	0
#define GIOP_ReplyStatusType_USER_EXCEPTION	1
#define GIOP_ReplyStatusType_SYSTEM_EXCEPTION	2
#define GIOP_ReplyStatusType_LOCATION_FORWARD	3

enum IIOP_LocateStatus { IIOP_UNKNOWN_OBJECT, IIOP_OBJECT_HERE, IIOP_OBJECT_FORWARD };

enum byte_order { BigEndian, LittleEndian };

typedef struct ilu_packet_s {
  struct ilu_packet_methods_s *methods;
  ilu_Transport bs;
  ilu_boolean bs_needs_closing_on_finish_call;
  ilu_byte *vop;
  enum byte_order byteorder;
  int ptype;
  ilu_bytes objKey;
  ilu_cardinal objKeyLen;
  ilu_cardinal size;
  ilu_bytes principal;
  ilu_cardinal principalLen;
  ilu_cardinal short_char_codeset;
  ilu_cardinal char_codeset;
} * PACKET;

struct ilu_packet_methods_s {
  /*for access and calling: L2 >= {call's conn's iomu};
			    L1, Main unconstrained*/

  void	(*put_s8) (PACKET p, signed char l, ILU_ERRS((IoErrs)) *);
  void	(*put_s16) (PACKET p, ilu_shortinteger l, ILU_ERRS((IoErrs)) *);
  void	(*put_s32) (PACKET p, ilu_integer l, ILU_ERRS((IoErrs)) *);
  void	(*put_s64) (PACKET p, ilu_longinteger *l, ILU_ERRS((IoErrs)) *);
  void	(*put_u8) (PACKET p, ilu_byte l, ILU_ERRS((IoErrs)) *);
  void	(*put_u16) (PACKET p, ilu_shortcardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_u32) (PACKET p, ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_u64) (PACKET p, ilu_longcardinal *l, ILU_ERRS((IoErrs)) *);
  void	(*put_r32) (PACKET p, ilu_shortreal l, ILU_ERRS((IoErrs)) *);
  void	(*put_r64) (PACKET p, ilu_real l, ILU_ERRS((IoErrs)) *);
  void	(*put_r128) (PACKET p, ilu_longreal l, ILU_ERRS((IoErrs)) *);
  void	(*put_bytes) (PACKET p, ilu_bytes bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void	(*put_opaque) (PACKET p, ilu_bytes bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) *);

  void	(*get_s8) (PACKET p, signed char *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s16) (PACKET p, ilu_shortinteger *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s32) (PACKET p, ilu_integer *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_s64) (PACKET p, ilu_longinteger *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u8) (PACKET p, ilu_bytes l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u16) (PACKET p, ilu_shortcardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u32) (PACKET p, ilu_cardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_u64) (PACKET p, ilu_longcardinal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r32) (PACKET p, ilu_shortreal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r64) (PACKET p, ilu_real *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_r128) (PACKET p, ilu_longreal *l, ILU_ERRS((IoErrs)) * err);
  void	(*get_bytes) (PACKET p, ilu_bytes *bytes, ilu_cardinal *l, ilu_cardinal max_size, ILU_ERRS((IoErrs)) * err);
  void	(*get_opaque) (PACKET p, ilu_bytes *bytes, ilu_cardinal l, ILU_ERRS((IoErrs)) * err);

  /* L1.sup < trmu; !p->bs->tr_wc */
  void	(*destroy) (PACKET p, ilu_Error *err);
};

/*L2 >= {call's conn's iomu}; L1, Main unconstrained*/

#define packet_put_s8(p,b,e)		((*((p)->methods)->put_s8)((p),(b),(e)))
#define packet_put_s16(p,b,e)		((*((p)->methods)->put_s16)((p),(b),(e)))
#define packet_put_s32(p,b,e)		((*((p)->methods)->put_s32)((p),(b),(e)))
#define packet_put_s64(p,b,e)		((*((p)->methods)->put_s64)((p),(b),(e)))
#define packet_put_u8(p,b,e)	((*((p)->methods)->put_u8)((p),(b),(e)))
#define packet_put_u16(p,b,e)	((*((p)->methods)->put_u16)((p),(b),(e)))
#define packet_put_u32(p,b,e)	((*((p)->methods)->put_u32)((p),(b),(e)))
#define packet_put_u64(p,b,e)	((*((p)->methods)->put_u64)((p),(b),(e)))
#define packet_put_r32(p,b,e)		((*((p)->methods)->put_r32)((p),(b),(e)))
#define packet_put_r64(p,b,e)	((*((p)->methods)->put_r64)((p),(b),(e)))
#define packet_put_r128(p,b,e)	((*((p)->methods)->put_r128)((p),(b),(e)))
#define packet_put_bytes(p,b,l,e)	((*((p)->methods)->put_bytes)((p),(b),(l),(e)))
#define packet_put_opaque(p,b,l,e)	((*((p)->methods)->put_opaque)((p),(b),(l),(e)))

#define packet_get_s8(p,b,e)	((*((p)->methods)->get_s8)((p),(b),(e)))
#define packet_get_s16(p,b,e)	((*((p)->methods)->get_s16)((p),(b),(e)))
#define packet_get_s32(p,b,e)	((*((p)->methods)->get_s32)((p),(b),(e)))
#define packet_get_s64(p,b,e)	((*((p)->methods)->get_s64)((p),(b),(e)))
#define packet_get_u8(p,b,e)	((*((p)->methods)->get_u8)((p),(b),(e)))
#define packet_get_u16(p,b,e)	((*((p)->methods)->get_u16)((p),(b),(e)))
#define packet_get_u32(p,b,e)	((*((p)->methods)->get_u32)((p),(b),(e)))
#define packet_get_u64(p,b,e)	((*((p)->methods)->get_u64)((p),(b),(e)))
#define packet_get_r32(p,b,e)	((*((p)->methods)->get_r32)((p),(b),(e)))
#define packet_get_r64(p,b,e)	((*((p)->methods)->get_r64)((p),(b),(e)))
#define packet_get_r128(p,b,e)	((*((p)->methods)->get_r128)((p),(b),(e)))
#define packet_get_bytes(p,b,l,lim,e)	((*((p)->methods)->get_bytes)((p),(b),(l),(lim),(e)))
#define packet_get_opaque(p,b,l,e)	((*((p)->methods)->get_opaque)((p),(b),(l),(e)))

#define packet_destroy(p,e)	((*((p)->methods)->destroy)((p),(e)))
#define packet_vop(p)		((p)->vop)

#define iiop_packet(x)		((PACKET)((x)->ca_prdata2))
#define iiop_set_packet(x,p)	((x)->ca_prdata2 = ((ilu_refany)(p)))
#define iiop_transport(x)	(iiop_packet(x)->bs)
#define iiop_packetType(x)	(iiop_packet(x)->ptype)
#define iiop_objKey(x)		(iiop_packet(x)->objKey)
#define iiop_objKeyLen(x)	(iiop_packet(x)->objKeyLen)
#define iiop_size(x)		(iiop_packet(x)->size)
#define iiop_vop(x)		(iiop_packet(x)->vop)
#define iiop_incr_vop(x,n)	((iiop_packet(x)->vop)+=(n))
#define iiop_byte_order(x)	(iiop_packet(x)->byteorder)
#define iiop_principal(x)	(iiop_packet(x)->principal)
#define iiop_principalLen(x)	(iiop_packet(x)->principalLen)
#define iiop_data_block(x)	((struct IIOP_DataBlock *)(call_connection(x)->co_protocol_data))
#define iiop_major_version(x)	(((struct IIOP_DataBlock *)(call_connection(x)->co_protocol_data))->major)
#define iiop_minor_version(x)	(((struct IIOP_DataBlock *)(call_connection(x)->co_protocol_data))->minor)
#define iiop_short_char_codeset(x)	(iiop_packet(x)->short_char_codeset)
#define iiop_char_codeset(x)	(iiop_packet(x)->char_codeset)

#define ODD(x)		(((x)&0x1)!=0)
#define EVEN(x)		(((x)&0x1)==0)  

/* These macros assume that a pointer and an int are the same size; this is not true under WIN16. */

#if defined(WIN16)
#define PAD2(x)		(2-(((unsigned long)(x))&0x1))
#define PAD4(x)		(4-(((unsigned long)(x))&0x3))
#define PAD8(x)		(8-(((unsigned long)(x))&0x7))

#define PADDED_PTR(p,x)		(((unsigned long)((p)+((x)-1)))&(~(((unsigned long)(x))-1)))
#define PADDING_NEC(p,x)	(PADDED_PTR((p),(x))-((unsigned long)(p)))
#define PADDED(x,p)		(PADDED_PTR((p),(x))-((unsigned long)(p))+(x))

#else
#define PAD2(x)		(2-(((unsigned)(x))&0x1))
#define PAD4(x)		(4-(((unsigned)(x))&0x3))
#define PAD8(x)		(8-(((unsigned)(x))&0x7))

#define PADDED_PTR(p,x)		(((unsigned)((p)+((x)-1)))&(~(((unsigned)(x))-1)))
#define PADDING_NEC(p,x)	(PADDED_PTR((p),(x))-((unsigned)(p)))
#define PADDED(x,p)		(PADDED_PTR((p),(x))-((unsigned)(p))+(x))
#endif

#define PACKET_ADJUST(p,a)	(p) = (unsigned char *)PADDED_PTR((p),(a))
#define PACKET_INCR(p,x,a)	((p)->vop += (PADDING_NEC((p)->vop, (a)) + (x)))

#define PTR_ADJUST(x,p,a)	((x)+=PADDING_NEC((p)->vop,(a)))

#define PACKET_OBTAIN(p,x,a,e)	(transport_get_input_buffer((p)->bs, PADDING_NEC((p)->vop,(a)) + (x), (e)))
#define PACKET_ALLOC(p,x,a,e)	(transport_get_output_buffer((p)->bs, PADDING_NEC((p)->vop,(a)) + (x), (e)))
#define PACKET_READ(p,b,l,e)	(transport_read_bytes((p)->bs, (b), (l), (e)))
#define PACKET_WRITE(p,b,l,e)	(transport_write_bytes((p)->bs, (b), (l), (e)))

#define MEMCPY(to,from,len)	memcpy((void *)(to),(void *)(from),(len))

#ifdef WORDS_BIGENDIAN
#define NATIVE_BYTE_ORDER BigEndian
#else
#define NATIVE_BYTE_ORDER LittleEndian
#endif

#define ENDIAN_MATCH(p)		((p)->byteorder == NATIVE_BYTE_ORDER)

/* typecode values */

#define CORBA_tk_null		0
#define CORBA_tk_void		1
#define CORBA_tk_shortinteger	2
#define CORBA_tk_integer	3
#define CORBA_tk_shortcardinal	4
#define CORBA_tk_cardinal	5
#define CORBA_tk_shortreal	6
#define CORBA_tk_real		7
#define CORBA_tk_boolean	8
#define CORBA_tk_shortcharacter	9
#define CORBA_tk_byte		10
#define CORBA_tk_variant	11
#define CORBA_tk_type		12
#define CORBA_tk_Principal	13
#define CORBA_tk_object		14
#define CORBA_tk_record		15
#define CORBA_tk_union		16
#define CORBA_tk_enumeration	17
#define CORBA_tk_string		18
#define CORBA_tk_sequence	19
#define CORBA_tk_array		20
#define CORBA_tk_alias		21
#define CORBA_tk_except		22

#define CORBA_tk_longinteger	23
#define CORBA_tk_longcardinal	24
#define CORBA_tk_longreal	25
#define CORBA_tk_character	26
#define CORBA_tk_optional	27
#define CORBA_tk_reference	28
