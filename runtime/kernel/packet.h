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
/* $Id: packet.h,v 1.16 1999/08/03 01:52:53 janssen Exp $ */

typedef struct ilu_packet_s *PACKET;

struct ilu_packet_s {
  /*for access and calling: L2 >= {call's conn's iomu};
			    L1, Main unconstrained*/
  ilu_bytes buf;
  ilu_bytes next;
  ilu_cardinal count;
  ilu_byte byte_order;

  void    (*put_byte) (PACKET p, signed char l, ILU_ERRS((IoErrs)) *);
  void    (*put_short) (PACKET p, ilu_shortinteger l, ILU_ERRS((IoErrs)) *);
  void    (*put_long) (PACKET p, ilu_integer l, ILU_ERRS((IoErrs)) *);
  void    (*put_u_byte) (PACKET p, ilu_byte l, ILU_ERRS((IoErrs)) *);
  void    (*put_u_short) (PACKET p, ilu_shortcardinal l, ILU_ERRS((IoErrs)) *);
  void    (*put_u_long) (PACKET p, ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void    (*put_float) (PACKET p, ilu_shortreal l, ILU_ERRS((IoErrs)) *);
  void    (*put_double) (PACKET p, ilu_real l, ILU_ERRS((IoErrs)) *);
  void    (*put_bytes) (PACKET p, ilu_bytes bytes,
			   ilu_cardinal l, ILU_ERRS((IoErrs)) *);
  void    (*put_opaque) (PACKET p, ilu_bytes bytes,
			    ilu_cardinal l, ILU_ERRS((IoErrs)) *);

  void            (*get_byte) (PACKET p, signed char *l,
			       ILU_ERRS((IoErrs)) * err);
  void            (*get_short) (PACKET p, ilu_shortinteger *l,
				ILU_ERRS((IoErrs)) * err);
  void            (*get_long) (PACKET p, ilu_integer *l,
			       ILU_ERRS((IoErrs)) * err);
  void            (*get_u_byte) (PACKET p, ilu_bytes l,
				 ILU_ERRS((IoErrs)) * err);
  void            (*get_u_short) (PACKET p, ilu_shortcardinal *l,
				  ILU_ERRS((IoErrs)) * err);
  void            (*get_u_long) (PACKET p, ilu_cardinal *l,
				 ILU_ERRS((IoErrs)) * err);
  void            (*get_float) (PACKET p, ilu_shortreal *l,
				ILU_ERRS((IoErrs)) * err);
  void            (*get_double) (PACKET p, ilu_real *l,
				 ILU_ERRS((IoErrs)) * err);
  void            (*get_bytes) (PACKET p, ilu_bytes *bytes,
				ilu_cardinal *l,
				ilu_cardinal max_size,
				ILU_ERRS((IoErrs)) * err);
  void            (*get_opaque) (PACKET p,
				 ilu_bytes *bytes,
				 ilu_cardinal l,
				 ILU_ERRS((IoErrs)) * err);

  void (*destroy)(PACKET p);
};

/*L2 >= {call's conn's iomu}; L1, Main unconstrained*/

#define packet_put_byte(p,b,e)		((*(p)->put_byte)((p),(b),(e)))
#define packet_put_short(p,b,e)		((*(p)->put_short)((p),(b),(e)))
#define packet_put_long(p,b,e)		((*(p)->put_long)((p),(b),(e)))
#define packet_put_u_byte(p,b,e)	((*(p)->put_u_byte)((p),(b),(e)))
#define packet_put_u_short(p,b,e)	((*(p)->put_u_short)((p),(b),(e)))
#define packet_put_u_long(p,b,e)	((*(p)->put_u_long)((p),(b),(e)))
#define packet_put_float(p,b,e)		((*(p)->put_float)((p),(b),(e)))
#define packet_put_double(p,b,e)	((*(p)->put_double)((p),(b),(e)))
#define packet_put_bytes(p,b,l,e)	((*(p)->put_bytes)((p),(b),(l),(e)))
#define packet_put_opaque(p,b,l,e)	((*(p)->put_opaque)((p),(b),(l),(e)))

#define packet_get_byte(p,b,e)	((*(p)->get_byte)((p),(b),(e)))
#define packet_get_short(p,b,e)	((*(p)->get_short)((p),(b),(e)))
#define packet_get_long(p,b,e)	((*(p)->get_long)((p),(b),(e)))
#define packet_get_u_byte(p,b,e)	((*(p)->get_u_byte)((p),(b),(e)))
#define packet_get_u_short(p,b,e)	((*(p)->get_u_short)((p),(b),(e)))
#define packet_get_u_long(p,b,e)	((*(p)->get_u_long)((p),(b),(e)))
#define packet_get_float(p,b,e)	((*(p)->get_float)((p),(b),(e)))
#define packet_get_double(p,b,e)	((*(p)->get_double)((p),(b),(e)))
#define packet_get_bytes(p,b,l,lim,e)	((*(p)->get_bytes)((p),(b),(l),(lim),(e)))
#define packet_get_opaque(p,b,l,e)	((*(p)->get_opaque)((p),(b),(l),(e)))

#define packet_destroy(p)	((*(p)->destroy)(p))

#define packet_buffer(p)	((p)->buf)
#define packet_size(p)		(((p)->next)-((p)->buf))
#define packet_allocation(p)	((p)->count)
#define packet_pointer(p)	((p)->next)
