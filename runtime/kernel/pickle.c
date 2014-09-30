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

$Id: pickle.c,v 1.31 1999/08/03 01:53:08 janssen Exp $
*/
/* Last edited by Mike Spreitzer April 30, 1998 11:44 am PDT */

#include "iluntrnl.h"
#include "call.h"
#include "connect.h"
#include "iluprotocol.h"

#define PICKLE_VERSION(x)		(((x) >> 5) & 0x7)

ilu_string
  ilu_PickleType (ilu_Pickle pickle,
		  ilu_Error *err)
{
  if (0) { }
#ifdef ADD_PICKLE2_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 2)
    return _ilu_pickle2_PickleType(pickle, err);
#endif
#ifdef ADD_PICKLE3_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 3)
    return _ilu_pickle3_PickleType(pickle, err);
#endif
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_PickleType:  pickle with unsupported format %lu encounted.\n",
	      PICKLE_VERSION(pickle.pi_bytes[0])));
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupportedPickleFormat, err);
    return NIL;
  }
}

ilu_TypeKind
  ilu_PickleTypeKind (ilu_Pickle pickle,
		      ilu_Error *err)
{
  if (0) { }
#ifdef ADD_PICKLE2_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 2)
    return _ilu_pickle2_PickleTypeKind(pickle, err);
#endif
#ifdef ADD_PICKLE3_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 3)
    return _ilu_pickle3_PickleTypeKind(pickle, err);
#endif
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_PickleType:  pickle with unsupported format %lu encounted.\n",
	      PICKLE_VERSION(pickle.pi_bytes[0])));
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupportedPickleFormat, 0);
    return 0;
  }
}

ilu_boolean
  ilu_PickleTypes (ilu_Pickle pickle,
		   ilu_string **types_out,
		   ilu_cardinal *types_count_out,
		   ilu_Error *err)
{
  if (0) { }
#ifdef ADD_PICKLE2_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 2)
    return _ilu_pickle2_PickleTypes(pickle, types_out, types_count_out, err);
#endif
#ifdef ADD_PICKLE3_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 3)
    return _ilu_pickle3_PickleTypes(pickle, types_out, types_count_out, err);
#endif
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_PickleType:  pickle with unsupported format %lu encounted.\n",
	      PICKLE_VERSION(pickle.pi_bytes[0])));
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupportedPickleFormat, ilu_FALSE);
    return ilu_FALSE;
  }
}

ilu_boolean
  ilu_StartPickle (ilu_Call call,
		   ilu_Type type_id,
		   ilu_Error *err)
{
#if (defined(ADD_PICKLE2_SUPPORT))
  return _ilu_pickle2_StartPickle(call, type_id, err);
#elif (defined(ADD_PICKLE3_SUPPORT))
  return _ilu_pickle3_StartPickle(call, type_id, err);
#else
#error "No pickle formats compiled in!"
#endif
}

ilu_boolean
  ilu_WritePickle (ilu_Call call,
		   ilu_cardinal argSize,	/* hint; pickle will resize if necessary */
		   ilu_string type_id,
		   ilu_Error *err)
{
  if (0) {}
#ifdef ADD_PICKLE2_SUPPORT
  else if (call_connection(call) == (&_ilu_pickle2_Format))
    return _ilu_pickle2_WritePickle(call, argSize, type_id, err);
#endif /* defined(ADD_PICKLE2_SUPPORT) */
#ifdef ADD_PICKLE3_SUPPORT
  else if (call_connection(call) == (&_ilu_pickle3_Format))
    return _ilu_pickle3_WritePickle(call, argSize, type_id, err);
#endif /* ADD_PICKLE3_SUPPORT */
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_WritePickle:  bad pickle format encountered!\n"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, ilu_FALSE);
  }
}

ilu_boolean
  ilu_ReadPickle (ilu_Call call,
		  ilu_Pickle pickle,
		  ilu_Error *err)
{
  if (0) {}
#ifdef ADD_PICKLE2_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 2) {
    _ilu_pickle2_StartPickle(call, (ilu_Type) call->ca_prdata2, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    return _ilu_pickle2_ReadPickle(call, pickle, err);
  }
#endif
#ifdef ADD_PICKLE3_SUPPORT
  else if (PICKLE_VERSION(pickle.pi_bytes[0]) == 3) {
    _ilu_pickle3_StartPickle(call, (ilu_Type) call->ca_prdata2, err);
    if (ILU_ERRNOK(*err)) return ilu_FALSE;
    return _ilu_pickle3_ReadPickle(call, pickle, err);
  }
#endif
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_ReadPickle:  pickle with unsupported format %lu encounted.\n",
	      PICKLE_VERSION(pickle.pi_bytes[0])));
    ILU_ERR_CONS1(imp_limit, err, minor, ilu_ilm_unsupportedPickleFormat, err);
    return ilu_FALSE;
  }
}

ilu_boolean
  ilu_EndPickle (ilu_Call call,
		 ilu_Pickle *pickle,
		 ilu_Error *err)
{
  if (0) {}
#ifdef ADD_PICKLE2_SUPPORT
  else if (call_connection(call) == &_ilu_pickle2_Format)
    return _ilu_pickle2_EndPickle(call, pickle, err);
#endif
#ifdef ADD_PICKLE3_SUPPORT
  else if (call_connection(call) == &_ilu_pickle3_Format)
    return _ilu_pickle3_EndPickle(call, pickle, err);
#endif
  else {
    ILU_NOTE(TYPE_DEBUG,
	     ("ilu_EndPickle:  bad pickle format encountered!\n"));
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, ilu_FALSE);
  }
}

/**********************************************************************/
/**********************************************************************/
/******  Pickle marshalling routines  *********************************/
/**********************************************************************/
/**********************************************************************/

/* Main invariant holds */

void 
  _ilu_OutputPickle (ilu_Call call,	/* the call in progress */
		     ilu_Pickle t,
		     ilu_Type the_type,
		     ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
    return;
  }
  (void) protocol_output_bytes(call_proto(call), call, t.pi_bytes, t.pi_len, 0, err);
  return;
}


ilu_cardinal
  _ilu_SizeOfPickle (ilu_Call call,	/* the call in progress */
		     ilu_Pickle t,
		     ilu_Type the_type,
		     ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_bytes(call_proto(call), call, t.pi_bytes, t.pi_len, 0, err);
  return (ILU_ERROK(*err) ? size : 0);
}


ilu_boolean
  _ilu_InputPickle (ilu_Call call,	/* the call in progress */
		    ilu_Pickle *t,
		    ilu_Type the_type,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
    return ilu_FALSE;
  }
  t->pi_bytes = NIL;
  protocol_input_bytes(call_proto(call), call, &t->pi_bytes, &t->pi_len, 0, err);
  return ILU_ERROK(*err);
}

void 
  ilu_OutputPickle (ilu_Call call,	/* the call in progress */
		    ilu_Pickle t,
		    ilu_Type the_type,
		    ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
    return;
  } else if (t.pi_bytes == NIL || t.pi_len < 2) {
    ILU_ERR_CONS1(marshal, err, minor, ilu_mm_badPickle, 0);
    return;
  }
  (void) protocol_output_pickle(call_proto(call), call, t, the_type, err);
  return;
}


ilu_cardinal
  ilu_SizeOfPickle (ilu_Call call,	/* the call in progress */
		    ilu_Pickle t,
		    ilu_Type the_type,
		    ILU_ERRS((IoErrs)) * err)
{
  ilu_cardinal    size;
  if (call_connection(call) == NIL)
    return ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
  else if (t.pi_bytes == NIL || t.pi_len < 2)
    return ILU_ERR_CONS1(marshal, err, minor, ilu_mm_badPickle, 0);
  else if (!protocol_needs_sizing(connection_protocol(call_connection(call))))
    { ILU_CLER(*err); return 0; };
  size = protocol_size_of_pickle(call_proto(call), call, t, the_type, err);
  return (ILU_ERROK(*err) ? size : 0);
}

ilu_boolean
  ilu_InputPickle (ilu_Call call,	/* the call in progress */
		   ilu_Pickle *t,
		   ilu_Type the_type,
		   ILU_ERRS((IoErrs)) * err)
{
  if (call_connection(call) == NIL) {
    ILU_ERR_CONS1(internal, err, minor, ilu_im_pickleFormat, 0);
    return ilu_FALSE;
  }
  t->pi_bytes = NIL;
  return (protocol_input_pickle(call_proto(call), call, t, the_type, err));
}

ilu_boolean
  ilu_FreePickle (ilu_Pickle *t,
		  ilu_Error *err)
{
  ilu_free(t->pi_bytes);
  t->pi_bytes = NIL;
  t->pi_len = 0;
  return (ILU_CLER(*err));
}
