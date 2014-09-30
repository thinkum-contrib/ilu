/*  -*- Mode: C; -*-
 *
 * This code contributed by Siemens Corporate Research, Inc.
 */
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

#ifndef _ILU_SCM_PRIVATE_H_
#define _ILU_SCM_PRIVATE_H_

#include "ilu-type.h"

#define iluguile_prefer_success(e) iluguile_full_prefer_success(e,__FILE__,__LINE__)

/* callable by C */
ilu_cardinal iluguile_scheme_lang_idx();
void iluguile_full_prefer_success(ilu_Error* e, const char* atf, unsigned int atl);
ilu_Object iluguile_input_object_id(iluguile_SCMCall, ilu_boolean, ilu_Class);
SCM iluguile_get_language_specific_object(ilu_Object obj);
ilu_Server iluguile_get_object_server(ilu_Object obj);
ilu_boolean iluguile_output_object_id(iluguile_SCMCall,ilu_Object,ilu_boolean,ilu_Class);
ilu_cardinal iluguile_size_of_object_id(iluguile_SCMCall,ilu_Object,ilu_boolean,ilu_Class);
void iluguile_exit_server(ilu_Server, ilu_Class);
ilu_RcvReqStat iluguile_receive_request(ilu_Connection, iluguile_SCMCall, ilu_boolean*,
					ilu_Class*, ilu_Method*, ilu_cardinal*);
ilu_Port iluguile_create_port(ilu_Server, ilu_string, ilu_TransportInfo);
ilu_Connection iluguile_handle_new_connection(ilu_Port port);

/* scheme stubs */
SCM iluguile__scheme_lang_idx();
SCM iluguile__full_prefer_success(SCM _e, SCM _atf, SCM _atl);
SCM iluguile__enter_ot_mu();
SCM iluguile__exit_ot_mu();
SCM iluguile__object_type_defined(SCM _t);
SCM iluguile__define_exception(SCM _i, SCM _e);
SCM iluguile__define_object_type(SCM, SCM, SCM, SCM, SCM, SCM, SCM, SCM, SCM, SCM);
SCM iluguile__define_method(SCM, SCM, SCM, SCM, SCM, SCM, SCM, SCM);
SCM iluguile__start_call(SCM _call, SCM _s, SCM _intro_type, SCM _method);
SCM iluguile__begin_sizing_exception(SCM _call, SCM _eIndex);
SCM iluguile__begin_exception(SCM _call, SCM _eIndex, SCM _argSize);
SCM iluguile__start_request(SCM _call, SCM _argSize);
SCM iluguile__begin_sizing_reply(SCM _call, SCM _exns_possible);
SCM iluguile__begin_reply(SCM _call, SCM _exceptions, SCM _argSize);
SCM iluguile__end_sequence(SCM _call);
SCM iluguile__end_union(SCM _call);
SCM iluguile__end_record(SCM _call);
SCM iluguile__end_array(SCM _call);
SCM iluguile__exception_of_method(SCM _method, SCM _index);
SCM iluguile__exception_count_of_method(SCM _m);
SCM iluguile__get_object_class(SCM _obj);
SCM iluguile__get_object_server(SCM _obj);
SCM iluguile__reply_read(SCM _call);
SCM iluguile__finish_call(SCM _call);
SCM iluguile__finish_exception(SCM _call);
SCM iluguile__finish_request(SCM _call);
SCM iluguile__finish_reply(SCM _call);
SCM iluguile__no_reply(SCM _call);
SCM iluguile__finish_parameters(SCM _call, SCM _obj);
SCM iluguile__input_byte(SCM _call);
SCM iluguile__input_boolean(SCM _call);
SCM iluguile__input_optional(SCM _call);
SCM iluguile__input_cardinal(SCM _call);
SCM iluguile__input_character(SCM _call);
SCM iluguile__input_enum(SCM _call);
SCM iluguile__input_integer(SCM _call);
SCM iluguile__input_real(SCM _call);
SCM iluguile__input_short_cardinal(SCM _call);
SCM iluguile__input_short_integer(SCM _call);
SCM iluguile__input_short_real(SCM _call);
SCM iluguile__input_long_cardinal(SCM _call);
SCM iluguile__input_long_integer(SCM _call);
SCM iluguile__input_long_real(SCM _call);
SCM iluguile__input_bytes(SCM _call, SCM _limit);
SCM iluguile__input_opaque(SCM _call, SCM _limit);
SCM iluguile__input_string(SCM _call, SCM _limit);
SCM iluguile__input_string_vec(SCM _call, SCM _len);
SCM iluguile__input_w_string(SCM _call, SCM _limit);
SCM iluguile__input_w_string_vec(SCM _call, SCM _len);
SCM iluguile__input_object_id(SCM _call, SCM _discriminator_p, SCM _putative_class);
SCM iluguile__input_sequence(SCM _call, SCM _limit);
SCM iluguile__input_union(SCM _call, SCM _limit);
SCM iluguile__input_record(SCM _call);
SCM iluguile__input_array(SCM _call);
SCM iluguile__enter_server(SCM _ks, SCM _c);
SCM iluguile__exit_server(SCM _ks, SCM _c);
SCM iluguile__sbh_to_object(SCM _sbh, SCM _c);
SCM iluguile__sbh_of_object(SCM _obj);
SCM iluguile__output_byte(SCM _call, SCM _byte);
SCM iluguile__output_boolean(SCM _call, SCM _b);
SCM iluguile__output_optional(SCM _call, SCM _b);
SCM iluguile__output_cardinal(SCM _call, SCM _v);
SCM iluguile__output_character(SCM _call, SCM _v);
SCM iluguile__output_enum(SCM _call, SCM _v);
SCM iluguile__output_integer(SCM _call, SCM _v);
SCM iluguile__output_real(SCM _call, SCM _v);
SCM iluguile__output_short_integer(SCM _call, SCM _v);
SCM iluguile__output_short_real(SCM _call, SCM _v);
SCM iluguile__output_long_cardinal(SCM _call, SCM _v);
SCM iluguile__output_long_integer(SCM _call, SCM _v);
SCM iluguile__output_long_real(SCM _call, SCM _v);
SCM iluguile__output_bytes(SCM _call, SCM _buff, SCM _len, SCM _limit);
SCM iluguile__output_opaque(SCM _call, SCM _buff, SCM _len);
SCM iluguile__output_string(SCM _call, SCM _buff, SCM _len, SCM _limit);
SCM iluguile__output_string_vec(SCM _call, SCM _buff, SCM _len);
SCM iluguile__output_w_string(SCM _call, SCM _buf, SCM _len, SCM _limit);
SCM iluguile__output_w_string_vec(SCM _call, SCM _buf, SCM _len);
SCM iluguile__output_object_id(SCM _call, SCM _obj, SCM, SCM);
SCM iluguile__output_sequence(SCM _call, SCM _length, SCM _limit);
SCM iluguile__output_union(SCM _call, SCM _discriminator, SCM _discriminator_size);
SCM iluguile__output_array(SCM _call);
SCM iluguile__output_record(SCM _call);
SCM iluguile__size_of_byte(SCM _call, SCM _byte);
SCM iluguile__size_of_boolean(SCM _call, SCM _b);
SCM iluguile__size_of_optional(SCM _call, SCM _opt);
SCM iluguile__size_of_cardinal(SCM _call, SCM _val);
SCM iluguile__size_of_character(SCM _call, SCM _val);
SCM iluguile__size_of_enum(SCM _call, SCM _val);
SCM iluguile__size_of_integer(SCM _call, SCM _val);
SCM iluguile__size_of_real(SCM _call, SCM _val);
SCM iluguile__size_of_short_cardinal(SCM _call, SCM _val);
SCM iluguile__size_of_short_integer(SCM _call, SCM _val);
SCM iluguile__size_of_short_real(SCM _call, SCM _val);
SCM iluguile__size_of_long_cardinal(SCM _call, SCM _val);
SCM iluguile__size_of_long_integer(SCM _call, SCM _val);
SCM iluguile__size_of_long_real(SCM _call, SCM _val);
SCM iluguile__size_of_bytes(SCM _call, SCM _buf, SCM _len, SCM _limit);
SCM iluguile__size_of_opaque(SCM _call, SCM _buf, SCM _len);
SCM iluguile__size_of_string(SCM _call, SCM _buf, SCM _len, SCM _limit);
SCM iluguile__size_of_string_vec(SCM _call, SCM _buf, SCM _len);
SCM iluguile__size_of_w_string(SCM _call, SCM _buf, SCM _len, SCM _limit);
SCM iluguile__size_of_w_string_vec(SCM _call, SCM _buf, SCM _len);
SCM iluguile__size_of_object_id(SCM call, SCM obj, SCM, SCM);
SCM iluguile__size_of_sequence(SCM _call, SCM _len, SCM _limit);
SCM iluguile__size_of_union(SCM _call, SCM _discriminator, SCM _discriminator_size);
SCM iluguile__size_of_array(SCM _call);
SCM iluguile__size_of_record(SCM _call);
SCM iluguile__wait_for_reply(SCM _call);
SCM iluguile__set_default_server(SCM _s);
SCM iluguile__get_default_server();
SCM iluguile__create_port(SCM _server, SCM _protocolType, SCM _transportType);
SCM iluguile__set_server_default_port(SCM _s, SCM _p);
SCM iluguile__parse_sbh(SCM _sbh);
SCM iluguile__receive_request(SCM _conn);
SCM iluguile__handle_new_connection(SCM _port);
SCM iluguile__close_connection(SCM _conn);
SCM iluguile__wait_for_port_connection_request(SCM _p);
SCM iluguile__create_true_kernel_object(SCM _ih, SCM _server, SCM _c, SCM _lspo);
SCM iluguile__find_class_from_type_name(SCM type_name);
SCM iluguile__find_class_from_id(SCM _id);
SCM iluguile__get_language_specific_object(SCM _obj);
SCM iluguile__set_language_specific_object(SCM _obj, SCM _lspo);
SCM iluguile__make_main_loop_id();
SCM iluguile__free_main_loop_id(SCM id);
SCM iluguile__run_main_loop(SCM stop);
SCM iluguile__exit_main_loop(SCM stop);
SCM iluguile__register_input_handler(SCM _fd, SCM _handlerProc);
SCM iluguile__unregister_input_handler(SCM _fd);
SCM iluguile__register_output_handler(SCM _fd, SCM _handlerProc);
SCM iluguile__unregister_output_handler(SCM _fd);
SCM iluguile__publish_object(SCM _obj);
SCM iluguile__withdraw_object(SCM _obj, SCM _proof);
SCM iluguile__server_of_object(SCM _obj);
SCM iluguile__set_main_loop(SCM ml);
SCM iluguile__create_alarm();
SCM iluguile__set_alarm(SCM _alarm, SCM _time, SCM _proc);
SCM iluguile__clear_alarm(SCM _alarm);
SCM iluguile__time_now();
SCM iluguile__err_nok(SCM _err);
SCM iluguile__set_method_stub_proc(SCM _m, SCM _proc);
SCM iluguile__get_gc_callback_class();
SCM iluguile__is_gc_client_set();
SCM iluguile__create_object_table(SCM, SCM);
SCM iluguile__get_ilu_corba_object_type_id();
SCM iluguile_call__create();
SCM iluguile_call__destroy(SCM _call);
SCM iluguile_call__caller(SCM _call);
SCM iluguile_call__protocol_exception(SCM _call);
SCM iluguile_call__error(SCM _call);
SCM iluguile_call__method_of_call(SCM _call);
SCM iluguile_class__method(SCM _c, SCM _num);
SCM iluguile_class__name(SCM _c);
SCM iluguile_class__unique_id(SCM _c);
SCM iluguile_server__add_port(SCM _server, SCM _prot, SCM _trans, SCM _default);
SCM iluguile_server__create(SCM _name, SCM _objtab);

#endif
