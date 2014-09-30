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

#include <stdlib.h>
#include "ilu-type.h"
#include "iluobj-scm.h"
#include "ilusrvr-scm.h"
#include "ilu-scm-private.h"

SCM* iluguile_scm_retval = 0;

static void iluguile_init()
{
  iluguile_scm_retval = iluguile_scm_make_array(1);

  /* from ilu-scm.c */
  iluguile_define_func(_scheme_lang_idx, 0, 0, 0);
  iluguile_define_func(_full_prefer_success, 3, 0, 0);
  iluguile_define_func(_enter_ot_mu, 0, 0, 0);
  iluguile_define_func(_exit_ot_mu, 0, 0, 0);
  iluguile_define_func(_object_type_defined, 1, 0, 0);
  iluguile_define_func(_define_exception, 2, 0, 0);
  iluguile_define_func(_define_object_type, 10, 0, 0);
  iluguile_define_func(_define_method, 8, 0, 0);
  iluguile_define_func(_start_call, 4, 0, 0);
  iluguile_define_func(_begin_sizing_exception, 2, 0, 0);
  iluguile_define_func(_begin_exception, 3, 0, 0);
  iluguile_define_func(_start_request, 2, 0, 0);
  iluguile_define_func(_begin_sizing_reply, 2, 0, 0);
  iluguile_define_func(_begin_reply, 3, 0, 0);
  iluguile_define_func(_end_sequence, 1, 0, 0);
  iluguile_define_func(_end_union, 1, 0, 0);
  iluguile_define_func(_end_record, 1, 0, 0);
  iluguile_define_func(_end_array, 1, 0, 0);
  iluguile_define_func(_exception_of_method, 2, 0, 0);
  iluguile_define_func(_exception_count_of_method, 1, 0, 0);
  iluguile_define_func(_get_object_class, 1, 0, 0);
  iluguile_define_func(_get_object_server, 1, 0, 0);
  iluguile_define_func(_reply_read, 1, 0, 0);
  iluguile_define_func(_finish_call, 1, 0, 0);
  iluguile_define_func(_finish_exception, 1, 0, 0);
  iluguile_define_func(_finish_request, 1, 0, 0);
  iluguile_define_func(_finish_reply, 1, 0, 0);
  iluguile_define_func(_no_reply, 1, 0, 0);
  iluguile_define_func(_finish_parameters, 2, 0, 0);
  iluguile_define_func(_input_byte, 1, 0, 0);
  iluguile_define_func(_input_boolean, 1, 0, 0);
  iluguile_define_func(_input_optional, 1, 0, 0);
  iluguile_define_func(_input_cardinal, 1, 0, 0);
  iluguile_define_func(_input_character, 1, 0, 0);
  iluguile_define_func(_input_enum, 1, 0, 0);
  iluguile_define_func(_input_integer, 1, 0, 0);
  iluguile_define_func(_input_real, 1, 0, 0);
  iluguile_define_func(_input_short_cardinal, 1, 0, 0);
  iluguile_define_func(_input_short_integer, 1, 0, 0);
  iluguile_define_func(_input_short_real, 1, 0, 0);
  iluguile_define_func(_input_long_cardinal, 1, 0, 0);
  iluguile_define_func(_input_long_integer, 1, 0, 0);
  iluguile_define_func(_input_long_real, 1, 0, 0);
  iluguile_define_func(_input_bytes, 2, 0, 0);
  iluguile_define_func(_input_opaque, 2, 0, 0);
  iluguile_define_func(_input_string, 2, 0, 0);
  iluguile_define_func(_input_string_vec, 2, 0, 0);
  iluguile_define_func(_input_w_string, 2, 0, 0);
  iluguile_define_func(_input_w_string_vec, 2, 0, 0);
  iluguile_define_func(_input_object_id, 3, 0, 0);
  iluguile_define_func(_input_sequence, 2, 0, 0);
  iluguile_define_func(_input_union, 2, 0, 0);
  iluguile_define_func(_input_record, 1, 0, 0);
  iluguile_define_func(_input_array, 1, 0, 0);
  iluguile_define_func(_enter_server, 2, 0, 0);
  iluguile_define_func(_exit_server, 2, 0, 0);
  iluguile_define_func(_sbh_to_object, 2, 0, 0);
  iluguile_define_func(_sbh_of_object, 1, 0, 0);
  iluguile_define_func(_output_byte, 2, 0, 0);
  iluguile_define_func(_output_boolean, 2, 0, 0);
  iluguile_define_func(_output_optional, 2, 0, 0);
  iluguile_define_func(_output_cardinal, 2, 0, 0);
  iluguile_define_func(_output_character, 2, 0, 0);
  iluguile_define_func(_output_enum, 2, 0, 0);
  iluguile_define_func(_output_integer, 2, 0, 0);
  iluguile_define_func(_output_real, 2, 0, 0);
  iluguile_define_func(_output_short_integer, 2, 0, 0);
  iluguile_define_func(_output_short_real, 2, 0, 0);
  iluguile_define_func(_output_long_cardinal, 2, 0, 0);
  iluguile_define_func(_output_long_integer, 2, 0, 0);
  iluguile_define_func(_output_long_real, 2, 0, 0);
  iluguile_define_func(_output_bytes, 4, 0, 0);
  iluguile_define_func(_output_opaque, 3, 0, 0);
  iluguile_define_func(_output_string, 4, 0, 0);
  iluguile_define_func(_output_string_vec, 3, 0, 0);
  iluguile_define_func(_output_w_string, 4, 0, 0);
  iluguile_define_func(_output_w_string_vec, 3, 0, 0);
  iluguile_define_func(_output_object_id, 4, 0, 0);
  iluguile_define_func(_output_sequence, 3, 0, 0);
  iluguile_define_func(_output_union, 3, 0, 0);
  iluguile_define_func(_output_array, 1, 0, 0);
  iluguile_define_func(_output_record, 1, 0, 0);
  iluguile_define_func(_size_of_byte, 2, 0, 0);
  iluguile_define_func(_size_of_boolean, 2, 0, 0);
  iluguile_define_func(_size_of_optional, 2, 0, 0);
  iluguile_define_func(_size_of_cardinal, 2, 0, 0);
  iluguile_define_func(_size_of_character, 2, 0, 0);
  iluguile_define_func(_size_of_enum, 2, 0, 0);
  iluguile_define_func(_size_of_integer, 2, 0, 0);
  iluguile_define_func(_size_of_real, 2, 0, 0);
  iluguile_define_func(_size_of_short_cardinal, 2, 0, 0);
  iluguile_define_func(_size_of_short_integer, 2, 0, 0);
  iluguile_define_func(_size_of_short_real, 2, 0, 0);
  iluguile_define_func(_size_of_long_cardinal, 2, 0, 0);
  iluguile_define_func(_size_of_long_integer, 2, 0, 0);
  iluguile_define_func(_size_of_long_real, 2, 0, 0);
  iluguile_define_func(_size_of_bytes, 4, 0, 0);
  iluguile_define_func(_size_of_opaque, 3, 0, 0);
  iluguile_define_func(_size_of_string, 4, 0, 0);
  iluguile_define_func(_size_of_string_vec, 3, 0, 0);
  iluguile_define_func(_size_of_w_string, 4, 0, 0);
  iluguile_define_func(_size_of_w_string_vec, 3, 0, 0);
  iluguile_define_func(_size_of_object_id, 4, 0, 0);
  iluguile_define_func(_size_of_sequence, 3, 0, 0);
  iluguile_define_func(_size_of_union, 3, 0, 0);
  iluguile_define_func(_size_of_array, 1, 0, 0);
  iluguile_define_func(_size_of_record, 1, 0, 0);
  iluguile_define_func(_wait_for_reply, 1, 0, 0);
  iluguile_define_func(_set_default_server, 1, 0, 0);
  iluguile_define_func(_get_default_server, 0, 0, 0);
  iluguile_define_func(_create_port, 3, 0, 0);
  iluguile_define_func(_set_server_default_port, 2, 0, 0);
  iluguile_define_func(_parse_sbh, 1, 0, 0);
  iluguile_define_func(_receive_request, 1, 0, 0);
  iluguile_define_func(_handle_new_connection, 1, 0, 0);
  /*  iluguile_define_func(_close_connection, 1, 0, 0);*/
  iluguile_define_func(_wait_for_port_connection_request, 1, 0, 0);
  iluguile_define_func(_create_true_kernel_object, 4, 0, 0);
  iluguile_define_func(_find_class_from_type_name, 1, 0, 0);
  iluguile_define_func(_find_class_from_id, 1, 0, 0);
  iluguile_define_func(_get_language_specific_object, 1, 0, 0);
  iluguile_define_func(_set_language_specific_object, 2, 0, 0);
  iluguile_define_func(_make_main_loop_id, 0, 0, 0);
  iluguile_define_func(_free_main_loop_id, 1, 0, 0);
  iluguile_define_func(_run_main_loop, 1, 0, 0);
  iluguile_define_func(_exit_main_loop, 1, 0, 0);
  iluguile_define_func(_register_input_handler, 2, 0, 0);
  iluguile_define_func(_unregister_input_handler, 1, 0, 0);
  iluguile_define_func(_register_output_handler, 2, 0, 0);
  iluguile_define_func(_unregister_output_handler, 1, 0, 0);
  iluguile_define_func(_publish_object, 1, 0, 0);
  iluguile_define_func(_withdraw_object, 2, 0, 0);
  iluguile_define_func(_server_of_object, 1, 0, 0);
  iluguile_define_func(_set_main_loop, 1, 0, 0);
  iluguile_define_func(_create_alarm, 0, 0, 0);
  iluguile_define_func(_set_alarm, 3, 0, 0);
  iluguile_define_func(_clear_alarm, 1, 0, 0);
  iluguile_define_func(_time_now, 0, 0, 0);
  iluguile_define_func(_err_nok, 1, 0, 0);
  iluguile_define_func(_set_method_stub_proc, 2, 0, 0);
  iluguile_define_func(_get_gc_callback_class, 0, 0, 0);
  iluguile_define_func(_is_gc_client_set, 0, 0, 0);
  iluguile_define_func(_create_object_table, 2, 0, 0);
  iluguile_define_func(_get_ilu_corba_object_type_id, 0, 0, 0);

  /* ilu call stuff */
  iluguile_define_func(call__create, 0, 0, 0);
  iluguile_define_func(call__destroy, 1, 0, 0);
  iluguile_define_func(call__caller, 1, 0, 0);
  iluguile_define_func(call__protocol_exception, 1, 0, 0);
  iluguile_define_func(call__error, 1, 0, 0);
  iluguile_define_func(call__method_of_call, 1, 0, 0);

  /* class stuff */
  iluguile_define_func(class__method, 2, 0, 0);
  iluguile_define_func(class__name, 1, 0, 0);
  iluguile_define_func(class__unique_id, 2, 0, 0);

  /* from iluobj-scm.cc */
  iluguile_define_func(object__register_surrogate_creator, 2, 0, 0);
  iluguile_define_func(object__create_from_registry, 2, 0, 0);
  iluguile_define_func(object__input_object, 3, 0, 0);
  iluguile_define_func(object__output_object, 3, 0, 0);
  iluguile_define_func(object__size_of_object, 3, 0, 0);
  iluguile_define_func(object__lookup, 3, 0, 0);
  iluguile_define_func(object__register_as_gc_callback, 1, 0, 0);

  /* from ilusrvr-scm.cc */
  iluguile_define_func(server__add_port, 4, 0, 0);
  iluguile_define_func(server__create, 2, 0, 0);
  iluguile_define_func(server__id, 1, 0, 0);
}

void
iluguile_scheme_main(int argc, char* argv[],
		void (*real_main)(int, char*[]),
		char* scm_file, char* scm_main)
{
  extern void iluguile_init_hash_tables();

  iluguile_init_hash_tables();
  iluguile_scm_init(argc, argv, real_main, iluguile_init, scm_file, scm_main);
}
