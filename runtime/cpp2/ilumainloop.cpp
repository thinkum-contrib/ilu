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
/* $Id: ilumainloop.cpp,v 1.14 1999/08/03 01:55:51 janssen Exp $ */

// include ILU C++ header file
#include "ilu.hpp"


//////////////////////////////////////////////////////////////////
// iluMainLoop - Subclass from this class if you want to have your
// own version of the main loop. A single threaded application should
// supply all functions.  An application making use of ILU's OS multi-threaded
// operation should not use a different mainloop.  If you're using your
// own thread package, you must supply all functions, and see the comment
// for iluCPPRuntime::iluInitialize


//////////////////////////////////////////////////////////////////
// globals and statics

// holds pointer to the main loop that was either set via a call to 
// iluSetMainLoop (before initialization), or the default main
// loop (set if the user does not call iluSetMainLoop before initialization.
iluMainLoop* iluMainLoop::m_p_mainloop;


// used to get around some compilers complaining about passing a c++
// function where a c function is expected.

extern "C" {
  typedef void(*C_v_intstar_proc)(int*);
  typedef void(*C_v_alarm_proc)(iluAlarm);
  typedef void(*C_v_int_vstar_handler_proc)(int, void*);
  typedef iluBoolean(*C_b_i_handler_vstar_proc)(int, C_v_int_vstar_handler_proc, void*);
  typedef C_v_int_vstar_handler_proc* C_p_v_int_vstar_handler_proc;
  typedef iluBoolean(*C_b_i_phandler_vstarstar_proc)(int, C_p_v_int_vstar_handler_proc, void**);
  typedef void*(*C_vstar_proc)();
  typedef void(*C_alarm_time_handler_vstar_proc)(iluAlarm, 
						 iluFineTime, 
						 void (*pfunction_alarm_handler)(void*), 
						 void*);
}


// an ilu kernel mainloop structure, the iluDispatch... members dispatch
// to the functions of the iluMainLoop set in m_p_mainloop

ilu_MainLoop iluMainLoop::m_kernel_to_cpp_loop_dispatcher = {
  REINTERPRET_CAST(C_v_intstar_proc, iluMainLoop::iluDispatchRun), 
  REINTERPRET_CAST(C_v_intstar_proc, iluMainLoop::iluDispatchExit),
  REINTERPRET_CAST(C_b_i_handler_vstar_proc, iluMainLoop::iluDispatchRegisterInputHandler), 
  REINTERPRET_CAST(C_b_i_phandler_vstarstar_proc, iluMainLoop::iluDispatchUnregisterInputHandler),
  REINTERPRET_CAST(C_b_i_handler_vstar_proc, iluMainLoop::iluDispatchRegisterOutputHandler), 
  REINTERPRET_CAST(C_b_i_phandler_vstarstar_proc, iluMainLoop::iluDispatchUnregisterOutputHandler),
  REINTERPRET_CAST(C_vstar_proc, iluMainLoop::iluDispatchCreateAlarm), 
  REINTERPRET_CAST(C_alarm_time_handler_vstar_proc, iluMainLoop::iluDispatchSetAlarm), 
  REINTERPRET_CAST(C_v_alarm_proc, iluMainLoop::iluDispatchClearAlarm),
  REINTERPRET_CAST(C_v_alarm_proc, iluMainLoop::iluDispatchDestroyAlarm) 
};


//////////////////////////////////////////////////////////////////
// The iluDispatch... members that dispatch
// to the functions of the iluMainLoop set in m_p_mainloop

void iluMainLoop::iluDispatchRun (int* p_i_stop_on_non_zero) {
	m_p_mainloop->iluRun (p_i_stop_on_non_zero);
}


void iluMainLoop::iluDispatchExit (int* p_i_stop_on_non_zero) {
	m_p_mainloop->iluExit (p_i_stop_on_non_zero);
}


iluBoolean iluMainLoop::iluDispatchRegisterInputHandler (int i_fd,
														 void (*pfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
														 void* pv_input_handler_arg ) {
			 return(m_p_mainloop->iluRegisterInputHandler (i_fd,
				 pfunction_input_handler,
				 pv_input_handler_arg ) ? iluTRUE : iluFALSE);
}


iluBoolean	iluMainLoop::iluDispatchUnregisterInputHandler (int i_fd,
			void (**ppfunction_input_handler)(int i_fd, void* pv_input_handler_arg),
			void** ppv_input_handler_arg ) {
	return (m_p_mainloop->iluUnregisterInputHandler (i_fd, ppfunction_input_handler, ppv_input_handler_arg) ? iluTRUE : iluFALSE);
}		


iluBoolean	iluMainLoop::iluDispatchRegisterOutputHandler (int i_fd,
														   void (*pfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
														   void* pv_output_handler_arg ) {
			 return (m_p_mainloop->iluRegisterOutputHandler(i_fd, 
				 pfunction_output_handler, 
				 pv_output_handler_arg) ? iluTRUE : iluFALSE);
}


iluBoolean	iluMainLoop::iluDispatchUnregisterOutputHandler (int i_fd,
															 void (**ppfunction_output_handler)(int i_fd, void* pv_output_handler_arg),
															 void** ppv_output_handler_arg ) {
	return (m_p_mainloop->iluUnregisterOutputHandler (i_fd, ppfunction_output_handler, ppv_output_handler_arg) ? iluTRUE : iluFALSE);
}


iluAlarm iluMainLoop::iluDispatchCreateAlarm() {
			 return m_p_mainloop->iluCreateAlarm();
}

void iluMainLoop::iluDispatchSetAlarm(iluAlarm the_alarm, 
									  iluFineTime alarm_time, 
									  void (*pfunction_alarm_handler)(void* pv_alarm_handler_arg), 
									  void* pv_alarm_handler_arg) {
	m_p_mainloop->iluSetAlarm(the_alarm, alarm_time, 
		pfunction_alarm_handler, pv_alarm_handler_arg);
}


void iluMainLoop::iluDispatchClearAlarm (iluAlarm the_alarm) {
	m_p_mainloop->iluClearAlarm (the_alarm);
}


void iluMainLoop::iluDispatchDestroyAlarm (iluAlarm the_alarm) {
	m_p_mainloop->iluDestroyAlarm (the_alarm);
}


//////////////////////////////////////////////////////////////////
// Setting the Main Loop to be used

// Call iluSetMainLoop set your mainloop as the one for ilu to use.
// It should called before any ilu initialization.

void iluMainLoop::iluSetMainLoop(iluMainLoop* p_mainloop_instance) {
	
	// assign the mainloop pointer
	m_p_mainloop = p_mainloop_instance;
	
	// tell ilu to use m_kernel_to_cpp_loop_dispatcher as the main loop
	// - the DIspatch functions in m_kernel_to_cpp_loop_dispatcher will
	// dispatch the calls to m_p_mainloop
	
	ilu_SetMainLoop(&m_kernel_to_cpp_loop_dispatcher);
	
}

extern "C" {typedef void(*C_Alarm_proc)(void*);}


void iluMainLoop::iluDefaultLoopSetAlarm(iluAlarm the_alarm, iluFineTime alarm_time,
						 void (*pfunction_alarm_handler)(void* pv_alarm_handler_arg), 
						 void* pv_alarm_handler_arg) { 
  ilu_SetAlarm(the_alarm, alarm_time, REINTERPRET_CAST(C_Alarm_proc, pfunction_alarm_handler), pv_alarm_handler_arg);
}


// utility function to sets the pointed to ilu_FineTime to a time i_secs + i_msecs in the future
void iluMainLoop::iluSetFineTimeFromNow(ilu_FineTime* p_finetime, ilu_integer i_secs, ilu_cardinal i_msecs) {

  *p_finetime = ilu_FineTime_Now();		/* set to current time */

  p_finetime->ft_s = p_finetime->ft_s + i_secs;	    /* add seconds from now */

  if (i_msecs + p_finetime->ft_t > ilu_FineTimeRate) { /* if overflow on msec */
  	(p_finetime->ft_s)++;
  	p_finetime->ft_t = i_msecs + p_finetime->ft_t - ilu_FineTimeRate;
  }
  else p_finetime->ft_t = p_finetime->ft_t + i_msecs;	/* add milliseconds from now*/
};


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




