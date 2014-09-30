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
/* $Id: iluobject.cpp,v 1.28 1999/08/03 01:55:50 janssen Exp $ */


// include ILU C++ header file
#include "ilu.hpp"

#include <stdio.h>


//////////////////////////////////////////////////////////////////
// iluObject - The most base class for all ILU C++ objects.  CORBA::Object
// inherits from this class.  All non-static member functions are virtual
// to allow creative overrides (at your own risk of course).


//////////////////////////////////////////////////////////////////
// globals and statics

// sm_card_object_counter (initially 0) is
// used to automatically generate unique object id's when needed.
iluCardinal iluObject::sm_card_object_counter = 0;


// mutex used to control access to operations on object incrementing and
// decrementing of reference counts. Note that this is a global mutex used 
// for all iluObjects.  Since the iluIncrementReferenceCount and 
// iluDeccrementReferenceCount are short quick operations, having a 
// single global mutex seemed a better choice than to burden every object
// with its own mutex. Note: Something to potentially reconsider though.
iluMutex iluObject::sm_reference_count_mutex;


//////////////////////////////////////////////////////////////////
// constructor and destructor 

// If no instance handle is specified, then the value of a monotonicaly
// increasing, iluServer specific counter will be used to generate one.  
// If no server is specified, then the default server will be used. 
// (The default server is generated automatically if needed, and has the
// an id based on time, hostname, and process id.)  Caller owns pc_instance_handle.
// The new object has a reference count of 1

iluObject::iluObject(iluClass the_class, char* pc_instance_handle, 
					 iluServer& the_server, ILUCPP_BOOL b_within_object_table) {
	
	char* pc_instance_handle_to_use;
	char  c_generated_instance_handle[11];
	
	if (pc_instance_handle == NULL) {  // need to generate a unique id
		sprintf(c_generated_instance_handle, "%lu", ((unsigned long)sm_card_object_counter++));
		pc_instance_handle_to_use = c_generated_instance_handle;
	}
	else pc_instance_handle_to_use = pc_instance_handle;
	
	// enter the object's server
	if (!b_within_object_table)
		the_server.iluEnterServerMutex(the_class);
	
    // set the kernel object of this c++ object
	m_kernel_object = ilu_FindOrCreateTrueObject (pc_instance_handle_to_use, 
		the_server.iluGetKernelServer(), the_class, this);
	
	// get a pointer to the kernels copy of the instance handle
	m_pc_instance_handle = ilu_IhOfObject(m_kernel_object);
	
	// exit the object's server
	if (!b_within_object_table)
		the_server.iluExitServerMutex(the_class);
	
	// set the c++ server representing the kernel server for this c++ object
	m_p_cpp_server = &the_server;	
	
	// set to not yet published
	m_pc_publish_proof = NULL;
	
	// set to initial reference count of 1
	m_i_reference_count = 1;
	
	// the class of this object
	m_class = the_class;
	
	ILUCPP_DEBUG4("iluObject::iluObject created %s object %s in server %s\n", 
		the_class->cl_name, m_pc_instance_handle, the_server.iluGetKernelServerId()); 
}


// default constructor - used when creating a surrogate
iluObject::iluObject() {
	
	ILUCPP_DEBUG1("iluObject::iluObject default constructor\n"); 
	
	// the kernel object of this c++ object
	m_kernel_object = NULL;
	
	// the c++ server representing the kernel server for this c++ object
	m_p_cpp_server = NULL;	
	
	// retains the 'proof' a publish operation returns so that it can be withdrawn
	m_pc_publish_proof = NULL;
	
	// the object's instance handle
	m_pc_instance_handle = NULL;
	
	// reference count used with duplicate and release
	m_i_reference_count = 0;
	
	// the class of this object
	m_class = NULL;
}



// Destructor ensures that this object is completely disassociated from the ilu kernel 
// The most specific destructor of an object should call iluDeactivate
// on the object to block any further incoming calls, and wait for any 
// ongoing calls to complete.  Next it should perform any object specific
// cleanup.  Finally, the destructor in iluObject will break the association
// between the kernel object and this object, allowing the kernel object
// to be potentially freed.

iluObject::~iluObject() {
		
	ILUCPP_DEBUG4("iluObject::~iluObject destructing %s object %s in server %s\n", 
		m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
	
	
	if (m_kernel_object) {
		
		iluError an_error;

		iluDeactivate();  // make sure we're deactivated
		
		iluKernelServer the_kernel_server = ilu_ServerOfObject(m_kernel_object);
		
		if (m_pc_publish_proof)  // take us out of any publication
			iluWithdraw();
		
		ilu_EnterServer(the_kernel_server, m_class);
		
		// null the kernels pointer to us
		ilu_RegisterLSO(m_kernel_object, m_class, NULL, 
			iluCppInternal::iluGetCppLanguageIndex(), &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		ILUCPP_DEBUG1("iluObject::~iluObject kernel object nulled\n"); 
		
		if (iluCppInternal::iluGetRunningThreaded())
			// tell the activation table to remove our kernel object
			iluObjectActivationTable::sm_p_object_activation_table->iluRemove(m_kernel_object);
		
		ilu_ExitServer(the_kernel_server, m_class);

		// null out entries just in case something odd goes on
		m_kernel_object = NULL;
		m_p_cpp_server = NULL;

	}
	
	else {
		// we must have been disassociated from the kernel object (perhaps 
		// because someone deleted our iluServer) or else never had one?
	}
}


//////////////////////////////////////////////////////////////////
// given a string binding handle (e.g. as obtained from iluObjectToString)
// returns an iluObject* for that object, with the reference count incremented.

iluObject* iluObject::iluStringToObject (char* pc_string_binding_handle) { 
	
	ILUCPP_BOOL b_parse_result;
	ilu_string  p_mstid = NULL;
	iluClass objects_class;
	iluKernelObject kernelobject;
	iluObject*	p_ilu_object;
	iluError an_error;

	// get the parts of the string object reference		   
	b_parse_result = (ilu_ParseSBH(pc_string_binding_handle, NULL, NULL, &p_mstid, NULL, NULL, NULL, &an_error)
		? ILUCPP_TRUE : ILUCPP_FALSE);
	
	if (ILU_ERRNOK(an_error)) {
		ILUCPP_ERRWARN(an_error, "iluObject::iluStringToObject calling ilu_ParseSBH", ILUCPP_TRUE);
		return NULL;
	}
	
	if (!b_parse_result)
		return NULL;
	
	// get the iluClass of this object
	objects_class = ilu_FindClassFromID(p_mstid);
	iluCppRuntime::iluFree(p_mstid);
	if (!objects_class) 
		return NULL;
	
	// get a kernel object based on this sbh and class
	kernelobject = ilu_ObjectOfSBH (pc_string_binding_handle, objects_class, &an_error);
	if (ILU_ERRNOK(an_error)) {
		ILUCPP_ERRWARN(an_error, "iluObject::iluStringToObject calling ilu_ObjectOfSBH", ILUCPP_TRUE);
		return NULL;
	}
	
	if (!kernelobject)
		return NULL;
	
	// get the language specific object corresponding to this kernel object
	p_ilu_object = (iluObject*)ilu_GetLanguageSpecificObject(kernelobject,
		iluCppInternal::iluGetCppLanguageIndex());
	
	ILUCPP_DEBUG3("iluObject::iluStringToObject found lso %p for kernel object %p\n", p_ilu_object, kernelobject);
	
	if (p_ilu_object == NULL)
		// we didn't find an object already present for this, so create a new surrogate
		p_ilu_object = iluCppInternal::iluCreateSurrogate(objects_class, kernelobject);
	
	// exit the object's server
	ilu_ExitServer(ilu_ServerOfObject(kernelobject), objects_class);
	
	if (p_ilu_object == NULL)  // had no object true or no ability to create surrogate
		return NULL;
	
	// bump the reference count
	p_ilu_object->iluIncrementReferenceCount();
	
	return ((iluObject*) p_ilu_object->iluDowncast(NULL));
}



//////////////////////////////////////////////////////////////////
// Simple Object Publication Operations 

//////////////////////////////////////////////////////////////////
// publishes binding information for this object in the binding service

ILUCPP_BOOL iluObject::iluPublish () {
	
	// enter the object's server
	iluEnterServerMutex(m_class);
	
	// now Inside(server, class), ilu_PublishObject exits it
	if ((m_pc_publish_proof = ilu_PublishObject(m_kernel_object)) == NULL)
		return ILUCPP_FALSE;
	
	ILUCPP_DEBUG4("iluObject::iluPublish %s object %s in server %s\n", 
		m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
	
    return ILUCPP_TRUE;
}


//////////////////////////////////////////////////////////////////
// Removes binding information for this object from the binding service

ILUCPP_BOOL	iluObject::iluWithdraw () {
	
	ILUCPP_BOOL b_status;
	
	// enter the object's server
	iluEnterServerMutex(m_class);
	
	/* now Inside(kobj->server, kobj->class) */
	b_status = (ilu_WithdrawObject (m_kernel_object, m_pc_publish_proof) ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	ILUCPP_DEBUG4("iluObject::iluWithdraw %s object %s in server %s\n", 
		m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
	
	if (b_status)
		m_pc_publish_proof = NULL;
	
	return b_status;
}



//////////////////////////////////////////////////////////////////
// Used by stubber generated iluLookup functions in derived classes to 
// lookup an object in the binding service based on its instance and server id
// and class. Increments reference count of object.
void* iluObject::iluLookup (char* pc_instance_handle, char* pc_server_id, ilu_Class the_class) {
	
	iluKernelObject the_kernel_object;
	iluObject*	p_ilu_object;
	iluError	an_error;
	ILUCPP_BOOL  b_changed;
	

	b_changed = (ilu_ReLookupObject(pc_server_id, pc_instance_handle, the_class, &the_kernel_object) 
		? ILUCPP_TRUE : ILUCPP_FALSE);

	if (the_kernel_object == NULL) { 
		// no object found cons up and throw an error
		ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}	
	
	
	ILUCPP_DEBUG3("iluObject::iluLookup found the kernel object %p, changed = %s\n", 
		the_kernel_object, (b_changed ? "TRUE" : "FALSE")); 
	
	// now Inside(object's server, class)
	
	// get the language specific object corresponding to this kernel object
	p_ilu_object = (iluObject*)ilu_GetLanguageSpecificObject(the_kernel_object,
		iluCppInternal::iluGetCppLanguageIndex());
	
	if (p_ilu_object == NULL) {
		ILUCPP_DEBUG1("iluObject::iluLookup no LSO found, creating surrogate\n"); 
		
		// we didn't find an object already present for this, so create a new surrogate
		p_ilu_object = iluCppInternal::iluCreateSurrogate(the_class, the_kernel_object);
	}
	
	
	// exit the object's server
	ilu_ExitServer(ilu_ServerOfObject(the_kernel_object), the_class);
	
	
	if (p_ilu_object == NULL) { // bad no object true or no ability to create surrogate
		
		ILUCPP_DEBUG1("iluObject::iluLookup no LSO found, and couldn't create surrogate\n"); 
		
		// cons up and throw an error
		ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	// bump the reference count by one
	p_ilu_object->iluIncrementReferenceCount();

	ILUCPP_DEBUG4("iluObject::iluLookup downcasting object %s in server %s to class %s\n", 
		p_ilu_object->iluId(), ilu_IDOfServer(ilu_ServerOfObject(p_ilu_object->iluGetKernelObject())), the_class->cl_name); 
	
	// We now call the virtual function iluDowncast on the p_ilu_object.  The will invoke 
	// iluDowncast on the most specific object that the language specific object 'really' is.
	// The iluDowncast member function will return a void pointer that is really pointing to the
	// most specific object. We leave this pointer in the wrapper's m_pv_iluobject member
	// for casting (back in the stub) to the most specific object type
	
	return p_ilu_object->iluDowncast(the_class);
}



//////////////////////////////////////////////////////////////////
// Object activation deactivation and destruction
// (activate and deactivate are really BOA-like kinds of operations)
// Use is only sensible in threaded situations.


//////////////////////////////////////////////////////////////////
// Ensures this object is available from the outside (i.e. possible to 
// to do remote invocations on)

void iluObject::iluActivate() {
	
	if (iluCppInternal::iluGetRunningThreaded()) {
		
		ILUCPP_DEBUG4("iluObject::iluActivate activating %s object %s in server %s\n", 
			m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
		
		if (m_kernel_object)
			// tell the activation table to activate our kernel object
			iluObjectActivationTable::sm_p_object_activation_table->iluActivate(m_kernel_object);
		else {
			char c_new_format_string[1024];
			ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "iluObject::iluActivate() on object at\
				%p has NULL m_kernel_object! - ignoring\n"), this);				
		}
	}
}


//////////////////////////////////////////////////////////////////
// Ensures this object is not available from the outside. This must
// be the first thing called by the most specific destructor of an 
// object. If if isn't, then the potential exists (in multithread case) 
// for a call to come in for an object that's in the middle of
// destruction - a bad thing!  This function blocks until there are
// zero ongoing calls.

void iluObject::iluDeactivate() {
	
	if (iluCppInternal::iluGetRunningThreaded()) {
		
		ILUCPP_DEBUG4("iluObject::iluDeactivate deactivating %s object %s in server %s\n", 
			m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object)));
		
		if (m_kernel_object)
			// tell the activation table to deactivate our kernel object
			iluObjectActivationTable::sm_p_object_activation_table->iluDeactivate(m_kernel_object);
		else {
			char c_new_format_string[1024];
			ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "iluObject::iluDeactivate() on object at\
				%p has NULL m_kernel_object! - ignoring\n"), this);				
		}
	}
}


//////////////////////////////////////////////////////////////////
// Called by iluUnlinkKernelObject - you can override this virtual 
// function in your objects to do whatever you like when the association
// between your object and the kernel object is broken - e.g. delete yourself
// The implementation in iluObject deletes this.

void iluObject::iluKernelObjectUnlinked () {
	delete this;
}



//////////////////////////////////////////////////////////////////
// Accessors

// Returns pointer to the iluServer that this object resides in,
// this is NULL is the object is a surrogate
iluServer* iluObject::iluGetServer () {
	return m_p_cpp_server;}

// Returns the objects instance id
const char* /* ILUowned */ iluObject::iluId (){
	return m_pc_instance_handle;}


// Returns the id of the object's ilu_Server
const char* /* ILUowned */ iluObject::iluServerId () {

	if (m_p_cpp_server) 
		// must be a true object since we have a C++ iluServer,
		// ask the server for the id
		return m_p_cpp_server->iluGetKernelServerId();

	// must be a surrogate object, go direct to kernel calls
	if (m_kernel_object)
		return ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object));

	return NULL;
}


//////////////////////////////////////////////////////////////////
// Informational 

// Returns the ILU string binding handle for the object
// caller get ownership of the string
// (this should probably be 'object_to_string' in some 'orb' object)

iluCString	iluObject::iluObjectToString () {
	
	ILU_ERRS((no_memory)) an_error;
	
	iluCString pc_kernel_result_string;
	iluCString pc_copy_of_result;
	
	// enter the object's server
	iluEnterServerMutex(m_class);
	
	pc_kernel_result_string = ilu_SBHOfObject(m_kernel_object);
	
	pc_copy_of_result = ilu_StrdupE(pc_kernel_result_string, &an_error);
	
	if (ILU_ERRNOK(an_error)) {
		// exit the object's server
		iluExitServerMutex(m_class);
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	// exit the object's server
	iluExitServerMutex(m_class);
	
	return pc_copy_of_result;
}


#ifdef IIOP_PROTOCOL

// Returns a string which is the object's name and contact information
// as specified by the CORBA IIOP spec - caller gets ownership of the string
// May return NULL if the object is not exported through an IIOP ilu_Port

iluCString	iluObject::iluObjectToIORString () {
	
	iluCString pc_kernel_result_string;
	iluError an_error;
	
	// enter the object's server
	iluEnterServerMutex(m_class);
	
	pc_kernel_result_string = ilu_IOROfObject(m_kernel_object, &an_error);
	
	// exit the object's server
	iluExitServerMutex(m_class);

	if (ILU_ERRNOK(an_error)) {
		ILU_HANDLED(an_error);
		pc_kernel_result_string = NULL;
	}
	
	return pc_kernel_result_string;
}

#endif


#ifdef HTTP_PROTOCOL

// Returns a string which is the object's name and contact information
// as specified by an HTTP URL - caller gets ownership of the string
// May return NULL if the object is not exported through an HTTP ilu_Port
iluCString	iluObject::iluObjectToURLString () {
	
	iluCString pc_kernel_result_string;
	iluError an_error;
	
	pc_kernel_result_string = ilu_URLOfObject(m_kernel_object, &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	return pc_kernel_result_string;
}

#endif


// Returns true if the true object exists, and the process
// serving it can be contacted, otherwise false

ILUCPP_BOOL iluObject::iluPing() {
	
	iluError an_error;
	iluConnection  new_connnection_to_monitor = ILU_NIL;
	ILUCPP_BOOL b_successful_ping;
	
	ILU_CLER(an_error);
	
	if (!m_kernel_object) { // make sure we hava a kernel object
		ILU_ERR_CONS1(bad_param, &an_error, minor, ilu_bpm_closed, ilu_FALSE);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); 
	}
	
	// increment the objects hold count
	iluEnterServerMutex(m_class);
	an_error = ilu_DeltaHolds(m_kernel_object, 1);
	iluExitServerMutex(m_class);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); 
	
	// do the actual ping
	b_successful_ping = (ilu_PingObject(m_kernel_object, &new_connnection_to_monitor) ? ILUCPP_TRUE : ILUCPP_FALSE);
	
	// monitor any new connection returned
	if (new_connnection_to_monitor != ILU_NIL)
		iluCppInternal::iluThrowingFork(iluCppInternal::iluMonitorOutgoingConnection, new_connnection_to_monitor);
	
	
	// decrement the objects hold count
	iluEnterServerMutex(m_class);
	an_error = ilu_DeltaHolds(m_kernel_object, -1);
	iluExitServerMutex(m_class);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error); 
	
	return b_successful_ping;
}


//////////////////////////////////////////////////////////////////
// Return the ilu class name and type id - primarily informational use

const char*	/* ILUowned */	iluObject::iluClassName () {
	return m_class->cl_name;
}

const char*	/* ILUowned */	iluObject::iluClassId   () {
	return m_class->cl_unique_id;
}



//////////////////////////////////////////////////////////////////
// Stub and internal use only

// Downcasting 

void* iluObject::iluDowncast (iluClass class_to_cast_down_to) {
	
	if (!class_to_cast_down_to || class_to_cast_down_to == ilu_rootClass)
		return ((void *) this);
	
    return (NULL);
}


//////////////////////////////////////////////////////////////////
// returns the kernel object of this C++ object

iluKernelObject iluObject::iluGetKernelObject() {
	return m_kernel_object;
}


//////////////////////////////////////////////////////////////////
// associates the passed kernel object with this object - used only for
// creating surrogates 

void iluObject::iluAssociateKernelObject(iluKernelObject a_kernel_object) {
	
	// Note we're Inside(obj->server, obj->class) since this function
	// should only be caled when creating surrogates

	iluError	an_error;

	if (m_kernel_object != NULL) {
	char c_new_format_string[1024];
	ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "Warning: iluObject::iluAssociateKernelObject\
			called on iluObject with non NULL m_kernel_object member!, file %s, line %i\n"),
	__FILE__, __LINE__);
	}

	// the class of this object
	m_class = ilu_ClassOfObject(a_kernel_object);

	// tell the kernel that this iluObject is a language specific object
	// corresponding to the kernel object
	ilu_RegisterLSO(a_kernel_object, m_class, this, 
		iluCppInternal::iluGetCppLanguageIndex(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);

	// set the kernel object of this c++ object
	m_kernel_object = a_kernel_object;
	
	// set the c++ server representing the kernel server for this c++ object
	// since we're really a surrogate here, there is no C++ iluServer object
	// representing the kernel's representation of the server
	m_p_cpp_server = NULL;	
	
	// retains the 'proof' a publish operation returns so that it can be withdrawn
	m_pc_publish_proof = NULL;
	
	// get a pointer to the kernels copy of the instance handle
	m_pc_instance_handle = ilu_IhOfObject(m_kernel_object);
	
	// reference count used with duplicate and release - initially 0
	m_i_reference_count = 0;
		
	ILUCPP_DEBUG4("iluObject::iluAssociateKernelObject associated %s object %s in server %s\n", 
		m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
}




// Reference count operations - when an object is first created, it has
// a reference count of one.  If the reference count ever goes to zero,
// delete is called on this.
void iluObject::iluIncrementReferenceCount() {
	
	iluMutexer ref_count_mutex(sm_reference_count_mutex);
	
	if (m_i_reference_count < 0 || m_kernel_object == NULL) {
		// object must be in the middle of destruction - throw an error
		char c_new_format_string[1024];
		iluError	an_error;
		ILU_ERR_CONS1(inv_objref, &an_error, minor, ilu_iom_inst_nf, ilu_FALSE);
		ILU_ERRPRINTF(iluCppTagFormatString(c_new_format_string, "Error Warning: \
			iluObject::iluIncrementReferenceCount on object with ref count < 0, file %s, line %i, Throwing Error: %s\n"),
			__FILE__, __LINE__, ILU_ERR_NAME(an_error));
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	}
	
	m_i_reference_count++;
	
	ILUCPP_DEBUG5("iluObject::iluIncrementReferenceCount to %ld - %s object %s in server %s\n", 
		m_i_reference_count, m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object)));
}


void iluObject::iluDecrementReferenceCount() {
	
	if (m_kernel_object != NULL) {
		{
			iluMutexer ref_count_mutex(sm_reference_count_mutex);
			
			m_i_reference_count--;
			
			if (m_i_reference_count > 0) {
				
				ILUCPP_DEBUG5("iluObject::iluDecrementReferenceCount to %ld : %s object %s in server %s\n", 
					m_i_reference_count, m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
				
				return;
			}
			
			// no one references us any more - cleanup
			m_i_reference_count--; // ensure the count is negative so we can know we're destructing
		}
		
		ILUCPP_DEBUG5("iluObject::iluDecrementReferenceCount to %ld <= 0 : %s object %s in server %s\n", 
			m_i_reference_count, m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object))); 
		
		delete this;
	}
	else { // kernel object is null!
		ILUCPP_DEBUG4("iluObject::iluDecrementReferenceCount - NULL kernel object in %s object %s in server %s\n", 
			m_class->cl_name, m_pc_instance_handle, ilu_IDOfServer(ilu_ServerOfObject(m_kernel_object)));
	}
}


void iluObject::iluScheduledDecrementReferenceCount(iluCardinal seconds_from_now, iluCardinal plus_msec_from_now) {
	
	iluMainLoop* the_mainloop = iluMainLoop::iluGetMainLoop();
	iluAlarm dec_reffing_alarm;
	iluScheduledDecrementReferenceCountArg* p_args;
	iluFineTime alarm_time;

	if (the_mainloop)  // non default mainloop 
		dec_reffing_alarm = the_mainloop->iluCreateAlarm();
	else 
		dec_reffing_alarm = iluMainLoop::iluDefaultLoopCreateAlarm();
	
	p_args = new iluScheduledDecrementReferenceCountArg(this, dec_reffing_alarm, the_mainloop);

	iluMainLoop::iluSetFineTimeFromNow(&alarm_time, seconds_from_now, plus_msec_from_now);

	if (the_mainloop)  // non default mainloop 
		the_mainloop->iluSetAlarm(dec_reffing_alarm, alarm_time, 
			iluObject::iluScheduledDecrementReferenceCountHandler, p_args);
	else 
		iluMainLoop::iluDefaultLoopSetAlarm(dec_reffing_alarm, alarm_time,
			iluObject::iluScheduledDecrementReferenceCountHandler, p_args);
	
}


// the alarm handler for doing scheduled dec refs
void iluObject::iluScheduledDecrementReferenceCountHandler(void* pv_alarm_handler_arg) {
	iluScheduledDecrementReferenceCountArg* p_args = (iluScheduledDecrementReferenceCountArg*)pv_alarm_handler_arg;
	(p_args->m_p_object_to_be_decreffed)->iluDecrementReferenceCount();
	delete p_args;
}


iluCardinal iluObject::iluGetReferenceCount() {
	return m_i_reference_count;
}



//////////////////////////////////////////////////////////////////
// For ilu cpp runtime use only - returns the m_class set at construction time

iluClass iluObject::iluGetClassRecord() {
	return m_class;
}


//////////////////////////////////////////////////////////////////
// For ilu cpp runtime use only - used during iluServer deletion to break 
// the linkage between an iluObject and it's kernel object
// Note there are some thread safety issues here, but we're going to assume
// that if an app decided to destroy an iluServer, that it has sense 
// enough not to perform operations on the objects that were served by that server,
// and has done reasonable things first e.g. withdraws, deactivations, etc.

int iluObject::iluUnlinkKernelObject (iluKernelObject the_kernel_object, 
									  ilu_refany /* pv_iluserver */ ) {
	
	iluError an_error;

	// get a pointer to the iluObject associated with this kernel object
	iluObject* p_cpp_object = (iluObject*) ilu_GetLanguageSpecificObject(the_kernel_object, 
		iluCppInternal::iluGetCppLanguageIndex());

	ILUCPP_DEBUG4("iluObject::iluUnlinkKernelObject - %s object %s in server %s\n", 
		p_cpp_object->iluClassName(), p_cpp_object->iluId(), 
		ilu_IDOfServer(ilu_ServerOfObject(p_cpp_object->iluGetKernelObject()))); 

	if (p_cpp_object->m_pc_publish_proof)  // take us out of any publication
			p_cpp_object->iluWithdraw();

	// null the kernels pointer to to the c++ object
	ilu_RegisterLSO(the_kernel_object, p_cpp_object->m_class, NULL, 
		iluCppInternal::iluGetCppLanguageIndex(), &an_error);
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
	
	if (iluCppInternal::iluGetRunningThreaded())
			// tell the activation table to remove our kernel object
			iluObjectActivationTable::sm_p_object_activation_table->iluRemove(the_kernel_object);

	// null the c++ object's kernel object and iluServer members
	p_cpp_object->m_kernel_object = NULL;	
	p_cpp_object->m_p_cpp_server = NULL;

	// call the objects unlinked function
	p_cpp_object->iluKernelObjectUnlinked();

	return 0;
}


//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




