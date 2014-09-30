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
/* $Id */


// include ILU C++ header file

#include "ilu.hpp"

#ifdef ILU_OS_THREADED
#include <threads.h>
#endif

//////////////////////////////////////////////////////////////////////
// iluPassport 

//////////////////////////////////////////////////////////////////
// statics

// hash table (and lock) used to store each thread's passport (if any)
iluHashTable iluPassport::sm_outgoing_passport_hash_table = NULL;
iluMutex iluPassport::sm_outgoing_passport_table_mutex = NULL;
iluHashTable iluPassport::sm_incoming_passport_hash_table = NULL;
iluMutex iluPassport::sm_incoming_passport_table_mutex = NULL;


//////////////////////////////////////////////////////////////////
// constructor - creates and returns a passport, optionally containing the specified identity

iluPassport::iluPassport(iluIdentityInfo p_identity_info) {
	
	iluError	an_error;

	m_b_destroy_on_destruct = ILUCPP_TRUE;
	m_p_next = NULL;

	m_p_passport = ilu_CreatePassport(p_identity_info, &an_error);

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);	

	ILUCPP_DEBUG1("iluPassport::iluPassport(iluIdentityInfo)\n")
}


//////////////////////////////////////////////////////////////////
// Internal use only: this constructor is used (only) by true side stubs

iluPassport::iluPassport(ilu_Passport kernel_passport) {
	m_p_passport = kernel_passport;
	m_b_destroy_on_destruct = ILUCPP_FALSE;
	m_p_next = NULL;
	ILUCPP_DEBUG2("iluPassport::iluPassport(ilu_Passport %p)\n", kernel_passport)
}



//////////////////////////////////////////////////////////////////
// set up the passport hash table (use pointer style to work on iluCardinals as keys)

void iluPassport::iluInitialize() {
	if (sm_outgoing_passport_hash_table == NULL) {
		// set up the outgoing passport hash table (use pointer style to work on iluCardinals as keys)
		sm_outgoing_passport_hash_table = ilu_hash_MakeNewTable(31, ilu_hash_HashPointer, ilu_hash_PointerCompare);
		// create the mutex for exclusive access to the outgoing passport table
		sm_outgoing_passport_table_mutex = ilu_CreateMutex(CONST_CAST(char*, "ilucpp"), CONST_CAST(char*, "outgoing_passport"));
	}
	if (sm_incoming_passport_hash_table == NULL) {
		// set up the incoming passport hash table (use pointer style to work on iluCardinals as keys)
		sm_incoming_passport_hash_table = ilu_hash_MakeNewTable(31, ilu_hash_HashPointer, ilu_hash_PointerCompare);
		// create the mutex for exclusive access to the incoming passport table
		sm_incoming_passport_table_mutex = ilu_CreateMutex(CONST_CAST(char*, "ilucpp"), CONST_CAST(char*, "incoming_passport"));
	}
	ILUCPP_DEBUG1("iluPassport::iluInitialize() complete\n")
}


//////////////////////////////////////////////////////////////////
// destructor - frees any associated identities in addition to freeing the passport

iluPassport::~iluPassport() {
	
	if (m_b_destroy_on_destruct) {
		// must be caller side passport
		// remove all entries from the passport hashtable which refer to this passport
		iluMutexer passports_mutex(sm_outgoing_passport_table_mutex);
		
		void* pv_some_key_of_me;
		
		while(iluFindSomeKeyOfMe(&pv_some_key_of_me))
			ilu_hash_RemoveFromTable(sm_outgoing_passport_hash_table, pv_some_key_of_me);
		
		iluError	an_error;
		ilu_DestroyPassport(m_p_passport, &an_error);
		
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);	
		ILUCPP_DEBUG1("iluPassport::~iluPassport() client side\n")
	}	
	else {
		// must be server side passport
		iluMutexer passports_mutex(sm_incoming_passport_table_mutex);
		iluCardinal card_thread_id = iluCppInternal::iluGetThreadID();

		// get the linked list of incoming passports for this thread
		iluPassport* p_current_head = (iluPassport*)(ilu_hash_FindInTable(sm_incoming_passport_hash_table, 
			(void*)card_thread_id));
		
		// (should be at the head!)
		if (!p_current_head || p_current_head != this) {
			ILUCPP_WARN("iluPassport::~iluPassport(), null call passport list or passport not at list head - ignoring!");
			return;
		}
		
		// remove this passport from the list - delete the entry for this thread
		ilu_hash_RemoveFromTable(sm_incoming_passport_hash_table, (void*)card_thread_id);
		
		if (p_current_head->m_p_next != NULL)  // not last one on the list, and a thread entry back in
			ilu_hash_AddToTable(sm_incoming_passport_hash_table, (void*)card_thread_id, p_current_head->m_p_next);

		ILUCPP_DEBUG1("iluPassport::~iluPassport() server side\n")
	}
}


//////////////////////////////////////////////////////////////////
// Sets r_keyvalue to some value for which the passport table has an entry for 
// this passport.  Returns ILUCPP_TRUE if valid, else ILUCPP_FALSE if none found

ILUCPP_BOOL iluPassport::iluFindSomeKeyOfMe(void** ppv_keyvalue) {

	iluMutexer passports_mutex(sm_outgoing_passport_table_mutex);
	HashEnumerator hash_enumerator;
	ilu_refany key, data;
	
	ilu_hash_BeginEnumeration (sm_outgoing_passport_hash_table, &hash_enumerator);
	while (ilu_hash_Next(&hash_enumerator, &key, &data)) {
		if (this == data) {
			*ppv_keyvalue = key;
			return ILUCPP_TRUE;
		}
	}
	return ILUCPP_FALSE;
}


//////////////////////////////////////////////////////////////////
// Get and set the passport being used for outgoing calls - in the multi-threaded case, this is per-thread
// Set returns the old iluPassport.  Note that before your thread exits, you should either call 
// iluSetPassport(NULL), or delete the iluPassport in use (assuming it's only in use for a single thread).

iluPassport* iluPassport::iluGetPassport() {
	iluMutexer passports_mutex(sm_outgoing_passport_table_mutex);
	return (iluPassport*)(ilu_hash_FindInTable(sm_outgoing_passport_hash_table, 
		(void*)(iluCppInternal::iluGetThreadID())));
}

iluPassport* iluPassport::iluSetPassport(iluPassport* p_passport) {
	iluCardinal card_thread_id = iluCppInternal::iluGetThreadID();
	iluPassport* p_old_passport = iluGetPassport();
	iluMutexer passports_mutex(sm_outgoing_passport_table_mutex);
	if (p_old_passport) // remove old entry if there was one
		ilu_hash_RemoveFromTable(sm_outgoing_passport_hash_table, (void*)card_thread_id);
	if (p_passport) // add in the new entry if there really is one
		ilu_hash_AddToTable(sm_outgoing_passport_hash_table, (void*)card_thread_id, p_passport);
	return p_old_passport;
}


//////////////////////////////////////////////////////////////////
// get the passport of the caller

iluPassport* /* ILUowned */ iluPassport::iluGetCallerPassport() {
	iluMutexer passports_mutex(sm_incoming_passport_table_mutex);
	
	// get the linked list of incoming passports for this thread
	// get the passport at the head of the list
	iluPassport* p_current_head = (iluPassport*)(ilu_hash_FindInTable(sm_incoming_passport_hash_table, 
		(void*)iluCppInternal::iluGetThreadID()));

	if (!p_current_head)
		return NULL;

	if (p_current_head->iluGetIluPassport()) // if it's not just a call placeholder, return it
		return p_current_head;
	
	return NULL; // must be just a placeholder for a call that had no (kernel) ilu_Passport
}


//////////////////////////////////////////////////////////////////
// Internal use only: set the passport of the caller

void iluPassport::iluSetCallerPassport(iluPassport* p_passport) {
	
	if (!p_passport) {
		ILUCPP_WARN("iluPassport::iluSetCallerPassport, null p_passport - ignoring!");
		return;
	}
	
	iluMutexer passports_mutex(sm_incoming_passport_table_mutex);
	iluCardinal card_thread_id = iluCppInternal::iluGetThreadID();
	
	// get the linked list of incoming passports for this thread
	iluPassport* p_current_head = (iluPassport*)(ilu_hash_FindInTable(sm_incoming_passport_hash_table, 
		(void*)card_thread_id));
	
	if (p_current_head) { // push the passport onto the head of the list if there's something there already
		p_passport->m_p_next = p_current_head;
		ilu_hash_RemoveFromTable(sm_incoming_passport_hash_table, (void*)card_thread_id);
	}	
	ilu_hash_AddToTable(sm_incoming_passport_hash_table, (void*)card_thread_id, p_passport);

	ILUCPP_DEBUG2("iluPassport::iluSetCallerPassport() %p\n", p_passport)
}


//////////////////////////////////////////////////////////////////
// adds identity to Passport.  Only one identity of each type is allowed.

void iluPassport::iluAddIdentity(iluIdentityInfo p_identity_info /* ILUowned */) {
	
	iluError	an_error;

	ilu_AddIdentity (m_p_passport, p_identity_info,  &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
}


//////////////////////////////////////////////////////////////////
// returns identity of specified type, if present else null

iluIdentityInfo /* ILUowned */ iluPassport::iluFindIdentity(iluIdentityType p_identity_type) {
	
	return ilu_FindIdentity (m_p_passport, p_identity_type);
}


//////////////////////////////////////////////////////////////////
// returns a copy of the passed identity

iluIdentityInfo iluPassport::iluCopyIdentity(iluIdentityInfo p_identity_info) {
	
	iluError	an_error;
	iluIdentityInfo p_copy;

	p_copy = ilu_CopyIdentity (p_identity_info,  &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		

	return p_copy;
}



#ifdef SECURE_TRANSPORT

//////////////////////////////////////////////////////////////////////
// iluGSS 
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////

iluIdentityInfo iluGSS::iluAcquireGSSIdentity (gss_cred_id_t gss_credential) {

	iluIdentityInfo p_return;
	iluError	an_error;
	
	p_return = ilu_AcquireGSSIdentity (gss_credential, &an_error);

	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
	
	return p_return;
}


//////////////////////////////////////////////////////////////////

ILUCPP_BOOL iluGSS::iluDecodeGSSIdentity (
					  iluIdentityInfo p_identity_info,	// input; retain; info to decode
					  gss_name_t* p_name,			// output; name in identity 
					  iluFineTime* p_good_till_time,	// output; good-till
					  gss_OID mechanism,			// input; actual mechanism desired; optional 
					  ILUCPP_BOOL* p_b_local,		// if TRUE, local; otherwise remote 
					  iluCardinal* p_card_flags		// connection flags, as in gss_inquire_context 
					  ) {
	
  iluError	an_error;
  ilu_boolean b_return;
  ilu_boolean b_local;
	
  b_return = ilu_DecodeGSSIdentity (p_identity_info, p_name, p_good_till_time, 
				    mechanism, &b_local, p_card_flags, &an_error);

  ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
  
  *p_b_local = (b_local ? ILUCPP_TRUE : ILUCPP_FALSE);

  return (b_return ? ILUCPP_TRUE : ILUCPP_FALSE);
}



//////////////////////////////////////////////////////////////////

gss_cred_id_t iluGSS::iluAcquireGSSCredForName (
												char* pc_name,
												iluCardinal card_lifetime,
												gss_OID mechanism,
												ILUCPP_BOOL b_accept_only
												) {
	
	iluError	an_error;
	gss_cred_id_t	p_return;
	p_return = ilu_AcquireGSSCredForName (pc_name, card_lifetime, mechanism, 
					      (b_accept_only ? ilu_TRUE : ilu_FALSE), &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
	
	return p_return;
}



//////////////////////////////////////////////////////////////////

iluCString iluGSS::iluGSSNameToString (gss_name_t name) {
	
	iluError	an_error;
	iluCString	pc_return;
	
	pc_return = ilu_GSSNameToString (name, &an_error);
	
	ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);		
	
	return pc_return;
}


#endif /* def SECURE_TRANSPORT */
