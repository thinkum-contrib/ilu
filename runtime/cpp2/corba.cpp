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
/* $Id: corba.cpp,v 1.44 1999/08/17 19:09:36 pnewman Exp $ */


#include <stdio.h>	/* for sscanf */

// include Corba C++ header file
#include "corba.hpp"
#include "ilutypes.h"

	//////////////////////////////////////////////////////////////////
	// Exception Class
	
	// all inlined	


	//////////////////////////////////////////////////////////////////
	// SystemException Class
	
	// all inlined	


	//////////////////////////////////////////////////////////////////
	// UserException Class
	
	// all inlined	
	
	
	
	//////////////////////////////////////////////////////////////////
	// release and is_nil
	
	ILU_RUNTIME_PUBLIC void  CORBA(release)( CORBA(Object_ptr) an_object_ptr){ 
		
		if (an_object_ptr == CORBA(OBJECT_NIL))
			return;
		// decrement the object's reference count
		an_object_ptr->iluDecrementReferenceCount();
		
	}
	

	ILU_RUNTIME_PUBLIC CORBA(Boolean) CORBA(is_nil)(CORBA(Object_ptr) an_obj_ptr) {
		return (((void*)an_obj_ptr) == CORBA(OBJECT_NIL)) ? ILUCPP_TRUE : ILUCPP_FALSE;
	}
	

	//////////////////////////////////////////////////////////////////
	// string operations
	
	char* CORBA(string_alloc)(CORBA(ULong) ul_length_not_including_null) {
		return (char*) iluCppRuntime::iluMalloc(ul_length_not_including_null + 1);
	}
	
	
	char* CORBA(string_dup)(const char* pc_string_to_duplicate) {
		char* pc_string_copy;
		iluError an_error;
		pc_string_copy = ilu_StrdupE((char* const)pc_string_to_duplicate, &an_error);
		ILUCPP_ERRWARN(an_error, "ilu_StrdupE", ILUCPP_TRUE );
		return pc_string_copy;
	}
	
	
	void CORBA(string_free)(char* pc_string_to_free) {
		iluCppRuntime::iluFree(pc_string_to_free);
	}


	//////////////////////////////////////////////////////////////////
	// String_var Class
	
	//////////////////////////////////////////////////////////////////
	// Constructors and destructor
	
	CORBA(String_var)::	CORBA_(String_var)(){
		m_pc_string = NULL;
		m_b_release = ILUCPP_TRUE;
	}
	
	
	CORBA(String_var)::	CORBA_(String_var)(char* pc_string){
		// non const arg is consumed per corba spec
		m_pc_string = pc_string;
		m_b_release = ILUCPP_TRUE;
	}
	
	
	CORBA(String_var)::	CORBA_(String_var)(const char* pc_string) {
		// const arg is copied per corba spec 
		m_pc_string = CORBA(string_dup)(pc_string);
		m_b_release = ILUCPP_TRUE;
	}
	
	
	CORBA(String_var)::	CORBA_(String_var)(const CORBA(String_var)& r_string_to_copy){
		// String_var arg is copied per corba spec 
		m_pc_string = CORBA(string_dup)(r_string_to_copy.m_pc_string);
		m_b_release = ILUCPP_TRUE;
	}
	
	
	CORBA(String_var)::~CORBA_(String_var)() {
		if (m_b_release)
			CORBA(string_free)(m_pc_string);
	}
	
	
	//////////////////////////////////////////////////////////////////
	// assignment operations
	
	CORBA(String_var)& CORBA(String_var):: operator=(char* pc_string){
		if (m_b_release)
			CORBA(string_free)(m_pc_string);
		// arg is consumed per corba spec
		m_pc_string = pc_string;
		m_b_release = ILUCPP_TRUE;
		return *this;
	}
	
	CORBA(String_var)& CORBA(String_var)::operator=(const char* pc_string){
		if (m_b_release)
			CORBA(string_free)(m_pc_string);
		// const arg is copied per corba spec 
		m_pc_string = CORBA(string_dup)(pc_string);
		m_b_release = ILUCPP_TRUE;
		return *this;
	}
	
	CORBA(String_var)& CORBA(String_var)::operator=(const CORBA(String_var)& r_string_to_copy) {
		if (&r_string_to_copy == this)
			return *this; // assigning to self
		if (m_b_release)
			CORBA(string_free)(m_pc_string);
		// const arg is copied per corba spec 
		m_pc_string = CORBA(string_dup)(r_string_to_copy.m_pc_string);
		m_b_release = ILUCPP_TRUE;
		return *this;
	}
	
	
	CORBA(String_var)::operator char*(){
		return m_pc_string;
	}
	
	CORBA(String_var)::operator const char*() const{
		return m_pc_string;
	}
	
	char& CORBA(String_var)::operator[](CORBA(ULong) ul_index){
		return m_pc_string[ul_index];
	}
	
	char CORBA(String_var)::operator[](CORBA(ULong) ul_index) const{
		return m_pc_string[ul_index];
	}
	
	// following is for use by ILU stubs 
	char*& CORBA(String_var)::iluStringVarReference() const {
		return ((char *&)m_pc_string);
	}


	// accessors on the m_b_release member
	void CORBA(String_var)::iluSetRelease(CORBA(Boolean) b_release_on_destruct) const {
		// cast around constness
		CORBA(Boolean)* p_release = CONST_CAST(CORBA(Boolean)*, &m_b_release);
		*p_release = b_release_on_destruct;
	}
	
	
	CORBA(Boolean) CORBA(String_var)::iluGetRelease() const {
		return m_b_release;
	}
	

	//////////////////////////////////////////////////////////////////
	// Object Class

	  // holds the ilu class record for CORBA objects
	iluClass CORBA(Object)::m_ILUClassRecord;

	
	CORBA(Object*) CORBA(Object)::_duplicate(CORBA(Object*) p_obj){ 
		if (p_obj) p_obj->iluIncrementReferenceCount();
		return p_obj;
	}


	CORBA(Object*) CORBA(Object)::_nil(){ 	
		return (CORBA(Object*)) CORBA(OBJECT_NIL);
	}

	void CORBA(Object)::_release(){ 	
		iluDecrementReferenceCount();
	}

	CORBA(Boolean) CORBA(Object)::_non_existent(){ 	
		return (iluGetKernelObject() ? ILUCPP_TRUE : ILUCPP_FALSE);
	}

	CORBA(Boolean) CORBA(Object)::_is_equivalent(CORBA(Object*) p_obj){ 
		return (((this == p_obj) || (iluGetKernelObject() == p_obj->iluGetKernelObject())) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}

	CORBA(ULong) CORBA(Object)::_hash(CORBA(ULong) maximum) {

		return ilu_hash_HashPointer(this, maximum);
	}

 	CORBA(Boolean) CORBA(Object)::_is_a(char* logical_type_id) {

		iluClass the_class = ilu_FindClassFromID(logical_type_id);

		return ((the_class && ilu_IsSubObjectType(iluGetClassRecord(), the_class)) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}

	// for use in narrowing
	void* CORBA(Object)::iluDowncast (iluClass class_to_cast_down_to) {
		if (!class_to_cast_down_to || class_to_cast_down_to == ilu_rootClass)
			return((void *)((iluObject*) this));
		if (class_to_cast_down_to == m_ILUClassRecord)
			return ((void*) this);
		return (NULL);
	}


  // initialize to use of this class of objects - called by iluCppRuntime::iluInitialize

  void CORBA(Object)::iluInitialize() {
    // create and set the class record member to be the iluClass for this object type
    m_ILUClassRecord = 
      iluCppInternal::iluDefineObjectType(
      CONST_CAST(char*, "ilu.CORBA-Object"), // ILU name
      NULL,	// brand
      (char *) ilu_TypeID_ilu_CORBA_Object, // type id
      NULL,	 // singleton?
      ILUCPP_TRUE, // optional?
      ILUCPP_FALSE, // collectible?
      NULL,	 // authentication
      0,	// number of methods
      0,	// number of superclasses
      NULL	// no superclass uids 
    );

#ifdef ADD_VARIANT_SUPPORT
    // init the typecodes 
    CORBA(_tc_null) = new _ilu_corba_null_TypeCode();
    CORBA(_tc_boolean) = new _ilu_corba_boolean_TypeCode();
    CORBA(_tc_octet) = new _ilu_corba_octet_TypeCode();
    CORBA(_tc_short) = new _ilu_corba_short_TypeCode();
    CORBA(_tc_long) = new _ilu_corba_long_TypeCode();
    CORBA(_tc_ushort) = new _ilu_corba_ushort_TypeCode();
    CORBA(_tc_ulong) = new _ilu_corba_ulong_TypeCode();
    CORBA(_tc_char) = new _ilu_corba_char_TypeCode();
    CORBA(_tc_float) = new _ilu_corba_float_TypeCode();
    CORBA(_tc_double) = new _ilu_corba_double_TypeCode();
    CORBA(_tc_string) = new _ilu_corba_string_TypeCode();
    CORBA(_tc_any) = new _ilu_corba_any_TypeCode();
    CORBA(_tc_Object) = new _ilu_corba_object_TypeCode();
    CORBA(_tc_ilu_object) = new _ilu_ilu_object_TypeCode();
    CORBA(_tc_ilu_character) = new _ilu_ilu_character_TypeCode();
    CORBA(_tc_ilu_longinteger) = new _ilu_ilu_longinteger_TypeCode();
    CORBA(_tc_ilu_longcardinal) = new _ilu_ilu_longcardinal_TypeCode();
    CORBA(_tc_ilu_longreal) = new _ilu_ilu_longreal_TypeCode();
#endif /* ADD_VARIANT_SUPPORT */
  }

	CORBA(Object)* const CORBA(OBJECT_NIL) = (CORBA(Object)*)NULL;

	//////////////////////////////////////////////////////////////////
	// ORB Class
	
	// points to the one and only instance of CORBA_(ORB)
	CORBA(ORB_ptr) CORBA(ORB)::sm_p_single_orb_instance = (CORBA(ORB)*)NULL;

	// holds any initial refs passed in on command line
	iluHashTable CORBA(ORB)::sm_initial_references_hash_table = (iluHashTable) NULL;			

	// holds any default initial ref string passed in on command line
	char * CORBA(ORB)::sm_pc_default_init_ref = (char *)NULL;			

	// ORB initialization function
	CORBA(ORB_ptr) CORBA(ORB_init)(int& argc, char** argv, const char* pc_orb_identifier) {
		
		ILUCPP_BOOL	b_use_native_threads = ILUCPP_FALSE;
		int		i_index;
		
		// ensure ilu is being asked for
		if (strcmp(pc_orb_identifier, "ilu") != 0) 
			return (CORBA(ORB)*)NULL;
		
		// create single orb instance if we haven't already
		if (! CORBA(ORB)::sm_p_single_orb_instance) {
		  CORBA(ORB)::sm_p_single_orb_instance = new CORBA(ORB);
		  CORBA(ORB)::sm_initial_references_hash_table =
		    ilu_hash_MakeNewTable(3, ilu_hash_HashString, ilu_hash_StringCompare);
		  if (CORBA(ORB)::sm_initial_references_hash_table == NULL) {
		    ILUCPP_WARN("CORBA::ORB_init:  Can't create InitialReferences hash table!\n");
		    return (CORBA(ORB)*) NULL;
		  }
		} else // return our one and onlu ORB
		  return  CORBA(ORB)::sm_p_single_orb_instance;
		
		// see if we should run threaded
		for (i_index = 0; i_index < argc ; i_index++) {
			
			if (strcmp(argv[i_index], "-iluthreaded") == 0) {
				
				// found run threaded flag
				b_use_native_threads = ILUCPP_TRUE;
				
				// shuffle the arguments down - note also shuffles down the null that
				// should be at the end of argv
				for (int i_shuffle_down_to_index = i_index; 
				i_shuffle_down_to_index < argc; i_shuffle_down_to_index++)
					argv[i_shuffle_down_to_index] = argv[i_shuffle_down_to_index + 1];
				argc--;	
			}
		}
		
		// Check ILU_COS_NAMING_IOR
		char *pc_nameservice = getenv("ILU_COS_NAMING_IOR");
		if (pc_nameservice != NULL)
		  ilu_hash_AddToTable(CORBA(ORB)::sm_initial_references_hash_table,
				  CONST_CAST(char *, "NameService"), pc_nameservice);

		// Check ORBInitRef and ORBDefaultInitRef args

		for (i_index = 0; i_index < argc ; i_index++) {
			
			if (strcmp(argv[i_index], "-ORBInitRef") == 0) {
			  if ((i_index + 1) < argc) {
			    char c_name[1000], c_url[2000];
			    iluError an_error;
			    if (sscanf(argv[i_index+1], "%999[^=]=%1999s", c_name, c_url) != 2) {
			      ILUCPP_WARN("CORBA::ORB_init:  Bad -ORBInitRef parameter.\n");
			      return (CORBA(ORB) *)NULL;
			    } else {
			      char *pc_name;
			      char *pc_url;
			      pc_name = ilu_StrdupE(c_name, &an_error);
			      ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			      pc_url = ilu_StrdupE(c_name, &an_error);
			      ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
			      ilu_hash_AddToTable(CORBA(ORB)::sm_initial_references_hash_table,
						  pc_name, pc_url);
			    }
			  }
			  // shuffle the arguments down - note also shuffles down the null that
			  // should be at the end of argv
			  for (int i_shuffle_down_to_index = i_index; 
			       argv[i_shuffle_down_to_index] != NULL; i_shuffle_down_to_index++)
			    argv[i_shuffle_down_to_index] = argv[i_shuffle_down_to_index + 2];
			  argc -= 2;	

			} else if (strcmp(argv[i_index], "-ORBDefaultInitRef") == 0) {
			  if ((i_index + 1) < argc) {
			    CORBA(ORB)::sm_pc_default_init_ref = argv[i_index + 1];
			    // shuffle the arguments down - note also shuffles down the null that
			    // should be at the end of argv
			    for (int i_shuffle_down_to_index = i_index; 
				 argv[i_shuffle_down_to_index] != NULL; i_shuffle_down_to_index++)
			      argv[i_shuffle_down_to_index] = argv[i_shuffle_down_to_index + 2];
			    argc -= 2;
			  } else {
			    ILUCPP_WARN("CORBA::ORB_init:  -ORBDefaultInitRef without parameter!\n");
			    return (CORBA(ORB) *) NULL;
			  }
			}
		}
		

		// call the runtimes initializer
		iluCppRuntime::iluInitialize(b_use_native_threads);
		
		// return our one and only ORB
		return  CORBA(ORB)::sm_p_single_orb_instance;
	}


	// object <-> string methods
	CORBA(Object*) CORBA(ORB)::string_to_object(char* pc_sbh) { 
		
		iluObject* p_iluobject = iluObject::iluStringToObject(pc_sbh);

		if (!p_iluobject)
			return CORBA(OBJECT_NIL);

		return ((CORBA(Object*)) 
			p_iluobject->iluDowncast(CORBA(Object)::iluGetILUClassRecord()));
	}
	

	char* CORBA(ORB)::object_to_string(CORBA(Object*) p_object) {
#ifdef IIOP_PROTOCOL
		// prefer the IOR representation if we have IIOP configured in
		char* pc_string = p_object->iluObjectToIORString();
		if (pc_string) 
			return pc_string;
#endif
		return p_object->iluObjectToString();
	}


	CORBA(Object)* CORBA(ORB)::resolve_initial_references(const char* pc_identifier) {
		
	  char *pc_url;
	  CORBA(Object)* p_obj;

	  if (sm_initial_references_hash_table &&
	      (pc_url = (char *) ilu_hash_FindInTable(sm_initial_references_hash_table,
						      (void *) pc_identifier))) {
	    p_obj = string_to_object(pc_url);
	    if (!p_obj) {
	      ILUCPP_WARN("CORBA::ORB::resolve_initial_references:  couldn't convert URL to object\n");
	    }
	    return p_obj;

	  } else if (sm_pc_default_init_ref) {

	    char urlbuf[3000];
	    sprintf (urlbuf, "%s/%s", sm_pc_default_init_ref, pc_identifier);
	    p_obj = string_to_object(pc_url);
	    if (!p_obj) {
	      ILUCPP_WARN("CORBA::ORB::resolve_initial_references:  couldn't convert DefaultInitRef URL to object\n");
	    }
	    return p_obj;
	    
	  } else {

	    // must be a name we don't know
	    CORBA(ORB)::InvalidName _exception;
	    throw(_exception);

	  }
	  // make some compilers happy
	  return CORBA(OBJECT_NIL);
	}


#ifdef ADD_VARIANT_SUPPORT

	//////////////////////////////////////////////////////////////////
	// Any Class
	
	// constructors and destructor
	CORBA(Any)::CORBA_(Any)() {
		m_p_typecode = CORBA(_tc_null);
		m_pv_value = NULL;
		m_pickle.pi_len = 0;
		m_pickle.pi_bytes = NULL;
		m_b_release = ILUCPP_TRUE;
	}
	
	CORBA(Any)::CORBA_(Any)(const CORBA(Any)& r_any){
		m_p_typecode = CORBA(TypeCode)::_duplicate(r_any.m_p_typecode);
		m_b_release  = ILUCPP_TRUE;
		m_pv_value = NULL;
		// xxx supposed to deep copy the value, but can't really do this on m_pv_value since
		// we don't know its structure - so we'll copy the pickle
		m_pickle.pi_len = r_any.m_pickle.pi_len;
		m_pickle.pi_bytes = (ilu_bytes) new char [m_pickle.pi_len];
		memcpy(m_pickle.pi_bytes, r_any.m_pickle.pi_bytes, m_pickle.pi_len);
	}
	
	CORBA(Any)::CORBA_(Any)(CORBA(TypeCode_ptr) p_typecode, void* pv_value, 
		CORBA(Boolean) b_release){
		m_p_typecode = p_typecode;
		m_b_release  = b_release;
		m_pv_value = pv_value;
		m_pickle.pi_len = 0;
		m_pickle.pi_bytes = NULL;
	}
	

	// destructor
	CORBA(Any)::~CORBA_(Any)() {

		iluDeleteContent();
	}

	
	// assignment
	CORBA(Any)& CORBA(Any)::operator=(const CORBA(Any)& r_any){
				
		if (&r_any == this)
			return *this; // assigning to self

		iluDeleteContent();
		
		m_p_typecode = CORBA(TypeCode)::_duplicate(r_any.m_p_typecode);
		
		// only one of r_any.m_pv_value or r_any.m_pickle will be set
		// if it's the pickle, then just copy the bytes as below
		// if its the value then what do we do, have the TypeCode do a deep copy
		
		if (r_any.m_pv_value) {
			m_pv_value = (r_any.m_p_typecode)->iluDeepCopy(r_any.m_pv_value);
		}
		else {
			m_pickle.pi_len = r_any.m_pickle.pi_len; 
			m_pickle.pi_bytes = (ilu_bytes) new char [m_pickle.pi_len];
			memcpy(m_pickle.pi_bytes, r_any.m_pickle.pi_bytes, m_pickle.pi_len);
		}
		
		return *this;
	}
	


	// insertion
	void CORBA(Any)::operator<<=(CORBA(Short) a_short) {
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_short, Short, a_short)
	}

	void CORBA(Any)::operator<<=(CORBA(UShort) a_ushort){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_ushort, UShort, a_ushort)
	}
	
	void CORBA(Any)::operator<<=(CORBA(Long) a_long){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_long, Long, a_long)
	}
	
	void CORBA(Any)::operator<<=(CORBA(ULong) a_ulong){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_ulong, ULong, a_ulong)
	}
	
	void CORBA(Any)::operator<<=(CORBA(Float) a_float){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_float, Float, a_float)
	}
	
	void CORBA(Any)::operator<<=(CORBA(Double) a_double){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_double, Double, a_double)
	}	

	void CORBA(Any)::operator<<=(const CORBA(Any)& r_any){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_any, Any, r_any)
	}	
	

	void CORBA(Any)::operator<<=(const char* pc_char){
		// free up anything we had inside
		iluDeleteContent();
		// assign the appropriate typecode
		m_p_typecode = CORBA(_tc_string);
		// unbounded strings copied per corba spec section 16.14.2
		char* pc_copy = new char[strlen(pc_char) + 1];
		strcpy(pc_copy, pc_char);
		m_pv_value = pc_copy;
	}
	

	void CORBA(Any)::operator<<=( CORBA(Object_ptr) p_object){
		// free up anything we had inside
		iluDeleteContent();
		// assign the appropriate typecode
		m_p_typecode = CORBA(_tc_Object);
		m_pv_value = p_object;
	}


	// ilu specific insertion

	void CORBA(Any)::operator<<=( iluObject* p_object){
		// free up anything we had inside
		iluDeleteContent();
		// assign the appropriate typecode
		m_p_typecode = CORBA(_tc_ilu_object);
		m_pv_value = p_object;
	}

	void CORBA(Any)::operator<<=(iluLongInteger an_ilu_longinteger){
		// free up anything we had inside 
		iluDeleteContent();
		// assign the appropriate typecode 
		m_p_typecode = CORBA(_tc_ilu_longinteger);
		// get our own copy of the data 
		iluLongInteger* p_content = new iluLongInteger;
		*p_content = an_ilu_longinteger;
		m_pv_value = p_content;
	}	
	

	void CORBA(Any)::operator<<=(iluLongReal an_ilu_longreal){
		// free up anything we had inside 
		iluDeleteContent();
		// assign the appropriate typecode 
		m_p_typecode = CORBA(_tc_ilu_longreal);
		// get our own copy of the data 
		iluLongReal* p_content = new iluLongReal;
		*p_content = an_ilu_longreal;
		m_pv_value = p_content;
	}	

	
	void CORBA(Any)::operator<<=(iluLongCardinal an_ilu_long_cardinal){
		// free up anything we had inside 
		iluDeleteContent();
		// assign the appropriate typecode 
		m_p_typecode = CORBA(_tc_ilu_longcardinal);
		// get our own copy of the data 
		iluLongCardinal* p_content = new iluLongCardinal;
		*p_content = an_ilu_long_cardinal;
		m_pv_value = p_content;
	}	
	


	// extraction
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Short)& r_a_short) const {
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_short, Short, r_a_short)
	}
	
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(UShort)& r_a_ushort) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_ushort, UShort, r_a_ushort)
	}	

	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Long)& r_a_long) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_long, Long, r_a_long)
	}		

	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(ULong)& r_a_ulong) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_ulong, ULong, r_a_ulong)
	}		
	
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Float)& r_a_float) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_float, Float, r_a_float)
	}		
	

	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Double)& r_a_double) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_double, Double, r_a_double)
	}		

	
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)& r_any) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_any, Any, r_any)
	}		
	

	CORBA(Boolean) CORBA(Any)::operator>>=(char*& r_pc_char) const{
		
		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);
		
		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_string))) { 
			char* pc_copy = (CORBA(string)) m_pv_value;
			r_pc_char = new char [strlen(pc_copy) + 1];
			strcpy(r_pc_char, pc_copy);
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE;
	}


	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Object_ptr)& r_p_object) const{

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);

		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_Object))) { 
			r_p_object = (CORBA(Object)*) m_pv_value;
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE;
	}


	// ilu specific extraction
	
	CORBA(Boolean) CORBA(Any)::operator>>=(iluObject*& r_p_object) const{

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);

		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_ilu_object))) { 
			r_p_object = (iluObject*) m_pv_value;
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE;
	}

	CORBA(Boolean) CORBA(Any)::operator>>=(iluLongInteger& r_ilu_longinteger) const{
		
		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);
		
		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_ilu_longinteger))) { 
			r_ilu_longinteger = *((iluLongInteger*) m_pv_value);
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE; // there's no LSR value or now expected type
	}
	
	
	CORBA(Boolean) CORBA(Any)::operator>>=(iluLongReal& r_ilu_longreal) const{

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);
		
		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_ilu_longreal))) { 
			r_ilu_longreal = *((iluLongReal*) m_pv_value);
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE; // there's no LSR value or now expected type
	}
	

	CORBA(Boolean) CORBA(Any)::operator>>=(iluLongCardinal& r_ilu_long_cardinal) const{

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this);
		
		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_ilu_longcardinal))) { 
			r_ilu_long_cardinal = *((iluLongCardinal*) m_pv_value);
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE; // there's no LSR value or now expected type
	}
	
	
	// from structure insertion
	void CORBA(Any)::operator<<=(CORBA(Any)::from_boolean a_from_boolean){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_boolean, Boolean, a_from_boolean.val)
	}
	
	void CORBA(Any)::operator<<=(CORBA(Any)::from_char a_from_char){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_char, Char, a_from_char.val)
	}
	
	void CORBA(Any)::operator<<=(CORBA(Any)::from_octet a_from_octet){
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(_tc_octet, Octet, a_from_octet.val)
	}
	

	void CORBA(Any)::operator<<=(CORBA(Any)::from_string a_from_string){
		// free up anything we had inside
		iluDeleteContent();
		// assign the appropriate typecode
		m_p_typecode = CORBA(_tc_string);
		m_pv_value = a_from_string.val;
	}
	
	
	// to structure extraction
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)::to_boolean a_to_boolean) const {
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_boolean, Boolean, a_to_boolean.ref)
	}
		
	
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)::to_char a_to_char) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_char, Char, a_to_char.ref)
	}
	
	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)::to_octet a_to_octet) const{
		ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(_tc_octet, Octet, a_to_octet.ref)
	}
	

	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)::to_object a_to_object) const{	

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this); 

		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_Object))) {
			a_to_object.ref = (CORBA(Object)*) m_pv_value;
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE; // there's no LSR value or now expected type
	}
	

	CORBA(Boolean) CORBA(Any)::operator>>=(CORBA(Any)::to_string a_to_string) const{

		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this); 

		if (p_nc_myself->iluGetFromPickle(CORBA(_tc_string))) {
			a_to_string.val = (CORBA(string)) m_pv_value;
			a_to_string.bound = strlen(a_to_string.val);
			return ILUCPP_TRUE;
		}
		return ILUCPP_FALSE; // there's no LSR value or now expected type
	}
	

	// replace
	void CORBA(Any)::replace(CORBA(TypeCode_ptr) p_typecode, void* pv_value, 
		CORBA(Boolean) b_release){
		
		// free up anything we had inside
		iluDeleteContent();
		
		m_p_typecode = p_typecode;
		m_b_release  = b_release;
		m_pv_value = pv_value;
		m_pickle.pi_len = 0;
		m_pickle.pi_bytes = NULL;
	}
	

	// return the type of the contents
	CORBA(TypeCode_ptr) CORBA(Any)::type() {
		if (m_p_typecode != CORBA(_tc_null))
			return m_p_typecode;
		if (iluGetFromPickle())
			return m_p_typecode;
		return CORBA(_tc_null);
	}
	
	// Deprecated - return the LSR value
	const void* CORBA(Any)::value() {
		if (m_pv_value)
			return m_pv_value;
		if (iluGetFromPickle())
			return m_pv_value;
		
		return NULL;
	}

	// return a pointer to the internal pickle
	iluPickle* CORBA(Any)::iluPickleValue() {
		if (m_pickle.pi_len != 0)
			return &m_pickle;
		if (iluPutToPickle())
			return &m_pickle;
		
		return NULL;
	}


	// deletes any storage associated with the any - and sets up as per default constructor
	void CORBA(Any)::iluDeleteContent() {

		if (m_b_release && m_p_typecode)
			m_p_typecode->iluDeleteValue(m_pv_value);
		delete m_pickle.pi_bytes;

		m_p_typecode = CORBA(_tc_null);
		m_pv_value = NULL;
		m_pickle.pi_len = 0;
		m_pickle.pi_bytes = NULL;
		m_b_release = ILUCPP_TRUE;
	}



	// convert back and forth between the internal LSR and pickle representations
	// return true if successful

	CORBA(Boolean) CORBA(Any)::iluPutToPickle() {

		CORBA(Boolean) b_result;

		if (m_pickle.pi_len != 0) // already pickled
			return ILUCPP_TRUE;

		if (m_pv_value == NULL || !m_p_typecode ||
			(m_p_typecode == CORBA(_tc_null))) { // empty LSR value
			m_pickle.pi_len = 0;
			m_pickle.pi_bytes = NULL;
			return ILUCPP_TRUE;
		}

		try {
			iluCallStruct a_call;
			// we need ot create the iluPickleCall using the typecode of what the 
			// most specific type of the object really is
			CORBA(TypeCode)* p_most_specific_type_code = m_p_typecode->iluMostSpecificTypeCode(m_pv_value);
			if (!p_most_specific_type_code) {
				ILUCPP_DEBUG2("CORBA::Any::iluPutToPickle(), Couldn't find most specific typecode of %s\n", m_p_typecode->iluTypeName());
				return ILUCPP_FALSE;
			}
			iluPickleCall the_pickle_call(&a_call, p_most_specific_type_code, &m_pickle);
			// tell the typecode to convert the value to the pickle
			b_result = m_p_typecode->iluValueToPickle (m_pv_value, the_pickle_call);
			the_pickle_call.iluEndPickle();
		}
		catch (...) {
			ILUCPP_DEBUG2("CORBA::Any::iluPutToPickle(), Couldn't pickle a %s\n", m_p_typecode->iluTypeName());
			return ILUCPP_FALSE;
		}

		if (b_result) { // successful, so flush out the value
			ILUCPP_DEBUG2("CORBA::Any::iluPutToPickle(), pickled a %s\n", m_p_typecode->iluTypeName());
			if (m_b_release && m_p_typecode) 
				m_p_typecode->iluDeleteValue(m_pv_value);
			m_pv_value = NULL;
			// null our typecode ptr
			m_p_typecode =  CORBA(_tc_null);
			return ILUCPP_TRUE;
		}
		
		ILUCPP_DEBUG2("CORBA::Any::iluPutToPickle(), Couldn't pickle a %s\n", m_p_typecode->iluTypeName());

		return ILUCPP_FALSE;
	}	
	

	CORBA(Boolean) CORBA(Any)::iluGetFromPickle(CORBA(TypeCode_ptr) p_should_be_typecode) {
		
		CORBA(TypeCode_ptr) p_the_corba_typecode;
		iluCString pc_ilu_type_id;
		iluError an_error;
		
		if (m_pv_value)			// already unpickled
			return ILUCPP_TRUE;
		
		if (m_pickle.pi_len == 0) // empty pickle
			return ILUCPP_FALSE;
		
		pc_ilu_type_id = ilu_PickleType (m_pickle, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_HANDLED(an_error);
			ILUCPP_DEBUG1("CORBA::Any::iluGetFromPickle() couldn't get type from pickle\n");
			return ILUCPP_FALSE;
		}
		
		// get the CORBA::TypeCode object from the ilu type id
		p_the_corba_typecode = CORBA(TypeCode)::LookupTypeCode(pc_ilu_type_id);
		
		if (!p_the_corba_typecode) {// no locally known type
			ILUCPP_DEBUG2("CORBA::Any::iluGetFromPickle() LookupTypeCode failed for typeid %s\n", pc_ilu_type_id);
			return ILUCPP_FALSE;
		}
		
		// see if a certain typecode is expected
		if (p_should_be_typecode && (!(p_the_corba_typecode->equal(p_should_be_typecode)))) {
			// perhaps the type being asked for is a base type of the type in the pickle
			if (!p_the_corba_typecode->iluIsBaseType(p_should_be_typecode)) {
				ILUCPP_DEBUG2("CORBA::Any::iluGetFromPickle() %s not the requested (or derived) type\n", p_the_corba_typecode->iluTypeName());
				return ILUCPP_FALSE;
			}
		}
				
		// tell the typecode to convert the pickle to the value
		try {
			iluCallStruct a_call;
			iluPickleCall the_pickle_call(&a_call, p_the_corba_typecode, &m_pickle);
			the_pickle_call.iluInputPickle();
			m_pv_value = p_the_corba_typecode->iluPickleToValue (the_pickle_call);
			the_pickle_call.iluEndPickle();
		}
		catch (...) { 
			ILUCPP_DEBUG2("CORBA::Any::iluGetFromPickle() pickle extraction to %s failed\n", p_the_corba_typecode->iluTypeName());
			m_pv_value = NULL; 
			return ILUCPP_FALSE;
		}
		
		if (m_pv_value) {
			ILUCPP_DEBUG2("CORBA::Any::iluGetFromPickle() pickle extracted %s\n", p_the_corba_typecode->iluTypeName());
			
			if (p_should_be_typecode && (!p_should_be_typecode->equal(p_the_corba_typecode))) {

				// now, m_pv_value is a pointer to a type of p_the_corba_typecode, so
				// we need to cast it up to a type of p_should_be_typecode
				ILUCPP_DEBUG3("CORBA::Any::iluGetFromPickle() upcasting %s to %s\n", p_the_corba_typecode->iluTypeName(),
					p_should_be_typecode->iluTypeName());

				void* pc_cast_value = p_the_corba_typecode->iluUpcast(m_pv_value, p_should_be_typecode->iluGetIluClass());

				if (!pc_cast_value) {

					// couldn't cast up
					ILUCPP_DEBUG3("CORBA::Any::iluGetFromPickle() upcasting %s to %s failed\n", p_the_corba_typecode->iluTypeName(),
					p_should_be_typecode->iluTypeName());

					// delete whatever was unpickled
					m_p_typecode->iluDeleteValue(m_pv_value);
					m_pv_value = NULL;

					// null our typecode ptr
					m_p_typecode =  CORBA(_tc_null);

					return ILUCPP_FALSE;
				}
				else // reassign to the correctly cast pointer
					m_pv_value = pc_cast_value;
			}

			// we successfully converted, so null out the pickle
			iluCppRuntime::iluFree(m_pickle.pi_bytes);
			m_pickle.pi_bytes = NULL;
			m_pickle.pi_len = 0;

			// and set our typecode ptr
			if (p_should_be_typecode)
				m_p_typecode = p_should_be_typecode;
			else 
				m_p_typecode = p_the_corba_typecode;

			return ILUCPP_TRUE;
		}

		return ILUCPP_FALSE;
	}
	

	//////////////////////////////////////////////////////////////////
	// input output and size a Corba any


	ILU_RUNTIME_PUBLIC iluBaseCall& operator+=(iluBaseCall& r_call, CORBA(Any)& r_any) {	
		
		iluError an_error;
		
		r_call.iluAddToSize(ilu_SizeOfPickle(r_call.iluGetCall(), *(r_any.iluPickleValue()), (iluType)ILU_NIL, &an_error));
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		return r_call;
	}
	
	ILU_RUNTIME_PUBLIC iluBaseCall& operator+=(iluBaseCall& r_call, const CORBA(Any)& r_any) {	
		
		iluError an_error;
		CORBA(Any)* p_nc_any = CONST_CAST(CORBA(Any)*, &r_any);
		r_call.iluAddToSize(ilu_SizeOfPickle(r_call.iluGetCall(), *(p_nc_any->iluPickleValue()), (iluType)ILU_NIL, &an_error));
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		return r_call;
	}
	
	ILU_RUNTIME_PUBLIC iluBaseCall& operator<<(iluBaseCall& r_call, CORBA(Any)& r_any) {	
		
		iluError an_error;
		
		ilu_OutputPickle(r_call.iluGetCall(), *(r_any.iluPickleValue()), (iluType)ILU_NIL, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		return r_call;
	}
	
	ILU_RUNTIME_PUBLIC iluBaseCall& operator<<(iluBaseCall& r_call, const CORBA(Any)& r_any) {	
		
		iluError an_error;
		CORBA(Any)* p_nc_any = CONST_CAST(CORBA(Any)*, &r_any);	
		ilu_OutputPickle(r_call.iluGetCall(), *(p_nc_any->iluPickleValue()), (iluType)ILU_NIL, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		return r_call;
	}
	
	ILU_RUNTIME_PUBLIC iluBaseCall& operator>>(iluBaseCall& r_call, CORBA(Any)& r_any) {	
		
		iluError an_error;
		
		r_any.iluDeleteContent();
		
		ilu_InputPickle(r_call.iluGetCall(), r_any.iluPickleValue(), (iluType)ILU_NIL, &an_error);
		ILUCPP_THROW_EXCEPTION_IF_ERROR(an_error);
		
		return r_call;
	}
	

	//////////////////////////////////////////////////////////////////
	// TypeCode Class

	// used to find the CORBA::TypeCode Objects from the ilu type ids
	iluHashTable CORBA(TypeCode)::sm_typecode_hash_table = NULL;

	// used for the null type code (really put in to satisfy SunPro sc3.0.1/solaris-1/CC )
	iluCString CORBA(TypeCode)::sm_pc_emptystring = CONST_CAST(char*, "");
	
	
	// construct a typecode	based on an iluTypeKind
	CORBA(TypeCode)::CORBA_(TypeCode) (iluTypeKind enum_type_kind, const iluCString str_type_id)
		: m_enum_type_kind(enum_type_kind), m_str_type_id(str_type_id ? str_type_id : sm_pc_emptystring) {

		if (sm_typecode_hash_table == NULL)
			// set up the hash table 
			sm_typecode_hash_table = ilu_hash_MakeNewTable(271, ilu_hash_HashString, ilu_hash_StringCompare);
		
		// add this typecode to the ilu_HashTable used to find the CORBA::TypeCode Objects from the ilu type ids
		if (!(ilu_hash_AddToTable(sm_typecode_hash_table, m_str_type_id, this))) {
			// ILUCPP_WARN("CORBA::TypeCode constructor, type already in table");
			 ILUCPP_DEBUG2("CORBA::TypeCode constructor, Duplicate Typecode for %s\n", str_type_id);
		}
		else {
			ILUCPP_DEBUG2("CORBA::TypeCode constructor, added Typecode for %s\n", str_type_id ? str_type_id : "NULL");
		}
	}
	
	// destructor
	CORBA(TypeCode)::~CORBA_(TypeCode) () {

		// remove this typecode from the ilu_HashTable used to find the CORBA::TypeCode Objects from the ilu type ids
		if (ilu_hash_RemoveFromTable(sm_typecode_hash_table, m_str_type_id) == ILU_NIL) {
			ILUCPP_WARN("CORBA::TypeCode destructor, type not in table");
			ILUCPP_DEBUG2("CORBA::TypeCode destructor, No table entry for %s\n", m_str_type_id);
		}
		else {
			ILUCPP_DEBUG2("CORBA::TypeCode destructor, removed Typecode for %s\n", m_str_type_id);
		}
	}


	// conversions between ilu and corba typekinds, return false if no valid conversion
	CORBA(Boolean) CORBA(TypeCode)::iluIluTypeKindToCorbaTypeKind(
		iluTypeKind ilukind, CORBA(TCKind)& r_corbakind) {
		
		switch (ilukind) {
		case ilu_byte_tk:			r_corbakind = CORBA(tk_octet);		return ILUCPP_TRUE;
		case ilu_boolean_tk:		r_corbakind = CORBA(tk_boolean);	return ILUCPP_TRUE;
		case ilu_shortcharacter_tk:	r_corbakind = CORBA(tk_char);		return ILUCPP_TRUE;
		case ilu_shortinteger_tk:	r_corbakind = CORBA(tk_short);		return ILUCPP_TRUE;
		case ilu_integer_tk:		r_corbakind = CORBA(tk_long);		return ILUCPP_TRUE;
		case ilu_shortcardinal_tk:	r_corbakind = CORBA(tk_ushort);		return ILUCPP_TRUE;
		case ilu_cardinal_tk:		r_corbakind = CORBA(tk_ulong);		return ILUCPP_TRUE;
		case ilu_real_tk:			r_corbakind = CORBA(tk_double);		return ILUCPP_TRUE;
		case ilu_shortreal_tk:		r_corbakind = CORBA(tk_float);		return ILUCPP_TRUE;
		case ilu_object_tk:			r_corbakind = CORBA(tk_objref);		return ILUCPP_TRUE;
		case ilu_alias_tk:			r_corbakind = CORBA(tk_alias);		return ILUCPP_TRUE;
		case ilu_union_tk:			r_corbakind = CORBA(tk_union);		return ILUCPP_TRUE;
		case ilu_sequence_tk:		r_corbakind = CORBA(tk_sequence);	return ILUCPP_TRUE;
		case ilu_record_tk:			r_corbakind = CORBA(tk_struct);		return ILUCPP_TRUE;
		case ilu_array_tk:			r_corbakind = CORBA(tk_array);		return ILUCPP_TRUE;
		case ilu_enumeration_tk:	r_corbakind = CORBA(tk_enum);		return ILUCPP_TRUE;
		case ilu_pickle_tk:			r_corbakind = CORBA(tk_any);		return ILUCPP_TRUE;
			
			// no corba analogs for the following
		case ilu_character_tk:		
		case ilu_longinteger_tk:	
		case ilu_longcardinal_tk:	
		case ilu_longreal_tk:		
		case ilu_pipe_tk:			
		case ilu_optional_tk:	
		default: 
			r_corbakind = CORBA(tk_null); return ILUCPP_FALSE;
		}
	}
	

	CORBA(Boolean) CORBA(TypeCode)::iluCorbaTypeKindToIluTypeKind(
		CORBA(TCKind) corbakind, iluTypeKind& r_ilukind) {

		switch (corbakind) {

			case CORBA(tk_short) :		r_ilukind = ilu_shortinteger_tk;	return ILUCPP_TRUE;
			case CORBA(tk_long) :		r_ilukind = ilu_integer_tk;			return ILUCPP_TRUE;
			case CORBA(tk_ushort) :		r_ilukind = ilu_shortcardinal_tk;	return ILUCPP_TRUE;
			case CORBA(tk_ulong) :		r_ilukind = ilu_cardinal_tk;		return ILUCPP_TRUE;
			case CORBA(tk_float) :		r_ilukind = ilu_shortreal_tk;		return ILUCPP_TRUE;
			case CORBA(tk_double) :		r_ilukind = ilu_real_tk;			return ILUCPP_TRUE;
			case CORBA(tk_boolean) :	r_ilukind = ilu_boolean_tk;			return ILUCPP_TRUE;
			case CORBA(tk_char) :		r_ilukind = ilu_shortcharacter_tk;	return ILUCPP_TRUE;
			case CORBA(tk_octet) :		r_ilukind = ilu_byte_tk;			return ILUCPP_TRUE;
			case CORBA(tk_any) :		r_ilukind = ilu_pickle_tk;			return ILUCPP_TRUE;
			case CORBA(tk_objref) :		r_ilukind = ilu_object_tk;			return ILUCPP_TRUE;
			case CORBA(tk_struct) :		r_ilukind = ilu_record_tk;			return ILUCPP_TRUE;
			case CORBA(tk_union) :		r_ilukind = ilu_union_tk;			return ILUCPP_TRUE;
			case CORBA(tk_enum) :		r_ilukind = ilu_enumeration_tk;		return ILUCPP_TRUE;
			case CORBA(tk_string) : 
			case CORBA(tk_sequence) :	r_ilukind = ilu_sequence_tk;		return ILUCPP_TRUE;
			case CORBA(tk_array) :		r_ilukind = ilu_array_tk;			return ILUCPP_TRUE;
			case CORBA(tk_alias) :		r_ilukind = ilu_alias_tk;			return ILUCPP_TRUE;
			case CORBA(tk_except) :
			// no ilu analogs for the following
			case CORBA(tk_null):
			case CORBA(tk_void) : 
			case CORBA(tk_TypeCode) : 
			case CORBA(tk_Principal) : 
			default:
				return ILUCPP_FALSE;
		}
	}			


	const char* CORBA(TypeCode)::iluTypeName() {
		iluClass aclass = ilu_FindClassFromID(m_str_type_id);
		if (aclass)
			return aclass->cl_name;
		iluError an_error;
		iluType atype = ilu_FindTypeByUID(m_str_type_id, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_HANDLED(an_error);
			return m_str_type_id;
		}
		if (atype)
			return type_name(atype);
		
		return m_str_type_id;
	}
	
	
	// returns true if p_possible_base_typecode is a base type of this typecode 
	CORBA(Boolean) CORBA(TypeCode)::iluIsBaseType(CORBA(TypeCode_ptr) p_possible_base_typecode) {
		return (ilu_IsSubObjectType(iluGetIluClass(), p_possible_base_typecode->iluGetIluClass()) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}


	// returns true iff the pickle matches this typecode
	CORBA(Boolean) CORBA(TypeCode)::iluMatchPickle(iluPickle& r_pickle) {
		
		iluCString pc_pickles_id;
		iluError an_error;
		
		// get the type id from the pickle
		pc_pickles_id = ilu_PickleType (r_pickle, &an_error);
		if (ILU_ERRNOK(an_error)) {
			ILU_HANDLED(an_error);
			return ILUCPP_FALSE;
		}

		return ((pc_pickles_id && (strcmp(m_str_type_id, pc_pickles_id) == 0)) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}



	// return a pointer to the CORBA::TypeCode having the given ilu type id, null if none found
	CORBA(TypeCode_ptr) CORBA(TypeCode)::LookupTypeCode(const char* pc_ilu_type_id) {
	
		CORBA(TypeCode_ptr) p_typecode;

		// find the typecode in the ilu_HashTable used to find the CORBA::TypeCode Objects from the ilu type ids
		p_typecode = (CORBA(TypeCode_ptr)) ilu_hash_FindInTable(sm_typecode_hash_table, ((void*)pc_ilu_type_id));

		if (p_typecode == ILU_NIL) {
			ILUCPP_WARN("CORBA::TypeCode LookupTypeCode, type not in table");
			ILUCPP_DEBUG2("CORBA::TypeCode LookupTypeCode, No Typecode for %s\n", pc_ilu_type_id);
		}
		else {
			ILUCPP_DEBUG2("CORBA::TypeCode LookupTypeCode, Got Typecode for %s\n", pc_ilu_type_id);
		}

		return p_typecode;
	}

	// Used situations where someone want's to extract a base object type from an any
	// containing a derived object type.  Note that only typecodes for object types
	// should and must override this member function
	void* CORBA(TypeCode)::iluUpcast (void* p_an_object_of_your_type, iluClass) {
		ILUCPP_WARN("CORBA::TypeCode iluUpcast, must be being called on a non object type - should not occur!");
		return p_an_object_of_your_type;
	}

	// Used situations where someone want's to insert a base object type into an any
	// where that object is really a derived object type.  Note that only typecodes for object types
	// should & must override this member function
	CORBA(TypeCode)* CORBA(TypeCode)::iluMostSpecificTypeCode (void*) {
		return this;
	}


	//////////////////////////////////////////////////////////////////
	// type code constants and definition of the type code member functions for all the primitive types
	//////////////////////////////////////////////////////////////////

	void _ilu_corba_null_TypeCode::iluDeleteValue (void*)  {
	}

	void* _ilu_corba_null_TypeCode::iluDeepCopy (void*)  {
		return NULL;
	}

	CORBA(Boolean) _ilu_corba_null_TypeCode::iluValueToPickle (void*, iluPickleCall& r_pickle_call) {
		r_pickle_call.iluOutputPickle();
		return ILUCPP_TRUE;
	}
	
	void* _ilu_corba_null_TypeCode::iluPickleToValue (iluPickleCall&) {
		return NULL;
	}

	CORBA(TypeCode_ptr) CORBA(_tc_null) = NULL;


	//////////////////////////////////////////////////////////////////

	ILUCPP_DEFINE_CORBA_TYPE_CODE_DELETEVALUE(corba_boolean, Boolean)
	ILUCPP_DEFINE_CORBA_TYPE_CODE_DEEPCOPY(corba_boolean, Boolean)

	// booleans require wrappers
	CORBA(Boolean) _ilu_corba_boolean_TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {
		try {
			iluBoolWrapper _p_thevalue(*((CORBA(Boolean)*)pv_value));
			r_pickle_call += _p_thevalue;
			r_pickle_call.iluOutputPickle();
			r_pickle_call << _p_thevalue;
		}
		catch (...) {return ILUCPP_FALSE;}
		return ILUCPP_TRUE;
	}
	
	void* _ilu_corba_boolean_TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {
		CORBA(Boolean)* p_thevalue = new CORBA(Boolean);
		try {
			iluBoolWrapper _the_value_wrapper(*p_thevalue);
			r_pickle_call >> _the_value_wrapper;
			}
		catch (...) { delete p_thevalue; return NULL;}
		return p_thevalue;
	}

	CORBA(TypeCode_ptr) CORBA(_tc_boolean) = NULL;


	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_octet, Octet);
	
        CORBA(TypeCode_ptr) CORBA(_tc_octet) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_short,		Short);		
	
         CORBA(TypeCode_ptr) CORBA(_tc_short) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_long,		Long);		

        CORBA(TypeCode_ptr) CORBA(_tc_long) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_ushort,		UShort);		

	CORBA(TypeCode_ptr) CORBA(_tc_ushort) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
    ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_ulong,		ULong);		

	CORBA(TypeCode_ptr) CORBA(_tc_ulong) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
    ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_char,		Char);

	CORBA(TypeCode_ptr) CORBA(_tc_char) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_float,		Float);

        CORBA(TypeCode_ptr) CORBA(_tc_float) = NULL;
	
	//////////////////////////////////////////////////////////////////

	ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(corba_double,		Double);
	
        CORBA(TypeCode_ptr) CORBA(_tc_double) = NULL;
	
	//////////////////////////////////////////////////////////////////
		
	void _ilu_corba_string_TypeCode::iluDeleteValue (void* pv_value) {
		delete [] (char*) pv_value;
	}

	void* _ilu_corba_string_TypeCode::iluDeepCopy (void* pv_value) {
		CORBA(string) p_thevalue = (CORBA(string)) pv_value;
		CORBA(string) p_return = new char[strlen(p_thevalue) + 1];
		strcpy(p_return, p_thevalue);
		return p_return;
	}


	// strings require wrappers
	CORBA(Boolean) _ilu_corba_string_TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {
		try {
			iluCardinal card_length = strlen((iluCString) pv_value);
			iluCString the_string = ((iluCString) pv_value);
			iluCStringWrapper _p_thevalue(the_string, card_length);
			r_pickle_call += _p_thevalue;
			r_pickle_call.iluOutputPickle();
			r_pickle_call << _p_thevalue;
		}
		catch (...) {return ILUCPP_FALSE;}
		return ILUCPP_TRUE;
	}
	
	void* _ilu_corba_string_TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {
			iluCString pc_string = NULL;
		try {
			iluCardinal card_length;
			iluCStringWrapper _the_value_wrapper(pc_string, card_length);
			r_pickle_call >> _the_value_wrapper;
			}
		catch (...) { delete pc_string; return NULL;}
		return pc_string;
	}


        CORBA(TypeCode_ptr) CORBA(_tc_string) = NULL;
	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_CORBA_TYPE_CODE_DELETEVALUE(corba_any, Any);

	void* _ilu_corba_any_TypeCode::iluDeepCopy (void* pv_value) {
		CORBA(Any)* p_thevalue = (CORBA(Any)*) pv_value;
		CORBA(Any)* p_return = new CORBA(Any)(*p_thevalue);
		return p_return;
	}

	ILUCPP_DEFINE_CORBA_TYPE_CODE_VALUETOPICKLE(corba_any, Any);
	ILUCPP_DEFINE_CORBA_TYPE_CODE_PICKLETOVALUE(corba_any, Any);


        CORBA(TypeCode_ptr) CORBA(_tc_any) = NULL;

	
	//////////////////////////////////////////////////////////////////
		
	void _ilu_corba_object_TypeCode::iluDeleteValue (void* pv_value) {
		CORBA(Object)* p_thevalue = (CORBA(Object)*) pv_value;
		if (p_thevalue)
			p_thevalue->_release();
	}

	void* _ilu_corba_object_TypeCode::iluDeepCopy (void* pv_value) {
		CORBA(Object)* p_thevalue = (CORBA(Object)*) pv_value;
		if (p_thevalue)
			return CORBA(Object)::_duplicate(p_thevalue);
		return NULL;
	}

	// objects require wrappers
	CORBA(Boolean) _ilu_corba_object_TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {
		try {
			CORBA(Object)* p_thevalue = (CORBA(Object)*) pv_value;
			iluObjectWrapper _p_thevalue(*p_thevalue, ILUCPP_FALSE);
			r_pickle_call += _p_thevalue;
			r_pickle_call.iluOutputPickle();
			r_pickle_call << _p_thevalue;
		}
		catch (...) {return ILUCPP_FALSE;}
		return ILUCPP_TRUE;
	}
	
	void* _ilu_corba_object_TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {
		iluObjectWrapper _the_value_wrapper(ILUCPP_FALSE, CORBA(Object)::iluGetILUClassRecord());
		try {
			r_pickle_call >> _the_value_wrapper;
		}
		catch (...) { return NULL;}
		return _the_value_wrapper.m_pv_iluobject;
	}

	void* _ilu_corba_object_TypeCode::iluUpcast (void* p_an_object_of_your_type, iluClass casttoclass) {
		CORBA(Object_ptr) p_object = (CORBA(Object_ptr)) p_an_object_of_your_type;
		if ((!casttoclass) || casttoclass == ilu_rootClass)
			return ((void*)((iluObject*)p_object));
		if (casttoclass == CORBA(Object)::iluGetILUClassRecord())
			return ((void*)p_object);
		return NULL;
	}

	CORBA(TypeCode)* _ilu_corba_object_TypeCode::iluMostSpecificTypeCode (void* p_an_object_of_your_type) {
		CORBA(Object_ptr) p_object = (CORBA(Object_ptr)) p_an_object_of_your_type;
		return CORBA(TypeCode)::LookupTypeCode(p_object->iluClassId());
	}


        CORBA(TypeCode_ptr) CORBA(_tc_Object) = NULL;


	//////////////////////////////////////////////////////////////////
		
	void _ilu_ilu_object_TypeCode::iluDeleteValue (void* pv_value) {
		iluObject* p_thevalue = (iluObject*) pv_value;
		if (p_thevalue)
			p_thevalue->iluDecrementReferenceCount();
	}

	void* _ilu_ilu_object_TypeCode::iluDeepCopy (void* pv_value) {
		iluObject* p_thevalue = (iluObject*) pv_value;
		if (p_thevalue)
			p_thevalue->iluIncrementReferenceCount();
		return p_thevalue;
	}

	// objects require wrappers
	CORBA(Boolean) _ilu_ilu_object_TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {
		try {
			iluObject* p_thevalue = (iluObject*) pv_value;
			iluObjectWrapper _p_thevalue(*p_thevalue, ILUCPP_FALSE);
			r_pickle_call += _p_thevalue;
			r_pickle_call.iluOutputPickle();
			r_pickle_call << _p_thevalue;
		}
		catch (...) {return ILUCPP_FALSE;}
		return ILUCPP_TRUE;
	}
	
	void* _ilu_ilu_object_TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {
		iluObjectWrapper _the_value_wrapper(ILUCPP_FALSE, ilu_rootClass);
		try {
			r_pickle_call >> _the_value_wrapper;
		}
		catch (...) { return NULL;}
		return _the_value_wrapper.m_pv_iluobject;
	}

	void* _ilu_ilu_object_TypeCode::iluUpcast (void* p_an_object_of_your_type, iluClass casttoclass) {
		iluObject* p_object = (iluObject*) p_an_object_of_your_type;
		if ((!casttoclass) || casttoclass == ilu_rootClass )
			return ((void*)p_object);
		return NULL;
	}

	CORBA(TypeCode)* _ilu_ilu_object_TypeCode::iluMostSpecificTypeCode (void* p_an_object_of_your_type) {
		iluObject* p_object = (iluObject*) p_an_object_of_your_type;
		return CORBA(TypeCode)::LookupTypeCode(p_object->iluClassId());
	}

        CORBA(TypeCode_ptr) CORBA(_tc_ilu_object) = NULL;

	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_ILU_TYPE_CODE_DELETEVALUE(ilu_character, iluCharacter)
	ILUCPP_DEFINE_ILU_TYPE_CODE_DEEPCOPY(ilu_character, iluCharacter)

	// characters require wrappers
	CORBA(Boolean) _ilu_ilu_character_TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {
		try {
			iluCharacterWrapper _p_thevalue(*(iluCharacter*) pv_value);
			r_pickle_call += _p_thevalue;
			r_pickle_call.iluOutputPickle();
			r_pickle_call << _p_thevalue;
		}
		catch (...) {return ILUCPP_FALSE;}
		return ILUCPP_TRUE;
	}
	
	void* _ilu_ilu_character_TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {
			iluCharacter* p_thevalue = new iluCharacter;
		try {
			iluCharacterWrapper _the_value_wrapper(*p_thevalue);
			r_pickle_call >> _the_value_wrapper;
			}
		catch (...) { delete p_thevalue; return NULL;}
		return p_thevalue;
	}

        CORBA(TypeCode_ptr) CORBA(_tc_ilu_character) = NULL;

	
	//////////////////////////////////////////////////////////////////
	
	ILUCPP_DEFINE_ILU_TYPE_CODE_MEMBERFUNCTIONS(ilu_longinteger, iluLongInteger);	

        CORBA(TypeCode_ptr) CORBA(_tc_ilu_longinteger) = NULL;
	
	//////////////////////////////////////////////////////////////////

	ILUCPP_DEFINE_ILU_TYPE_CODE_MEMBERFUNCTIONS(ilu_longcardinal, iluLongCardinal);

	CORBA(TypeCode_ptr) CORBA(_tc_ilu_longcardinal) = NULL;

	
	//////////////////////////////////////////////////////////////////

	ILUCPP_DEFINE_ILU_TYPE_CODE_MEMBERFUNCTIONS(ilu_longreal, iluLongReal);

	CORBA(TypeCode_ptr) CORBA(_tc_ilu_longreal) = NULL;

	
	



#endif /* ADD_VARIANT_SUPPORT */

//////////////////////////////////////////////////////////////////
// End of File
//////////////////////////////////////////////////////////////////




