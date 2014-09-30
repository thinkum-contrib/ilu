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
/* $Id: corba.hpp,v 1.55 1999/09/14 17:25:28 janssen Exp $ */

// Note: This file started off life as a direct copy of Appendix E of 
// the CORBA 2.0 specification

//////////////////////////////////////////////////////////////////
// Preprocessing checks and inclusions

// prevent multiple inclusions 
#ifndef __corba_H_
#define __corba_H_

// error out if we're not being processed by a C++ compiler
#ifndef __cplusplus
#error "corba.hpp is a C++ header file"
#endif

#include <ilu.hpp>
#include <corba-templates.hpp>

#ifndef CORBA_
#define CORBA_(name) NAME_INSIDE_SCOPE(CORBA, name)
#endif

#ifndef CORBA
#define CORBA(name) NAME_OUTSIDE_SCOPE(CORBA, name)
#endif




//////////////////////////////////////////////////////////////////

#ifdef _MSC_VER
#ifndef CPLUSPLUSMAPPING_NESTEDCLASSES
// for MSVC, just export it as usual unless we're nested classes
#define EXPORTED_INSIDE_SCOPE ILU_RUNTIME_PUBLIC
#else
#define EXPORTED_INSIDE_SCOPE static
#endif
#else
// must be on non Windows platform so we don't need explicit dll exports anyway
#define EXPORTED_INSIDE_SCOPE NS_EXTERN
#endif


//////////////////////////////////////////////////////////////////

BEGIN_NAMESPACE(CORBA)
	
	
	//////////////////////////////////////////////////////////////////
	// typedefs
	
	typedef ILUCPP_BOOL				CORBA_(Boolean);
	typedef ::iluShortCharacter		CORBA_(Char);
	typedef ::iluByte				CORBA_(Octet);
	typedef ::iluShortInteger		CORBA_(Short);
	typedef ::iluShortCardinal		CORBA_(UShort);
	typedef ::iluInteger			CORBA_(Long);
	typedef ::iluCardinal			CORBA_(ULong);
	typedef ::iluShortReal			CORBA_(Float);
	typedef ::iluReal				CORBA_(Double);	
	typedef ::iluCString			CORBA_(string);	


	ILU_RUNTIME_PUBLIC_CLASS CORBA_(Object);
	typedef CORBA_(Object*)  CORBA_(Object_ptr);
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(ORB);
	typedef CORBA_(ORB*)  CORBA_(ORB_ptr);

#ifdef ADD_VARIANT_SUPPORT

	ILU_RUNTIME_PUBLIC_CLASS CORBA_(TypeCode);
	typedef CORBA_(TypeCode*)  CORBA_(TypeCode_ptr);
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(Any);

#endif /* ADD_VARIANT_SUPPORT */
	
	
	//////////////////////////////////////////////////////////////////
	// Exception Class
	
	// XXX eventually need to support narrow operation if we provide DII
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(Exception) {
public:
	CORBA_(Exception)(const CORBA_(Exception) &) {}
	~CORBA_(Exception)() {}
	CORBA_(Exception)& operator=(const CORBA_(Exception) &) {return *this;}
	
	virtual void _raise() = 0;

protected:
	CORBA_(Exception)() {}
	};
	
	
	//////////////////////////////////////////////////////////////////
	// SystemException Classes
	
	enum CORBA_(CompletionStatus) { CORBA_(COMPLETED_YES),
		CORBA_(COMPLETED_NO), 
		CORBA_(COMPLETED_MAYBE) };
	
	// used to distinguish among different kinds of system exceptions
	typedef ::ilu_ErrorType CORBA_(SystemExceptionKind);
	
	// defines CORBA namespace equivalents to the ILU error types
#define CORBA_ERRTYP(id) ilu_ET_##id
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(SystemException) : public CORBA_(Exception) {
		
public:
	
	// constructors
	CORBA_(SystemException)() : m_ul_minor_code(0), 
		m_completion_status(CORBA_(COMPLETED_NO)),
		m_exception_kind(CORBA_ERRTYP(unknown)) {}
	
	CORBA_(SystemException)(const CORBA_(SystemException)& r_sysexception) {
		m_ul_minor_code = r_sysexception.m_ul_minor_code;
		m_completion_status = r_sysexception.m_completion_status;
		m_exception_kind = r_sysexception.m_exception_kind;
		m_exception_name = r_sysexception.m_exception_name;
	}
	
	CORBA_(SystemException)(CORBA_(ULong) minor, 
		CORBA_(CompletionStatus) status, 
		CORBA_(SystemExceptionKind) exception_kind = CORBA_ERRTYP(unknown),
		CORBA_(string) exception_name = CONST_CAST(iluCString,"UNKNOWN")) : 
	m_ul_minor_code(minor), m_completion_status(status), m_exception_kind(exception_kind),
			m_exception_name(exception_name) {}
	
	~CORBA_(SystemException)() {};
	
	CORBA_(SystemException)& operator=(const CORBA_(SystemException)& r_sysexception) {
		m_ul_minor_code = r_sysexception.m_ul_minor_code;
		m_completion_status = r_sysexception.m_completion_status;
		m_exception_kind = r_sysexception.m_exception_kind;
		m_exception_name = r_sysexception.m_exception_name;
		return *this;
	}

/* xxx would like to issuing a warning somehow here about undeffing minor */
#ifdef minor
#undef minor
#endif
	
	// accessors
	CORBA_(ULong) minor() const {return m_ul_minor_code;}
	void minor(CORBA_(ULong) ul_minor_code) {m_ul_minor_code = ul_minor_code;}
	
	CORBA_(CompletionStatus) completed() const {return m_completion_status;}
	void completed(CORBA_(CompletionStatus) status) {m_completion_status = status;}

	CORBA_(SystemExceptionKind) exception_kind() const {return m_exception_kind;}

	const CORBA_(string) exception_name() const {return m_exception_name;}
	
	const char * minor_desc() const { return iluCppMinorErrorDescription(m_exception_kind, m_ul_minor_code); }

private:
	
	// member variables
	CORBA_(ULong) m_ul_minor_code;
	CORBA_(CompletionStatus) m_completion_status;
	// ilu addition to allow system exceptions to be distinguished when you just have
	// a pointer to a system exception
	CORBA_(SystemExceptionKind) m_exception_kind;
	// a pointer to a string constant giving the name of the exception
	CORBA_(string) m_exception_name;
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(UNKNOWN)			: public CORBA_(SystemException) {
	public:
		CORBA_(UNKNOWN) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(unknown), "UNKNOWN") {}
		void _raise() { throw *this; }
	};
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(BAD_PARAM)			: public CORBA_(SystemException) {
	public:
		CORBA_(BAD_PARAM) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(bad_param), "BAD_PARAM") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(NO_MEMORY)			: public CORBA_(SystemException) {
	public:
		CORBA_(NO_MEMORY) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(no_memory), "NO_MEMORY") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(IMP_LIMIT)			: public CORBA_(SystemException) {
	public:
		CORBA_(IMP_LIMIT) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(imp_limit), "IMP_LIMIT") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(COMM_FAILURE)		: public CORBA_(SystemException) {
	public:
		CORBA_(COMM_FAILURE) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(comm_failure), "COMM_FAILURE") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INV_OBJREF)		: public CORBA_(SystemException) {
	public:
		CORBA_(INV_OBJREF) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(inv_objref), "INV_OBJREF") {}
		void _raise() { throw *this; }
	};
	

	ILU_RUNTIME_PUBLIC_CLASS CORBA_(NO_PERMISSION)		: public CORBA_(SystemException) {
	public:
		CORBA_(NO_PERMISSION) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(no_permission), "NO_PERMISSION") {}
		void _raise() { throw *this; }
	};
	

	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INTERNAL)			: public CORBA_(SystemException) {
	public:
		CORBA_(INTERNAL) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(internal), "INTERNAL") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(MARSHAL)			: public CORBA_(SystemException) {
	public:
		CORBA_(MARSHAL) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(marshal), "MARSHAL") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INITIALIZE)		: public CORBA_(SystemException) {
	public:
		CORBA_(INITIALIZE) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(initialize), "INITIALIZE") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(NO_IMPLEMENT)		: public CORBA_(SystemException) {
	public:
		CORBA_(NO_IMPLEMENT) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(no_implement), "NO_IMPLEMENT") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(BAD_TYPECODE)		: public CORBA_(SystemException) {
	public:
		CORBA_(BAD_TYPECODE) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(bad_typecode), "BAD_TYPECODE") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(BAD_OPERATION)		: public CORBA_(SystemException) {
	public:
		CORBA_(BAD_OPERATION) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(bad_operation), "BAD_OPERATION") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(NO_RESOURCES)		: public CORBA_(SystemException) {
	public:
		CORBA_(NO_RESOURCES) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(no_resources), "NO_RESOURCES") {}
		void _raise() { throw *this; }
	};
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(NO_RESPONSE)		: public CORBA_(SystemException) {
	public:
		CORBA_(NO_RESPONSE) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(no_response), "NO_RESPONSE") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(PERSIST_STORE)		: public CORBA_(SystemException)  {
	public:
		CORBA_(PERSIST_STORE) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(persist_store), "PERSIST_STORE") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(BAD_INV_ORDER)		: public CORBA_(SystemException) {
	public:
		CORBA_(BAD_INV_ORDER) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(bad_inv_order), "BAD_INV_ORDER") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(TRANSIENT)			: public CORBA_(SystemException) {
	public:
		CORBA_(TRANSIENT) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(transient), "TRANSIENT") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(FREE_MEM)			: public CORBA_(SystemException) {
	public:
		CORBA_(FREE_MEM) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(free_mem), "FREE_MEM") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INV_IDENT)			: public CORBA_(SystemException) {
	public:
		CORBA_(INV_IDENT) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(inv_ident), "INV_IDENT") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INV_FLAG)			: public CORBA_(SystemException) {
	public:
		CORBA_(INV_FLAG) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(inv_flag), "INV_FLAG") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(INTF_REPOS)		: public CORBA_(SystemException) {
	public:
		CORBA_(INTF_REPOS) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(intf_repos), "INTF_REPOS") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(BAD_CONTEXT)		: public CORBA_(SystemException) {
	public:
		CORBA_(BAD_CONTEXT)	 (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(bad_context), "BAD_CONTEXT") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(OBJ_ADAPTER)		: public CORBA_(SystemException) {
	public:
		CORBA_(OBJ_ADAPTER) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(obj_adapter), "OBJ_ADAPTER") {}
		void _raise() { throw *this; }
	};
	
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(DATA_CONVERSION)	: public CORBA_(SystemException) {
	public:
		CORBA_(DATA_CONVERSION) (CORBA_(ULong) minor = 0, 
			CORBA_(CompletionStatus) status = CORBA_(COMPLETED_NO))
			: CORBA_(SystemException)(minor, status, CORBA_ERRTYP(data_conversion), "DATA_CONVERSION") {}
		void _raise() { throw *this; }
	};
	
	
	
	//////////////////////////////////////////////////////////////////
	// UserException Class
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(UserException) : public  CORBA_(Exception) {
	public:
		CORBA_(UserException)() {}
		CORBA_(UserException)(const CORBA_(UserException) &) {}
		~CORBA_(UserException)() {}
		CORBA_(UserException)& operator=(const CORBA_(UserException) &) {return *this;}
	};
	
	
	//////////////////////////////////////////////////////////////////
	// UnknownUserException Class
	/*	
	class UnknownUserException : public UserException {
	public:
	Any &exception();
	};
	*/
	
	//////////////////////////////////////////////////////////////////
	// release and is_nil
	
	EXPORTED_INSIDE_SCOPE  void CORBA_(release) (CORBA_(Object_ptr) an_object_ptr);

	EXPORTED_INSIDE_SCOPE  CORBA_(Boolean) CORBA_(is_nil)(CORBA_(Object_ptr) an_obj_ptr);

#ifdef ADD_VARIANT_SUPPORT
#ifdef ANY_FINALLY_IMPLEMENTED	

	EXPORTED_INSIDE_SCOPE  void CORBA_(release) (CORBA_(TypeCode_ptr) a_typecode_ptr) {};

	EXPORTED_INSIDE_SCOPE  CORBA_(Boolean) CORBA_(is_nil)(CORBA_(TypeCode_ptr) a_typecode_ptr) {
		return (a_typecode_ptr != CORBA_(TypeCode)::_nil());
	}
#endif
#endif
	

	//////////////////////////////////////////////////////////////////
	// string operations

	EXPORTED_INSIDE_SCOPE char* CORBA_(string_alloc)(CORBA_(ULong) ul_length_not_including_null);

	EXPORTED_INSIDE_SCOPE char* CORBA_(string_dup)(const char* pc_string_to_duplicate);

	EXPORTED_INSIDE_SCOPE void CORBA_(string_free)(char* pc_string_to_free);



	//////////////////////////////////////////////////////////////////
	// Object Class
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(Object) : public virtual ::iluObject {
	public:
		// XXX for some reason, msvc complains about the Object_ptrs
		//	static Object_ptr _duplicate(Object_ptr obj);
		//	static Object_ptr _nil();
		static CORBA_(Object*) _duplicate(CORBA_(Object*) obj);
		static CORBA_(Object*) _nil();
		CORBA_(Boolean) _non_existent();
		CORBA_(Boolean) _is_equivalent(CORBA_(Object*) obj);
		CORBA_(Boolean) _is_a(char* logical_type_id);
		CORBA_(ULong) _hash(CORBA_(ULong) maximum);
		virtual void _release();

        static CORBA_(Object*) _narrow(CORBA_(Object*) an_object_ptr) {
          return ( (CORBA_(Object*)) (an_object_ptr ? (an_object_ptr->iluDowncast(m_ILUClassRecord))
			  : NULL) );
        }

		// initialize to use of this class of objects - called by (Interface)::initialize
        static void iluInitialize();

        // For ILU C++ runtime - stub use only - returns the m_ILUClassRecord member variable
        static ilu_Class iluGetILUClassRecord() {
          return m_ILUClassRecord;
        }

        // for use in narrowing
        virtual void* iluDowncast(iluClass class_to_cast_down_to);

      protected:

		// Holds the kernel class for this kind of object
        static ilu_Class m_ILUClassRecord;

	};
	
#if (defined(_MSC_VER) && defined(CPLUSPLUSMAPPING_NAMESPACES))	
	EXPORTED_INSIDE_SCOPE extern CORBA_(Object)* const CORBA_(OBJECT_NIL);
#else
	EXPORTED_INSIDE_SCOPE CORBA_(Object)* const CORBA_(OBJECT_NIL);
#endif

	//////////////////////////////////////////////////////////////////
	// ORB Related
	
	// ORB initialization function
	EXPORTED_INSIDE_SCOPE CORBA_(ORB_ptr) CORBA_(ORB_init)(int& argc, char** argv, const char* pc_orb_identifier = "ilu");

	// ORB class
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(ORB)  {
	
#ifdef _MSC_VER
EXPORTED_INSIDE_SCOPE
#endif
// CHANGE LOC
#ifdef __SUNPRO_CC
		friend CORBA_(ORB_ptr) CORBA(ORB_init)(int& argc, char** argv, const char* pc_orb_identifier);
#else
		friend CORBA_(ORB_ptr) CORBA_(ORB_init)(int& argc, char** argv, const char* pc_orb_identifier);
#endif

	public:
		
		// object <-> string methods
		CORBA_(Object*) string_to_object(char* pc_sbh);
		
		// Note, object_to_string prefers the IOR representation if IIOP is configured into ILU
		char* object_to_string(CORBA_(Object*) p_object);
		
		// Returns name service reference if possible, else throws InvalidName
		CORBA_(Object)* resolve_initial_references(const char* pc_identifier);
		
		// TODO:  should implement list_initial_services

		class InvalidName : public CORBA_(UserException) { public: void _raise() { throw *this; } };
		
		private:
			
			// points to the one and only instance of CORBA_(ORB)
			static CORBA_(ORB_ptr) sm_p_single_orb_instance;
			
			// holds any ORBInitRef values passed in on the command line
			// and also result of ILU_COS_NAMING_IOR env var, if set
			static iluHashTable sm_initial_references_hash_table;

			// holds DefaultInitRef string, if any
			static char * sm_pc_default_init_ref;

	};


	//////////////////////////////////////////////////////////////////
	// String_var Class
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(String_var) {
	public:
		CORBA_(String_var)();
		CORBA_(String_var)(char* pc_string);
		CORBA_(String_var)(const char* pc_string);
		CORBA_(String_var)(const CORBA_(String_var)& r_string_to_copy);
		~CORBA_(String_var)();
		
		CORBA_(String_var) &operator=(char* pc_string);
		CORBA_(String_var) &operator=(const char* pc_string);
		CORBA_(String_var) &operator=(const CORBA_(String_var)& r_string_to_copy);
		operator char*();
		operator const char*() const;
		char &operator[](CORBA_(ULong) ul_index);
		char operator[](CORBA_(ULong) ul_index) const;

		// following is for use by ILU stubs 
		char*& iluStringVarReference() const;

		// accessors on the m_b_release member
		void iluSetRelease(CORBA_(Boolean) b_release_on_destruct) const;
		CORBA_(Boolean) iluGetRelease() const;

		// returns true if the _var contains a null pointer
		CORBA_(Boolean) iluIsNull() const {
			return (m_pc_string ? ILUCPP_FALSE : ILUCPP_TRUE); 
		}

	private:
		char* m_pc_string;
		// whether or not a delete should be done when the var destructs
		CORBA_(Boolean) m_b_release;
	};

#ifdef ADD_VARIANT_SUPPORT

	//////////////////////////////////////////////////////////////////
	// Any Class
	
	ILU_RUNTIME_PUBLIC_CLASS CORBA_(Any) {
	public:

		// constructors and destructor
		CORBA_(Any)();
		CORBA_(Any)(const CORBA_(Any)& r_any);
		CORBA_(Any)(CORBA_(TypeCode_ptr) p_typecode, void* pv_value, 
			CORBA_(Boolean) b_release = ILUCPP_FALSE);
		~CORBA_(Any)();
		
		// assignment
		CORBA_(Any)& operator=(const CORBA_(Any)&);
		
		// insertion
		void operator<<=(CORBA_(Short) a_short);
		void operator<<=(CORBA_(UShort) a_ushort);
		void operator<<=(CORBA_(Long) a_long);
		void operator<<=(CORBA_(ULong) a_ulong);
		void operator<<=(CORBA_(Float) a_float);
		void operator<<=(CORBA_(Double) a_double);
		void operator<<=(const CORBA_(Any)& r_any);
		void operator<<=(const char* pc_char);
		void operator<<=(CORBA_(Object_ptr) p_object);
		void operator<<=(iluObject* p_object);
		// ilu specific insertion
		void operator<<=(iluLongInteger an_ilu_longinteger);
		void operator<<=(iluLongReal an_ilu_longreal);
		void operator<<=(iluLongCardinal an_ilu_long_cardinal);
		
		// extraction
		CORBA_(Boolean) operator>>=(CORBA_(Short)& r_a_short) const;
		CORBA_(Boolean) operator>>=(CORBA_(UShort)& r_a_ushort) const;
		CORBA_(Boolean) operator>>=(CORBA_(Long)& r_a_long) const;
		CORBA_(Boolean) operator>>=(CORBA_(ULong)& r_a_ulong) const;
		CORBA_(Boolean) operator>>=(CORBA_(Float)& r_a_float) const;
		CORBA_(Boolean) operator>>=(CORBA_(Double)& r_a_double) const;	
		CORBA_(Boolean) operator>>=(CORBA_(Any)& r_any) const;
		CORBA_(Boolean) operator>>=(char*& r_pc_char) const;
		CORBA_(Boolean) operator>>=(CORBA_(Object_ptr)& r_p_object) const;
		CORBA_(Boolean) operator>>=(iluObject*& r_p_object) const;
		// ilu specific extraction
		CORBA_(Boolean) operator>>=(iluLongInteger& r_ilu_longinteger) const;
		CORBA_(Boolean) operator>>=(iluLongReal& r_ilu_longreal) const;
		CORBA_(Boolean) operator>>=(iluLongCardinal& r_ilu_long_cardinal) const;

		// special types needed for boolean, octet, char,
		// and bounded string insertion
		struct from_boolean {
			from_boolean (CORBA_(Boolean) b_a_boolean) : val(b_a_boolean) {}
			CORBA_(Boolean) val;
		};
		struct from_octet {
			from_octet(CORBA_(Octet) an_octet) : val(an_octet) {}
			CORBA_(Octet) val;
		};
		struct from_char {
			from_char(CORBA_(Char) a_char) : val(a_char) {}
			CORBA_(Char) val;
		};
		struct from_string {
			from_string(char* pc_string, CORBA_(ULong) l_bound) : val(pc_string), bound(l_bound) {}
			char* val;
			 CORBA_(ULong) bound;
		};
		
		void operator<<=(CORBA_(Any)::from_boolean a_from_boolean);
		void operator<<=(CORBA_(Any)::from_char a_from_char);
		void operator<<=(CORBA_(Any)::from_octet a_from_octet);
		void operator<<=(CORBA_(Any)::from_string a_from_string);
		
		// special types needed for boolean, octet, char extraction
		struct to_boolean {
			to_boolean (CORBA_(Boolean) & r_a_boolean) : ref(r_a_boolean) {}
			CORBA_(Boolean) &ref;
		};
		struct to_char {
			to_char(CORBA_(Char)& r_char) : ref(r_char) {}
			CORBA_(Char)& ref;
		};
		struct to_octet {
			to_octet(CORBA_(Octet)& r_octet) : ref(r_octet) {}
			CORBA_(Octet)& ref;
		};
		struct to_object {
			to_object(CORBA_(Object_ptr)& r_object) : ref(r_object) {}
			CORBA_(Object_ptr)& ref;
		};
		struct to_string {
			to_string (char *& r_pc_string, CORBA_(ULong) l_bound) : val(r_pc_string), bound(l_bound) {}
			char*& val;
			CORBA_(ULong) bound;
		};
		
		CORBA_(Boolean) operator>>=(CORBA_(Any)::to_boolean a_to_boolean) const;
		CORBA_(Boolean) operator>>=(CORBA_(Any)::to_char a_to_char) const;
		CORBA_(Boolean) operator>>=(CORBA_(Any)::to_octet a_to_octet) const;
		CORBA_(Boolean) operator>>=(CORBA_(Any)::to_object a_to_object) const;
		CORBA_(Boolean) operator>>=(CORBA_(Any)::to_string a_to_string) const;
		
		void replace(CORBA_(TypeCode_ptr) p_typecode, void* pv_value, 
			CORBA_(Boolean) b_release = ILUCPP_FALSE);
		
		// return the type of the contents
		CORBA_(TypeCode_ptr) type();

		// return the LSR value
		const void* value();

		// return a pointer to the internal pickle
		iluPickle* iluPickleValue();

                // disambiguate _var references 
                CORBA_(Any)& self() { return *this; };
		
		// next three intended for ilu use only
		// deletes any storage associated with the any - and sets up as per default constructor
		void iluDeleteContent();
		// convert back and forth between the internal LSR and pickle representations
		// return true if successful. For iluGetFromPickle p_should_be_typecode can be 
		// a CORBA::TypeCode_ptr, and if the pickle really isn't of that type, CORBA_(
		// will be returned without actually unpickling.
		CORBA_(Boolean) iluPutToPickle();
		CORBA_(Boolean) iluGetFromPickle(CORBA_(TypeCode_ptr) p_should_be_typecode = NULL);

		private:
			// these are hidden and should not be implemented
			// so as to catch erroneous attempts to insert or extract
			// multiple IDL types mapped to unsigned char
			void operator<<=(unsigned char);
			CORBA_(Boolean) operator>>=(unsigned char&) const;


		// pointer to the typecode object for this any
		CORBA_(TypeCode_ptr) m_p_typecode;		
		

		// Case 1: If m_pv_value is null and m_pickle is non empty, then we must have got
		// a pickle from the wire, and nobody has extracted its value out of the any.
		// Case 2: If m_pv_value is non-null and m_pickle is empty, then either somebody 
		// has previously extracted the value out of the any, OR the any has had a value 
		// inserted that has not (yet) had a need to be transmitted across the wire.
		// Case 3: If both are null, then we must be newly constructed.
		// Case 4: Both should never be non-null
		
		void* m_pv_value;			// points to LSR value
		iluPickle m_pickle;			// contains ILU pickle

		CORBA_(Boolean) m_b_release;		// ownership control

	};

		//////////////////////////////////////////////////////////////////
		// macros to define common insertion and extraction bodies for the 
		// basic types.  Note that for the primitive types (e.g. integer) 
		// the m_pv_value pointer in an Any actually points to some allocated
		// heap space that holds the value.

#define ILUCPP_DEFINE_CORBA_ANY_SIMPLE_INSERTION_BODY_CONTENT(corba_typecode_constant, corba_type, entity_to_insert)\
		iluDeleteContent();\
		m_p_typecode = CORBA(corba_typecode_constant);\
		CORBA(corba_type)* p_content = new CORBA(corba_type);\
		*p_content = entity_to_insert;\
		m_pv_value = p_content;


#define ILUCPP_DEFINE_CORBA_ANY_SIMPLE_EXTRACTION_BODY_CONTENT(corba_typecode_constant, corba_type, entity_to_extract)\
		CORBA(Any)* p_nc_myself = CONST_CAST(CORBA(Any)*, this); \
		if (p_nc_myself->iluGetFromPickle(CORBA(corba_typecode_constant))) {  \
			entity_to_extract = *((CORBA(corba_type)*) m_pv_value); \
			return ILUCPP_TRUE;\
		}\
		return ILUCPP_FALSE; 

	
	//////////////////////////////////////////////////////////////////
	// TypeCode Class

    // Possible type codes
	enum CORBA_(TCKind) {
		CORBA_(tk_null),
			CORBA_(tk_void), 
			CORBA_(tk_short), 
			CORBA_(tk_long), 
			CORBA_(tk_ushort), 
			CORBA_(tk_ulong),
			CORBA_(tk_float), 
			CORBA_(tk_double), 
			CORBA_(tk_boolean), 
			CORBA_(tk_char), 
			CORBA_(tk_octet), 
			CORBA_(tk_any),
			CORBA_(tk_TypeCode), 
			CORBA_(tk_Principal), 
			CORBA_(tk_objref), 
			CORBA_(tk_struct), 
			CORBA_(tk_union),
			CORBA_(tk_enum), 
			CORBA_(tk_string), 
			CORBA_(tk_sequence), 
			CORBA_(tk_array), 
			CORBA_(tk_alias), 
			CORBA_(tk_except)
	};

#define ILUCPP_BOGUS_TYPECODE_FOR_CORBA_NULL ((iluTypeKind)65535)


	//////////////////////////////////////////////////////////////////

	ILU_RUNTIME_PUBLIC_CLASS CORBA_(TypeCode) {

	public:
	
	// construct a typecode	based on an iluTypeKind
	CORBA_(TypeCode) (iluTypeKind	enum_type_kind, const iluCString str_type_id);
	
	// note - typecodes normally only destruct on program termination
	virtual ~CORBA_(TypeCode) ();

	// determine whether two typecodes are equal
	CORBA_(Boolean) equal(CORBA_(TypeCode_ptr) p_some_typecode) const {
		return (((this == p_some_typecode ) || 
			(p_some_typecode && (strcmp(m_str_type_id, p_some_typecode->m_str_type_id) == 0))) ? ILUCPP_TRUE : ILUCPP_FALSE);
	}

	// returns the iluClass corresponding to this type
	iluClass iluGetIluClass () {return ilu_FindClassFromID(m_str_type_id);}

	// returns true if p_possible_base_typecode is a base type of this typecode 
	CORBA_(Boolean) iluIsBaseType(CORBA_(TypeCode_ptr) p_possible_base_typecode);

	// returns true iff the pickle matches this typecode
	CORBA_(Boolean) iluMatchPickle(iluPickle& r_pickle);

	// conversions between ilu and corba typekinds, return false if no valid conversion
	static CORBA_(Boolean) iluIluTypeKindToCorbaTypeKind(iluTypeKind ilukind, CORBA_(TCKind)& r_corbakind);
	static CORBA_(Boolean) iluCorbaTypeKindToIluTypeKind(CORBA_(TCKind) corbakind, iluTypeKind& r_ilukind);


	// gets the corba type kind, returns tk_null if there's no corba analog
	CORBA_(TCKind) kind() const {
		CORBA_(TCKind) the_tckind;
		return ((iluIluTypeKindToCorbaTypeKind(m_enum_type_kind, the_tckind)) ? the_tckind : CORBA_(tk_null));
	}

	// returns the ilu type kind
	iluTypeKind iluKind() const { return m_enum_type_kind; }

	// returns the type id string
	const char* id() const { return m_str_type_id; }

	// returns the name of the type
	const char* iluTypeName();

	// _duplicate and _nil do nothing
	static CORBA_(TypeCode_ptr) _duplicate (CORBA_(TypeCode_ptr) a_typecode_ptr) {
		return a_typecode_ptr;}
	static CORBA_(TypeCode_ptr) _nil () {return NULL;};


	// return a pointer to the CORBA::TypeCode having the given ilu type id, null if none found
	static CORBA_(TypeCode_ptr) LookupTypeCode(const char* pc_ilu_type_id);


	// since the Corba C++ spec lets Any's retain ownership of values, the Any
	// must be able to properly delete its value.  Now the only information the 
	// any has on the value is really the typecode, so we have an operation 
	// that the typecode provides that can cast a void pointer into the 
	// appropriate type and then call delete.  Also, there are situations (like
	// assignment of anys) that call for a need to deep-copy.
	// It seems natural then to also let the typecodes
	// contain the oeprations for putting values in and out of ilu pickles.
	virtual void iluDeleteValue (void* pv_value) = 0;
	virtual void* iluDeepCopy (void* pv_value) = 0;
	virtual CORBA_(Boolean) iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) = 0;
	virtual void* iluPickleToValue (iluPickleCall& r_pickle_call) = 0;

	// Used situations where someone want's to extract a base object type from an any
	// containing a derived object type.  Note that only typecodes for object types
	// should and must override this member function
	virtual void* iluUpcast (void* p_an_object_of_your_type, iluClass casttoclass);

	// Used situations where someone want's to insert a base object type into an any
	// where that object is really a derived object type.  Note that only typecodes for object types
	// should and must override this member function
	virtual CORBA_(TypeCode)* iluMostSpecificTypeCode (void* p_an_object_of_your_type);


	/* following are currently unimplemented for ILU 
	// xxx the Corba spec is unclear on whether or not these exceptions
	// should be user exceptions or system exceptions, and if so, what kind
	class Bounds : public CORBA_(BAD_OPERATION) {};
	class BadKind : public CORBA_(BAD_TYPECODE) {};
	const char* name() const;
	ULong member_count() const;
	const char* member_name(ULong index) const;
	TypeCode_ptr member_type(ULong index) const;
	Any *member_label(ULong index) const;
	TypeCode_ptr discriminator_type() const;
	Long default_index() const;
	ULong length() const;
	TypeCode_ptr content_type() const;
	Long param_count() const;
	Any *parameter(Long) const;
	*/

	protected:

	// used for the null type code (really put in to satisfy SunPro sc3.0.1/solaris-1/CC )
	static iluCString sm_pc_emptystring;


	private:

	// members 

	// used to find the CORBA::TypeCode Objects from the ilu type ids
	static iluHashTable sm_typecode_hash_table; 

	iluTypeKind			m_enum_type_kind;
	const iluCString	m_str_type_id;
	};	


	//////////////////////////////////////////////////////////////////
	// typecode constants
// XXX these really should be constants, but you get to a point where trying to 
// get all the various compilers to properly run the initializers for statically
// constructed objects when in a dynamic library causes you to take the practical route

    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_null);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_boolean);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_octet);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_short);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_long);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ushort);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ulong);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_char);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_float);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_double);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_string);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_any);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_Object);

    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ilu_object);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ilu_character);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ilu_longinteger);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ilu_longcardinal);
    NS_EXTERN CORBA_(TypeCode_ptr) CORBA_(_tc_ilu_longreal);

    //////////////////////////////////////////////////////////////////
    // Any_var Class

    typedef iluTemplatableT_var<CORBA_(Any)> CORBA_(Any_var);


#endif /* ADD_VARIANT_SUPPORT */

END_NAMESPACE; // end corba namespace



#ifdef ADD_VARIANT_SUPPORT

//////////////////////////////////////////////////////////////////
// classes for primitive typecodes 
//////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////
// macros to define some common member functions in CORBA typecode classes

// delete a value after casting the void pointer to the appropriate CORBA type
#define ILUCPP_DEFINE_CORBA_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename)\
	void _ilu_ ## sometypecodeclass ## _TypeCode::iluDeleteValue (void* pv_value) {\
		delete (CORBA(sometypename)*) pv_value;\
	}

// deep copy a value after casting the void pointer to the appropriate CORBA type
#define ILUCPP_DEFINE_CORBA_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluDeepCopy (void* pv_value) {\
		CORBA(sometypename)* p_return = new CORBA(sometypename);\
		CORBA(sometypename)* p_thevalue = (CORBA(sometypename)*) pv_value;\
		*p_return = *p_thevalue;\
		return p_return;\
	}

// put a value to a pickle after casting the void pointer to the appropriate CORBA type
#define ILUCPP_DEFINE_CORBA_TYPE_CODE_VALUETOPICKLE(sometypecodeclass, sometypename)\
	CORBA(Boolean) _ilu_ ## sometypecodeclass ## _TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {\
		try {\
			CORBA(sometypename)* p_thevalue = (CORBA(sometypename)*) pv_value;\
			r_pickle_call += *p_thevalue;\
			r_pickle_call.iluOutputPickle();\
			r_pickle_call << *p_thevalue;\
		}\
		catch (...) {return ILUCPP_FALSE;}\
		return ILUCPP_TRUE;\
	}


// put a pickle to a value after creating a void pointer to the appropriate CORBA type	
#define ILUCPP_DEFINE_CORBA_TYPE_CODE_PICKLETOVALUE(sometypecodeclass, sometypename)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {\
		CORBA(sometypename)* p_thevalue = new CORBA(sometypename);\
		try {\
			r_pickle_call >> *p_thevalue;\
			}\
			catch (...) { delete p_thevalue; return NULL;}\
		return p_thevalue;\
	}
	
#define ILUCPP_DEFINE_CORBA_TYPE_CODE_MEMBERFUNCTIONS(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_CORBA_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_CORBA_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_CORBA_TYPE_CODE_VALUETOPICKLE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_CORBA_TYPE_CODE_PICKLETOVALUE(sometypecodeclass, sometypename)


//////////////////////////////////////////////////////////////////
// macros to define some common member functions in non CORBA typecode classes

// delete a value after casting the void pointer to the appropriate type
#define ILUCPP_DEFINE_ILU_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename)\
	void _ilu_ ## sometypecodeclass ## _TypeCode::iluDeleteValue (void* pv_value) {\
		delete (sometypename*) pv_value;\
	}

#define ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_DELETEVALUE(sometypecodeclass, sometypename)\
	void _ilu_ ## sometypecodeclass ## _TypeCode::iluDeleteValue (void* pv_value) {\
		delete [] (sometypename*) pv_value;\
	}

// deep copy a value after casting the void pointer to the appropriate type
#define ILUCPP_DEFINE_ILU_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluDeepCopy (void* pv_value) {\
		sometypename* p_return = new sometypename;\
		sometypename* p_thevalue = (sometypename*) pv_value;\
		*p_return = *p_thevalue;\
		return p_return;\
	}

#define ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_DEEPCOPY(sometypecodeclass, sometypename, dupefunctionname)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluDeepCopy (void* pv_value) {\
	  return dupefunctionname((const sometypename*) pv_value);\
	}

// put a value to a pickle after casting the void pointer to the appropriate type
#define ILUCPP_DEFINE_ILU_TYPE_CODE_VALUETOPICKLE(sometypecodeclass, sometypename)\
	CORBA(Boolean) _ilu_ ## sometypecodeclass ## _TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {\
		try {\
			sometypename* p_thevalue = (sometypename*) pv_value;\
			r_pickle_call += *p_thevalue;\
			r_pickle_call.iluOutputPickle();\
			r_pickle_call << *p_thevalue;\
		}\
		catch (...) {return ILUCPP_FALSE;}\
		return ILUCPP_TRUE;\
	}
	
#define ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_VALUETOPICKLE(sometypecodeclass, sometypename, sizefunctionname, outputfunctionname)\
	CORBA(Boolean) _ilu_ ## sometypecodeclass ## _TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {\
		try {\
			sometypename* p_thevalue = (sometypename*) pv_value;\
		    sizefunctionname(r_pickle_call, p_thevalue);\
			r_pickle_call.iluOutputPickle();\
		    outputfunctionname(r_pickle_call, p_thevalue);\
		}\
		catch (...) {return ILUCPP_FALSE;}\
		return ILUCPP_TRUE;\
	}
	
// put a pickle to a value after creating a void pointer to the appropriate type	
#define ILUCPP_DEFINE_ILU_TYPE_CODE_PICKLETOVALUE(sometypecodeclass, sometypename)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {\
		sometypename* p_thevalue = new sometypename;\
		try {\
			r_pickle_call >> *p_thevalue;\
			}\
		catch (...) { delete p_thevalue; return NULL;}\
		return p_thevalue;\
	}

#define ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_PICKLETOVALUE(sometypecodeclass, sometypename, allocfunctionname, inputfunctionname)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {\
		sometypename* p_thevalue = allocfunctionname();\
		try {\
			inputfunctionname(r_pickle_call, p_thevalue);\
			}\
		catch (...) { delete [] p_thevalue; return NULL;}\
		return p_thevalue;\
	}

// enums require wrappers
#define ILUCPP_DEFINE_ILU_TYPE_CODE_ENUM_VALUETOPICKLE(sometypecodeclass, sometypename)\
	CORBA(Boolean) _ilu_ ## sometypecodeclass ## _TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {\
		try {\
			iluEnumWrapper _p_thevalue((iluDummyEnum &) *((sometypename*)pv_value));\
			r_pickle_call += _p_thevalue;\
			r_pickle_call.iluOutputPickle();\
			r_pickle_call << _p_thevalue;\
		}\
		catch (...) {return ILUCPP_FALSE;}\
		return ILUCPP_TRUE;\
	}
	
#define ILUCPP_DEFINE_ILU_TYPE_CODE_ENUM_PICKLETOVALUE(sometypecodeclass, sometypename)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {\
		sometypename* p_thevalue = new sometypename;\
		try {\
			iluEnumWrapper _the_value_wrapper((iluDummyEnum &) *p_thevalue);\
			r_pickle_call >> _the_value_wrapper;\
			}\
		catch (...) { delete p_thevalue; return NULL;}\
		return p_thevalue;\
	}


#define ILUCPP_DEFINE_ILU_TYPE_CODE_OPTIONAL_VALUETOPICKLE(sometypecodeclass, sometypename_forany)\
	CORBA(Boolean) _ilu_ ## sometypecodeclass ## _TypeCode::iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call) {\
		try {\
			sometypename_forany* p_forany = (sometypename_forany*)pv_value;\
			iluOptionalWrapper _p_thevalue(p_forany->m_p_value);\
			r_pickle_call += _p_thevalue;\
			if (p_forany->m_p_value) \
				r_pickle_call += *(p_forany->m_p_value);\
			r_pickle_call.iluOutputPickle();\
			r_pickle_call << _p_thevalue;\
			if (p_forany->m_p_value) \
				r_pickle_call << *(p_forany->m_p_value);\
		}\
		catch (...) {return ILUCPP_FALSE;}\
		return ILUCPP_TRUE;\
	}


#define ILUCPP_DEFINE_ILU_TYPE_CODE_OPTIONAL_PICKLETOVALUE(sometypecodeclass, sometypename, sometypename_forany)\
	void* _ilu_ ## sometypecodeclass ## _TypeCode::iluPickleToValue (iluPickleCall& r_pickle_call) {\
		sometypename_forany* p_thevalue = new sometypename_forany;\
		try {\
			iluOptionalWrapper _the_value_wrapper;\
			r_pickle_call >> _the_value_wrapper;\
			if (_the_value_wrapper.m_present) {\
				p_thevalue->m_p_value = new sometypename;\
				r_pickle_call >> *(p_thevalue->m_p_value);\
			}\
			}\
			catch (...) { delete p_thevalue; return NULL;}\
		return p_thevalue;\
	}

	
#define ILUCPP_DEFINE_ILU_TYPE_CODE_MEMBERFUNCTIONS(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_VALUETOPICKLE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_PICKLETOVALUE(sometypecodeclass, sometypename)

#define ILUCPP_DEFINE_ILU_TYPE_CODE_ENUM_MEMBERFUNCTIONS(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_ENUM_VALUETOPICKLE(sometypecodeclass, sometypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_ENUM_PICKLETOVALUE(sometypecodeclass, sometypename)

#define ILUCPP_DEFINE_ILU_TYPE_CODE_SIMPLE_OPTIONAL_MEMBERFUNCTIONS(sometypecodeclass, sometypename, actualtypename)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DELETEVALUE(sometypecodeclass, sometypename ## _forany)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_DEEPCOPY(sometypecodeclass, sometypename ## _forany)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_OPTIONAL_VALUETOPICKLE(sometypecodeclass, sometypename ## _forany)\
	ILUCPP_DEFINE_ILU_TYPE_CODE_OPTIONAL_PICKLETOVALUE(sometypecodeclass, actualtypename, sometypename ## _forany)

#define ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_MEMBERFUNCTIONS(sometypecodeclass, sometypename, allocfunctionname, dupefunctionname, sizefunctionname, outputfunctionname, inputfunctionname)\
  ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_DELETEVALUE(sometypecodeclass, sometypename)\
  ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_DEEPCOPY(sometypecodeclass, sometypename, dupefunctionname)\
  ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_VALUETOPICKLE(sometypecodeclass, sometypename, sizefunctionname, outputfunctionname)\
  ILUCPP_DEFINE_ILU_TYPE_CODE_ARRAY_PICKLETOVALUE(sometypecodeclass, sometypename, allocfunctionname, inputfunctionname)

//////////////////////////////////////////////////////////////////
// macro to define typecode class

#define ILUCPP_DEFINE_TYPE_CODE_CLASS(the_name, the_ilu_typecode, the_ilu_typeid)\
	ILU_RUNTIME_PUBLIC_CLASS _ilu_ ## the_name ## _TypeCode : public CORBA(TypeCode) {\
	public: \
	_ilu_ ## the_name ## _TypeCode () : CORBA(TypeCode)(the_ilu_typecode, the_ilu_typeid) {};\
	virtual void iluDeleteValue (void* pv_value); \
	virtual void* iluDeepCopy (void* pv_value); \
	virtual CORBA(Boolean) iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call); \
	virtual void* iluPickleToValue (iluPickleCall& r_pickle_call); \
	}

#define ILUCPP_DEFINE_TYPE_CODE_OBJECT_CLASS(the_name, the_ilu_typecode, the_ilu_typeid)\
	ILU_RUNTIME_PUBLIC_CLASS _ilu_ ## the_name ## _TypeCode : public CORBA(TypeCode) {\
	public: \
	_ilu_ ## the_name ## _TypeCode () : CORBA(TypeCode)(the_ilu_typecode, the_ilu_typeid) {};\
	virtual void iluDeleteValue (void* pv_value); \
	virtual void* iluDeepCopy (void* pv_value); \
	virtual CORBA(Boolean) iluValueToPickle (void* pv_value, iluPickleCall& r_pickle_call); \
	virtual void* iluPickleToValue (iluPickleCall& r_pickle_call); \
	virtual void* iluUpcast (void* p_derived_object, iluClass casttoclass);\
	virtual CORBA(TypeCode)* iluMostSpecificTypeCode (void* p_an_object_of_your_type);\
	}

#define ILUCPP_DEFINE_STUB_TYPE_CODE_CLASS(the_name, the_ilu_typecode, the_ilu_typeid)\
	class _ilu_ ## the_name ## _TypeCode : public CORBA(TypeCode) {\
	public: \
	_ilu_ ## the_name ## _TypeCode () : CORBA(TypeCode)(the_ilu_typecode, the_ilu_typeid) {};\
	virtual void iluDeleteValue (void* pv_value); \
	virtual void* iluDeepCopy (void* pv_value); \
	virtual CORBA(Boolean) iluValueToPickle (void* pv_value, iluPickleCall& r_pickle); \
	virtual void* iluPickleToValue (iluPickleCall& r_pickle_call); \
	}

#define ILUCPP_DEFINE_STUB_TYPE_CODE_OBJECT_CLASS(the_name, the_ilu_typecode, the_ilu_typeid)\
	class _ilu_ ## the_name ## _TypeCode : public CORBA(TypeCode) {\
	public: \
	_ilu_ ## the_name ## _TypeCode () : CORBA(TypeCode)(the_ilu_typecode, the_ilu_typeid) {};\
	virtual void iluDeleteValue (void* pv_value); \
	virtual void* iluDeepCopy (void* pv_value); \
	virtual CORBA(Boolean) iluValueToPickle (void* pv_value, iluPickleCall& r_pickle); \
	virtual void* iluPickleToValue (iluPickleCall& r_pickle_call); \
	virtual void* iluUpcast (void* p_derived_object, iluClass casttoclass);\
	virtual CORBA(TypeCode)* iluMostSpecificTypeCode (void* p_an_object_of_your_type);\
	}


// expands to the class name of the typecode used for the named type
#define ILUCPP_TYPE_CODE_CLASS_NAME(the_name) _ilu_ ## the_name ## _TypeCode


//////////////////////////////////////////////////////////////////
// macros to define the Any insertion and extraction operators for arrays

#define ILUCPP_DEFINE_ARRAY_INSERTION_OPERATOR(classslice, foranyclassname, typecodeconstant, dupefunctionname)\
  void operator<<=(CORBA(Any)& r_any, const foranyclassname& r_somearray_forany){\
	 if (r_somearray_forany.iluShouldCopy())\
		 r_any.replace(typecodeconstant, dupefunctionname((const classslice *) r_somearray_forany.iluGetSlice()), ILUCPP_TRUE);\
	 else {\
		 r_any.replace(typecodeconstant, r_somearray_forany.iluGetSlice(), ILUCPP_TRUE);\
		 r_somearray_forany.iluSetRelease(ILUCPP_FALSE);\
	 }\
  }
  

#define ILUCPP_DEFINE_ARRAY_EXTRACTION_OPERATOR(foranyclassname, typecodeconstant, slicename)\
  CORBA(Boolean) operator>>=(const CORBA(Any)& r_any, foranyclassname& r_somearray_forany) {\
  		CORBA(Any)* p_nc_any = CONST_CAST(CORBA(Any)*, &r_any);\
		if (p_nc_any->iluGetFromPickle(typecodeconstant)) { \
			r_somearray_forany = REINTERPRET_CAST(slicename*, CONST_CAST(void*, p_nc_any->value()));\
			r_somearray_forany.iluSetRelease(ILUCPP_FALSE);\
			r_somearray_forany.iluSetNoCopy(ILUCPP_FALSE); \
			return ILUCPP_TRUE;\
		}\
		return ILUCPP_FALSE;\
  }


//////////////////////////////////////////////////////////////////
// macros to define the Any insertion and extraction operators for non arrays

#define ILUCPP_DEFINE_COPYING_INSERTION_OPERATOR(classname, typecodeconstant)\
  void operator<<=(CORBA(Any)& r_any, const classname& r_someinstance){\
		classname* p_content = new classname;\
		*p_content = r_someinstance;\
		r_any.replace(typecodeconstant, p_content, ILUCPP_TRUE);\
  }
  
#define ILUCPP_DEFINE_NONCOPYING_INSERTION_OPERATOR(classname, typecodeconstant)\
  void operator<<=(CORBA(Any)& r_any, classname* p_someinstance){\
		r_any.replace(typecodeconstant, p_someinstance, ILUCPP_TRUE);\
  }
  

#define ILUCPP_DEFINE_EXTRACTION_OPERATOR(classname, typecodeconstant)\
  CORBA(Boolean) operator>>=(const CORBA(Any)& r_any, classname*& rp_someinstance) {\
		CORBA(Any)* p_nc_any = CONST_CAST(CORBA(Any)*, &r_any); \
		if (p_nc_any->iluGetFromPickle(typecodeconstant)) {  \
			rp_someinstance = ((classname*) p_nc_any->value()); \
			return ILUCPP_TRUE;\
		}\
		return ILUCPP_FALSE; \
  }

#define ILUCPP_DEFINE_REF_EXTRACTION_OPERATOR(classname, typecodeconstant)\
  CORBA(Boolean) operator>>=(const CORBA(Any)& r_any, classname& r_someinstance) {\
		CORBA(Any)* p_nc_any = CONST_CAST(CORBA(Any)*, &r_any); \
		if (p_nc_any->iluGetFromPickle(typecodeconstant)) {  \
			r_someinstance = *((classname*) p_nc_any->value()); \
			return ILUCPP_TRUE;\
		}\
		return ILUCPP_FALSE; \
  }


   
//////////////////////////////////////////////////////////////////
// declarations of all the builtin typecode classes
	
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_null,			ILUCPP_BOGUS_TYPECODE_FOR_CORBA_NULL, (ilu_string) NULL);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_boolean,		ilu_boolean_tk,			(ilu_string) &ilu_TypeID_ilu_boolean);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_octet,			ilu_byte_tk,			(ilu_string)&ilu_TypeID_ilu_byte);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_short,			ilu_shortinteger_tk,	(ilu_string)&ilu_TypeID_ilu_shortinteger);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_long,			ilu_integer_tk,			(ilu_string)&ilu_TypeID_ilu_integer);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_ushort,		ilu_shortcardinal_tk,	(ilu_string)&ilu_TypeID_ilu_shortcardinal);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_ulong,			ilu_cardinal_tk,		(ilu_string)&ilu_TypeID_ilu_cardinal);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_char,			ilu_shortcharacter_tk,	(ilu_string)&ilu_TypeID_ilu_shortcharacter);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_float,			ilu_shortreal_tk,		(ilu_string)&ilu_TypeID_ilu_shortreal);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_double,		ilu_real_tk,			(ilu_string)&ilu_TypeID_ilu_real);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_string,		ilu_sequence_tk,		(ilu_string)&ilu_TypeID_ilu_CString);
ILUCPP_DEFINE_TYPE_CODE_CLASS(corba_any,			ilu_pickle_tk,			(ilu_string)&ilu_TypeID_ilu_pickle);
ILUCPP_DEFINE_TYPE_CODE_OBJECT_CLASS(corba_object,	ilu_object_tk,			(ilu_string)&ilu_TypeID_ilu_CORBA_Object);
ILUCPP_DEFINE_TYPE_CODE_OBJECT_CLASS(ilu_object,	ilu_object_tk,			(ilu_string)"ilu:root-object-type");
ILUCPP_DEFINE_TYPE_CODE_CLASS(ilu_character,		ilu_character_tk,		(ilu_string)&ilu_TypeID_ilu_character);
ILUCPP_DEFINE_TYPE_CODE_CLASS(ilu_longinteger,		ilu_longinteger_tk,		(ilu_string)&ilu_TypeID_ilu_longinteger);
ILUCPP_DEFINE_TYPE_CODE_CLASS(ilu_longcardinal,	ilu_longcardinal_tk,	(ilu_string)&ilu_TypeID_ilu_longcardinal);
ILUCPP_DEFINE_TYPE_CODE_CLASS(ilu_longreal,		ilu_longreal_tk,		(ilu_string)&ilu_TypeID_ilu_longreal);


//////////////////////////////////////////////////////////////////
// basecall overloads for stubs to use when sending receiving anys

ILU_RUNTIME_PUBLIC iluBaseCall& operator<<(iluBaseCall& r_call, CORBA(Any)& r_any);
ILU_RUNTIME_PUBLIC iluBaseCall& operator<<(iluBaseCall& r_call, const CORBA(Any)& r_any);
ILU_RUNTIME_PUBLIC iluBaseCall& operator>>(iluBaseCall& r_call, CORBA(Any)& r_any);
ILU_RUNTIME_PUBLIC iluBaseCall& operator+=(iluBaseCall& r_call, CORBA(Any)& r_any);
ILU_RUNTIME_PUBLIC iluBaseCall& operator+=(iluBaseCall& r_call, const CORBA(Any)& r_any);


#endif /* ADD_VARIANT_SUPPORT */



#endif //  __corba_H_


