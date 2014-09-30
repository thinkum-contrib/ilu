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
/* $Id: corba-templates.hpp,v 1.22 1999/08/03 01:55:48 janssen Exp $ */


//////////////////////////////////////////////////////////////////
// Preprocessing checks and inclusions

// prevent multiple inclusions 
#ifndef __ilucpp_templates_H_
#define __ilucpp_templates_H_

// error out if we're not being processed by a c++ compiler
#ifndef __cplusplus
#error "corba-templates.hpp is a C++ header file"
#endif

#ifdef __GNUC__
#define INLINE 
#else
#define INLINE  inline
#endif

//////////////////////////////////////////////////////////////////
// Template for creating _var s of objects
template <class T>
class  iluTemplatableObject_var : public ilu_var {
	
public:
	
	// constructors & destructor
	iluTemplatableObject_var ();
	
	iluTemplatableObject_var ( T* a_ptr);
	
	iluTemplatableObject_var (const iluTemplatableObject_var<T>& a_var);
	
	~iluTemplatableObject_var();
	
	// assignment from ptr
	iluTemplatableObject_var<T> &operator=( T* a_ptr );

	// assignment from another _var on same object type
	iluTemplatableObject_var<T>& operator=(const iluTemplatableObject_var<T> & r_a_iluTemplatableObject_var);

	// conversion to ptr
	operator T* &();
	
	// member function invocation
	T* operator->() const;

	// return the internal object pointer
	T* iluGetObjectPointer() const;

	// accessors on the m_b_release member
	void iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const;
	ILUCPP_BOOL iluGetRelease() const;

	// for use by ILU stubs
	iluObject& iluObjectReference() const;
	void iluDeleteWrapper() const;
	void iluSetWrapper(iluObjectWrapper* p_wrapper) const;
	iluObjectWrapper* iluGetWrapper() const;

	// returns true if the _var contains a null pointer
	ILUCPP_BOOL iluIsNull() const;

protected:
	
	// pointer to the actual C++ instance
	T* m_ptr;
	
	// pointer control operations
	void reset(T* p);

	
private:

	// ilu stub use only - points to the wrapper used to input and output objects
	iluObjectWrapper* m_p_wrapper;

	// whether or not a release should be done when the var destructs
	ILUCPP_BOOL m_b_release;

	// hidden assignment operators for var types to
	// fulfill the rules specified in Section 16.3.2 of CORBA 2.0 spec.
	iluTemplatableObject_var (const ilu_var &);
	void operator=(const ilu_var &);
};


//////////////////////////////////////////////////////////////////
// iluTemplatableObject_var<T> implementations

// constructors & destructor
template <class T>
INLINE
iluTemplatableObject_var<T>::iluTemplatableObject_var () : m_ptr( T::_nil()), m_p_wrapper(NULL), m_b_release(ILUCPP_TRUE) {
}

template <class T>
INLINE
iluTemplatableObject_var<T>::iluTemplatableObject_var ( T* a_ptr) : m_ptr(a_ptr), m_p_wrapper(NULL), m_b_release(ILUCPP_TRUE) {
}

template <class T>
INLINE
iluTemplatableObject_var<T>::iluTemplatableObject_var (const iluTemplatableObject_var<T>& a_var) : 
m_ptr( T::_duplicate (  
	  // XXX this cast has problems? ( (T*) a_var )
	  a_var.m_ptr
	  )), m_p_wrapper(NULL), m_b_release(ILUCPP_TRUE) 
{}

template <class T>
INLINE
iluTemplatableObject_var<T>::~iluTemplatableObject_var() {
	delete m_p_wrapper;
	reset(NULL); 
}

// assignment from ptr
template <class T>
INLINE
iluTemplatableObject_var<T> & iluTemplatableObject_var<T>::operator=( T* a_ptr ) { 
	// Blindly reset per spec section 16.3.6, "but they must provide the same 
	// semantics and usage as this example" So don't do:   if (m_ptr != a_ptr) 
	// This seems in conflict with the statement section 16.3.1, 3rd paragraph,
	// first sentence "... new object reference".  Apparently they have a 
	// different interpretation of 'new' than I do?
		reset( a_ptr ); 
	return *this; 
}


// assignment from another _var on same object type
template <class T>
INLINE
iluTemplatableObject_var<T> & iluTemplatableObject_var<T>::operator=(const iluTemplatableObject_var<T> & r_a_iluTemplatableObject_var) {
	if (this != &r_a_iluTemplatableObject_var) {
		if (r_a_iluTemplatableObject_var.m_ptr)
			reset(T::_duplicate (  
			// XXX this cast has problems? ( (T*) a_var )
			r_a_iluTemplatableObject_var.m_ptr
			)
			);
		else 
			reset (NULL);
	}
	return *this; 
}

// conversion to ptr
template <class T>
INLINE
iluTemplatableObject_var<T>::operator T*&() { 
	return m_ptr; 
}

// member function invocation
template <class T>
INLINE
T* iluTemplatableObject_var<T>::operator->() const { 
	return m_ptr; 
}

// return the internal object pointer
template <class T>
INLINE	T* iluTemplatableObject_var<T>::iluGetObjectPointer() const {
	return m_ptr;
}

template <class T>
INLINE	void iluTemplatableObject_var<T>::iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const {
	// cast around constness
	ILUCPP_BOOL* p_release = CONST_CAST(ILUCPP_BOOL*, &m_b_release);
	*p_release = b_release_on_destruct;
}

template <class T>
INLINE	ILUCPP_BOOL iluTemplatableObject_var<T>::iluGetRelease() const {
	return m_b_release;
}

// for use by ILU stubs
template <class T>
INLINE	iluObject& iluTemplatableObject_var<T>::iluObjectReference() const {
	return *m_ptr;
}

template <class T>
INLINE	void iluTemplatableObject_var<T>::iluDeleteWrapper() const {
	delete m_p_wrapper;
	// cast around constness
	iluObjectWrapper** pp_wrapper = (iluObjectWrapper**)((void*)(&m_p_wrapper));
	*pp_wrapper = NULL;
}

template <class T>
INLINE	void iluTemplatableObject_var<T>::iluSetWrapper(iluObjectWrapper* p_wrapper) const {
	delete m_p_wrapper;
	// cast around constness
	iluObjectWrapper** pp_wrapper = (iluObjectWrapper**)((void*)(&m_p_wrapper));
	*pp_wrapper = p_wrapper;
}

template <class T>
INLINE	iluObjectWrapper* iluTemplatableObject_var<T>::iluGetWrapper() const {
	return m_p_wrapper;
}


// returns true if the _var contains a null pointer
template <class T>
INLINE	ILUCPP_BOOL iluTemplatableObject_var<T>::iluIsNull() const {
	return (m_ptr ? ILUCPP_FALSE : ILUCPP_TRUE);
}


// pointer control operations
template <class T>
INLINE
void iluTemplatableObject_var<T>::reset(T* p) {  
	if (m_ptr && m_b_release)
		m_ptr->_release();
	m_ptr = p; 
}



//////////////////////////////////////////////////////////////////
// Template for creating T_var s 

template <class T>
class iluTemplatableT_var : public ilu_var {
	
public:
	
	// default constructor
	iluTemplatableT_var ();
	
	// construct from pointer
	iluTemplatableT_var (T* a_ptr);
	
	// copy constructor
    iluTemplatableT_var (const iluTemplatableT_var<T> & r_iluTemplatableT_var);
	
	// destructor
    ~iluTemplatableT_var ();
	
	// assignment from a pointer
    iluTemplatableT_var<T> & operator= (T * a_ptr);
	
	// assignment from T var
    iluTemplatableT_var<T> & operator= (const iluTemplatableT_var<T> & r_iluTemplatableT_var);
	
	// -> overload
    T * operator-> () const;

	// conversion operations to allow parameter passing per Corba Spec chap 16, Table 16.2
    operator const T& () const ;	// in parameters
	operator T& () ;				// inout and fixed out parameters
	operator T*& () ;				// variable out parameters

	// accessors on the m_b_release member
	void iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const;
	ILUCPP_BOOL iluGetRelease() const;

	// returns true if the _var contains a null pointer
	ILUCPP_BOOL iluIsNull() const;

	
protected:
	
	// pointer to the actual C++ instance
	T* m_ptr;

	// whether or not a delete should be done when the var destructs
	ILUCPP_BOOL m_b_release;

private:

	// hidden assignment operators for var types to
	// fulfill the rules specified in Section 16.3.2 of CORBA 2.0 spec.
	iluTemplatableT_var<T> (const ilu_var &);
	void operator=(const ilu_var &);
	
};


//////////////////////////////////////////////////////////////////
// iluTemplatableT_var<T> implementations

// default constructor
template <class T>
INLINE
iluTemplatableT_var<T>::iluTemplatableT_var () : m_ptr(NULL), m_b_release(ILUCPP_TRUE) {
}


// construct from pointer
template <class T>
INLINE
iluTemplatableT_var<T>::iluTemplatableT_var (T* a_ptr) : m_ptr(a_ptr), m_b_release(ILUCPP_TRUE) {
}


// copy constructor
template <class T>
INLINE
iluTemplatableT_var<T>::iluTemplatableT_var (const iluTemplatableT_var<T> & r_iluTemplatableT_var) : m_b_release(ILUCPP_TRUE) {
	if (r_iluTemplatableT_var.m_ptr) {
		m_ptr = new T;
		*m_ptr = *(r_iluTemplatableT_var.m_ptr);
	}
	else m_ptr = NULL;
}


// destructor
template <class T>
INLINE
iluTemplatableT_var<T>::~iluTemplatableT_var () { 
	if (m_b_release) 
		delete m_ptr;
}


// assignment from a pointer
template <class T>
INLINE
iluTemplatableT_var<T> & iluTemplatableT_var<T>:: operator= (T * a_ptr) {
	if (m_ptr != a_ptr) {
		if (m_b_release) 
			delete m_ptr;
		m_ptr = a_ptr;
	}
	return *this;
}

// assignment from T var
template <class T>
INLINE
iluTemplatableT_var<T> & iluTemplatableT_var<T>::operator= (const iluTemplatableT_var<T> & r_iluTemplatableT_var) {
	if (m_ptr != r_iluTemplatableT_var.m_ptr) {
		if (m_b_release) 
			delete m_ptr;
	}
	if (r_iluTemplatableT_var.m_ptr) {
		m_ptr = new T;
		*m_ptr = *(r_iluTemplatableT_var.m_ptr);
	}
	else m_ptr = NULL;
	return *this;
}

// -> overload
template <class T>
INLINE
T * iluTemplatableT_var<T>::operator-> () const { 
	return m_ptr; 
}

// conversion operations to allow parameter passing per Corba Spec chap 16, Table 16.2
template <class T>
INLINE
iluTemplatableT_var<T>::operator const T& () const { 	// in parameters
	return *m_ptr; 
}

template <class T>
INLINE
iluTemplatableT_var<T>::operator T& ()  { // inout and fixed out parameters
				return *m_ptr; 
}				

template <class T>
INLINE
iluTemplatableT_var<T>::operator T*& () { // variable out parameters
				return m_ptr; 
}				


template <class T>
INLINE	void iluTemplatableT_var<T>::iluSetRelease(ILUCPP_BOOL b_release_on_destruct) const {
	// cast around constness
	ILUCPP_BOOL* p_release = CONST_CAST(ILUCPP_BOOL*, &m_b_release);
	*p_release = b_release_on_destruct;
}

template <class T>
INLINE	ILUCPP_BOOL iluTemplatableT_var<T>::iluGetRelease() const {
	return m_b_release;
}

// returns true if the _var contains a null pointer
template <class T>
INLINE	ILUCPP_BOOL iluTemplatableT_var<T>::iluIsNull() const {
	return (m_ptr ? ILUCPP_FALSE : ILUCPP_TRUE);
}



//////////////////////////////////////////////////////////////////
// Template for creating Sequence _var s 

template <class S, class E>
  // S is a Sequence Type
  // E is S's element Type
class iluTemplatableSequence_var : public iluTemplatableT_var<S> {
	
public:
	
	// default constructor
	iluTemplatableSequence_var ();
	
	// construct from pointer
	iluTemplatableSequence_var (S* a_ptr);
	
	// copy constructor
    iluTemplatableSequence_var (const iluTemplatableSequence_var<S, E> &
r_iluTemplatableSequence_var);
	
	// assignment from a pointer
    iluTemplatableSequence_var<S, E> & operator= (S * a_ptr);
	
	// assignment from S var
    iluTemplatableSequence_var<S, E> & operator= (const
iluTemplatableSequence_var<S, E> & r_iluTemplatableSequence_var);

	// indexing operators
	const E& operator [] (iluCardinal index) const;
	E& operator [] (iluCardinal index);
	

private:

	// hidden assignment operators for var types to
	// fulfill the rules specified in Section 16.3.2 of CORBA 2.0 spec.
	iluTemplatableSequence_var<S, E> (const ilu_var &);
	void operator=(const ilu_var &);
	
};


//////////////////////////////////////////////////////////////////
// iluTemplatableSequence_Var<S, E> implementations

// default constructor
template <class S, class E>
INLINE
iluTemplatableSequence_var<S, E>::iluTemplatableSequence_var () :
iluTemplatableT_var<S>() {
}


// construct from pointer
template <class S, class E>
INLINE
iluTemplatableSequence_var<S, E>::iluTemplatableSequence_var (S* a_ptr) :
iluTemplatableT_var<S>(a_ptr) {
}


// copy constructor
template <class S, class E>
INLINE
iluTemplatableSequence_var<S, E>::iluTemplatableSequence_var (const
iluTemplatableSequence_var<S, E> & r_iluTemplatableSequence_var) :
iluTemplatableT_var<S>(r_iluTemplatableSequence_var) {
}


// assignment from a pointer
template <class S, class E>
INLINE
iluTemplatableSequence_var<S, E> & iluTemplatableSequence_var<S, E>::
operator= (S * a_ptr) {
	iluTemplatableT_var<S>::operator=(a_ptr);
	return *this;
}

// assignment from iluTemplatableSequence_var<S, E>
template <class S, class E>
INLINE
iluTemplatableSequence_var<S, E> & iluTemplatableSequence_var<S,
E>::operator= (const iluTemplatableSequence_var<S, E> &
r_iluTemplatableSequence_var) {
	iluTemplatableT_var<S>::operator=(r_iluTemplatableSequence_var);
	return *this;
}

// right-hand-side indexing operator
template <class S, class E>
INLINE
const E&
iluTemplatableSequence_var<S, E>::operator [] (iluCardinal index) const {
	return (*m_ptr)[index];
}

// left-hand-side indexing operator
template <class S, class E>
INLINE
E&
iluTemplatableSequence_var<S, E>::operator [] (iluCardinal index) {
	return (*m_ptr)[index];
}



#endif //  __ilucpp_templates_H_
