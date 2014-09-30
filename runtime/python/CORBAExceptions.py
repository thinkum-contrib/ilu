class Exception:
	pass

class UserException (Exception):
	pass

class SystemException (Exception):
	def __init__(self, _minor, _completed):
		self.minor = long(_minor)
		self.completed = _completed

class completion_status:
	COMPLETED_YES = 0
	COMPLETED_NO = 1
	COMPLETED_MAYBE = 2
	__image__ = {
		COMPLETED_MAYBE : 'COMPLETED_MAYBE',
		COMPLETED_NO : 'COMPLETED_NO',
		COMPLETED_YES : 'COMPLETED_YES' }

COMPLETED_YES = 0
COMPLETED_NO = 1
COMPLETED_MAYBE = 2

class UNKNOWN (SystemException):
	# the unknown exception 
	pass

class BAD_PARAM (SystemException):
	# an invalid parameter was passed 
	pass

class NO_MEMORY (SystemException):
	# dynamic memory allocation failure 
	pass

class IMP_LIMIT (SystemException):
	# violated implementation limit 
	pass

class COMM_FAILURE (SystemException):
	# communication failure 
	pass

class INV_OBJREF (SystemException):
	# invalid object reference 
	pass

class NO_PERMISSION (SystemException):
	# no permission for attempted op. 
	pass

class INTERNAL (SystemException):
	# ORB internal error 
	pass

class MARSHAL (SystemException):
	# error marshalling param/result 
	pass

class INITIALIZE (SystemException):
	# ORB initialization failure 
	pass

class NO_IMPLEMENT (SystemException):
	# operation implementation unavailable 
	pass

class BAD_TYPECODE (SystemException):
	# bad typecode 
	pass

class BAD_OPERATION (SystemException):
	# invalid operation 
	pass

class NO_RESOURCES (SystemException):
	# insufficient resources for req. 
	pass

class NO_RESPONSE (SystemException):
	# response to req. not yet available 
	pass

class PERSIST_STORE (SystemException):
	# persistent storage failure 
	pass

class BAD_INV_ORDER (SystemException):
	# routine invocations out of order 
	pass

class TRANSIENT (SystemException):
	# transient failure - reissue request 
	pass

class FREE_MEM (SystemException):
	# cannot free memory 
	pass

class INV_IDENT (SystemException):
	# invalid identifier syntax 
	pass

class INV_FLAG (SystemException):
	# invalid flag was specified 
	pass

class INTF_REPOS (SystemException):
	# error accessing interface repository 
	pass

class BAD_CONTEXT (SystemException):
	# error processing context object 
	pass

class OBJ_ADAPTER (SystemException):
	# failure detected by object adapter 
	pass

class DATA_CONVERSION (SystemException):
	# data conversion error 
	pass

class OBJECT_NOT_EXIST (SystemException):
	# non-existent object, delete reference 
	pass

class TRANSACTION_REQUIRED (SystemException):
	# transaction required 
	pass

class TRANSACTION_ROLLEDBACK (SystemException):
	# transaction rolled back 
	pass

class INVALID_TRANSACTION (SystemException):
	# invalid transaction
	pass

class INTERRUPTED (SystemException):
	# invalid transaction
	pass

