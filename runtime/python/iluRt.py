# BeginILUCopyright
# 
# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.
# 
# Unlimited use, reproduction, modification, and distribution of this
# software and modified versions thereof is permitted.  Permission is
# granted to make derivative works from this software or a modified
# version thereof.  Any copy of this software, a modified version
# thereof, or a derivative work must include both the above copyright
# notice of Xerox Corporation and this paragraph.  Any distribution of
# this software, a modified version thereof, or a derivative work must
# comply with all applicable United States export control laws.  This
# software is made available AS IS, and XEROX CORPORATION DISCLAIMS ALL
# WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
# LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
# EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
# NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGES.
# 
# EndILUCopyright
#
# $Id: iluRt.py,v 1.65 1999/09/12 02:42:30 janssen Exp $
#

from iluPr import *

class IluObject:

    _IluClass = ilu_rootClass

    def IluTypeName(self):
	return self._IluClass.name()

    def IluTypeID(self):
	return self._IluClass.id()


class IluObjRemote(IluObject):

    def IluSBH(self):
	return SBHOfObject(self)

    def IluObjectID(self):
	sbh = self.IluSBH()
	ih, sid, mstid, cinfo = ParseSBH(sbh)
	return (sid, ih,)

    def IluKernelServer(self):
        return self._IluInstVars.Server()
		

class IluObjSurr(IluObjRemote):

    def IluTrueP(self):
        return 0

    def IluPing(self):
        return PingObject(self)

    def IluSetCachedValue(self, value_name, value):
	    if not self.__dict__.has_key('__cached_values__'):
		    self.__cached_values__ = {}
	    self.__cached_values__[value_name] = value

    def IluGetCachedValue(self, value_name):
	    if (self.__dict__.has_key('__cached_values__') and self.__cached_values__.has_key(value_name)):
		    return (self.__cached_values__[value_name],)
	    else:
		    return None
	    
    def __str__(self):
	    oid = self.IluObjectID()
	    return "<%s:%s/%s>" % (self.IluTypeName(), oid[0], oid[1])
	    
    def __del__(self):
	try:
	    if hasattr(self, "_IluInstVars"):
		del self._IluInstVars
	except:
	    pass

#     def __getstate__(self):
#       cState = {}
#       sbh = self.IluSBH()
#       cState["sbh"] = sbh
#       return cState

#     def __setstate__(self, state):
#       sbh = state["sbh"]
#       inst = ObjectOfSBH(self.__class__, sbh)
#       self._oldsurrinst = inst  ## to prevent against gc
#       self._IluInstVars = inst._IluInstVars

    __repr__ = __str__

class IluObjTrue(IluObjRemote):

    def __init__(self, ih=None, server=None):
	    self.IluInstHandle = ih
	    self.IluServer = server

    def IluTrueP(self):
        return 1

    def IluPing(self):
        return 1

    def IluPublish(self):
	PublishObject(self)

    def IluWithdraw(self):
	WithdrawObject(self)

    if CORBAMapping:

	    def _this(self):		# for CORBA mapping
		    return self

	    def _default_POA(self):
		    return None

    def __str__(self):
	    oid = self.IluObjectID()
	    return "<%s=%s/%s>" % (self.IluTypeName(), oid[0], oid[1])
	    
    __repr__ = __str__


class IluObjLocal(IluObject):

    def _get(self, key, typecode=None):
	    if typecode:
		    return self.__states__[typecode.id()][key]
	    else:
		    return self.__states__[self.IluTypeID()][key]

    def _set(self, key, value, typecode=None):
	    if typecode:
		    self.__states__[typecode.id()][key] = value
	    else:
		    self.__states__[self.IluTypeID()][key] = value

    def __str__(self):
        return '<%s:%s>' % (self.IluTypeName(), hash(self))
		
    __repr__ = __str__


class IluRecord:

    def __len__(self):
	return len(self.__dict__)

    def __getitem__(self, key):
        return self.__dict__[key]

    def __setitem__(self, key, val):
        self.__dict__[key] = val

    def keys(self):
	return self.__dict__.keys()

    def values(self):
	return self.__dict__.values()

    def items(self):
	return self.__dict__.items()

    def __str__(self):
        return '<%s:%s>' % (self.__ilu_type_name__, self.__dict__)
		
    def __cmp__(self, other):
        if (type(self) == type(other)):
		if (self.__dict__ < other.__dict__):
			return -1
		elif (self.__dict__ == other.__dict__):
			return 0
		else:
			return 1
	else:
		return 1

    def IluTypeName(self):
	return self.__ilu_type_name__

    __repr__ = __str__


if CORBAMapping:

    # the normal ILU mapping just uses tuples for union types, so this is
    # only needed for the CORBA mapping
    class IluUnion:
	def __cmp__(self, other):
	    if ((type(self) == type(other)) and	(self.__class__ == other.__class__)):
		if ((self.d == other.d) and (self.v == other.v)):
		    return 0;
		else:
		    return -1
	    else:
		return -1

# this code implements a type registry, and allows iluRt.{Input,Output,SizeOf}Value to work

# define I/O functions for ilu.CString

def OutputCString (call, string):
	OutputString(call, string, 0)

def SizeOfCString (call, string):
	return SizeOfString(call, string, 0)

def InputCString (call):
	return InputString (call, 0)

UnknownTypeID = "unknown ILU type UID"
BadPickleValue = "pickle doesn't support required value"

if HasVariantSupport:

	UnknownType = "UnknownType"
	TooManyArguments = "TooManyArguments"

	RegisteredTypes = {}
	TypeAliases = {}

	def RegisterIoFuncs (uid, input_func, output_func, sizeof_func, name):
		global RegisteredTypes
		RegisteredTypes[uid] = (input_func, output_func, sizeof_func, name)

	def NoteAlias (typename, typeuid, ur_typeuid):
		global TypeAliases
		TypeAliases[typename] = (ur_typeuid, typeuid)

	# this function allows me to access the (pre-registered) ilu primitive
	# types, and register the corresponding I/O functions for them
	def RegisterPrimitiveType (type_full_name, input_func, output_func, sizeof_func):
		import string
		typeid = FindTypeIDByName(type_full_name)
		if typeid:
			RegisterIoFuncs(typeid, input_func, output_func, sizeof_func, type_full_name)

	class Typecode:
		def __init__(self, type_name, *private):
			def GetTypeID(_typename):
				def find_type(typename):
					if TypeAliases.has_key(typename):
						tuid = TypeAliases[typename][0]
						if RegisteredTypes.has_key(tuid):
							return (tuid, RegisteredTypes[tuid],)
					for type_rec in RegisteredTypes.items():
						if typename == type_rec[1][3]:
							return type_rec
					return None
				import string
				type_rec = find_type(_typename)
				if (type_rec):
					return type_rec[0]
				else:
					raise UnknownType, _typename
			if (type_name != None):
				if (len(private) > 0):
					raise TooManyArguments, 'expected args of ("typename")'
				self.__type_id = GetTypeID(type_name)
				self.__type_name = type_name
			else:
				if (len(private) > 1):
					raise TooManyArguments, 'expected args of (None, "typeid")'
				if (type(private[0]) != type('')):
					raise TypeError, 'second argument should be type ID string'
				self.__type_id = private[0]
				if (RegisteredTypes.has_key(self.__type_id)):
					self.__type_name = RegisteredTypes[self.__type_id][3]
				else:
					self.__type_name = self.__type_id
		def id(self):
			if (type(self.__type_id) == type('')):
				return self.__type_id
			else:
				return self.__type_id.uid
		def __str__(self):
			return '<Typecode "' + self.__type_name + '">'
		__repr__ = __str__
		def __cmp__(self, other):
			return cmp(self.id(), other.id())
		def __hash__(self):
			return hash(self.id())
			
	class Pickle:
		def __init__(self, typecode, *value):
			def PickleValue (value, uid):
				if (RegisteredTypes.has_key(uid)):
					funcs = RegisteredTypes[uid]
				else:
					raise UnknownTypeID, uid
				pickle = None
				call = StartPickle()
				try:
					size = funcs[2](call, value)
					WritePickle(call, size, uid)
					funcs[1](call, value)
				finally:
					try:
						pickle = EndPickle(call)
					except:
						pass
				return pickle
			if (type(typecode) == type('')):
				# assume that it is really a pickled set of bytes
				if (len(value) > 0):
					raise TypeError, 'too many arguments'
				typeid = PickleTypeID(typecode)
				self.__typecode = Typecode(None, typeid)
				self.__pickled_value = typecode
			else:
				if (len(value) > 1):
					raise TypeError, 'too many arguments'
				self.__typecode = typecode
				self.__pickled_value = PickleValue(value[0], typecode.id())
		def __cmp__(self, other):
			if (type(other) == type(self)):
				if (self.__pickled_value < other.__pickled_value):
					return -1
				elif (self.__pickled_value == other.__pickled_value):
					return 0
				else:
					return 1
			else:
				return 1
		def __str__(self):
			return '<Pickle ' + str(self.__typecode) + ' ' + repr(self.value()) + '>'
		__repr__ = __str__
		def typecode(self):
			return self.__typecode
		def value(self):
			def UnpickleValue (pickle):
				import sys
				value = None
				call = StartPickle()
				uids = ReadPickle(call, pickle)
				try:
					for uid in uids:
						if (RegisteredTypes.has_key(uid)):
							funcs = RegisteredTypes[uid]
							value = funcs[0](call)
							return value
					raise UnknownTypeID, uid
				finally:
					try:
						EndPickle(call)
					except:
						pass
				return value		
			return (UnpickleValue(self.__pickled_value))
		def bytes(self):
			return self.__pickled_value

	def SizeOfPickle(call, pickle):
		return (SizeOfPickleBytes(call, pickle.bytes()))

	def OutputPickle(call, pickle):
		OutputPickleBytes (call, pickle.bytes())

	def InputPickle(call):
		return (Pickle(InputPickleBytes(call)))

	def SizeOfExtensibleRecord (call, val, base_type_uid):
		tmp = Pickle(Typecode(val.IluTypeName()), val)
		return SizeOfPickle(call, tmp)

	def OutputExtensibleRecord (call, val, base_type_uid):
		tmp = Pickle(Typecode(val.IluTypeName()), val)
		return OutputPickle(call, tmp)

	def InputExtensibleRecord (call, base_type_uid):
		tmp = InputPickle(call)
		# now to check that base_type_uid is one of the types
		# of the pickle...
		uids = PickleTypeIDs(tmp.bytes())
		if not base_type_uid in uids:
			raise BadPickleValue, tmp.bytes()
		else:
			return tmp.value()

	# now register the types
	RegisterPrimitiveType('ilu.byte', InputByte, OutputByte, SizeOfByte)
	RegisterPrimitiveType('ilu.boolean', InputBoolean, OutputBoolean, SizeOfBoolean)
	RegisterPrimitiveType('ilu.shortcardinal', InputShortCardinal, OutputShortCardinal, SizeOfShortCardinal)
	RegisterPrimitiveType('ilu.longcardinal', InputLongCardinal, OutputLongCardinal, SizeOfLongCardinal)
	RegisterPrimitiveType('ilu.cardinal', InputCardinal, OutputCardinal, SizeOfCardinal)
	RegisterPrimitiveType('ilu.shortinteger', InputShortInteger, OutputShortInteger, SizeOfShortInteger)
	RegisterPrimitiveType('ilu.longinteger', InputLongInteger, OutputLongInteger, SizeOfLongInteger)
	RegisterPrimitiveType('ilu.integer', InputInteger, OutputInteger, SizeOfInteger)
	RegisterPrimitiveType('ilu.shortreal', InputShortReal, OutputShortReal, SizeOfShortReal)
	RegisterPrimitiveType('ilu.longreal', InputLongReal, OutputLongReal, SizeOfLongReal)
	RegisterPrimitiveType('ilu.real', InputReal, OutputReal, SizeOfReal)
	RegisterPrimitiveType('ilu.shortcharacter', InputShortCharacter, OutputShortCharacter, SizeOfShortCharacter)
	RegisterPrimitiveType('ilu.character', InputCharacter, OutputCharacter, SizeOfCharacter)
	RegisterPrimitiveType('ilu.pickle', InputPickle, OutputPickle, SizeOfPickle)

	# register the two ILU constructed types

	RegisterPrimitiveType('ilu.CString', InputCString, OutputCString, SizeOfCString)
	RegisterPrimitiveType('ilu.ProtocolErrorDetail', InputEnum, OutputEnum, SizeOfEnum)

else:	# no variant support

	Pickle = None
	Typecode = None

# function to register objects

def RegisterObjectType (type, names, interface_brand, doc_string):
	tp = RegisterClass (type, names, interface_brand, doc_string)
	if HasVariantSupport:
		RegisterIoFuncs (type._IluClass.id(),
				 lambda call, disc=0, t=type: InputObjectID(call, disc, t),
				 lambda call, obj, disc=0, t=type: OutputObjectID(call, obj, disc, t),
				 lambda call, obj, disc=0, t=type: SizeOfObjectID(call, obj, disc, t),
				 type._IluClass.name());
	return tp

# catch and print callout exceptions

import traceback

def DefaultCalloutExceptionHandler (where, etype, val, tb):
	print "ILU runtime caught exception <" + str(etype) + "> in `" + where + "':"
	traceback.print_exception(etype, val, tb)

# SetCalloutExceptionHandler (DefaultCalloutExceptionHandler)

# define the built-in class ilu.CORBA-Object for OMG IDL support

class CORBA_Object(IluObjSurr):
    _IluClass = FormClassRecord(\
	'ilu.CORBA-Object',\
	None,\
	'IDL:omg.org/CORBA/Object:1.0',\
	None,\
	FALSE,\
	TRUE,\
	None,\
	(\
	),\
	(),\
	)

    def _is_a (self, type_id):
	    return IsA (self, type_id)

    def _is_nil (self):
	    return (self == None);

    def _non_existent (self):
	    return (not PingObject (self));

    def _is_equivalent (self, other):
	    return (self == other);

    def _duplicate (self):
	    return self

    def _release (self):
	    pass

    def _hash (self, max):
	    return hash(self) % (max + 1);

    def _get_implementation(self):
	    raise IluUnimplementedMethodError, 'CORBA.Object._get_implementation';

    def _get_interface (self):
	    raise IluUnimplementedMethodError, 'CORBA.Object._get_interface';

__types__={}
__types__['CORBA_Object'] = RegisterObjectType(CORBA_Object, ('ilu', 'CORBA-Object',), 'version 2',
					       'built-in ILU object that serves as the root of OMG IDL object types')

bad_param = "bad param"

def CheckSibling (disc, arg):
	disc_sid, disc_ih = disc.IluObjectID()
	arg_sid, arg_ih = arg.IluObjectID()
	if disc_sid != arg_sid:
		raise bad_param, str(arg) + " not sibling of " + str(disc)

BelowFixedPointMinNumerator = "value less than fixed-point type's minimum numerator value"
AboveFixedPointMaxNumerator = "value greater than fixed-point type's maximum numerator value"

def FixedPointTypeSize (min_num_v, max_num_v):
	if (not min_num_v) or (not max_num_v):
		return 0
	min_num = long(min_num_v)
	max_num = long(max_num_v)
	if (min_num >= 0 and max_num <= 255):
		return 1
	elif (min_num >= 0 and max_num <= 0xFFFF):
		return 2
	elif (min_num >= 0 and max_num <= 0xFFFFFFFF):
		return 3
	elif (min_num >= 0 and max_num <= 0xFFFFFFFFFFFFFFFFL):
		return 4
	elif (min_num >= -0x8000 and max_num <= 0x7FFF):
		return 5
	elif (min_num >= -0x80000000L and max_num <= 0x7FFFFFFFL):
		return 6
	elif (min_num >= -0x8000000000000000L and max_num <= 0x7FFFFFFFFFFFFFFFL):
		return 7
	else:
		return 0

class FixedPointType:

	# cache for values needed for kernel interaction, initially None

	def sizeof(self, call):
		kv = KernelBignumForValue(self.numerator)
		return SizeOfFixedpoint(call, kv,
					self.__class__._ilu_bignum_value_for_min_num,
					self.__class__._ilu_bignum_value_for_max_num,
					self.__class__._ilu_bignum_value_for_denom,
					self.__class__._corba_fixed_digits,
					self.__class__._corba_fixed_decimal_places,
					self.__class__._size)

	def output(self, call):
		kv = KernelBignumForValue(self.numerator)
		OutputFixedpoint(call, kv,
				 self.__class__._ilu_bignum_value_for_min_num,
				 self.__class__._ilu_bignum_value_for_max_num,
				 self.__class__._ilu_bignum_value_for_denom,
				 self.__class__._corba_fixed_digits,
				 self.__class__._corba_fixed_decimal_places,
				 self.__class__._size)

	def __str__(self):
		return str(self.numerator) + '/' + str(self.__class__._denom)

	def __repr__(self):
		return str(self.numerator) + '/' + str(self.__class__._denom)

	def __cmp__(self, other):
		if (type(self) == type(other)):
			return (cmp(float(self.numerator)/float(self.__class__._denom), float(other.numerator)/float(other.__class__.denom)))
		else:
			try:
				val = float(other)
			except:
				return -1
			return cmp(float(self.numerator)/float(self.__class__._denom), val)

	def __add__ (self, other):
		import types
		if (int(other) == other):
			return self.__class__(self.numerator + other * self.__class__._denom)
		if (long(other) == other):
			return self.__class__(self.numerator + other * self.__class__._denom)
		elif (type(other) == types.FloatType):
			return self.__class__(self.numerator + long(float(other) * self.__class__._denom))
		else:
			raise ValueError

	def __mul__ (self, other):
		import types
		if (int(other) == other):
			return self.__class__(self.numerator * other * self.__class__._denom)
		if (long(other) == other):
			return self.__class__(self.numerator * other * self.__class__._denom)
		elif (type(other) == types.FloatType):
			return self.__class__(self.numerator * long(float(other) * self.__class__._denom))
		else:
			raise ValueError

	def __int__(self):
		return self.numerator / self.__class__._denom
		
	def __float__(self):
		return float(self.numerator)/float(self.__class__._denom)

	def __long__(self):
		return long(self.numerator)/long(self.__class__._denom)

def InputFixedPoint (call, type):
	return type(InputFixedpoint (call,
				     type._ilu_bignum_value_for_min_num,
				     type._ilu_bignum_value_for_max_num,
				     type._ilu_bignum_value_for_denom,
				     type._corba_fixed_digits,
				     type._corba_fixed_decimal_places,
				     type._size))

def OutputFixedPoint (call, value):
	value.output(call);

def SizeOfFixedPoint (call, value):
	return value.sizeof(call);
