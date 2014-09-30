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
#   $Id: CORBA.py,v 1.20 1999/08/03 01:55:43 janssen Exp $
#
#######################################################################
#
#  This module exports some of the functionality expected of a CORBA
#  system...
#
#######################################################################
import iluRt

if iluRt.CORBAMapping:

	from CORBAExceptions import *

	if iluRt.HasVariantSupport:

		class TypeCode (iluRt.Typecode):
			def __init__(self, typename):
				def convert_corba_typename (typename):
					import string
					corbaname = string.translate(typename, string.maketrans('_', '-'))
					separator = '.'
					double_colon = string.find (corbaname, '::')
					if (double_colon < 0):
						return typename
					iluname = corbaname[:double_colon]
					corbaname = corbaname[double_colon + 2:]
					double_colon = string.find (corbaname, '::')
					while (double_colon >= 0):
						iluname = iluname + separator + corbaname[:double_colon]
						if separator == '.': separator = '-'
						corbaname = corbaname[double_colon + 2:]
						double_colon = string.find (corbaname, '::')
					iluname = iluname + separator + corbaname
					return iluname				

				if (typename[0:3] == 'IDL:'):
					iluRt.Typecode.__init__(self, None, typename)
				else:
					tname = convert_corba_typename(typename)
					iluRt.Typecode.__init__(self, tname)

		class Any (iluRt.Pickle):
			def __init__(self, typecode, value):
				iluRt.Pickle.__init__(self, typecode, value)

if iluRt.CORBAMapping:
	class InvalidName (UserException):
		# raised to indicate a bad name passed to resolve_initial_references
		pass
else:
	InvalidName = "CORBA.InvalidName"

DefaultInitialReference = None
InitialReferences = {}

import regex
InitRefPattern = regex.compile('\([^=]+\)=\(.*\)')

class ORB:

	def object_to_string (self, obj):
		return iluRt.IOROfObject(obj)

	def string_to_object (self, str, putativeClass=iluRt.CORBA_Object):
		return iluRt.ObjectOfSBH(putativeClass, str)

	def resolve_initial_references (self, str):
		import types
		if (InitialReferences.has_key(str)):
			value = InitialReferences[str]
			if (type(value) == types.StringType):
				return iluRt.ObjectOfSBH(iluRt.CORBA_Object, value)
			elif (type(value) == types.InstanceType):
				return value
			else:
				raise InvalidName
		elif DefaultInitialReference:
			value = DefaultInitialReference + '/' + str
			return iluRt.ObjectOfSBH(iluRt.CORBA_Object, value)
		else:
			raise InvalidName

theORB = ORB()

Object = iluRt.CORBA_Object

def ORB_init (argv=(), orb_id='ilu'):
	import os
	global DefaultInitialReference
	if orb_id != 'ilu':
		raise 'unknown orb_id', orb_id
	if (os.environ.has_key("ILU_COS_NAMING_IOR")):
		try:
			import CosNaming
			naming = iluRt.ObjectOfSBH(CosNaming.NamingContext, os.environ["ILU_COS_NAMING_IOR"])
			if naming:
				InitialReferences["NameService"] = naming
		except:
			import sys
			sys.stderr.write("CORBA.ORB_init:  Note:  Can't bind to COS Naming service specified in value of environment variable ILU_COS_NAMING_IOR.\n")
			pass
	i = 0
	while i < len(argv):
		if argv[i] == '-ORBInitRef':
			if (i + 1) >= len(argv):
				raise 'bad ORB_init parameter list', argv
			else:
				import regex
				i = i + 1
				if (InitRefPattern.match(argv[i]) < 0):
					raise 'bad ORBInitRef parameter', argv[i]
				matchname, matchurl = InitRefPattern.group(1,2)
				InitialReferences[matchname] = matchurl
				del argv[i-1:i+1]
		elif argv[i] == '-ORBDefaultInitRef':
			if (i + 1) >= len(argv):
				raise 'bad ORB_init parameter list', argv
			else:
				i = i + 1
				DefaultInitialReference = argv[i]
				del argv[i-1:i+1]
		i = i + 1
	return theORB



			
