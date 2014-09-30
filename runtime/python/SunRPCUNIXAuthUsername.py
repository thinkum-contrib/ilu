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

import nis, string

error = "error"

def username(passport):
	if not passport.has_key('sunrpc-unix'):
		raise error, 'no Sun RPC UNIX identity in ILU passport'
	SunRPCAuth = passport['sunrpc-unix']
	map = nis.cat('hosts.byaddr')
	if not map:
		raise error, 'no YP map "hosts.byaddr"'
	map = nis.cat('passwd.byuid')
	if not map:
		raise error, 'no YP map "passwd.byuid"'
	uid = str(SunRPCAuth['uid'])
	if not map.has_key(uid):
		raise error, 'unknown user ID ' + uid
	record = string.splitfields(map[uid], ':')
	return (record[4])

