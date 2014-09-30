#!/bin/sh
#
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
# $Id: fqdn.sh,v 1.6 1999/08/03 01:59:22 janssen Exp $
# 
# Determine the current Fully Qualified Domain Name.
# Returns 0, and prints FQDN to stdout, for success; returns 1 otherwise
#
get_fqdn() {
    domain=null

    myhost=`uname -n 2>/dev/null`

    # If uname did not get a fully qualified name, try hostname...

    case $myhost in
    *.* )
	;;
    * )
	myhost=`hostname 2>/dev/null`
	# If hostname failed entirely, go back to uname.
	if [ x$myhost = x ]; then
	    myhost=`uname -n 2>/dev/null`
	fi
	;;
    esac

    # If uname or hostname did not get a fully qualified name, try nslookup...

    case $myhost in
    *.* )
	fulldname=$myhost
	;;
    * )
	type nslookup > /dev/null 2>&1
	if [ $? = 0 ] ; then
		type awk > /dev/null 2>&1
		if [ $? = 0 ] ; then
			fulldname=`nslookup $myhost 2>&1 | awk '/Name:/ {print $2}'`
		fi
	fi;
    esac

    # if we didn't get it from uname|hostname+nslookup, try the smtp daemon...

    case $fulldname in
    *.* )
	;;
    * )
	# check the banner message from the smtp server...
	fulldname=`(sleep 1; echo quit) |telnet localhost smtp 2>&1 | \
		sed -n 's/^.*220[ -]\([^ ]*\) .*$/\1/p'`
    esac

    # trim off the first field, which is the machine name...

    domain=`echo "$fulldname" | sed 's/^[^.]*\.//g'`

    # if we don't have anything left, we failed...

    [ "$domain" = "" ] && return 1

    # but if we do, we succeeded, and "domain" is properly bound!

    return 0
}

get_fqdn; [ $? != 0 ] && exit 1
echo $domain
exit 0
