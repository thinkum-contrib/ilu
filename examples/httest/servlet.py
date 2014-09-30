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
# $Id: servlet.py,v 1.3 1999/08/03 01:58:24 janssen Exp $
#

import ilu, iluhttp__skel
from iluhttp import *

#
# A class which implements the GET method
#

def getParameter(postbody, pname):
	import string
	parms = string.split(postbody, '&')
	match = pname + '='
	for parm in parms:
		if (string.find(parm, match) == 0):
			return parm[len(match):]

class SimpleServlet (iluhttp__skel.Resource) :

	def __init__(self, ih, server):

		# this __init__ method will be built-in in 2.0alpha15, but is
		#  still necessary in 2.0alpha14

		self.IluInstHandle = ih
		self.IluServer = server

	def GET (self, request):

		title = "Simple Servlet Output"

		# A Response has three fields:  a status code, a list of headers, and
		#  an optional body

		return Response (StatusCode.OK,		# status code

				 # list of headers
				 [Header("Content-Type", "text/html")],

				 # optional body
				 "<HTML><HEAD><TITLE>" + title +
				 "</TITLE></HEAD><BODY>" +
				 "<H1>" + title + "</H1>" +
				 "<P>This is output from SimpleServlet.\n" +
				 "</BODY></HTML>\r\n");

	def POST (self, request):

		# you might access this method from an HTML page that looks like:
                #
		#  <form action="http://myhost:8080/" method=POST>
                #    your name, please: <input type=text name=name size=20>
                #  <br>
                #  <input type=submit value="say hello">
                #  <hr>
		#

		name = getParameter(request.body, "name");
		return Response (StatusCode.OK,
				 [Header("Content-Type", "text/html")],
				 "Hello " + name)

# Set up a server listening with HTTP 1.0 on port 8080

server = ilu.CreateServer(None,("tcp_0_8080",),"http_1_0")

# Create an instance of the servlet class in that server, with the name "/".
#  Note that you could export multiple instances of various servlet classes
#  from this single server.  But we'll just do one in this example.

obj = SimpleServlet("/", server)

# Export the instance

obj.IluSBH()

# And run the event loop

ilu.RunMainLoop(ilu.CreateLoopHandle())

