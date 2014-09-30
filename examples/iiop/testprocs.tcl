
# BeginILUCopyright

# Copyright (c) 1991-1999 Xerox Corporation.  All Rights Reserved.

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

# EndILUCopyright

#adapted from test1/testprocs

proc run-client  { {args {}} } {
   global serverlog  serverPID  clientlog  testlog  iserror 
   global iluhome bindingdir

  puts "Running C $args client" 
  exec echo "" >>& $clientlog 
  exec echo "Running C $args client" >>& $clientlog 
  set cmd "exec ./client $args >>& $clientlog"
  set cmd [join $cmd]  
  set cstatus [catch {eval $cmd} ]
  if {$cstatus == 0} {  
    puts "C client program successful"   
  } else { 
    set iserror 1
    puts "C client program reports errors:"
    puts "   logs are $testlog client server, bindingdir is $bindingdir"
  }
}

proc stop-server {} {
 global serverlog serverPID clientlog testlog iserror bindingdir

 exec kill $serverPID 

 if {$iserror==0} {
     exec rm $clientlog
     exec rm $serverlog
     exec rm -r -f $bindingdir
 } 

}

proc start-server { {args {}} } {
 global serverlog  serverPID  clientlog  testlog  iserror 
 global iluhome bindingdir ior

 exec rm -r -f $bindingdir
 exec mkdir $bindingdir
 puts "Starting C server for $args (waiting a bit for it to start up)..."
 exec echo "" >>& $clientlog 
 set cmd "exec ./server $args >>& $serverlog &"
 set cmd [join $cmd]
 set sstatus [ catch {eval $cmd} serverPID]
 if {$sstatus == 0} {  
    puts "started C server"   
    exec sleep 10
#   now get ior for use in subsequent client test if desired  
    set serverfile [open $serverlog r]
    if {$serverfile == ""} {
       puts  "couldn't open server log for read"
       return 0
   } 
   gets $serverfile ior
#  remove "exported" stuff 
   if {$ior == ""} { 
       puts  "exported not first line in server log, can't proceed"
       return 0
   } 
    set i [string first "exported" $ior]  
   if { $i < 0 } {
       puts  "exported not first line in server log, can't proceed"
       return 0
   }
   set j [string length "exported"]
   set i [expr $i + [string length "exported"] + 1]
   set ior [string range $ior $i end] 
   return 1
 } else { 
    set iserror 1
    puts "Couldn't start server"
    puts "   logs are $testlog client server, bindingdir is $bindingdir"
    return 0
  }

}

proc run-regression {} {
 global ior

 if [start-server] { 
  run-client 
  run-client "-n 10"
  run-client "-n 10 -O $ior"

  stop-server 
 }

}

