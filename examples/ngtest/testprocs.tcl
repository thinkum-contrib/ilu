

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


############################################################################
proc run-nggetbot {site protocol transport batch serial} {
   global serverlog  serverPID  clientlog  testlog  iserror defaultport
   global thishost

  exec echo "" >>& $clientlog 

  switch -exact $site {
   "mscape" {} 
   "aol" - 
   "bu" - 
   "ms" - 
    default {
       set iserror 1
       puts "Site parameter currently restricted to mscape"
       return 0
   }
  }

  set pwd [pwd]
  set urlf [format "$pwd/url-test-material/ngtesturls/%s" $site]
  set urlf [format "%s_%s_w3ng" $urlf $thishost]
  set urlf [format "%s_%s" $urlf $protocol]
  if {$transport != "tcp"} {
     set urlf [format "%s_%s" $urlf $transport]
  }
  set urlf [format "%s_%s" $urlf $defaultport]

  set urlf [format "%s.urls" $urlf ] 

  if {[file exists $urlf] != 1} { 
       set iserror 1
       puts "File $urlf implied by parameters not available"
       return 0 
  }
 
  set cmd [format "exec ./nggetbot -urls %s" $urlf]

  if {$batch == "batch"} {
     set cmd [format "%s -batch 50000" $cmd]
  }

  if {$serial == "serial"} { 
     set cmd [format "%s -pipeline -serial" $cmd]
  }

  if {$protocol == "http10"} { 
     set cmd [format "%s -httppinfo http_1_0 -httptinfo tcp_0_8080 -ngpinfo http_1_0 -ngtinfo" $cmd]
  }

  set cmd [format "%s -logfile %s -v 2 -mt 10 >>& %s" $cmd $clientlog $clientlog]

  puts " "
  puts "Starting nggetbot client with cmd $cmd " 
  exec echo " " >>&clientlog
  exec echo "Running nggetbot client with cmd $cmd " >>& $clientlog 

  set cmd [join $cmd]

  set cstatus [catch {eval $cmd}]

  if {$cstatus == 0} {  
    puts "nggetbot client program successful"   
  } else { 
    set iserror 1
    puts "nggetbot client program reports errors:"
    puts "   logs are $testlog client server"
  }
}


############################################################################
proc stop-server {keep} {
 global serverlog serverPID clientlog testlog iserror killsignal

 exec kill $killsignal $serverPID 

 if {$iserror == 0} {
     puts " " 
     if {$keep != "keep"} {
       exec rm $clientlog
       exec rm $serverlog
   }
 }
}

############################################################################
proc start-ngwebserver {protocol transport batch serial} {
 global serverlog  serverPID  clientlog  testlog  iserror 

 set pwd [pwd]
 set httppinfo " "
 set httptinfo  "-httptinfo tcp_0_8080"
 set ngpinfo " "
 set ngtinfo " "
 switch -exact $protocol {
   "http11" { 
        set ngpinfo  "-ngpinfo http_1_1"
        set ngtinfo "-ngtinfo tcp_0_2718"
    }
   "http10" {
        set httppinfo "-httppinfo http_1_0"
        set ngpinfo "-ngpinfo http_1_0"
        set ngtinfo "-ngtinfo"
   }
   "iiop" {
        set ngpinfo "-ngpinfo iiop"
        set ngtinfo "-ngtinfo"
        if { $transport != "tcp"} {  
           if { $transport == "w3mux" } {
              set ngtinfo "-ngtinfo $transport" 
           } else {
              set iserror 1
              puts "Transport for iiop protocol must be w3mux or tcp"
              return 0
           }
       }
    }
   "w3ng" {
        if { $transport == "sunrpcrm"} {
           set ngtinfo "-ngtinfo sunrpcrm"
        } else  {
          if {$transport != "w3mux"} {
              set iserror 1
              puts "Transport for w3ng protocol must be sunrpcrm or w3mux"
              return 0
           }
        }
   }
   default {
        puts "Protocol argument $protocol error"
        set iserror 1
        return 0
   }
  }

 set cmd "exec ./ngwebserver -filebase $pwd/url-test-material/serverdocs"

 set cmd [format "%s -verbose 2" $cmd]

 if {$batch == "batch"}  {
   set cmd [format "%s -batch 50000" $cmd]
 }

 if {$serial != "serial"} {
     set cmd [format "%s -fdbudget 128" $cmd]
  }

 set cmd [format "%s %s %s %s %s" $cmd $httppinfo $httptinfo $ngpinfo $ngtinfo]
 
 set cmd [format "%s -logfile %s >>& %s &" $cmd $serverlog $serverlog]
 
 puts " " 
 puts "Starting ngwebserver with cmd: $cmd"
 exec echo "" >>& $clientlog 
 set cmd [join $cmd]
 set sstatus [ catch {eval $cmd} serverPID]
 exec sleep 5
 if {$sstatus == 0} {
   return 1
 } else {
    set iserror 1
    puts "Couldnt start server. Logs are $testlog client server"
    return 0
  }

}

############################################################################
#w3ng url scheme only 
proc run-pair {site protocol transport batch serial {keep "nokeep"} } {
 global iserror

  set iserror 0
  start-ngwebserver $protocol $transport $batch $serial
  if {$iserror == 0} {
     run-nggetbot $site $protocol $transport $batch $serial
     stop-server $keep
  } else {
     stop-server $keep
  }
}

############################################################################
#single canned http scheme access pair
proc run-http-pair {batch serial {keep {"nokeep"}} } {
  global testlog serverlog clientlog serverPID thishost

  set iserror 0
  set pwd [pwd]
  set cmd "exec ./ngwebserver -filebase $pwd/url-test-material/serverdocs"
    
  if {$batch == "batch"} {
      set cmd [format "%s -batch 50000" $cmd]
  }

  set cmd [format "%s -httptinfo tcp_0_8080" $cmd]
  set cmd [format "%s -logfile %s >>& %s &" $cmd $serverlog $serverlog]
  puts " "
  puts "Starting ngwebserver; cmd is $cmd"
  exec echo "" >>& $clientlog 
  set cmd [join $cmd]
  set sstatus [ catch {eval $cmd} serverPID]
  exec sleep 5
  if {$sstatus == 0} {
    puts "started webserver"
  } else {
    set iserror 1
    puts "Couldnt start server. Logs are $testlog client server"
    return $iserror
  }

  set urlf [format "$pwd/url-test-material/ngtesturls/mscape"]
  set urlf [format "%s_%s_http_http_8080.urls" $urlf $thishost]

  set cmd [format "exec ./nggetbot -urls %s" $urlf]

  if {$batch == "batch"} {
     set cmd [format "%s -batch 50000" $cmd]
  }

  if {$serial == "serial"} {
     set cmd [format "%s -pipeline -serial" $cmd]
  }
# do something for fdbudget

  set cmd [format "%s -logfile %s -v 2 -mt 10 >>& %s" $cmd $clientlog $clientlog]

  puts $cmd

  exec echo "Running nggetbot client with cmd $cmd " >>& $clientlog

  set cmd [join $cmd]

  set cstatus [catch {eval $cmd}]
  if {$cstatus == 0} {
    puts "nggetbot client program successful"
  } else {
    set iserror 1
    puts "nggetbot client program reports errors:"
    puts "   logs are $testlog client server"
  }

  stop-server $keep
  return $iserror
}

############################################################################
proc run-some-tests {} {
  global iserror defaultport clientlog serverlog

   run-pair "mscape" "w3ng" "w3mux" "batch" "none" 
   if {$iserror != 0} {
       return 1
   }
   run-pair "mscape" "http11" "tcp" "batch" "serial"
   if {$iserror != 0} {
       return 1
   }
   run-pair "mscape" "w3ng" "w3mux" "batch" "serial"
   if {$iserror != 0} { 
       return 1
   }
   run-pair "mscape" "w3ng" "sunrpcrm" "batch"  "serial"
   if {$iserror != 0} {
       return 1
   }
   run-pair "mscape" "w3ng" "w3mux" "none" "none"
   if {$iserror != 0} {
       return 1
   }
   run-pair  "mscape" "iiop" "tcp" "batch" "none"
   if {$iserror != 0} {
       return 1
   }

   run-http-pair "batch" "serial"
   if {$iserror != 0} {
       return 1
   }

   run-pair "mscape" "http10" "tcp" "none" "none"
   if {$iserror != 0} {
      return 1
   }

   return 0
}
