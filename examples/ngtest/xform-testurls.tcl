
# for each of the model urlfiles, if an equivalent file for
# current host exists, do nothing.  Otherwise create an equivalent
# file for this host
proc transform-urls {} {
  global thishost
  set pwd [pwd]
  if {[file isdirectory "url-test-material"] != 1} {
     puts "Not able to set up url files for this host ...""
     puts "not in ILUHOME/examples/ngtest directory"
     return 0
  }

  cd "url-test-material/ngtesturls"

  set modelhost "modelhost"
  set findglob [format "mscape_%s_*" $modelhost]

  set urlfiles [glob -nocomplain  $findglob]
  if { [llength $urlfiles] == 0} {
     puts "Not able to set up url files for this host ... no model files"
     cd $pwd
     return 0
  }

  if {$thishost == ""} {
     puts "Not able to edit url files because"
     puts "environment variable thishost not set"
     cd $pwd
     return 0
  }

  foreach urlf $urlfiles { 
     regsub -all $modelhost $urlf $thishost newurlf
     if { [file exists $newurlf] != 1 } {
        set newfileID [open $newurlf "w"]
        set oldfileID [open $urlf "r"]
        while { [gets $oldfileID url] >= 0} { 
           regsub -all $modelhost $url $thishost newurl
           puts $newfileID $newurl
        }
        close $newfileID
     }
  }
  cd $pwd
  return 1
}
