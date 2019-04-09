#! /bin/ksh
#
# retarget.sh -- does a retarget for all the supported backend/OS 
#		combinations.
#

SML=../../bin/sml-cm

function retarget
{
  if [[ -d bin.$1-$2 ]] then
    echo retarget.sh:: bin.$1-$2 already exists.
  else
    $SML <<XXX
      CMR.retarget{bindir="bin.mipseb-unix", cpu="$1", os="$2"};
      CMB.make();
XXX
  fi      
}


retarget x86 win32 1>/dev/null 2>&1 
retarget hppa unix 1>/dev/null 2>&1 

retarget alpha32 unix 1>/dev/null 2>&1 
retarget sparc unix 1>/dev/null 2>&1 

retarget rs6000 unix 1>/dev/null 2>&1 
retarget x86 unix 1>/dev/null 2>&1 

retarget alpha32x unix 1>/dev/null 2>&1 

