#! /bin/ksh

CMD=${0##*/}\>

# determine the default path to SML/NJ
if [ -x /usr/local/smlnj/bin/sml ] ; then
  SML_PATH=/usr/local/smlnj/bin/sml
elif [ -x /usr/local/bin/sml ] ; then
  SML_PATH=/usr/local/bin/sml
else
  SML_PATH=sml
fi
SMLX=${SML:-"$SML_PATH"}

function printUsage {
 echo -u2 "doallseq.sh"
 echo -u2 "    [-sml <executable>    default=value of \$SML or $SMLX]"
 echo -u2 "    [-help]"
}

while [[ $# -ne 0 ]]
do
  	arg=$1; shift
	case $arg in
	  -sml)
		if [[ $# -eq 0 ]]
		then
			echo -u2 $CMD must name executable with -sml option
			exit 1
		fi
		SMLX=$1; shift
		;;
	  -help)
		 printUsage
		 exit 0
	        ;;
	  *)
	        echo -u2 ${CMD} bad option $arg
	        printUsage
		exit 1
       esac
done
bin/dotest.sh coresml -diff -sml $SMLX
bin/dotest.sh typing -diff -sml $SMLX
bin/dotest.sh modules -diff -sml $SMLX
bin/dotest.sh basis -diff -sml $SMLX
bin/dotest.sh bugs -diff -sml $SMLX
