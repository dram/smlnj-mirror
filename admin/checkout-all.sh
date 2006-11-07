#!/bin/sh

command=checkout

if [ $# -ge 1 ] ; then
    case $1 in
	--export|-e)
	    command=export
	    shift
	    ;;
	*)
	    ;;
    esac
fi

if [ $# -ge 1 ] ; then
    mkdir $1
    cd $1
fi

gf=svn://smlnj-gforge.cs.uchicago.edu
smlnj=$gf/smlnj

checkout(){
    if [ $# = 2 ] ; then
	target=$2
	case $1 in
	    svn://* )
		source=$1
		;;
	    * )
		source=$smlnj/$1/trunk
		;;
	esac
    else
	source=$smlnj/$1/trunk
	target=$1
    fi
    if [ ! -d $target ] ; then
	echo Checking out $source as $target.
	svn $command $source $target
    else
	echo Tree $target already exists.
    fi
}


checkout config
checkout sml base
checkout smlnj-lib
checkout MLRISC
checkout ml-yacc
checkout trace-debug-profile
checkout pgraph

checkout ckit
checkout cml
checkout eXene
checkout smlnj-c
checkout lexgen
checkout ml-burg
checkout ml-lex
checkout heap2asm
checkout nlffi

checkout $gf/ml-lpt ml-lpt
