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

# checkout source target
checkout(){
    source=$1/trunk
    target=$2
    if [ ! -d $target ] ; then
	echo Checking out $source as $target
	svn $command $source $target
    else
	echo Tree $target already exists.
    fi
}

# checkout from smlnj tree
smlnj(){
    checkout $smlnj/$1/trunk $1
}

smlnjbase(){
    checkout $smlnj/$1/trunk $2
}

smlnj config
smlnjbase sml base
smlnj smlnj-lib
smlnj MLRISC
smlnj ml-yacc
smlnj trace-debug-profile
smlnj pgraph

smlnj ckit
smlnj cml
smlnj eXene
smlnj smlnj-c
smlnj ml-burg
smlnj ml-lex
smlnj heap2asm
smlnj nlffi

checkout $gf/ml-lpt ml-lpt
