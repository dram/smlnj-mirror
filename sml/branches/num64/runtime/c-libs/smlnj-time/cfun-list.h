/*! \file cfun-list.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"SMLNJ-Time"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"December 16, 1994"
#endif

CFUNC("gettime",	_ml_Time_gettime,		"")
CFUNC("gettime_64",	_ml_Time_gettime_64,		"")
CFUNC("timeofday",	_ml_Time_timeofday,		"")
CFUNC("timeofday_64",	_ml_Time_timeofday_64,		"")
CFUNC("time_in_seconds", _ml_Time_time_in_seconds,	"")
