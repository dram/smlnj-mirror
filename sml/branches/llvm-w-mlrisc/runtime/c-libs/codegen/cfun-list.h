/*! \file cfun-list.h
 *
 * \author John Reppy
 *
 * This file lists the directory library of C functions that are callable by ML.
 */

/*
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"CodeGen"
#define CLIB_VERSION	"1.0"
#define CLIB_DATE	"January 4, 2020"
#endif

CFUNC("generate",		_ml_CodeGen_generate,		"")
