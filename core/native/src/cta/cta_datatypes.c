/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_datatypes.c $
$Revision: 3406 $, $Date: 2012-08-16 15:25:53 +0200 (Thu, 16 Aug 2012) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2005  Nils van Velzen

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <string.h>
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_datatypes.h"
#include "cta_message.h"

#define CTA_SIZEOF_F77  F77_CALL(cta_sizeof,CTA_SIZEOF)
#define CLASSNAME "CTA_Datatypes"

#undef METHOD
#define METHOD "SizeOf"
int CTA_SizeOf(CTA_Datatype datatype, int *size){

   switch(datatype){
   case CTA_HANDLE:
   case CTA_INTEGER:
   case CTA_STRING:
      *size=sizeof(int);
      break;
   case CTA_REAL:
      *size=sizeof(float);
      break;
   case CTA_DOUBLE:
      *size=sizeof(double);
      break;
   default:
      CTA_WRITE_ERROR("This datatype is unknown."); 
      return CTA_ILLEGAL_DATATYPE;
      break;
   }
   return CTA_OK;
}

const char *CTAI_Type2String(CTA_Datatype datatype) {
   switch(datatype){
   case CTA_HANDLE:
      return "CTA_HANDLE";
   case CTA_INTERFACE:
      return "CTA_INTERFACE";
   case CTA_FUNCTION:
      return "CTA_FUNCTION";
   case CTA_VECTOR:
      return "treeVectorLeaf";
   case CTA_VECTORCLASS:
      return "CTA_VECTORCLASS";
   case CTA_TREEVECTOR:
      return "treeVector";
   case CTA_SUBTREEVECTOR:
      return "subTreeVector";
   case CTA_MATRIXCLASS:
      return "CTA_MATRIXCLASS";
   case CTA_MATRIX:
      return "CTA_MATRIX";
   case CTA_COVMATCLASS:
      return "CTA_COVMATCLASS";
   case CTA_COVMAT:
      return "CTA_COVMAT";
   case CTA_INTPOL:
      return "CTA_INTPOL";
   case CTA_OBS:
      return "CTA_OBS";
   case CTA_MODELCLASS:
      return "CTA_MODELCLASS";
   case CTA_MODEL:
      return "CTA_MODEL";
   case CTA_TIME:
      return "CTA_TIME";
   case CTA_SOBS:
      return "CTA_SOBS";
   case CTA_SOBSCLASS: /* and CTA_OBSDESCRCLASS */
      return "CTA_SOBSCLASS";
   case CTA_OBSDESCR:
      return "CTA_OBSDESCR";
   case CTA_METHODCLASS:
      return "CTA_METHODCLASS";
   case CTA_METHOD:
      return "CTA_METHOD";
   case CTA_TREE:
      return "CTA_TREE";
   case CTA_VOID:
      return "CTA_VOID";
   case CTA_INTEGER:
      return "CTA_INTEGER";
   case CTA_REAL:
      return "CTA_REAL";
   case CTA_DOUBLE:
      return "CTA_DOUBLE";
   case CTA_FSTRING:
      return "CTA_FSTRING";
   case CTA_CSTRING:
      return "CTA_CSTRING";
   case CTA_STRING:
      return "CTA_STRING";
   case CTA_FILE:
      return "CTA_FILE";
   case CTA_1DINTEGER :
      return "CTA_1DINTEGER";
   case CTA_1DREAL:
      return "CTA_1DREAL";
   case CTA_1DDOUBLE:
      return "CTA_1DDOUBLE";
   case CTA_1DFSTRING:
      return "CTA_1DFSTRING";
   case CTA_1DCSTRING:
      return "CTA_1DCSTRING";
   case CTA_ARRAY:
      return "CTA_ARRAY";
   }
   return "CTA_ILLEGAL_DATATYPE";
}

CTA_Datatype CTAI_String2Type(const char *dt) {
   if (0 == strcmp("CTA_HANDLE", dt)) {
      return CTA_HANDLE;
   } else if (0 == strcmp("CTA_INTERFACE", dt)) {
      return CTA_INTERFACE;
   } else if (0 == strcmp("CTA_FUNCTION", dt)) {
      return CTA_FUNCTION;
   } else if (0 == strcmp("CTA_VECTOR", dt)) {
      return CTA_VECTOR;
   } else if (0 == strcmp("treeVectorLeaf", dt)) {
      return CTA_VECTOR;
   } else if (0 == strcmp("CTA_VECTORCLASS", dt)) {
      return CTA_VECTORCLASS;
   } else if (0 == strcmp("CTA_TREEVECTOR", dt)) {
      return CTA_TREEVECTOR;
   } else if (0 == strcmp("treeVector", dt)) {
      return CTA_TREEVECTOR;
   } else if (0 == strcmp("subTreeVector", dt)) {
      return CTA_TREEVECTOR;
   } else if (0 == strcmp("CTA_MATRIXCLASS", dt)) {
      return CTA_MATRIXCLASS;
   } else if (0 == strcmp("CTA_MATRIX", dt)) {
      return CTA_MATRIX;
   } else if (0 == strcmp("CTA_COVMATCLASS", dt)) {
      return CTA_COVMATCLASS;
   } else if (0 == strcmp("CTA_COVMAT", dt)) {
      return CTA_COVMAT;
   } else if (0 == strcmp("CTA_INTPOL", dt)) {
      return CTA_INTPOL;
   } else if (0 == strcmp("CTA_OBS", dt)) {
      return CTA_OBS;
   } else if (0 == strcmp("CTA_MODELCLASS", dt)) {
      return CTA_MODELCLASS;
   } else if (0 == strcmp("CTA_MODEL", dt)) {
      return CTA_MODEL;
   } else if (0 == strcmp("CTA_TIME", dt)) {
      return CTA_TIME;
   } else if (0 == strcmp("CTA_SOBS", dt)) {
      return CTA_SOBS;
   } else if (0 == strcmp("CTA_SOBSCLASS", dt)) {
      return CTA_SOBSCLASS;
   } else if (0 == strcmp("CTA_OBSDESCRCLASS", dt)) {
      return CTA_OBSDESCRCLASS;
   } else if (0 == strcmp("CTA_OBSDESCR", dt)) {
      return CTA_OBSDESCR;
   } else if (0 == strcmp("CTA_METHODCLASS", dt)) {
      return CTA_METHODCLASS;
   } else if (0 == strcmp("CTA_METHOD", dt)) {
      return CTA_METHOD;
   } else if (0 == strcmp("CTA_TREE", dt)) {
      return CTA_TREE;
   } else if (0 == strcmp("CTA_VOID", dt)) {
      return CTA_VOID;
   } else if (0 == strcmp("CTA_INTEGER", dt)) {
      return CTA_INTEGER;
   } else if (0 == strcmp("CTA_REAL", dt)) {
      return CTA_REAL;
   } else if (0 == strcmp("CTA_DOUBLE", dt)) {
      return CTA_DOUBLE;
   } else if (0 == strcmp("CTA_FSTRING", dt)) {
      return CTA_FSTRING;
   } else if (0 == strcmp("CTA_CSTRING", dt)) {
      return CTA_CSTRING;
   } else if (0 == strcmp("CTA_STRING", dt)) {
      return CTA_STRING;
   } else if (0 == strcmp("CTA_FILE", dt)) {
      return CTA_FILE;
   } else if (0 == strcmp("CTA_1DINTEGER ", dt)) {
      return CTA_1DINTEGER;
   } else if (0 == strcmp("CTA_1DREAL", dt)) {
      return CTA_1DREAL;
   } else if (0 == strcmp("CTA_1DDOUBLE", dt)) {
      return CTA_1DDOUBLE;
   } else if (0 == strcmp("CTA_1DFSTRING", dt)) {
      return CTA_1DFSTRING;
   } else if (0 == strcmp("CTA_1DCSTRING", dt)) {
      return CTA_1DCSTRING;
   } else if (0 == strcmp("CTA_ARRAY", dt)) {
      return CTA_ARRAY;
   }
   return CTA_NULL;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_SIZEOF_F77 (int *datatype, int *size, int *ierr){
   *ierr=CTA_SizeOf((CTA_Datatype) *datatype, size);
}



