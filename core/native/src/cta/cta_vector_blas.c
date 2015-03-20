/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_vector_blas.c $
$Revision: 4445 $, $Date: 2014-06-03 08:56:12 +0200 (Tue, 03 Jun 2014) $

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
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <string.h>
#include "cta_mem.h"
#include "cta_flush.h"
#include "cta_file.h"
#include "cta_errors.h"
#include "cta_string.h"
#include "cta_vector_blas.h"
#include "ctai.h"
#include "ctai_handles.h"
#include "cta_message.h"

/* Maximum length of a path index */
#define MAXIXLEN (31)

#define IDEBUG (0)
#define CLASSNAME "CTA_Vector_blas" 

/*
This file contains an implementation of the CTA_VECTOR_NUMFUNC operations
which a COSTA-vector should be able to perform. These implementations
have been created using BLAS.

 The function CTA_Vector_blas_initialise creates a CTA_VecClass, which 
 uses these implementations.

  The user may create different vector-classes, which use a different set of
  implementations. To do so, a function must be written which s similar to
  CTA_Vector_blas_initialise, and implementations must be supplied for 
  those functions which are differently implemented than the BLAS-ones.

   CTA_Vector_Create_Size          return memory-size required for creating a vector
   CTA_Vector_blas_initialise      create a vector class: set function pointers to all
   member variables.

*/
void CTA_Vector_blas_initialise(CTA_VecClass *hveccl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[I_CTA_VECTOR_NUMFUNC];

   // The vector h_func is filled with COSTA-function handles of the implementations in this
   // file.
   CTA_Func_Create(" ",&CTAI_Vector_Create_Size, hintf,&h_func[I_CTA_VECTOR_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_Vector_Create_Init, hintf,&h_func[I_CTA_VECTOR_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_Vector_getvals,    hintf,&h_func[I_CTA_VECTOR_GETVALS]    );
   CTA_Func_Create(" ",&CTAI_Vector_getval,     hintf,&h_func[I_CTA_VECTOR_GETVAL]     );
   CTA_Func_Create(" ",&CTAI_Vector_setvals,    hintf,&h_func[I_CTA_VECTOR_SETVALS]    );
   CTA_Func_Create(" ",&CTAI_Vector_setval,     hintf,&h_func[I_CTA_VECTOR_SETVAL]     );
   CTA_Func_Create(" ",&CTAI_Vector_setconst,   hintf,&h_func[I_CTA_VECTOR_SETCONST]   );
   CTA_Func_Create(" ",&CTAI_Vector_scal,       hintf,&h_func[I_CTA_VECTOR_SCAL]       );
   CTA_Func_Create(" ",&CTAI_Vector_copy,       hintf,&h_func[I_CTA_VECTOR_COPY]       );
   CTA_Func_Create(" ",&CTAI_Vector_axpy,       hintf,&h_func[I_CTA_VECTOR_AXPY]       );
   CTA_Func_Create(" ",&CTAI_Vector_dot,        hintf,&h_func[I_CTA_VECTOR_DOT]        );
   CTA_Func_Create(" ",&CTAI_Vector_nrm2,       hintf,&h_func[I_CTA_VECTOR_NRM2]       );
   CTA_Func_Create(" ",&CTAI_Vector_amax,       hintf,&h_func[I_CTA_VECTOR_AMAX]       );
   CTA_Func_Create(" ",&CTAI_Vector_GetMaxLen,  hintf,&h_func[I_CTA_VECTOR_GETMAXLEN]  );
   CTA_Func_Create(" ",&CTAI_Vector_export,     hintf,&h_func[I_CTA_VECTOR_EXPORT]     );
   CTA_Func_Create(" ",&CTAI_Vector_import,     hintf,&h_func[I_CTA_VECTOR_IMPORT]     );
   CTA_Func_Create(" ",&CTAI_Vector_Print_Table,hintf,&h_func[I_CTA_VECTOR_PRINT_TABLE]);
   CTA_Func_Create(" ",&CTAI_Vector_free,       hintf,&h_func[I_CTA_VECTOR_FREE]       );
   CTA_Func_Create(" ",&CTAI_Vector_appendval,  hintf,&h_func[I_CTA_VECTOR_APPENDVAL]  );
   CTA_Func_Create(" ",&CTAI_Vector_elmdiv,     hintf,&h_func[I_CTA_VECTOR_ELMDIV]     );
   CTA_Func_Create(" ",&CTAI_Vector_elmprod,    hintf,&h_func[I_CTA_VECTOR_ELMPROD]    );
   CTA_Func_Create(" ",&CTAI_Vector_elmsqrt,    hintf,&h_func[I_CTA_VECTOR_ELMSQRT]    );

   CTA_Vector_DefineClass("cta_vector_blas",h_func,hveccl);
}

void CTAI_Vector_Create_Size(
                             // INPUTS (all unused):
                             int *n,                 //
                             CTA_Datatype *datatype, //
                             CTA_Handle *usrdata,   //
                             // OUTPUTS:
                             int *retval,            // error code (see cta_datatypes.h for possible error codes)
                             int *memsize            // The number of bytes which are necessary to store one 
                             //     CTAI_Vector_blas, with a pointer to the contents (data), but without the
                             //     contents themselves.
                             ){

   *memsize=(int) sizeof(CTAI_Vector_blas);
   *retval=CTA_OK;
};


void CTAI_Vector_Create_Init(
/*
Allocate the memory which is neccesary to store the vector values
*/
// IN-OUTPUTS
CTAI_Vector_blas *x,    // The COSTA-BLAS-vector for which memory is reserved.
// INPUTS:
int *n,                 // dimension of the vector
CTA_Datatype *datatype, // data type
CTA_Handle *usrdata,    // ???
// OUTPUTS
int *retval)            // Error code. Possible error: Illegal data type
{
   *retval = CTA_OK;
   x->n=*n;
   x->size=*n;
   x->values = NULL;

   // allocate an array
   if (*datatype==CTA_REAL){
      if (n>0){     
         x->values=CTA_Malloc(*n * sizeof(float));
      }
   }
   else if (*datatype==CTA_DOUBLE){
      if (n>0) {     
         x->values=CTA_Malloc(*n * sizeof(double));
      }
   }
   else if (*datatype==CTA_INTEGER || *datatype==CTA_HANDLE){
      if (n>0){
         x->values=CTA_Malloc(*n * sizeof(int));
      }
   }
   else if (*datatype==CTA_STRING){
      // For strings: allocate the CTA_String array AND create the strings
      if (n>0){
         int i;
         CTA_String *StrVal;
      
         x->values=CTA_Malloc(*n * sizeof(CTA_String));
         StrVal = (CTA_String*) x->values;
         for (i=0; i<*n; i++)
         {
            *retval = CTA_String_Create(&(StrVal[i]));
            if (*retval != CTA_OK) return;
         }
      }
   }
   else {
      x->datatype = CTA_NULL;
      x->n=0;
      x->size=0;
      
      *retval=CTA_ILLEGAL_DATATYPE; 
      return;
   }

   // set datatype
   x->datatype=*datatype;
};

/* Copy single real array to double real array */
static void CTAI_Single2Double(float *rx, double *dx, int n){
   int i;
   for (i=0;i<n;i++){
      dx[i]=(double) rx[i];
   }
}

/* Copy double real array to single real array */
static void CTAI_Double2Single(double *dx, float *rx, int n){
   int i;
   for (i=0;i<n;i++){
      rx[i]=(float) dx[i];
   }
}

static void CTAI_Double2Double(double *dx, double *rx, int n){
   int i;
   for (i=0;i<n;i++){
      rx[i]= dx[i];
   }
}

void CTAI_Vector_getval(
                        // Get one value from a COSTA-BLAS-vector
                        // INPUTS
                        CTAI_Vector_blas *x,   // The COSTA-BLAS-vector of which the values is
                        //     returned 
                        int *i,                // Index of the value to be returned in variable val 
                        // OUTPUTS 
                        void *val,             // The i-th value of the vector *x
                        // INPUTS
                        CTA_Datatype *datatype,// Data-type of variable *val. This must be the
                        //      same datatype as that of x
                        int *retval            // Error code. Possible errors: 
                        //  * negative number of values asked OR no values asked OR 
                        //    more values asked than available
                        //  * data types of *val and *x differ
                        ){
   /* Local variables */
   float *fptr;
   double *dptr;
   int *iptr;
   
   /* check dimensions */
   if (*i<1 || *i>x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   //printf("CTAI_Vector_getval: returning the %d-th value\n",*i);

   /* check data types and handle conversion */
   if (*datatype!=x->datatype){
      if (x->datatype==CTA_DOUBLE && *datatype==CTA_REAL) {
         double *fptr= (double *)x->values;
         CTAI_Double2Single(fptr+(*i-1), val, 1);
      } else if (x->datatype==CTA_REAL && *datatype==CTA_DOUBLE){
         float *fptr= (float *)x->values;
         CTAI_Single2Double(fptr+(*i-1),val, 1);
      } else {
         *retval=CTA_INCOMPATIBLE_VECTORS;
         return;
      }
   }
   else if (*datatype==CTA_REAL){
      /* copy REAL-values using memcpy */
      fptr=x->values;
      memcpy(val,fptr+(*i-1),sizeof(float));
   }
   else if (*datatype==CTA_INTEGER || *datatype==CTA_HANDLE){
      /* copy INTEGER-values using memcpy */
      iptr=x->values;
      memcpy(val,iptr+(*i-1),sizeof(int));
   }
   else if (*datatype==CTA_DOUBLE){
      /* copy DOUBLE-values using memcpy */
      dptr=x->values;
      memcpy(val,dptr+(*i-1),sizeof(double));
   }   
   else if (*datatype==CTA_STRING){
      /* copy CTA_STRING-values using COSTA-functions */
      char *str;
      int len;
      // Cast the vector data to string-handles
      CTA_String *StrPtr = (CTA_String*) x->values;
      
      // Get the string length
      *retval = CTA_String_GetLength(StrPtr[*i-1],&len);
      if (*retval!=CTA_OK) return;
      
      // Get the string contents from the COSTA-string
      str=CTA_Malloc((len+1)*sizeof(char));
      *retval = CTA_String_Get(StrPtr[*i-1],str);
      if (*retval!=CTA_OK) return;
      
      // Set the string contents into output COSTA-string
      StrPtr = (CTA_String*) val;
      *retval = CTA_String_Set( *StrPtr ,str);
      if (*retval!=CTA_OK) return;
      free(str);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};


void CTAI_Vector_getvals(
                         // Get all the values from a COSTA-BLAS-vector
                         CTAI_Vector_blas *x,
                         void *vals,
                         int *n,
                         CTA_Datatype *datatype,
                         int *retval
                         ){
   /* Local variables */
   int one=1; 
   

   /* check dimensions */
   if (*n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   
   /* check data types and handle conversion */
   if (*datatype!=x->datatype){
      if (x->datatype==CTA_DOUBLE && *datatype==CTA_REAL) {
         CTAI_Double2Single(x->values, vals, *n);
      } else if (x->datatype==CTA_REAL && *datatype==CTA_DOUBLE){
         CTAI_Single2Double(x->values, vals, *n);
      } else {
         *retval=CTA_INCOMPATIBLE_VECTORS;
         return;
      }
   }
   /* copy values using BLAS copy */
   else if (*datatype==CTA_INTEGER || *datatype==CTA_HANDLE){
      SCOPY_F77(n,x->values,&one,vals,&one);
   }
   else if (*datatype==CTA_REAL){
      SCOPY_F77(n,x->values,&one,vals,&one);
   }
   else if (*datatype==CTA_DOUBLE){
      DCOPY_F77(n,x->values,&one,vals,&one);
   }   
   else if (*datatype==CTA_STRING){
      /* copy values using GetVal*/
      
      // Cast the vector data to string-handles
      CTA_String *StrPtr = (CTA_String*) vals;
      
      int i; 
      for (i=1; i<=*n; i++)
      {
         CTAI_Vector_getval( x, &i, &(StrPtr[i-1]), datatype, retval);
         if (*retval!=CTA_OK) return;
      }
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};



void CTAI_Vector_setval(
                        // Set one value of a COSTA-BLAS-vector
                        CTAI_Vector_blas *x,
                        int *i,
                        void *val,
                        CTA_Datatype *datatype,
                        int *retval
                        ){
   /* Local variables */

   /* check dimensions */
   if (*i<1 || *i>x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types and handle conversion */
   if (*datatype!=x->datatype){
      if (*datatype==CTA_REAL && x->datatype==CTA_DOUBLE ) {
         double *fptr= (double *)x->values;
         CTAI_Single2Double(val, fptr+(*i-1), 1);
      } else if (*datatype==CTA_DOUBLE && x->datatype==CTA_REAL){
         float *fptr= (float *)x->values;
         CTAI_Double2Single(val, fptr+(*i-1), 1);
      } else {
         *retval=CTA_INCOMPATIBLE_VECTORS;
         return;
      }
   /* copy values using memcpy */
   } else if (*datatype==CTA_REAL){
      float *fptr= (float *)x->values;
      memcpy(fptr+(*i-1),val,sizeof(float));
   }
   else if (*datatype==CTA_INTEGER || *datatype==CTA_HANDLE){
      int * iptr= (int *)x->values;
      memcpy(iptr+(*i-1),val,sizeof(int));
   }
   else if (*datatype==CTA_DOUBLE){
      double *dptr=(double *)x->values;
      memcpy(dptr+(*i-1),val,sizeof(double));
   }
   else if (*datatype==CTA_STRING){
      int len;
      char *str;
      CTA_String str_in;
      CTA_String *StrPtr;

      /* copy CTA_STRING-values using COSTA-functions */
      // Cast the vector data to string-handles
      str_in = *((CTA_String*)val);

      // Get the string length
      *retval = CTA_String_GetLength(str_in,&len);
      if (*retval!=CTA_OK) return;

      // Get the string contents from the COSTA-string
      str=CTA_Malloc((len+1)*sizeof(char));
      *retval = CTA_String_Get(str_in,str);
      if (*retval!=CTA_OK) return;

      // Set the string contents into output COSTA-string
      StrPtr = (CTA_String*) x->values;
      *retval = CTA_String_Set( StrPtr[*i-1] ,str);
      if (*retval!=CTA_OK) return;
      free(str);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};

void CTAI_Vector_setvals(
                         // Set all values of a COSTA-BLAS-vector
                         CTAI_Vector_blas *x,
                         void *vals,
                         int *n,
                         CTA_Datatype *datatype,
                         int *retval
                         ){
   /* Local variables */
   int one=1; 

   /* check dimensions */
   if (*n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types and handle conversion */
  if (*datatype!=x->datatype){
      if (*datatype==CTA_REAL && x->datatype==CTA_DOUBLE ) {
         CTAI_Single2Double(vals, x->values, *n);
      } else if (*datatype==CTA_DOUBLE && x->datatype==CTA_REAL){
         CTAI_Double2Single(vals, x->values, *n);
      } else {
         *retval=CTA_INCOMPATIBLE_VECTORS;
         return;
      }

   /* copy values using BLAS copy or memcpy*/
  } else if (*datatype==CTA_INTEGER || *datatype==CTA_HANDLE){
      SCOPY_F77(n,vals,&one,x->values,&one);
   }
   else if (*datatype==CTA_REAL){
      SCOPY_F77(n,vals,&one,x->values,&one);
   }
   else if (*datatype==CTA_DOUBLE){
      DCOPY_F77(n,vals,&one,x->values,&one);
   } 
   else if (*datatype==CTA_STRING){
      /* copy CTA_STRING-values using COSTA-functions */
      // Cast the vector data to string-handles
      CTA_String *StrPtr = (CTA_String*) vals;

      int i; 
      for (i=1; i<=*n; i++)
      {
         CTAI_Vector_setval( x, &i, &(StrPtr[i-1]), datatype, retval);
         if (*retval!=CTA_OK) return;
      }

   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};


#undef METHOD
#define METHOD "CTAI_Vector_setconst"
void CTAI_Vector_setconst(
                          // Set all values of a COSTA-BLAS-vector to the same value
                          CTAI_Vector_blas *x,
                          void *val,
                          CTA_Datatype *datatype,
                          int *retval
                          ){
   /* Local variables */
   float sval;
   double dval;

   /* check data types */
   if (*datatype==CTA_REAL) {
      memcpy(&sval,val,sizeof(float));
   } else if (*datatype==CTA_DOUBLE) {
      memcpy(&dval,val,sizeof(double));
   }


   if (x->datatype==CTA_REAL && *datatype==CTA_DOUBLE) {
      sval=(float) dval;
   } else if (x->datatype==CTA_DOUBLE && *datatype==CTA_REAL) {
      dval=(double) sval;
   } else if (*datatype!=x->datatype){
      CTA_WRITE_ERROR("DataType of vector and constant value is not compatible");
      *retval=CTA_INCOMPATIBLE_VECTORS;
      return ;
   }

   /* copy values using BLAS copy */
   if (x->datatype==CTA_REAL){
      float *svals = (float *) x->values;
      int i;
      for (i=0;i<x->n;i++){ svals[i]=sval; }
   }
   else if (x->datatype==CTA_INTEGER || x->datatype==CTA_HANDLE){
      int *ival  = (int *) val;
      int *ivals = (int *) x->values;
      int i;
      for (i=0;i<x->n;i++){ ivals[i]=*ival; }
   }
   else if (x->datatype==CTA_DOUBLE){
      double * dvals= (double *) x->values;
      int i;
      for (i=0;i<x->n;i++){ dvals[i]=dval; }
   }
   else if (x->datatype==CTA_STRING){
      /* copy CTA_STRING-values using COSTA-functions */
      char *str;
      // Cast the vector data to string-handles
      CTA_String *StrPtr = (CTA_String*) val;

      // Get the string length
      int len;
      int i;
      *retval = CTA_String_GetLength(*StrPtr,&len);
      if (*retval!=CTA_OK) return;

      // Get the string contents from the COSTA-string
      str=CTA_Malloc((len+1)*sizeof(char));
      *retval = CTA_String_Get(*StrPtr,str);
      if (*retval!=CTA_OK) return;

      StrPtr = (CTA_String*) x->values;
      for (i=0;i<x->n;i++)
      {
         // Set the string contents into output COSTA-string
         *retval = CTA_String_Set( StrPtr[i] ,str);
         if (*retval!=CTA_OK) return;
      }
      free(str);
   }
   else {
      CTA_WRITE_ERROR("DataType is not supported");
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};




void CTAI_Vector_scal(
                      // Multiply a COSTA-BLAS-vector with a scalar
                      CTAI_Vector_blas *x,
                      double *alpha,
                      int *retval
                      ){
   /* Local variables */
   int one=1; 
   float salpha;

   if (x->datatype==CTA_REAL){
      salpha=(float) *alpha;
      SSCAL_F77(&x->n,&salpha, x->values,&one);
   }
   else if (x->datatype==CTA_INTEGER){
      int i;
      int *ival = (int *)x->values;
      for (i=0; i<x->n; i++) 
      {
         ival[i] = (int) (*alpha * ival[i]);
      }
   }
   else if (x->datatype==CTA_DOUBLE){
      DSCAL_F77(&x->n,alpha, x->values,&one);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};


void CTAI_Vector_copy(
                      // Copy the values of a COSTA-BLAS-vector to another existing one.
                      CTAI_Vector_blas *x, 
                      CTAI_Vector_blas *y, 
                      int *retval
                      ){
   /* Local variables */
   int one=1; 

   /* check dimensions */
   if (y->n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types and handle conversion */
   if (x->datatype!=y->datatype){
      if (x->datatype==CTA_DOUBLE && y->datatype==CTA_REAL) {
         CTAI_Double2Single(x->values, y->values, x->n);

      } else if (x->datatype==CTA_REAL && y->datatype==CTA_DOUBLE){
         CTAI_Single2Double(x->values, y->values, x->n);
      } else {
         *retval=CTA_INCOMPATIBLE_VECTORS;
         return;
      }
   } else if (x->datatype==CTA_REAL){
      SCOPY_F77(&x->n,x->values,&one,y->values,&one);
   }
   else if (x->datatype==CTA_INTEGER || x->datatype==CTA_HANDLE ){
      SCOPY_F77(&x->n,x->values,&one,y->values,&one);
   }
   else if (x->datatype==CTA_DOUBLE){
      DCOPY_F77(&x->n,x->values,&one,y->values,&one);
   }
   else if (x->datatype==CTA_STRING){
      int i;
      CTA_String *yval = (CTA_String *)y->values;
      for (i=1; i<=x->n; i++)
      {
         CTAI_Vector_getval( x, &i, &(yval[i-1]), &y->datatype, retval);
         if (*retval!=CTA_OK) return;
      }
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }

   *retval=CTA_OK;
};

void CTAI_Vector_axpy(
                      CTAI_Vector_blas *y, 
                      double *alpha,
                      CTAI_Vector_blas *x, 
                      int *retval
                      ){
   /* Local variables */
   int one=1; 
   float salpha;

   /* check dimensions */
   if (y->n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (y->datatype!=x->datatype){
      printf("datatype y=%d datatype y=%d", y->datatype,x->datatype);
      *retval=CTA_INCOMPATIBLE_VECTORS;
      return;
   }

   if (x->datatype==CTA_REAL){
      salpha=(float) *alpha;
      SAXPY_F77(&x->n,&salpha,x->values,&one,y->values,&one);
   }
   else if (x->datatype==CTA_INTEGER){
      int i;
      int * xval = (int *) x->values;
      int * yval = (int *) y->values;
      for (i=0; i<x->n; i++) { yval[i] = (int) (yval[i] + *alpha * xval[i]);}
   }
   else if (x->datatype == CTA_DOUBLE) {
      DAXPY_F77(&x->n,alpha,x->values,&one,y->values,&one);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   
   *retval=CTA_OK;
};

void CTAI_Vector_dot(
                     // Calculate the dot-product of two COSTA-BLAS-vectors.
                     CTAI_Vector_blas *x, 
                     CTAI_Vector_blas *y, 
                     double *dotprod,
                     int *retval
                     ){
   /* Local variables */
   int one=1; 

   /* check dimensions */
   if (y->n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (y->datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_VECTORS;
      return;
   }

   if (x->datatype==CTA_REAL){
      *dotprod= (double) SDOT_F77(&x->n,x->values,&one,y->values,&one);
   }
   else if (x->datatype==CTA_INTEGER){
      int i;
      int * xval = (int *) x->values;
      int * yval = (int *) y->values;
      *dotprod=0.0;
      for (i=0; i<x->n; i++) { *dotprod += xval[i]*yval[i];}
   }
   else if (x->datatype==CTA_DOUBLE){
      *dotprod=DDOT_F77(&x->n,x->values,&one,y->values,&one);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }

   *retval=CTA_OK;
};

void CTAI_Vector_nrm2(
                      CTAI_Vector_blas *x,
                      double *norm2,
                      int *retval
                      ){
   
   /* Local variables */
   int one=1; 
   
   if (x->datatype==CTA_REAL){
      *norm2= (double) SNRM2_F77(&x->n,x->values,&one);
   }
   else if (x->datatype==CTA_INTEGER){
      int i;
      int * xval = (int *) x->values;
      *norm2=0.0;
      for (i=0; i<x->n; i++) { *norm2 += xval[i]*xval[i];}
      *norm2 = sqrt(*norm2);
   }
   else if (x->datatype==CTA_DOUBLE){
      *norm2=DNRM2_F77(&x->n,x->values,&one);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
   *retval=CTA_OK;
};

void CTAI_Vector_amax(
                      CTAI_Vector_blas *x,
                      int *iloc,
                      int *retval
                      ){

   /* Local variables */
   int one=1; 

						  
   /* quick return in case of 0-length vector*/
   if (x->n==0){
      *iloc=0;
      *retval=CTA_OK;
   }

   if (x->datatype==CTA_REAL){
      *iloc=ISAMAX_F77(&x->n,x->values,&one);
   }
   else if (x->datatype==CTA_INTEGER){
      int i;
      int * xval = (int *) x->values;

      // Find the location in C-notation (counting from zero)
      *iloc=0;
      for (i=1; i<x->n; i++) 
      { 
         if (abs(xval[i])>abs(xval[*iloc])) {*iloc = i;}
      }

      // Increase the index to get a vector-index (counting from one)
      (*iloc)++;
   }
   else if (x->datatype==CTA_DOUBLE){
      *iloc=IDAMAX_F77(&x->n,x->values,&one);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }

   *retval=CTA_OK;
};

void CTAI_Vector_GetMaxLen(
                           CTAI_Vector_blas *x,
                           int *maxlen,
                           int *retval
                           )
{
   int i;
   int length;
   CTA_String * values = (CTA_String*) x->values;
   if (x->datatype!=CTA_STRING)
   {
      *retval = CTA_ILLEGAL_DATATYPE; return;
   }

   *maxlen = 0;
   for (i=0; i<x->n; i++) 
   {
      *retval = CTA_String_GetLength(values[i], &length);
      if (*retval != CTA_OK) return;

      if (length>*maxlen) {*maxlen=length;}
   }

   *retval=CTA_OK;
};

void CTAI_Vector_export(
                        CTAI_Vector_blas *x,
                        CTA_Handle *usrdata,
                        int *retval
                        )
{
   /* Local variables */
   int i;
   FILE *file;
   int size;

   /* set pointers for easy printing */
   float      *xreal   = (float *)      x->values;
   double     *xdouble = (double *)     x->values;
   int        *xint    = (int *)        x->values;
   CTA_String *xstr    = (CTA_String *) x->values;

   if (IDEBUG>0) printf("Vector_blas: Start of export\n");
   if (IDEBUG>0) printf("Vector_blas: size of n %d\n",x->n);

   CTA_Message_Quiet(CTA_TRUE);
   if (CTA_Handle_Check(*usrdata,CTA_FILE)==CTA_OK) {
      CTA_Message_Quiet(CTA_FALSE);
      /* We have to export to a file */
      *retval=CTA_File_Get(*usrdata,&file);
      if (*retval != CTA_OK) return;

      if (CTA_FLUSH_ALWAYS) CTA_Flush();

      if (x->datatype == CTA_STRING) {
         fprintf(file,"{");
      } else {
         fprintf(file,"[");
      }
      /* Print the values */
      for (i=0;i<x->n;i++){
         if (x->datatype==CTA_REAL){
            fprintf(file,"%10.5g",xreal[i]);
         }
         else if (x->datatype==CTA_DOUBLE){
            fprintf(file,"%18.8le  ",xdouble[i]);
         } 
         else if (x->datatype == CTA_INTEGER){
            fprintf(file,"%d  ",xint[i]);
         }
         else if (x->datatype == CTA_STRING){
            int len;
            char *str;
            *retval = CTA_String_GetLength(xstr[i],&len);
            if (*retval != CTA_OK) return;

            str=CTA_Malloc((len+1)*sizeof(char));
            *retval = CTA_String_Get(xstr[i],str);
            if (*retval != CTA_OK) return;

            fprintf(file,"'%s'\n",str);
            free(str);
         }
         if (i+1<x->n) {
            fprintf(file,"\n");
         }
      }
      if (x->datatype == CTA_STRING) {
         fprintf(file,"};\n");
      } else {
         fprintf(file,"];\n");
      }
      if (CTA_FLUSH_ALWAYS) CTA_Flush();
   } 
   else if (CTA_Handle_Check(*usrdata,CTA_PACK)==CTA_OK) {
      CTA_Message_Quiet(CTA_FALSE);
      if (IDEBUG>0) printf("Vector_blas: packing vector\n");
      *retval=CTA_Pack_Add(*usrdata,x,sizeof(CTAI_Vector_blas));
      if (x->datatype==CTA_STRING) {
         for (i=0;i<x->n;i++){
            *retval=CTA_String_Export(xstr[i],*usrdata);
            if (retval!=CTA_OK) return;
         }
      } else {
         if (IDEBUG>0) printf("Vector_blas: packing values\n");
         *retval=CTA_SizeOf(x->datatype, &size);
         if (IDEBUG>0) printf("Vector_blas: size of data-type %d\n",size);
         if (IDEBUG>0) printf("Vector_blas: size of n %d\n",x->n);
         *retval=CTA_Pack_Add(*usrdata,x->values,x->n*size);
         if (IDEBUG>0) printf("Vector_blas: packing done\n");
      }
   } else {
      CTA_Message_Quiet(CTA_FALSE);
      *retval=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }
   *retval=CTA_OK;
};


void CTAI_Vector_import(
                        CTAI_Vector_blas *x,
                        CTA_Handle *usrdata,
                        int *retval
                        )
{
   /* Local variables */
   BOOL packout;
   int size;

   void *values;
   int n;
   int datatype;

   /* set pointers for easy printing */

   packout = (CTA_Handle_Check(*usrdata,CTA_PACK)==CTA_OK);

   datatype=x->datatype;
   n=x->n;
   values=x->values;

   if (packout) {
      *retval=CTA_Pack_Get(*usrdata,x,sizeof(CTAI_Vector_blas));
      x->values=values;
      *retval=CTA_SizeOf(x->datatype, &size);
      size=size*x->n;

      // Realloc values part when needed
      if (x->datatype!=datatype || n!=x->n) {
              x->values=realloc(x->values,size);
      }
      *retval=CTA_Pack_Get(*usrdata,x->values,size);
   } else {
      *retval=CTA_FORMAT_NOT_SUPPORTED;
   }
   *retval=CTA_OK;
};



void CTAI_Vector_Print_Table(
                             CTAI_Vector_blas **table,
                             int *ncolumns,
                             CTAI_Vector_blas *formats,
                             int *retval)
{
   int nrows =     /* number of rows in the table */
      table[0]->n; 
   int irow, icol; /* loop counters for rows and columns */
   char *str;

   CTA_String * formvalues =  /* */
      (CTA_String *) formats->values;

   char** formstr =  /* string array with the format strings */
      CTA_Malloc(sizeof(char *) * (*ncolumns));

   if (CTA_FLUSH_ALWAYS) CTA_Flush();

   // Check dimensions
   if (formats->n != *ncolumns) 
   {
      *retval = CTA_DIMENSION_ERROR; return;
   }

   for (icol=1; icol<*ncolumns; icol++) 
   {
      if (table[icol]->n != nrows)
      {
         *retval = CTA_DIMENSION_ERROR; return;
      }
   }

   // Retrieve the format strings
   for (icol=0; icol<*ncolumns; icol++) 
   {
      int length;
      *retval = CTA_String_GetLength(formvalues[icol],&length);
      if (*retval != CTA_OK) return;

      formstr[icol] = CTA_Malloc(sizeof(char)*(length+1));
      *retval = CTA_String_Get(formvalues[icol],formstr[icol]);
      if (*retval != CTA_OK) return;
   }

   // Print the table
   for (irow=0; irow < nrows; irow++)
   {
      for (icol=0; icol<*ncolumns; icol++)
      {
         // print item (irow,icol) of the table
         if (table[icol]->datatype == CTA_INTEGER || table[icol]->datatype == CTA_HANDLE)
         {
            int * tabvalues =(int *) table[icol]->values;
            printf(formstr[icol],tabvalues[irow]);
         }
         else if (table[icol]->datatype == CTA_REAL)
         {
            float * tabvalues =(float *) table[icol]->values;
            printf(formstr[icol],tabvalues[irow]);
         }
         else if (table[icol]->datatype == CTA_DOUBLE)
         {
            double * tabvalues =(double *) table[icol]->values;
            printf(formstr[icol],tabvalues[irow]);
         }
         else if (table[icol]->datatype == CTA_STRING)
         {
            int length;
            CTA_String * tabvalues =(CTA_String *) table[icol]->values;

            *retval = CTA_String_GetLength(tabvalues[irow],&length);
            if (*retval != CTA_OK) return;

            str = CTA_Malloc(sizeof(char)*(length+1));
            *retval = CTA_String_Get(tabvalues[irow],str);
            if (*retval != CTA_OK) return;

            printf(formstr[icol],str);
            free(str); 
         }
      }
      printf("\n");
   }

   // Delete items which are no longer necessary
   for (icol=1; icol<*ncolumns; icol++) 
   {
      free(formstr[icol]);
   }
   free(formstr);

   if (CTA_FLUSH_ALWAYS) CTA_Flush();

   *retval=CTA_OK;
};


/* Append a value to a vector */
void CTAI_Vector_appendval(
                           CTAI_Vector_blas *x,
                           void *val,
                           CTA_Datatype *datatype,
                           int *retval
                           ){
   int i = 1 + x->n; /* Index of the new element    */

   /* Update n */
   ++(x->n);

   /* Ensure the vector is long enough and update n */
   if (x->n > x->size)
   {
      x->size = MAX((int)(1.5 * (float)(x->size) + 0.5), 1 + x->n);

      /* Reallocate the values array */
      if (x->datatype==CTA_INTEGER || x->datatype==CTA_HANDLE || x->datatype==CTA_STRING){
         x->values=realloc(x->values, x->size * sizeof(int));
      }
      else if (x->datatype==CTA_REAL){
         x->values=realloc(x->values, x->size * sizeof(float));
      }
      else if (x->datatype==CTA_DOUBLE){
         x->values=realloc(x->values, x->size * sizeof(double));
      }
      else {
         *retval=CTA_ILLEGAL_DATATYPE; 
         return;
      }
   }

   /* Call setval() to set the value at the new element */
   CTAI_Vector_setval(x, &i, val, datatype, retval);
};


void CTAI_Vector_free(
                      // The idea of re-alloating a vector seems not to have been considered.
                      // It would not be so difficult to allow reallocation.
                      CTAI_Vector_blas *x,
                      int *retval
                      ){
   int i;
   CTA_String *xstr;
   CTA_Handle *xhdl;

   switch (x->datatype)
   {
   case CTA_STRING:
      xstr = (CTA_String *) x->values;
      for (i = 0; i<x->n; i++) 
      {
         *retval = CTA_String_Free(&xstr[i]);
         if (*retval != CTA_OK) return;
      }
      break;
   case CTA_HANDLE:
      xhdl = (CTA_Handle *) x->values;
      for (i = 0; i<x->n; i++) 
      {
         *retval = CTA_Handle_Free_All(&xhdl[i]);
         if (*retval != CTA_OK) return;
      }
      break;
   }

   free(x->values);
   *retval=CTA_OK;
};


/* Get the index from a path component */
/* Updates name so it will be without index */
/* The index is indicated with an ending on '<index>' */
static int CTAI_Vector_GetPathIndex(const char *name) {
   char       *open;
   const char *close;
   char       szix[MAXIXLEN+1];

   /* Find open and close brackets */
   open = strrchr(name, '<');
   close = strrchr(name, '>');
   if (!open || !close || (open + 2) > close ||
       (close - open) >= MAXIXLEN) {
      /* Default index: 1 */
      return 1;
   }

   /* Remove index from name */
   *open = '\0';

   /* Determine the index */
   strncpy(szix, open + 1, close - open - 1);
   szix[close - open - 1] = '\0';
   return atoi(szix);  
}


/* Find the handle with the given name (INTERNAL USE) */
/* Returns the handle or CTA_NULL if not found or in case of an error */
CTA_Handle CTAI_Vector_FindHandle(CTA_Vector hvec, const char* name)
{
   CTA_Vector *vector;
   CTA_Handle   h;
   int          i;
   int          n;
   int          counter;
   int          index;

   if (CTA_OK != CTA_Handle_GetData(hvec, (void*) &vector)) return CTA_NULL;   
   if (CTA_OK != CTA_Vector_GetSize(hvec, &n)) return CTA_NULL;      

   /* Get index from name */
   index = CTAI_Vector_GetPathIndex(name);

   /* Search for handle */
   counter = 0;
   for (i = 1; i <= n; ++i) {
      if (CTA_OK != CTA_Vector_GetVal(hvec, i, &h, CTA_HANDLE)) return CTA_NULL;      
      if (0 == strcmp(CTAI_Handle_GetName(h), name)) {
         ++counter;
         if (counter == index) return h;
      }
   }
   return CTA_NULL;
}

/* Count the number of handles with the given name (INTERNAL USE) */
/* Returns the count or -1 in case of an error */
int CTAI_Vector_CountHandles(CTA_Vector hvec, const char* name)
{
   CTA_Vector *vector;
   CTA_Handle   h;
   int          i;
   int          n;
   int          counter;

   if (CTA_OK != CTA_Handle_GetData(hvec, (void*) &vector)) return -1;   
   if (CTA_OK != CTA_Vector_GetSize(hvec, &n)) return -1;      

   counter = 0;
   for (i = 1; i <= n; ++i) {
      if (CTA_OK != CTA_Vector_GetVal(hvec, i, &h, CTA_HANDLE)) return -1;      
      if (0 == strcmp(CTAI_Handle_GetName(h), name)) {
         ++counter;
      }
   }
   return counter;
}

void CTAI_Vector_elmdiv(
                      CTAI_Vector_blas *y, 
                      CTAI_Vector_blas *x, 
                      int *retval
                      ){
   /* Local variables */
   float  *svec_y, *svec_x;
   double *dvec_y, *dvec_x;
   int i,n;

   /* check dimensions */
   if (y->n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

  /* Handle data-type stuff for x */
  if (x->datatype ==CTA_DOUBLE) {
      dvec_x=(double*) x->values;
   }
   else if (x->datatype ==CTA_REAL){
      svec_x=(float*) x->values;
   } else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }

    /* Handle data-type stuff for y */
  if (y->datatype ==CTA_DOUBLE) {
      dvec_y=(double*) y->values;
   }
   else if (y->datatype ==CTA_REAL){
      svec_y=(float*) y->values;
   } else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
 
   n=x->n;
   if (x->datatype ==CTA_DOUBLE && y->datatype ==CTA_DOUBLE){
      for (i=0;i<n;i++){
         dvec_y[i]=dvec_y[i]/dvec_x[i];
      }
   }
   else if (x->datatype ==CTA_REAL && y->datatype ==CTA_REAL){
      for (i=0;i<n;i++){
         svec_y[i]=svec_y[i]/svec_x[i];
      }
   }
   else if (x->datatype ==CTA_DOUBLE && y->datatype ==CTA_REAL){
      for (i=0;i<n;i++){
         svec_y[i]=((float) dvec_y[i])/svec_x[i];
      }
   }
   else if (x->datatype ==CTA_REAL && y->datatype ==CTA_DOUBLE){
      for (i=0;i<n;i++){
         dvec_y[i]=((double) svec_y[i])/dvec_x[i];
      }
   }
   *retval=CTA_OK;
};

void CTAI_Vector_elmprod(
                      CTAI_Vector_blas *y, 
                      CTAI_Vector_blas *x, 
                      int *retval
                      ){
   /* Local variables */
   float  *svec_y, *svec_x;
   double *dvec_y, *dvec_x;
   int i,n;

   /* check dimensions */
   if (y->n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

  /* Handle data-type stuff for x */
  if (x->datatype ==CTA_DOUBLE) {
      dvec_x=(double*) x->values;
   }
   else if (x->datatype ==CTA_REAL){
      svec_x=(float*) x->values;
   } else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }

    /* Handle data-type stuff for y */
  if (y->datatype ==CTA_DOUBLE) {
      dvec_y=(double*) y->values;
   }
   else if (y->datatype ==CTA_REAL){
      svec_y=(float*) y->values;
   } else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
 
   n=x->n;
   if (x->datatype ==CTA_DOUBLE && y->datatype ==CTA_DOUBLE){
      for (i=0;i<n;i++){
         dvec_y[i]=dvec_x[i]*dvec_y[i];
      }
   }
   else if (x->datatype ==CTA_REAL && y->datatype ==CTA_REAL){
      for (i=0;i<n;i++){
         svec_y[i]=svec_x[i]*svec_y[i];
      }
   }
   else if (x->datatype ==CTA_DOUBLE && y->datatype ==CTA_REAL){
      for (i=0;i<n;i++){
         svec_y[i]=((float) dvec_x[i])*svec_y[i];
      }
   }
   else if (x->datatype ==CTA_REAL && y->datatype ==CTA_DOUBLE){
      for (i=0;i<n;i++){
         dvec_y[i]=((double) svec_x[i])*dvec_y[i];
      }
   }
   *retval=CTA_OK;
};

void CTAI_Vector_elmsqrt(
                      CTAI_Vector_blas *y, 
                      int *retval
                      ){
   /* Local variables */
   float  *svec_y;
   double *dvec_y;
   int i,n;

   /* Handle data-type stuff for y */
   if (y->datatype ==CTA_DOUBLE) {
      dvec_y=(double*) y->values;
   }
   else if (y->datatype ==CTA_REAL){
      svec_y=(float*) y->values;
   } else {
      *retval=CTA_ILLEGAL_DATATYPE; return;
   }
 
   n=y->n;
   if (y->datatype ==CTA_DOUBLE){
      for (i=0;i<n;i++){
         dvec_y[i]=sqrt(dvec_y[i]);
      }
   }
   else if (y->datatype ==CTA_REAL){
      for (i=0;i<n;i++){
         svec_y[i]=(float) sqrt(svec_y[i]);
      }
   }
   *retval=CTA_OK;
};

