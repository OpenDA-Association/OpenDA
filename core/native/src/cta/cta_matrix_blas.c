/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_matrix_blas.c $
$Revision: 3361 $, $Date: 2012-07-04 16:52:30 +0200 (Wed, 04 Jul 2012) $

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
#include "cta_mem.h"
#include "cta_system.h"
#include "cta_flush.h"
#include "cta_matrix_blas.h"
#include "cta_errors.h"
#include "cta_file.h"
#include "ctai.h"
#include "cta_message.h"

#define CLASSNAME "CTA_Matrix_blas"


void CTA_Matrix_blas_initialise(CTA_MatClass *hmatcl)
{
   CTA_Intf hintf=0;
   CTA_Func h_func[CTA_MATRIX_NUMFUNC];

   CTA_Func_Create(" ",&CTAI_Matrix_Create_Size, hintf,&h_func[CTA_MATRIX_CREATE_SIZE]);
   CTA_Func_Create(" ",&CTAI_Matrix_Create_Init, hintf,&h_func[CTA_MATRIX_CREATE_INIT]);
   CTA_Func_Create(" ",&CTAI_Matrix_getvals,     hintf,&h_func[I_CTA_MATRIX_GETVALS]    );
   CTA_Func_Create(" ",&CTAI_Matrix_getval,      hintf,&h_func[I_CTA_MATRIX_GETVAL]     );
   CTA_Func_Create(" ",&CTAI_Matrix_setcol,      hintf,&h_func[I_CTA_MATRIX_SETCOL]    );
   CTA_Func_Create(" ",&CTAI_Matrix_setvals,     hintf,&h_func[I_CTA_MATRIX_SETVALS]    );
   CTA_Func_Create(" ",&CTAI_Matrix_setval,      hintf,&h_func[I_CTA_MATRIX_SETVAL]     );
   CTA_Func_Create(" ",&CTAI_Matrix_setconst,    hintf,&h_func[I_CTA_MATRIX_SETCONST]   );
   CTA_Func_Create(" ",&CTAI_Matrix_Export,      hintf,&h_func[I_CTA_MATRIX_EXPORT]     );
   CTA_Func_Create(" ",&CTAI_Matrix_Ger,         hintf,&h_func[I_CTA_MATRIX_GER]        );
   CTA_Func_Create(" ",&CTAI_Matrix_Inv,         hintf,&h_func[I_CTA_MATRIX_INV]        );
   CTA_Func_Create(" ",&CTAI_Matrix_Gemv,        hintf,&h_func[I_CTA_MATRIX_GEMV]        );
   CTA_Func_Create(" ",&CTAI_Matrix_Gemm,        hintf,&h_func[I_CTA_MATRIX_GEMM]        );
   CTA_Func_Create(" ",&CTAI_Matrix_Axpy,        hintf,&h_func[I_CTA_MATRIX_AXPY]        );
   CTA_Func_Create(" ",&CTAI_Matrix_free,        hintf,&h_func[I_CTA_MATRIX_FREE]       );

   CTA_Matrix_DefineClass("cta_matrix_blas",h_func,hmatcl);
}

void CTAI_Matrix_Create_Size(
   int *m,
   int *n,
   CTA_Datatype *datatype,
   CTA_Handle userdata,
   int *retval,
   int *memsize
   ){

   *memsize=(int) sizeof(CTAI_Matrix_blas);
   *retval=CTA_OK;
};

void CTAI_Matrix_Create_Init(
   CTAI_Matrix_blas *x,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   CTA_Handle userdata,
   int *retval
   ){
   if (*datatype==CTA_REAL){
      x->values=CTA_Malloc(sizeof(float)*(*m)*(*n));
   }
   else if (*datatype==CTA_DOUBLE){
      x->values=CTA_Malloc(sizeof(double)*(*m)*(*n));
   }
   else if (*datatype==CTA_INTEGER){
      x->values=CTA_Malloc(sizeof(int)*(*m)*(*n));
   }
   else {
     *retval=CTA_ILLEGAL_DATATYPE;
   }
   x->datatype=*datatype;
   x->n=*n;
   x->m=*m;
   x->inverse=FALSE;
   x->perm=NULL;
};


void CTAI_Matrix_construct_inverse(CTAI_Matrix_blas *A)
{
   void *lu;
   double *dA;
   int i, length, info;
   int one=1;
   char notrans='N';

   length=A->n*A->m;
   if (A->datatype==CTA_REAL){
      float  *sA;
      lu=CTA_Malloc(sizeof(float)*length);
      SCOPY_F77(&length,A->values,&one,(float *) lu,&one);
      // Create identity matrix
      sA=A->values;
      for (i=0;i<length; i++){ sA[i]=0.0;}
      for (i=0;i<length; i=i+A->n+1){ sA[i]=1.0;}
      // Compute inverse
      SGETRS_F77(&notrans,&(A->n), &A->n, lu, &(A->n), A->perm, A->values,
      &(A->n), &info,1);
      free(lu);
      
   }
   else if (A->datatype==CTA_DOUBLE){
      lu=CTA_Malloc(sizeof(double)*length);
      DCOPY_F77(&length,A->values,&one,(double *) lu,&one);
      // Create identity matrix
      dA=A->values;
      for (i=0;i<length; i++){ dA[i]=0.0;}
      for (i=0;i<length; i=i+A->n+1){ dA[i]=1.0;}
      // Compute inverse
      DGETRS_F77(&notrans,&(A->n), &A->n, lu, &(A->n), A->perm, A->values,
      &(A->n), &info,1);
      free(lu);
   }
   else {
      printf("INTERNAL ERROR IN CTAI_Matrix_construct_inverse\n");
      exit(-1);
   }
   A->inverse=FALSE;

}


void CTAI_Matrix_getvals(
   CTAI_Matrix_blas *A,
   void *vals,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   int *retval
   ){
   /* Local variables */
   int one=1; 
   int length;

   /* check dimensions */
   if (*m!=A->m){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   if (*n!=A->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (*datatype!=A->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   if (A->inverse) {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(A);
   }
   
   /* copy values using BLAS copy */
   length=(*n)*(*m);
   if (*datatype==CTA_REAL || *datatype == CTA_INTEGER){
      SCOPY_F77(&length,A->values,&one,vals,&one);
   }
   else {
      DCOPY_F77(&length,A->values,&one,vals,&one);
   }   
   *retval=CTA_OK;
};

void CTAI_Matrix_getval(
   CTAI_Matrix_blas *x,
   void *val,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   int *retval
   ){
   /* Local variables */

   if (x->inverse)  {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }
   /* check dimensions */
   if (*m>x->m || *m<1){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   if (*n>x->n || *n<1){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (*datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   if (*datatype==CTA_REAL)
   {
      float * values = (float *) x->values;
      float * value = (float *) val;
      *value = values[ (*m-1) + (x->m) * (*n-1) ];
   }
   else if (*datatype==CTA_INTEGER)
   {
      int * values = (int *) x->values;
      int * value = (int *) val;
      *value = values[ (*m-1) + (*n-1) * (x->m) ];
   }   
   else if (*datatype==CTA_DOUBLE)
   {
      double * values = (double *) x->values;
      double * value = (double *) val;
      *value = values[ (*m-1) + (x->m) * (*n-1) ];
   }
   else
   {
     *retval = CTA_ILLEGAL_DATATYPE;
     return;
   } 
   *retval=CTA_OK;
};



void CTAI_Matrix_setcol(
   CTAI_Matrix_blas *x,
   int *n,
   CTA_Vector *hvec,
   int *retval
  ){
   /* Local variables */
   CTA_Datatype datatype;
   int length;
   void * values;

   if (x->inverse){
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }
   /* Check datatype: must be same as matrix' */
   *retval = CTA_Vector_GetDatatype( *hvec, &datatype);
   if (*retval != CTA_OK) return;

   if (datatype!=x->datatype)
   {
      *retval = CTA_ILLEGAL_DATATYPE;
      return;
   }

   /* Check the dimensions */
   *retval = CTA_Vector_GetSize(*hvec, &length);
   if (*retval != CTA_OK) return;
   if (length != x->m) 
   {
      *retval = CTA_DIMENSION_ERROR; return;
   }

   /* Find the location where the values will be stored */
   if (datatype==CTA_REAL)
   {
      float * xvalues = (float*) x->values;
      values = &xvalues[ (x->m) * (*n-1)];
   }
   else if (datatype==CTA_INTEGER)
   {
      int * xvalues = (int *) x->values;
      values = &xvalues[ (x->m) * (*n-1)];
   }
   else if (datatype == CTA_DOUBLE)
   {
      double * xvalues = (double*) x->values;
      values = &xvalues[ (x->m) * (*n-1)];
   }
   else
   {
      *retval = CTA_ILLEGAL_DATATYPE;
      return;
   }

   *retval =  CTA_Vector_GetVals( *hvec, values, length, datatype);
   if (*retval != CTA_OK) return;

   *retval=CTA_OK;
};



void CTAI_Matrix_setvals(
   CTAI_Matrix_blas *x,
   void *vals,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   int *retval
  ){
   /* Local variables */
   int one=1; 
   int length;

   if (x->inverse){
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }
   /* check dimensions */
   if (*m!=x->m){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }   if (*n!=x->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (*datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   length=(*n)*(*m);
   /* copy values using BLAS copy */
   if (*datatype==CTA_REAL || *datatype==CTA_INTEGER){
      SCOPY_F77(&length,vals,&one,x->values,&one);
   }
   else {
      DCOPY_F77(&length,vals,&one,x->values,&one);
   }   
   *retval=CTA_OK;
};



void CTAI_Matrix_setval(
   CTAI_Matrix_blas *x,
   void *val,
   int *m,
   int *n,
   CTA_Datatype *datatype,
   int *retval
  ){

   if (x->inverse){
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }
   /* check dimensions */
   if ( *m > x->m || *m<1){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }   if (*n > x->n || *n<1){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }

   /* check data types */
   if (*datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   /* copy values using BLAS copy */
   if (*datatype==CTA_REAL)
   {
      float * values= (float*) x->values;
      float * value = (float*) val;
      values[ (*m-1) + (*n-1) * (x->m) ] = *value;
   }
   else if (*datatype==CTA_DOUBLE)
   {
      double * values= (double*) x->values;
      double * value = (double*) val;
      values[ (*m-1) + (*n-1)* (x->m) ] = *value;
   }   
   else if (*datatype==CTA_INTEGER)
   {
      int * values= (int*) x->values;
      int * value = (int*) val;
      values[ (*m-1) + (*n-1)* (x->m) ] = *value;
   }   
   *retval=CTA_OK;
};

void CTAI_Matrix_setconst(
   CTAI_Matrix_blas *x,
   void *val,
   CTA_Datatype *datatype,
   int *retval
  ){
   /* Local variables */
   int length;
   int i;

   if (x->inverse) {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }
   /* check data types */
   if (*datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   /* set values */
   length=x->m*x->n;
   if (*datatype==CTA_REAL)
   {
      float *value = (float *) val;
      float *values= (float *) x->values;
      for (i=0;i<length;i++){
         values[i]=*value;
      }
   }
   else if (*datatype==CTA_DOUBLE)
   {
      double *value = (double *) val;
      double *values= (double *) x->values;
      for (i=0;i<length;i++){
         values[i]=*value;
      }
   }   
   else if (*datatype==CTA_INTEGER)
   {
      int *value = (int *) val;
      int *values= (int *) x->values;
      for (i=0;i<length;i++){
         values[i]=*value;
      }
   }   
   *retval=CTA_OK;
};



void CTAI_Matrix_Export(
         CTAI_Matrix_blas *x,
         CTA_Handle userdata,
         int *retval)
{
   // The user-data must be 1 item: the output file. Look it up.
   FILE *file;

   if (x->inverse){
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(x);
   }

   if (CTA_Handle_Check(userdata,CTA_FILE)==CTA_OK) {
      *retval = CTA_File_Get(userdata,&file);

      if (CTA_FLUSH_ALWAYS) CTA_Flush();

      for (int i0=0; i0<x->n; i0+=5)
      {
         int j;
         fprintf(file,"matrix(:,%d:%d)=[",i0+1,MIN(x->n,i0+5));
         for (j=0; j<x->m; j++)
         {
            int i;
            fprintf(file,"\n    ");
            for (i=i0; i<MIN(x->n,i0+5);i++)
            {
               if (x->datatype == CTA_DOUBLE)
               {
                  double *values = (double*) x->values;
                  //fprintf(file," %10.3lg",values[j+x->m*i]);
                  fprintf(file," %18.8le",values[j+x->m*i]);
               }
               else if (x->datatype == CTA_REAL)
               {
                  float *values = (float *) x->values;
                  fprintf(file," %10.3g",values[j+x->m*i]);
               }
               else if (x->datatype == CTA_INTEGER)
               {
                  int *values = (int *) x->values;
                  fprintf(file," %10d",values[j+x->m*i]);
               }
            }
         }
         fprintf(file,"];\n\n");
         if (CTA_FLUSH_ALWAYS) CTA_Flush();
      }
   } else {
      *retval=CTA_FORMAT_NOT_SUPPORTED;
      return;
   }

   *retval = CTA_OK;
}

void CTAI_Matrix_Ger(
         CTAI_Matrix_blas *A,
         double *alpha,
         CTA_Vector *vx,
         CTA_Vector *vy,
         int *retval)
{
   void *x, *y;
   int nx, ny;
   int sizeoftyp;
   int one;
   float salpha;
   one=1;

   if (A->inverse){
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(A);
   }
   *retval=CTA_SizeOf(A->datatype, &sizeoftyp);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetSize(*vx,&nx);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetSize(*vy,&ny);
   if (*retval!=CTA_OK) return;
   x=CTA_Malloc(sizeoftyp*nx);
   y=CTA_Malloc(sizeoftyp*ny);
   *retval=CTA_Vector_GetVals(*vx,x,nx,A->datatype);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetVals(*vy,y,ny,A->datatype);
   if (*retval!=CTA_OK) return;
   if (A->datatype==CTA_REAL){
      salpha=(float) *alpha;
      SGER_F77(&(A->m),&(A->n),&salpha,x,&one,y,&one,A->values,&(A->m));
      *retval=CTA_OK;
   }
   else if (A->datatype==CTA_DOUBLE){
      DGER_F77(&(A->m),&(A->n),alpha,x,&one,y,&one,A->values,&(A->m));
      *retval=CTA_OK;
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE;
   }
   free(x);
   free(y);

   
   return;      
}

void CTAI_Matrix_Gemv(
         CTAI_Matrix_blas *A,
         int *trans,
         double *alpha,
         CTA_Vector *vx,
         double *beta,
         CTA_Vector *vy,
         int *retval)
{
   void *x, *y;
   int nx, ny, ma, na;
   int sizeoftyp;
   int one;
   float salpha;
   float sbeta;
   char ctrans;
   one=1;

   if (A->inverse)   {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(A);
   }

   if (*trans) {ctrans='T'; ma=A->n; na=A->m;}
   else        {ctrans='N'; ma=A->m; na=A->n;}

   *retval=CTA_SizeOf(A->datatype, &sizeoftyp);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetSize(*vx,&nx);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetSize(*vy,&ny);
   if (*retval!=CTA_OK) return;

   /* check dimensions */
   if (ny!=ma || nx!=na){
      *retval=CTA_DIMENSION_ERROR;
   }

   x=CTA_Malloc(sizeoftyp*nx);
   y=CTA_Malloc(sizeoftyp*ny);
   *retval=CTA_Vector_GetVals(*vx,x,nx,A->datatype);
   if (*retval!=CTA_OK) return;
   *retval=CTA_Vector_GetVals(*vy,y,ny,A->datatype);
   if (*retval!=CTA_OK) return;
   if (A->datatype==CTA_REAL){
      salpha=(float) *alpha;
      sbeta=(float)  *beta;
      SGEMV_F77(&ctrans,&(A->m),&(A->n),&salpha,A->values,&(A->m),x,&one,&sbeta,y,&one,1);
      *retval=CTA_OK;
   }
   else if (A->datatype==CTA_DOUBLE){
      DGEMV_F77(&ctrans,&(A->m),&(A->n),alpha,A->values,&(A->m),x,&one,beta,y,&one,1);
      *retval=CTA_OK;
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE;
   }
   *retval=CTA_Vector_SetVals(*vy,y,ny,A->datatype);
   if (*retval!=CTA_OK) return;
   
   free(x);
   free(y);

   
   return;      
}


void CTAI_Matrix_Gemm(
         CTAI_Matrix_blas *C,
         int *transa,
         int *transb,
         double *alpha,
         CTAI_Matrix_blas *A,
         CTAI_Matrix_blas *B,
         double *beta,
         int *retval)
{
   int ma, mb, mc, na, nb, nc;
   float salpha;
   float sbeta;
   char ctransa;
   char ctransb;

   // check dimensions
   mc=C->m;
   nc=C->n;
   if (*transa) {ctransa='T'; ma=A->n; na=A->m;}
   else        {ctransa='N'; ma=A->m; na=A->n;}
   if (*transb) {ctransb='T'; mb=B->n; nb=B->m;}
   else        {ctransb='N'; mb=B->m; nb=B->n;}

   if (ma!=mc || nb!=mc || na!=mb) {
      *retval=CTA_DIMENSION_ERROR;
   }

   if (A->datatype != B->datatype || A->datatype != C->datatype){
      *retval=CTA_NOT_YET_SUPPORTED; return;
   }

   if (A->inverse)   {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(A);
   }
   if (B->inverse)   {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(B);
   }
   if (C->inverse)   {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(C);
   }
   
   if (C->datatype==CTA_REAL){
      salpha=(float) *alpha;
      sbeta=(float)  *beta;
      SGEMM_F77(&ctransa,&ctransb,&mc,&nc,&na,&salpha,A->values,&(A->m),B->values,&(B->m),
               &sbeta,C->values,&(C->m),1,1);
      *retval=CTA_OK;
   }
   else if (C->datatype==CTA_DOUBLE){
      DGEMM_F77(&ctransa,&ctransb,&mc,&nc,&na,alpha,A->values,&(A->m),B->values,&(B->m),
               beta,C->values,&(C->m),1,1);
      *retval=CTA_OK;
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE;
   }
   return;      
}










void CTAI_Matrix_Inv(
   CTAI_Matrix_blas *A,
   int *retval
   )
{
   int nul=0;
   double *ddum;
   float  *sdum;
   int    info;

   info = 0;
   if (A->inverse)   {
      /* OK we are now going to really going to invert the matrix */
      CTAI_Matrix_construct_inverse(A);
   }
   if (A->m != A->m) {*retval=CTA_DIMENSION_ERROR; return;}
 
   // Create permutation array
   if (!A->perm) A->perm=CTA_Malloc(sizeof(int)*A->n); 

   // Compute LU-decomposition of A
   if (A->datatype==CTA_REAL){
	  sdum=NULL;
      SGESV_F77(&(A->n), &nul, A->values, &(A->n), A->perm, sdum, &(A->n), &info);
   }
   else if (A->datatype==CTA_DOUBLE){
	  ddum=NULL;
      DGESV_F77(&(A->n), &nul, A->values, &(A->n), A->perm, ddum, &(A->n), &info);
   }
   else {
      *retval=CTA_ILLEGAL_DATATYPE;
   }
   A->inverse=TRUE;

   if (info>0) {
      *retval=CTA_SINGULAR_MATRIX;
   }
   else if (info<0){
      *retval=CTA_INTERNAL_ERROR;
   } 
   else {
      *retval=CTA_OK;
   }
};

void CTAI_Matrix_Axpy(
   CTAI_Matrix_blas *y,
   double *alpha,
   CTAI_Matrix_blas *x,
   int *retval
  ){
   /* Local variables */
   int one=1; 
   int length;
   float salpha;

   if (y->inverse)   {
      CTAI_Matrix_construct_inverse(y);
   }
   if (x->inverse)   {
      CTAI_Matrix_construct_inverse(x);
   }
   /* check dimensions */
   if (x->m!=y->m){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   if (x->n!=y->n){
      *retval=CTA_DIMENSION_ERROR;
      return;
   }
   
   /* check data types */
   if (y->datatype!=x->datatype){
      *retval=CTA_INCOMPATIBLE_MATRICES;
      return;
   }

   length=(y->n)*(y->m);
   /* copy values using BLAS copy */
   if (y->datatype==CTA_REAL || y->datatype==CTA_INTEGER){
      salpha=(float) *alpha;
      SAXPY_F77(&length,&salpha,x->values,&one,y->values,&one);
   }
   else {
      DAXPY_F77(&length,alpha,x->values,&one,y->values,&one);
   }   
   *retval=CTA_OK;
};


void CTAI_Matrix_free(
   CTAI_Matrix_blas *x,
   int *retval
   )
{
   free(x->values);
   if (x->perm); free(x->perm);
   *retval=CTA_OK;
};





