/*
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

/**
\file  cta_f77blas.h
\brief Package of internally used functions for C/FORTRAN compatibility.
*/

#ifndef CTA_F77BLAS_H
#define CTA_F77BLAS_H
#include "cta_system.h"

/*------------------*/
/* Single precision */
/*------------------*/

#define SSCAL_F77 F77_FUNC(sscal,SSCAL)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SSCAL_F77(int *n, float *alpha, float *x, int *incx);

#define SCOPY_F77 F77_FUNC(scopy,SCOPY)
#ifdef __cplusplus
extern "C" /*bla */
#endif
CTAEXPORT void SCOPY_F77(int *n, float *x, int *incx, float *y, int *incy);

#define SAXPY_F77 F77_FUNC(saxpy,SAXPY)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SAXPY_F77(int *n, float *alpha, float *x, int *incx, float *y, int *incy);

#define SDOT_F77 F77_FUNC(sdot,SDOT)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT float SDOT_F77(int *n, float *x, int *incx, float *y, int *incy);

#define SNRM2_F77 F77_FUNC(snrm2,SNRM2)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT float SNRM2_F77(int *n, float *x, int *incx);

#define ISAMAX_F77 F77_FUNC(isamax,ISAMAX)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT int ISAMAX_F77(int *n, float *x, int *incx);

#define SGER_F77 F77_FUNC(sger,sger)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT int SGER_F77(int *m, int *n, float *alpha, float *x, int *incx,
             float *y, int *incy, float *a, int *lda);

#define SGEMV_F77 F77_FUNC(sgemv,SGEMV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SGEMV_F77(char *trans, int *m, int *n, float* alpha, float *A, int *lda,
                float *x, int *incx, float *beta, float *y, int *incy,
                int lentrans);

#define SGEMM_F77 F77_FUNC(sgemm,SGEMM)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SGEMM_F77(char *transa, char *transb,  int *m, int *n, int *k, 
               float* alpha, float *A, int *lda,
               float *B, int *ldb, float *beta, float *C, int *ldc,
               int lentrans1, int lenstran2);

/*------------------*/
/* Double precision */
/*------------------*/
#define DSCAL_F77 F77_FUNC(dscal,DSCAL)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DSCAL_F77(int *n, double *alpha, double *x, int *incx);

#define DCOPY_F77 F77_FUNC(dcopy,DCOPY)
#ifdef __cplusplus
extern "C" /*bla */
#endif
CTAEXPORT void DCOPY_F77(int *n, double *x, int *incx, double *y, int *incy);

#define DAXPY_F77 F77_FUNC(daxpy,DAXPY)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DAXPY_F77(int *n, double *alpha, double *x, int *incx, float *y, int *incy);

#define DDOT_F77 F77_FUNC(ddot,DDOT)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT double DDOT_F77(int *n, double *x, int *incx, double *y, int *incy);

#define DNRM2_F77 F77_FUNC(dnrm2,DNRM2)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT double DNRM2_F77(int *n, double *x, int *incx);

#define IDAMAX_F77 F77_FUNC(idamax,IDAMAX)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT int IDAMAX_F77(int *n, double *x, int *incx);

#define DGER_F77 F77_FUNC(dger,dger)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT int DGER_F77(int *m, int *n, double *alpha, double *x, int *incx,
             double *y, int *incy, double *a, int *lda);

#define DGEMV_F77 F77_FUNC(dgemv,DGEMV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DGEMV_F77(char *trans, int *m, int *n, double* alpha, double *A, int *lda,
                double *x, int *incx, double *beta, double *y, int *incy,
                int lentrans);

#define DGEMM_F77 F77_FUNC(dgemm,DGEMM)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DGEMM_F77(char *transa, char *transb,  int *m, int *n, int *k, 
               double* alpha, double *A, int *lda,
               double *B, int *ldb, double *beta, double *C, int *ldc,
                int lentrans1, int lenstran2);





#endif







