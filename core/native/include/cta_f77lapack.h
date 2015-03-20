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
\file  cta_f77lapack.h
\brief Package of internally used functions for C/FORTRAN compatibility.
*/

#ifndef CTA_F77LAPACK_H
#define CTA_F77LAPACK_H
#include "cta_system.h"

/*------------------*/
/* Single precision */
/*------------------*/

#define SGESV_F77 F77_FUNC(sgesv,SGESV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SGESV_F77(int *n, int *nrhs, float *A, int *lda, int *ipiv,
               float *B, int *ldb, int *info);

#define SGETRS_F77 F77_FUNC(sgetrs,SGETRS)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SGETRS_F77(char *trans, int *n, int *nrhs, float *A, int *lda, int *ipiv,
               float *B, int *ldb, int *info, int lentrans);

#define SGEEV_F77 F77_FUNC(sgeev,SGEEV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void SGEEV_F77( char *JOBVL, char *JOBVR, int *N, float *A,
                          int *LDA, float *WR, float *WI, float *VL,
                          int *LDVL, float *VR, int *LDVR, 
                          float *WORK, int *LWORK, int *INFO);

/*------------------*/
/* Double precision */
/*------------------*/
#define DGESV_F77 F77_FUNC(dgesv,DGESV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DGESV_F77(int *n, int *nrhs, double *A, int *lda, int *ipiv,
               double *B, int *ldb, int *info);

#define DGETRS_F77 F77_FUNC(dgetrs,DGETRS)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DGETRS_F77(char *trans, int *n, int *nrhs, double *A, int *lda, int *ipiv,
                double *B, int *ldb, int *info, int lentrans);

#define DPOSV_F77 F77_FUNC(dposv,DPOSV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DPOSV_F77(char *uplo, int *n, int *nrhs, double *A, int *lda, double *B, 
               int *ldb, int *info);

#define DLASRT_F77 F77_FUNC(dlasrt,DLASRT)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DLASRT_F77(char *ID, int *n, double *D, int *info, int lenid);

#define DGEEV_F77 F77_FUNC(dgeev,DGEEV)
#ifdef __cplusplus
extern "C" /* prevent C++ name mangling */
#endif
CTAEXPORT void DGEEV_F77( char *JOBVL, char *JOBVR, int *N, double *A,
                          int *LDA, double *WR, double *WI, double *VL,
                          int *LDVL, double *VR, int *LDVR, 
                          double *WORK, int *LWORK, int *INFO);

#endif






