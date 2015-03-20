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
\file cta_datatypes.h
\brief Definitions of constants and datatypes that are publically used in COSTA
*/


#ifndef CTA_DATA_TYPS_H
#define CTA_DATA_TYPS_H
#include <stdlib.h>
#include <math.h>
#include "cta_system.h"

/** The datatpe of a COSTA Datatype */
typedef int CTA_Datatype;
 
/** A COSTA-function CTA_Function *function
    is a void-pointer. If it points to an
    existing function, it may be used to call
    that function, simply by
    retval = function(arguments);
*/
typedef void CTA_Function();  

/*! handle to a COSTA object */
#define CTA_HANDLE      (-1)
/*! A handle of a COSTA interface object */
#define CTA_INTERFACE   (-2)
/*! A handle of a COSTA function object  */
#define CTA_FUNCTION    (-3)
/*! A handle of a COSTA vector object    */
#define CTA_VECTOR      (-4)
/*! A handle of a COSTA Vector class     */
#define CTA_VECTORCLASS (-5)
/*! A handle of a COSTA (sub) treevector      */
#define CTA_TREEVECTOR       (-6)
/*! A handle of a COSTA Matrix class     */
#define CTA_MATRIXCLASS (-7)
/*! A handle of a COSTA Matrix           */
#define CTA_MATRIX      (-8)
/*! A handle of a COSTA Covariance matrix class     */
#define CTA_COVMATCLASS (-9)
/*! A handle of a COSTA Covariance matrix    */
#define CTA_COVMAT      (-10)
/*! A handle of a COSTA Interpolation matrix */
#define CTA_INTPOL      (-11)
/*! A handle of a COSTA observation component */
#define CTA_OBS         (-12)
/*! A handle of a COSTA Model class     */
#define CTA_MODELCLASS  (-13)
/*! A handle of a COSTA Model    */
#define CTA_MODEL       (-14)
/*! A handle of a COSTA Time */
#define CTA_TIME        (-15)
/*! A handle of a COSTA StochObserver object    */
#define CTA_SOBS        (-16)
/*! A handle of a COSTA StochObserver class     */
#define CTA_SOBSCLASS   (-17)
/*! A handle of a COSTA ObserverDescription object */
#define CTA_OBSDESCR    (-18)
/*! A handle of a COSTA ObserverDescription class */
#define CTA_OBSDESCRCLASS (CTA_SOBSCLASS)
/*! A handle of a COSTA Method object */
#define CTA_METHODCLASS (-19)
/*! A handle of a COSTA Method object */
#define CTA_METHOD      (-20)
/*! A handle of a COSTA tree object    */
#define CTA_TREE        (-21)
/*! A handle of a COSTA Pack-object */
#define CTA_PACK        (-22)
/*! A handle to an unknown data object */
#define CTA_DATABLOCK   (-23)
/*! A handle of a COSTA meta information object */
#define CTA_METAINFO   (-24)
/*! A handle of a COSTA meta information object */
#define CTA_METAINFOCLASS   (-25)
/*! A handle of a COSTA relation table object */
#define CTA_RELTABLE        (-26)
/*! An explicit subtreevector (for XML-use)  */
#define CTA_SUBTREEVECTOR        (-27)
/*! CTA_Datatypes constant: COSTA-string  */
#define CTA_STRING      (-28)
/*! CTA_Datatypes constant: COSTA-file  */
#define CTA_FILE        (-29)
/*! CTA_Datatypes constant: COSTA-array  */
#define CTA_ARRAY        (-30)



// Codes to indicate CTA_Datatypes
/*! COSTA CTA_Datatypes: arbitrary void */
#define CTA_VOID        (-100)
/*! CTA_Datatypes constant: integer (scalar) */
#define CTA_INTEGER     (-101)
/*! CTA_Datatypes constant: single precision real (scalar) */
#define CTA_REAL        (-102)
/*! CTA_Datatypes constant: double precision real (scalar) */
#define CTA_DOUBLE      (-103)
/*! CTA_Datatypes constant: FORTRAN-string */
#define CTA_FSTRING     (-104)
/*! CTA_Datatypes constant: C-string */
#define CTA_CSTRING     (-105)


/*! CTA_Datatypes constant: integer array */
#define CTA_1DINTEGER   (-201)
/*! CTA_Datatypes constant: single precision real array */
#define CTA_1DREAL      (-202)
/*! CTA_Datatypes constant: double precision real array */
#define CTA_1DDOUBLE    (-203)
/*! CTA_Datatypes constant: array of FORTRAN-strings */
#define CTA_1DFSTRING   (-204)
/*! CTA_Datatypes constant: array of C-strings */
#define CTA_1DCSTRING   (-205)

// Common Constants

/*! COSTA flag/constant: Default */
#define CTA_DEFAULT          (0)
/*! COSTA flag/constant: NULL-Handle */
#define CTA_NULL             (0)
/*! COSTA flag/constant: FALSE */
#define CTA_FALSE            (0)
/*! COSTA flag/constant: TRUE */
#define CTA_TRUE             (1)

#ifndef BOOL
/*! Datatype for boolians */
#define BOOL int
/*! Boolean value of TRUE */
#ifndef TRUE
#define TRUE (1)
#endif
/*! Boolean value of FALSE */
#ifndef FALSE
#define FALSE (0)
#endif
#endif
#endif

#ifndef M_PI
/*! constant pi */
#define M_PI (3.14159265358979)
#endif

/*! The machine precision.  More precisely, `eps' is the largest
     relative spacing between any two adjacent numbers in the machine's
     floating point system.  This number is obviously system-dependent.
     On machines that support 64 bit IEEE floating point arithmetic,
     `eps' is approximately  2.2204e-16.
     This variable is not set automatically (yet)
*/
#ifndef M_EPS
#define M_EPS (2.22044604925031e-16)


#define CTA_STRLEN_TAG   (80)
#define CTA_STRLEN_NAME  (80)


//#define CTA_ASSIMOBS    (1)
//#define CTA_VALIDATEOBS (2)
//#define CTA_ALLOBS      (3)

/*! flush stdout and stderr in a number of the COSTA methods. useful for debugging */
#define CTA_FLUSH_ALWAYS          ( 1)

/*! FORTRAN file unit for standard output */
#define CTA_F77_STDOUT            ( 6) 

#ifdef __cplusplus
extern "C" {
#endif


/** \brief Get the result of the C-function sizeof for a COSTA datatype
 *
 *  \param datatype I COSTA data type
 *  \param size     O receives result sizeof-function
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_SizeOf(CTA_Datatype datatype, int *size);

#ifdef __cplusplus
}
#endif


#endif

