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
\file  cta_sobs_sqlite3.h
\brief SQLite3 user implementation of the stochastic observer interface.
*/

#ifndef CTA_SOBS_SQLITE3_H
#define CTA_SOBS_SQLITE3_H

#include "cta_f77blas.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_functions.h"
#include "cta_util_sqlite3.h"
#include "cta_sobs.h"
#include "cta_obsdescr.h"
#include <sqlite3.h>

typedef struct {
CTAI_util_sqlite3_database *database;
char* condition;
int nstations;
int nmeasr;
int * stations;
} CTAI_SObs_sqlite3;


#ifdef __cplusplus
extern "C" {
#endif

/** \brief Initialization function for defining the SObs_sqlite3 class.
 *
 * \return no return value
 */
CTANOEXPORT void CTA_SObs_sqlite3_initialise();


/** \brief Implementation function that is part of object create process
 *
 * \param memsize         O  receives size of new CTAI_SObs_sqlite3 object
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_Create_Size( int *memsize,int *retval );


/** \brief Implementation function that is part of object create process.
 *
 * \note userdata must contain handle of COSTA string giving the SQLITE3 database name.\n
 *
 * \param x               IO pointer to object data of stochastic observer
 * \param userdata        I  pointer to user data
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_Create_Init(CTAI_SObs_sqlite3 *x,
                            CTA_Handle userdata, int *retval);


/** \brief Create a new stochastic observer that is subset of existing stochastic observer.
 *
 * \note userdata must contain handle of COSTA string giving the selection condition.\n
 * \note This function fills in the object data of the new stochastic observer. The handle is created by CTA_SObs_CreateSel().
 *
 * \param obsin           I  pointer to object data of the existing stochastic observer
 * \param userdata        I  pointer to userdata
 * \param obsout          O  pointer to object data of the new COSTA-stochastic observer, must exist before calling
 * \param retval          I  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_CreateSel( CTAI_SObs_sqlite3 *obsin,
        CTA_Handle *userdata, CTAI_SObs_sqlite3 *obsout, int *retval);


/** \brief Count the number of measurements
 *
 * \param x               I pointer to object data of stochastic observer
 * \param nmeasr          O receives number of measurements
 * \param retval          O receives the return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_Count( CTAI_SObs_sqlite3 *x, int *nmeasr,
       int *retval);


/** \brief Get a vector with the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the measurements, must exist before calling
 * \param retval          O  receives the return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_GetVals( CTAI_SObs_sqlite3 *x, CTA_Vector *hvec,
       int *retval);


/** \brief Get times associated with the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the associated times, must exist before calling
 * \param retval          O  receives the return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_GetTimes( CTAI_SObs_sqlite3 *x, CTA_Vector *hvec,
       int *retval);


/** \brief Get variance of probability density function of the mesurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            I  handle of the vector that receives the variances, must exist before calling
 * \param returnvar       O  flag: TRUE for getting variance, FALSE for getting standard deviation
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_GetVariances( CTAI_SObs_sqlite3 *x,
       CTA_Vector *hvec, int *returnvar, int *retval);


/** \brief Draw random values (measurements) according to the probability density
 *         function of the mesurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the draw (measurements); must exist before calling
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_GetRealisation( CTAI_SObs_sqlite3 *x,
       CTA_Vector *hvec, int *retval);


/** \brief Get covariance matrix of probability density function of the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hmat            O  handle of matrix that receives the covariance matrix; must exist before calling
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_GetCovMat( CTAI_SObs_sqlite3 *x, CTA_Matrix *hmat,
       int *retval);


/** \brief Export a CTAI_SObs object to file or standard output.
 *
 * \note Supported:\n
 *       output to file (userdata must contain handle of COSTA file)\n
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param userdata        I  user data
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_export( CTAI_SObs_sqlite3 *x, CTA_Handle *userdata,
       int *retval);


/** \brief Free the object data and associated resources.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_SObs_sqlite3_Free( CTAI_SObs_sqlite3 *x, int *retval);

#ifdef __cplusplus
}
#endif

#endif
