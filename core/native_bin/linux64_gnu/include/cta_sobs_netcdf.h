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
\file  cta_sobs_netcdf.h
\brief Netcdf user implementation of the stochastic observer interface.
*/

#ifndef CTA_SOBS_NETCDF_H
#define CTA_SOBS_NETCDF_H

#include "cta_f77blas.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_functions.h"
#include "cta_sobs.h"
#include "cta_obsdescr.h"
#if HAVE_LIBNETCDF
//#include <netcdf.h>
#endif

typedef struct {
int     nusers;
int     ncid;
char    *dbname;
} CTAI_OMI_database;

typedef struct {
  CTAI_OMI_database *database;
  int nstations;
  double timeoffset;
  int nmeasr;
  int nmeasr_orig;  //number of observations in the netcdf-file
  CTA_RelTable selectionReltab;
  CTA_Time tspan;
  float bb_lon[2];
  float bb_lat[2];
} CTAI_SObs_netcdf;


#ifdef __cplusplus
extern "C" {
#endif

/** \brief Initialization function for defining the SObs_netcdf class.
 *
 * \return no return value
 */
CTANOEXPORT void CTA_SObs_netcdf_initialise();


/** \brief Implementation function that is part of object create process
 *
 * \param memsize         O  receives size of new CTAI_SObs_netcdf object
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_Create_Size( int *memsize,int *retval );


/** \brief Implementation function that is part of object create process.
 *
 * \note userdata must contain handle of COSTA string giving the NETCDF database name.\n
 *
 * \param x               IO pointer to object data of stochastic observer
 * \param userdata        I  pointer to user data
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_Create_Init(CTAI_SObs_netcdf *x,
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
CTANOEXPORT void CTAI_SObs_netcdf_CreateSel( CTAI_SObs_netcdf *obsin,
        CTA_Handle *userdata, CTAI_SObs_netcdf *obsout, int *retval);


/** \brief Count the number of measurements
 *
 * \param x               I pointer to object data of stochastic observer
 * \param nmeasr          O receives number of measurements
 * \param retval          O receives the return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_Count( CTAI_SObs_netcdf *x, int *nmeasr,
       int *retval);


/** \brief Get a vector with the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the measurements, must exist before calling
 * \param retval          O  receives the return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_GetVals( CTAI_SObs_netcdf *x, CTA_Vector *hvec,
       int *retval);


/** \brief Get times associated with the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the associated times, must exist before calling
 * \param retval          O  receives the return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_GetTimes( CTAI_SObs_netcdf *x, CTA_Vector *hvec,
       int *retval);


/** \brief Get variance of probability density function of the mesurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            I  handle of the vector that receives the variances, must exist before calling
 * \param returnvar       O  flag: TRUE for getting variance, FALSE for getting standard deviation
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_GetVariances( CTAI_SObs_netcdf *x,
       CTA_Vector *hvec, int *returnvar, int *retval);


/** \brief Draw random values (measurements) according to the probability density
 *         function of the mesurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hvec            O  handle of vector that receives the draw (measurements); must exist before calling
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_GetRealisation( CTAI_SObs_netcdf *x,
       CTA_Vector *hvec, int *retval);


/** \brief Get covariance matrix of probability density function of the measurements.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param hmat            O  handle of matrix that receives the covariance matrix; must exist before calling
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_GetCovMat( CTAI_SObs_netcdf *x, CTA_Matrix *hmat,
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
CTANOEXPORT void CTAI_SObs_netcdf_export( CTAI_SObs_netcdf *x, CTA_Handle *userdata,
       int *retval);


/** \brief Free the object data and associated resources.
 *
 * \param x               I  pointer to object data of stochastic observer
 * \param retval          O  receives return value
 * \return no return value
 */
CTANOEXPORT void CTAI_SObs_netcdf_Free( CTAI_SObs_netcdf *x, int *retval);

#ifdef __cplusplus
extern }
#endif

#endif

