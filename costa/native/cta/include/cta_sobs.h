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
\file  cta_sobs.h
\brief Interface description of the COSTA default stochastic observer component. For user implementation see cta_usr_sobs.h.
*/

#ifndef CTA_SOBS_H
#define CTA_SOBS_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"
#include "cta_vector.h"
#include "cta_matrix.h"
#include "cta_time.h"

/* StochObs and Decription Handles */
typedef CTA_Handle CTA_StochObs;
typedef CTA_Handle CTA_SObsClass;

#include "cta_obsdescr.h"

/* parameters for different functions */
#define CTA_SOBS_CREATE_SIZE         ( 1)
#define CTA_SOBS_CREATE_INIT         ( 2)
#define I_CTA_SOBS_FREE                ( 3)
#define CTA_SOBS_CREATE_SELECTION    ( 4)
#define I_CTA_SOBS_COUNT               ( 5)
#define CTA_SOBS_GET_OBS_DESCRIPTION ( 6)
#define CTA_SOBS_GET_VALUES          ( 7)
#define CTA_SOBS_GET_REALISATION     ( 8)
#define CTA_SOBS_GET_EXPECTATION     ( 9)
#define CTA_SOBS_EVALUATE_PDF        (10)
#define CTA_SOBS_GET_COV_MATRIX      (11)
#define CTA_SOBS_GET_VARIANCE        (12)
#define I_CTA_SOBS_EXPORT              (13)
#define CTA_SOBS_GET_TIMES           (14)
#define CTA_SOBS_NUMFUNC             (15)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new class (=implementation) of a COSTA stochastic observer component.
 *
 * \param name          I  name of the new stochastic observer class
 * \param h_func        I  COSTA function handles for functions that implement class,
 *                         missing functions must have value CTA_NULL
 * \param descrcl       I  class of the observation description that is created by stochastic observer
 * \param hstochobscl   O  handle of new stochastic observer class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_DefineClass(const char *name, const CTA_Func h_func[CTA_SOBS_NUMFUNC],
                         CTA_ObsDescrClass descrcl, CTA_SObsClass *hstochobscl);

/** \brief Create an instance of a stocastic observer
 *
 * \param hstochobscl I  stochastic observer class of new stochastic observer
 * \param userdata    IO userdata for creation (depends on class)
 * \param hstochobs   O  receives handle of new stochastic observer object
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_Create(CTA_SObsClass hstochobscl, CTA_Handle userdata,
                    CTA_StochObs *hstochobs);

/** \brief Create a new stochastic observer that is subset of existing stochastic observer.
 *
 * \param hsobsin  I  handle of the existing stochastic observer of
 *                    which  a selection is to be made
 * \param userdata IO inputs necessary for making a selection (depends on user implementation)
 * \param hsobsout O  receives handle of the new COSTA-stochastic observer, empty before calling, caller responsible for freeing after use
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_CreateSel(CTA_StochObs hsobsin, CTA_Handle userdata,
                       CTA_StochObs *hsobsout);

/** \brief Create a new stoch observer that is subset in time of existing stochastic observer.
 *
 *         All observations in the closed interval [t1,t2] of the time span are selected.
 *
 * \param hsobsin  I  handle of the stochastic observer of
 *                    which a selection is to be made
 * \param timespan I  time span over which selection has to be made
 * \param hsobsout O  receives handle of the new COSTA-stochastic observer, empty before calling
 * \return error states: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_CreateTimSel(CTA_StochObs hsobsin, CTA_Time timespan,
                          CTA_StochObs *hsobsout);


/** \brief Count the number of elements in stochastic observer.
 * \param hsobs    I  handle of the stochastic observer
 * \param nmeasr   O  receives number of measurements in this observer
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_Count(CTA_StochObs hsobs, int *nmeasr);


/** \brief Get a vector with the measurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle of vector that receives the measurements; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetVal(CTA_StochObs hsobs,CTA_Vector hvec);


/** \brief Count the times associated to the measurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle to vector that receives the times; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetTimes( CTA_StochObs hsobs,CTA_Vector hvec);


/** \brief Draw random values (measurements) according to the probability density
 *         function of the mesurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle of vector that receives the draw (measurements); must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetRealisation(CTA_StochObs hsobs, CTA_Vector hvec);


/** \brief Get expectation of the probability density function of the mesurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle of vector that receives the expectation values; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetExpectation(CTA_StochObs hsobs, CTA_Vector hvec);


/** \brief Get the value of the probability density function of the mesurements at given location.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvecx    I  handle of vector with location for evaluating pdf
 * \param hvecy    IO handle of vector that is to contain the pdf-value; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_EvalPDF(CTA_StochObs hsobs, CTA_Vector hvecx, CTA_Vector hvecy);


/** \brief Get covariance matrix of probability density function of the measurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hmat     IO handle of matrix that receives the covariance matrix; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetCovMat(CTA_StochObs hsobs, CTA_Matrix hmat);


/** \brief Get variance of probability density function of the mesurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle of vector that receives the variance; must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetVar( CTA_StochObs hsobs, CTA_Vector hvec);


/** \brief Get standard deviation of probability density function of the measurements.
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param hvec     IO handle of vector that is to contain the standard deviation
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetStd( CTA_StochObs hsobs, CTA_Vector hvec);


/** \brief Create the observation description corresponding to the stochastic observer.
 *
 * \note Caller is responsible for freeing the here created observation description
 *
 * \param hsobs      I  handle of the stochastic observer
 * \param hobsdescr  O  receives handle of newly created observation description class, empty before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_GetDescription(CTA_StochObs hsobs, CTA_ObsDescr *hobsdescr);


/** \brief Export the stochastic observer.
 *
 * \note Supported by CTA_DEFAULT_SOBS:\n
 *       output to file (userdata must contain handle of COSTA file)\n
 *
 * \param hsobs    I  handle of the stochastic observer
 * \param userdata I  configuration of output
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_Export(CTA_StochObs hsobs, CTA_Handle userdata);

/** \brief Free the stochastic observer
 *
 * \Note hsobs=CTA_NULL is allowed
 *
 * \param hsobs  IO handle of the stochastic observer, replaced by CTA_NULL on return
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_SObs_Free(
   CTA_StochObs *hsobs  /* Handle of stochastic observer  */
   );

#ifdef __cplusplus
}
#endif

#endif
