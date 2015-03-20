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
\file  cta_time.h
\brief Interface description of the default COSTA time component.

A time object describes a time span (time of start, time of end). Utility functions are provided for changing and evaluating or comparing time spans.
*/

#ifndef CTA_TIME_H
#define CTA_TIME_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
/* Function Handle */
#ifdef __cplusplus
extern "C" {
#endif

typedef CTA_Handle CTA_Time;

/* functions */

/** \brief Create a time object.
 *
 * \param htime  O  receives handle of newly created time object
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_Create(CTA_Time *htime);

/** \brief Free a time object.
 *
 * \param htime    IO handle of time object to be freed, replaced by CTA_NULL on return.
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_Free(CTA_Time *htime);

/** \brief Set the time span.
 *
 * \param htime    IO time object of which to set time span
 * \param tstart   I  starting time
 * \param tend     I  ending time
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_SetSpan(CTA_Time htime,double tstart, double tend);

/** \brief Get the time span.
 *
 * \param htime    I  time object of which to get time span
 * \param tstart   O  receives the starting time
 * \param tend     O  receives ending time
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_GetSpan(CTA_Time htime,double *tstart, double *tend);

/** \brief Set the time step.
 *
 * \param htime    IO time object of which to set time step
 * \param tstep    I  new time step
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_SetStep(CTA_Time htime,double tstep);

/** \brief Get time step.
 *
 * \param htime    IO time object of which to get time step
 * \param tstep    O  receives time step
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_GetStep(CTA_Time htime,double *tstep);

/** \brief Count number of timesteps in time 
 *
 * \param htime    I time object (see function description)
 * \param nsteps   O number of timesteps
 *
 * \return error status: CTA_OK if successful
 *
 * \note number of steps is rounded to nearest integer
 */
CTAEXPORT int CTA_Time_CountSteps(CTA_Time htime, int *nsteps);

/** \brief Get interval of i-th step
 *
 * \param htime    I time object (see function description)
 * \param istep    I interval of step
 * \param hstep    O time step of model
 *
 * \return error status: CTA_OK if successful
 *
 * \note intervals are counted from 1 to nsteps
 */
CTAEXPORT int CTA_Time_GetTimeStep(CTA_Time htime, int istep, CTA_Time hstep);

/** \brief Check whether htimesub is within time span of htime.
 *
 * \param htimesub I time object (see function description)
 * \param htime    I time object (see function description)
 * \param inspan   O receives TRUE if htimesub is within time span of htime or FALSE otherwise
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_InSpan(CTA_Time htimesub, CTA_Time htime,  BOOL *inspan);

/** \brief Check whether time step of time object equals t
 *
 * \param htime    I time object
 * \param t        I time step to compare
 * \param isstep   O receives TRUE if t equals time step of time object or FALSE otherwise
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_IsStep(CTA_Time htime, double t,  BOOL *isstep);

/** \brief Copy a time object.
 *
 * \param hfrom    I time object to copy from
 * \param hto      O handle of time object that receives copy, must exist before calling
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_Copy(CTA_Time hfrom, CTA_Time hto);

/** \brief Export a time object.
 * exports the whole internal state of the time object to given target
 * CTA_FILE will export the time component in a MATLAB/OCTAVE readable form
 * CTA_PACK will pack the content
 *
 * \param htime    I time object to export
 * \param hexport  I target for export (CTA_FILE or CTA_PACK)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_Export(CTA_Time htime, CTA_Handle hexport);

/** \brief Import a time object.
 * imports the whole internal state of the time object from given source
 *
 * \param htime    I time object to import to 
 * \param himport  I source of import (CTA_PACK)
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_Import(CTA_Time htime, CTA_Handle himport);


/** \brief Returns whether time object describes an timespan or a single
 * instance.
 *
 * \param htime    I time object to import to 
 * \param isspan   O time object is a time timespan (CTA_TRUE/CTA_FALSE) 
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Time_IsSpan(CTA_Time htime, int *isspan);









#ifdef __cplusplus
}
#endif


#endif
