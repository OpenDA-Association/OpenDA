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
\file  cta_util_methods.h
\brief Utility routines for creating data assimilation methods. 
\note This is just a set of utility routines it does not define any COSTA component

*/

#ifndef CTA_UTIL_METHODS_H
#define CTA_UTIL_METHODS_H

#ifdef __cplusplus
extern "C" {
#endif

/** \brief print the predicted values and the observed values
 *
 * \param fgModel     O Model of foreground run (with data
 *                      assimilation) or Vector with the predicted
 *                      values of the foreground run.
 *                      if set to CTA_NULL NaN will be printed as result
 * \param bgModel     I Model of background run (without data
 *                      assimilation) or Vector with the predicted
 *                      values of the background run.
 *                      if set to CTA_NULL NaN will be printed as result
 * \param sObs        I Stochastic observer
 * \param time        I Corresponding time
 * \param file        I Output file (note CTA_FILE_STDOUT prints to screen)
 * \param printHeader I Print header CTA_TRUE/CTA_FALSE
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Util_MethodsPrintObservations(CTA_Handle fgModel,
   CTA_Handle bgModel, CTA_StochObs sObs, CTA_Time time, CTA_File file,
    int printHeader);

/** \brief Make an initial selection of the observations
 *
 *  The selection of observations is based on the given simulation timespan
 *  and the criterion provided by the model (CTA_Model_GetObsSelect)
 *
 *  \note sObsSel is created and should be freed by the caller of this routine
 *
 * \param model      I Model (for CTA_Model_GetObsSelect)
 * \param sObsAll    I All observations
 * \param spanSim    I Simulation timespan
 * \param sObsSel    O Selection of observations
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Util_MethodsSelectObservations(CTA_Model model, CTA_StochObs sObsAll, CTA_Time spanSim, CTA_StochObs *sObsSel);

/** \brief Create an output file for filter predictions at station
 *         locations
 *
 *  The routine  CTA_Util_MethodsPrintObservations can be used for writing
 *  the results. 
 *  \note the header is written by this call
 *
 * \param   stationFile  (I) Name of result file
 * \param   fStationFile (O) Handle to result file
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Util_MethodsOpenResultFile(char *stationFile,
                                   CTA_File *fStationFile);

#ifdef __cplusplus
}
#endif
#endif

