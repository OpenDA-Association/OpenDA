/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2006  Nils van Velzen

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
\file  cta_modbuild_b3b_utils_h.h
\brief Description of the COSTA blackbox component utilisties
*/

#ifndef CTA_MODBUILD_BB_UTILS_H
#define CTA_MODBUILD_BB_UTILS_H

#include <libxml/encoding.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xinclude.h>
#include <libxml/xmlwriter.h>

#include "cta.h"
#include "cta_xml.h"
#include "cta_bb_modbuild.h"

/** \brief Copy a string
 *
 * \param name     I  string to be copied
 *
 * \return copy of the string
 */
CTANOEXPORT char *        BB_CopyText(const char *in);

/** \brief Reallocates a block of memory
 *
 * \param memblock  I  Pointer to previously allocated memory block
 * \param size      I  New size in bytes.
 *
 * \return pointer to the allocated space
 */
CTANOEXPORT void *        BB_Realloc(void *memblock, size_t size);

/** \brief Allocates a block of memory
 *
 * \param nelem     I  Number of elements
 * \param elsize    I  Element size
 *
 * \return pointer to the allocated space
 */
CTANOEXPORT void *        BB_Malloc(size_t nelem, size_t elsize);
   
/** \brief Frees a block of memory that is currently allocated
 *
 * \param pntr     I  starting address of the memory block to be freed
 *
 * \return no return value
 */
CTANOEXPORT void          BB_Free(void *pntr);

/** \brief Frees a model that is currently allocated
 *
 * \param pntr     I  starting address of the model to be freed
 *
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int           BB_Free_Model(BB_ModelPntr model);

/** \brief Frees a stateexchange that is currently allocated
 *
 * \param pntr     I  starting address of the stateexchange to be freed
 *
 * \return no return value
 */
CTANOEXPORT void          BB_Free_StateExchange(BB_StateExchangePntr stateexchange);

/** \brief Frees a parameter that is currently allocated
 *
 * \param pntr     I  starting address of the parameter to be freed
 * \return no return value
 */
CTANOEXPORT void          BB_Free_Parameter(BB_ParameterPntr parameter);

/** \brief Frees a forcing that is currently allocated
 *
 * \param pntr     I  starting address of the forcing to be freed
 * \return no return value
 */
CTANOEXPORT void          BB_Free_Forcing(BB_ForcingPntr forcings);

/** \brief Frees a station that is currently allocated
 *
 * \param pntr     I  starting address of the station to be freed
 * \return no return value
 */
CTANOEXPORT void          BB_Free_Station(BB_StationPntr stations);

/** \brief Convert COSTA state vector to BB model
 *
 * \param model    I  Pointer naar de model data
 * \param start    I  Simulatie starttijd in modified julian day
 * \param stop     I  Simulatie stoptijd in modified julian day
 * \param sstate   I  Model state to be written
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int BB_State2Model(BB_ModelPntr model, double start, double stop, CTA_TreeVector sstate);

/** \brief Convert BB model to COSTA state vector
 *
 * \param model    I  Pointer naar de model data
 * \param start    I  Simulatie starttijd in modified julian day
 * \param stop     I  Simulatie stoptijd in modified julian day
 * \param sstate   O  Pointer naar state vector
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int  BB_Model2State(BB_ModelPntr model, double start, double stop, CTA_TreeVector *sstate);

/** \brief Open model2state file 
 *
 * \param model     I  Pointer naar de model data
 * \param htree     O  Pointer naar costa tree
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int           BB_OpenModel2State(BB_ModelPntr model, CTA_Tree *htree);

/** \brief Get value of BB model 
 *
 * \param model     I  Pointer naar de model data
 * \param htree     I  Pointer naar costa tree
 * \param station   I  Name of the station
 * \param variable  I  Name of the variabele
 * \param time      I  tijdstip in MJD
 * \param value     O  waarde van de variabele op tijdstip van station
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int           BB_Model2StateByStationVariableTime(BB_ModelPntr model, CTA_Tree htree, char *station, char *variable, double time, double *value);

/** \brief 
 *
 * \param model    I  Pointer naar de model data
 * \return error status: CTA_OK, CTA_ILLEGAL_HANDLE
 */
CTANOEXPORT int           BB_Run_Model(BB_ModelPntr model);

/** \brief 
 *
 * \param 
 * \return 
 */
CTANOEXPORT BB_ModelPntr BB_Read_Model(int simulationNumber, CTA_Tree hmodelinput, CTA_Handle *sstate, CTA_Handle *sparam, CTA_Handle *sforc);

/** \brief Check for a COSTA error
 *
 * \param ierr     I  COSTA error code
 * \param msg      I  Error message
 * \return no return value
 */
CTANOEXPORT void          BB_CheckError(int ierr, const char *msg);

#endif
