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
\file  cta_obsdescr.h
\brief Interface description of the COSTA default observation descriptor component. For user implementation see cta_usr_obs_descr.h.

CTA_ObsDescr is used for describing observations.
*/

#ifndef CTA_OBSDESCR_H
#define CTA_OBSDESCR_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"
#include "cta_string.h"
#include "cta_vector.h"
#include "cta_matrix.h"
#include "cta_reltable.h"

/* Function Handle */
typedef CTA_Handle CTA_ObsDescr;
typedef CTA_Handle CTA_ObsDescrClass;

#include "cta_sobs.h"

/* parameters for different functions */
#define I_CTA_OBSDESCR_CREATE_SIZE         ( 1)
#define I_CTA_OBSDESCR_CREATE_INIT         ( 2)
#define I_CTA_OBSDESCR_FREE                ( 3)
#define I_CTA_OBSDESCR_GET_PROPERTIES      ( 4)
#define I_CTA_OBSDESCR_GET_KEYS            ( 5)
#define I_CTA_OBSDESCR_COUNT_OBSERVATIONS  ( 6)
#define I_CTA_OBSDESCR_COUNT_PROPERTIES    ( 7)
#define I_CTA_OBSDESCR_EXPORT              ( 8)
#define I_CTA_OBSDESCR_SELECTION           ( 9)
#define I_CTA_OBSDESCR_NUMFUNC             (10)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new class (=implementation) of a COSTA observation description component.
 *
 * \param name       I  name of the new observation description class
 * \param h_func     I  COSTA function handles for functions that implement class,
 *                      missing functions must have value CTA_NULL
 * \param hobsdscrcl O  receives handle of new observation description class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_DefineClass(const char *name,
   const CTA_Func h_func[I_CTA_OBSDESCR_NUMFUNC],
   CTA_ObsDescrClass *hobsdscrcl);


/** \brief Create a new observation description instance.
 *
 * \param hsobscl   I  class of new observation description
 * \param usrdat    IO data of the stochastic observer for which
 *                     a descriptor is to be created
 * \param hobsdscr  O  receives handle of created observation description
 *                     object
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Create( CTA_ObsDescrClass hsobscl,
                          CTA_Handle usrdat, CTA_ObsDescr *hobsdscr);

/** \brief Create a new observation description that is subset of existing observation description.
 *
 * \param hobsdescr     I the observation description to create a subset
 *                        from
 * \param selection     I selection criterion (subset of SQL)
 * \param reltab        O Relation table specifying the relation between
 *                        the original and new observation description
 *                        component. Note no relation table is created when 
 *                        reltab==CTA_NULL on entry
 * \param hobsdescrout  O the new COSTA-stochastic observer, empty before
 *                        calling, caller responsible for freeing after use
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_CreateSel( CTA_StochObs hobsdescr, CTA_String selection,   
                            CTA_RelTable reltab, CTA_StochObs *hobsdescrout);

/** \brief Create a new observation description that is subset of existing observation description.
 *         All observations in the interval (t1,t2] (note t1 is not part
 *         of the interval!) of the time span are selected.
 *
 * \param hobsdescr     I the observation description to create a subset
 *                        from
 * \param timespan      I  time span over which selection has to be made
 * \param reltab        O Relation table specifying the relation between
 *                        the original and new observation description
 *                        component. Note no relation table is created when 
 *                        reltab==CTA_NULL on enty
  * \param hobsdescrout  O the new COSTA-stochastic observer, empty before
 *                        calling, caller responsible for freeing after use
 * \return error status: CTA_OK if successful
 */

CTAEXPORT int CTA_ObsDescr_CreateTimSel( CTA_ObsDescr hobsdescr, CTA_Time timespan,
                               CTA_RelTable reltab, CTA_ObsDescr *hobsdescrout);

/** \brief Get properties/values that correspond to a given key.
 *
 * \param hobsdscr   I  handle of observation description
 * \param Key        I  key for which the value is asked
 * \param Properties IO COSTA-vector that is to receive the values
 * \param datatype   I  data type of elements in properties vector, must be the same as of queried properties
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Get_ValueProperties( CTA_ObsDescr hobsdscr, const char* Key,
   CTA_Vector Properties, CTA_Datatype datatype); 

/** \brief Get all keys names.
 *
 * \param hobsdscr   I  handle of observation description
 * \param Keys       O  receives all keys (COSTA-string vector); must exist before calling and be large enough
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Get_PropertyKeys(CTA_ObsDescr hobsdscr, CTA_Vector Keys);

/** \brief Get number of properties/keys.
 *
 * \param hobsdscr   I  handle of observation description
 * \param nkeys      O  receives number of properties/keys
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Property_Count( CTA_ObsDescr hobsdscr, int *nkeys);

/** \brief Get number of observations.
 *
 * \param hobsdscr   I  handle of observation description
 * \param nobs       O  receives the number of observations
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Observation_Count( CTA_ObsDescr hobsdscr, int *nobs);

/** \brief Export observation description.
 *
 * The default observation description CTA_DEFAULT_OBSDESC supports exporting to:\n
 * TODO
 *
 * \param hdescr     I  handle of observation description
 * \param usrdat     IO export configuration/medium
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Export(CTA_ObsDescr hdescr, CTA_Handle usrdat);

/** \brief Free observation description object.
 *
 * \param hobsdscr  IO handle of observation description, replaced by CTA_NULL on return.
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_ObsDescr_Free(
   CTA_ObsDescr *hobsdscr  /* Handle of observation description */
   );

#ifdef __cplusplus
}
#endif

#endif
