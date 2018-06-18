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
\file  cta_obsdescr_sqlite3.h
\brief SQLite3 user implementation of the observation descriptor interface.
*/

#ifndef CTA_OBSDESCR_SQLITE3_H
#define CTA_OBSDESCR_SQLITE3_H

#include "cta_f77blas.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_functions.h"
#include "cta_obsdescr.h"
#include "cta_reltable.h"
#include "cta_util_sqlite3.h"
#include "cta_sobs_sqlite3.h"

typedef struct {
CTA_Handle myhandle;
CTAI_util_sqlite3_database *database;
char* condition;
int n_keys;
int nmeasr;
CTA_String *Keys;
} CTAI_ObsDescr_sqlite3;


#ifdef __cplusplus
extern "C" {
#endif
/** \brief Initialization function for defining ObsDescr_SQLite3 class
 *
 * \return no return value
 */
CTANOEXPORT void CTA_ObsDescr_sqlite3_initialise();


/** \brief Implementation function as part of the create process.
 *
 * \param memsize         O  receives the size of object data of new observation description
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Create_Size( int *memsize,int *retval );


/** \brief Implementation function as part of the create process.
           Makes object ready to use.
 *
 * \note userdata must contain handle of COSTA stochastic observer belonging to observation description
 *
 * \param myhandle        I  Handle assigned by COSTA
 * \param descr           IO pointer to object data of observation description
 * \param usrdat          I  user data (see note)
 * \param retval          O  receives the return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Create_Init(CTA_ObsDescr *myhandle, 
                                       CTAI_ObsDescr_sqlite3 *descr,
                                       CTA_Handle *usrdat, int *retval);


/** \brief Count the number of properties in the observation descriptor object.
 *
 * \param descr           I  pointer to object data of observation description
 * \param nkeys           O  receives number of property keys
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Property_Count(CTAI_ObsDescr_sqlite3 *descr, 
                            int* nkeys, int *retval);


/** \brief Count the number of observations in the observation descriptor object.
 *
 * \param descr           I  pointer to object data of observation description
 * \param nobs            O  receives the number of observations
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Observation_Count(CTAI_ObsDescr_sqlite3 *descr, 
                            int* nobs, int *retval);


/** \brief Get keys of observation descriptor object.
 *
 * \param descr           I  pointer to object data of observation description
 * \param Keys            I  handle of string vector that receives key descriptions, must exist before calling and have enough elements
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Get_Keys(CTAI_ObsDescr_sqlite3 *descr, 
                            CTA_Vector * Keys, int *retval);


/** \brief Get properties of CTAI_ObsDescr object.
 *
 * \param descr           I  pointer to object data of observation description
 * \param Key             I  pointer to C string describing key of which to get the properties
 * \param Properties      I  handle of vector that receives properties, must exist before calling and have enough elements
 * \param datatype        O  data type of vector elements
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Get_Properties(CTAI_ObsDescr_sqlite3 *descr, 
          const char *Key, CTA_Vector *Properties, CTA_Datatype *datatype,
          int *retval);


/** \brief Free the object data and associated resources.
 *
 * \param descr           IO object data of observation description to be freed
 * \param retval          O  receives return value
 * \return no return value
 */
void CTAI_ObsDescr_sqlite3_Free(CTAI_ObsDescr_sqlite3 *descr, 
                                int *retval);

/** \brief Create a new observation description that is subset of existing observation description.
 *
 * \param descr         I  object data of observation description to be freed
 * \param selection     I  selection criterion (subset of SQL)
 * \param reltab        O  Relation table specifying the relation between
 *                         the original and new observation description
 *                         component. Note no relation table is created when 
 *                         reltab==CTA_NULL on entry
 * \param my_handle_out I  Handle of new relation table 
 * \param descrout      O  new observation description created subset
 * \param retval        O  receives return value
 */
void CTAI_ObsDescr_sqlite3_CreateSel(CTAI_ObsDescr_sqlite3 *descr,
          CTA_String *selection, CTA_RelTable *reltab, 
          CTA_ObsDescr *myhandle_out,
          CTAI_ObsDescr_sqlite3 *descrout, int *retval);

#ifdef __cplusplus
}
#endif
#endif
