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

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include "cta.h"

typedef struct {
int nkeys;
int nmeasr;
char **Keys;
char ***Columns;
} CTAI_ObsDescr_table;

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Explanation
 *
 * \param memsize         O  The number of bytes which are necessary to 
 *                           store one CTAI_SObs_sqlite3, with a 
 *                           pointer to the contents (data), 
 *                           but without the contents themselves.
 * \param retval          O  Error code (see cta_datatypes.h for possible 
 *                           error codes)
 * \return no return value
 */
void CTAI_ObsDescr_table_Create_Size(int *memsize, int *retval);

 /** \brief Allocate the memory which is necessary to 
 *          store the data necessary for a sqlite3-observer
 *
 * \param myhandle        I  Handle assigned by COSTA
 * \param descr           I  The sqlite3-observation description
 *                           for which the memory must be
 *                           allocated
 * \param usrdat          I  User data
 * \param retval          O  Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Create_Init(CTA_ObsDescr myhandle, CTAI_ObsDescr_table *descr,
CTA_Handle this, CTA_Handle *usrdat, int *retval);

/** \brief Explanation
 *
 * \param                 IO descr
 * \param                 IO Keys
 * \param retval          O  Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Get_Keys(CTAI_ObsDescr_table *descr, CTA_Vector *Keys, int *retval);

/** \brief Explanation
 *
 * \param                 IO descr
 * \param                 IO nkeys
 * \param retval          O  Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Property_Count(CTAI_ObsDescr_table *descr, int *nkeys, int *retval);

/** \brief Explanation
 *
 * \param descr           I 
 * \param nobs            I 
 * \param retval          O Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Observation_Count(CTAI_ObsDescr_table *descr, int *nobs, int *retval);

/** \brief Explanation
 *
 * \param descr           I 
 * \param Key             I 
 * \param Properties      I 
 * \param datatype        I 
 * \param nobs            I 
 * \param retval          O Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Get_Properties(CTAI_ObsDescr_table *descr, const char *Key, CTA_Vector *Properties,
         CTA_Datatype *datatype,int *retval);

/** \brief Explanation
 *
 * \param descr           I 
 * \param nobs            I 
 * \param usrdat          I 
 * \param retval          O Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Export(CTAI_ObsDescr_table *descr, CTA_Handle *usrdat, int *retval);
 
/** \brief Explanation
 *
 * \param descr           I 
 * \param retval          O Error code 
 * \return no return value
 */
void CTAI_ObsDescr_table_Free(CTAI_ObsDescr_table *descr, int *retval);


/** \brief The vector h_func is filled with COSTA-function handles of the 
 *         implementations in this file.
 * \param hobsdescrcl     I 
 * \return no return value
 */
CTANOEXPORT void CTA_ObsDescr_table_initialise(CTA_ObsDescrClass *hobsdescrcl);

#ifdef __cplusplus
}
#endif

