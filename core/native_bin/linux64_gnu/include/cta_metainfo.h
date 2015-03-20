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

#ifndef CTA_METAINFO_H
#define CTA_METAINFO_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"
#include "cta_string.h"
#include "cta_vector.h"
#include "cta_matrix.h"

/* Function Handle */
typedef CTA_Handle CTA_Grid;
typedef CTA_Handle CTA_Metainfo;
typedef CTA_Handle CTA_MetainfoClass;

#include "cta_sobs.h"

/* parameters for different functions */
#define CTA_METAINFO_CREATE_SIZE         ( 1)
#define CTA_METAINFO_CREATE_INIT         ( 2)
#define I_CTA_METAINFO_FREE                ( 3)
#define I_CTA_METAINFO_GET_REST            ( 4)
#define I_CTA_METAINFO_GET_KEYS            ( 5)
#define I_CTA_METAINFO_COUNT_OBSERVATIONS  ( 6)
#define I_CTA_METAINFO_COUNT_PROPERTIES    ( 7)
#define I_CTA_METAINFO_EXPORT              ( 8)
#define CTA_METAINFO_NUMFUNC             ( 9)




typedef struct {
char name[CTA_STRLEN_TAG];
  int type;
  int nx, ny, nz,nsize;
double x_origin, y_origin, z_origin;
double dx,dy,dz;
   char refdimp[10][80];              // possible reference to  tree node where coordinates are
} CTAI_Gridm;

/* EXPLANATION TYPE OF GRID:
-99: undefined
-3: 3D regular grid, only by reference 
-2: 2D regular grid, only by reference 
-1: 1D regular grid, only by reference 
1: 1D regular grid
2: 2D regular grid
3: 3D regular grid
10: curve (?)
*/

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
CTAEXPORT int CTA_Metainfo_DefineClass(const char *name,
   const CTA_Func h_func[CTA_METAINFO_NUMFUNC],
   CTA_MetainfoClass *hobsdscrcl);

/** \brief Create a new observation description instance.
 *
 * \param hsobscl   I  class of new observation description
 * \param usrdat    IO data of the stochastic observer for which
                       a descriptor is to be created
 * \param hobsdscr  O  receives handle of created observation description object
 * \return error status: CTA_OK if successful
 */
 CTAEXPORT int CTA_Metainfo_Create( CTA_Metainfo *hobsdscr);
// int CTA_Metainfo_Create( CTA_MetainfoClass hsobscl,
//                          CTA_Handle usrdat, CTA_Metainfo *hobsdscr);

///** \brief Check whether given stochastic observer corresponds to this observation description
// *
// * \param hobsdscr  I  handle of observation description
// * \param hsobs     I  handle of stochastic observer
// * \return error status: CTA_OK if successful
// */
//int CTA_Metainfo_Check_SObs( CTA_Metainfo hobsdscr, CTA_StochObs hsobs);

/** \brief Get properties/values that correspond to a given key.
 *
 * \param hobsdscr   I  handle of observation description
 * \param Key        I  key for which the value is asked
 * \param Properties IO COSTA-vector that is to receive the values
 * \param datatype   I  data type of elements in properties vector, must be the same as of queried properties
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Metainfo_SetUnit( CTA_Metainfo hobsdscr,  char* Key); 



/** \brief set tag of metainfo

*/
CTANOEXPORT int CTA_Metainfo_GetDescription(CTA_Metainfo hobsdescr,  char **description);

CTANOEXPORT int CTA_Metainfo_IsEqual( CTA_Metainfo hmeta_x, CTA_Metainfo hmeta_y );

CTANOEXPORT int CTAI_Grid_IsEqual( CTAI_Gridm *hgrid1, CTAI_Gridm *hgrid2 );

CTANOEXPORT int CTA_Metainfo_Import(CTA_Metainfo minfo, CTA_Handle usrdata);


CTANOEXPORT int CTA_Metainfo_SetTag(CTA_Metainfo hobsdescr,  char* tagname);


CTANOEXPORT int CTA_Metainfo_Free( CTA_Metainfo *hobsdscr);


CTANOEXPORT int CTA_Metainfo_SetDescription(CTA_Metainfo hobsdescr,  char* description);

CTANOEXPORT int CTA_Metainfo_SetRest(CTA_Metainfo hobsdescr, int *rest);

CTANOEXPORT int CTA_Metainfo_SetGrid(CTA_Metainfo hobsdescr, CTAI_Gridm *hgrid);

CTANOEXPORT int CTA_Metainfo_GetGrid(CTA_Metainfo hobsdescr, CTAI_Gridm *hgrid);

CTANOEXPORT int CTA_Metainfo_Copy(CTA_Metainfo hmeta_x, CTA_Metainfo hmeta_y);

CTANOEXPORT int CTA_Metainfo_GetRest(CTA_Metainfo hobsdescr, int *rest);

CTANOEXPORT int CTA_Metainfo_GetTag(CTA_Metainfo hobsdescr,  char *tagname);

CTANOEXPORT int CTA_Metainfo_SetBelongsTo(CTA_Metainfo hobsdescr,  char* tagname);

CTANOEXPORT int CTA_Metainfo_GetBelongsTo(CTA_Metainfo hobsdescr,  char *tagname);

CTANOEXPORT int CTAI_Grid_Interpolate(CTAI_Gridm gridy, CTAI_Gridm gridx, CTA_Vector vecx, CTA_Vector vecx_to_y);

CTANOEXPORT int CTA_Metainfo_Export(CTA_Metainfo minfo, CTA_Handle usrdata);

CTANOEXPORT int CTA_Metainfo_GetUnit(CTA_Metainfo hobsdescr,  char *nameofunit);

CTANOEXPORT int CTAI_XML_CreateGrid(xmlNode *cur_node, CTAI_Gridm *thisgrid);


/* tijdelijk */
CTANOF90 CTAEXPORT int CTA_Metainfo_setRegGrid(CTA_Metainfo hdescr, char *name, 
                            int nx, int ny, int nz,
                            double x_origin, double y_origin,
                            double z_origin,
                            double dx, double dy, double dz);

CTANOF90 CTAEXPORT int CTA_Metainfo_getRegGrid(CTA_Metainfo hdescr, char *name, 
                            int *nx, int *ny, int *nz,
                            double *x_origin, double *y_origin,
                            double *z_origin,
                            double *dx, double *dy, double *dz);

#ifdef __cplusplus
}
#endif


#endif
