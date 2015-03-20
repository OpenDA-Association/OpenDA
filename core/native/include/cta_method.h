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
\file  cta_method.h
\brief The interface description of the COSTA method component (this component is still under construction). For user implementation see cta_usr_method.h.

The COSTA method component is provided for calling data assimilation and calibration methods in a uniform way.
*/

#ifndef CTA_METHOD_H
#define CTA_METHOD_H
#include "cta_system.h"
#include "cta_errors.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_functions.h"

/* Method handle */
typedef CTA_Handle CTA_Method;
typedef CTA_Handle CTA_MethClass;

/* parameters for different functions */
#define CTA_METH_CREATE_SIZE  ( 1)
#define CTA_METH_CREATE_INIT  ( 2)
#define CTA_METH_RUN          ( 3)
#define CTA_METH_FREE         ( 4)
#define CTA_METH_NUMFUNC      ( 5)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a new class (=implementation) of a COSTA method.
 *
 * \param name     I  name of the new method class
 * \param h_func   I  COSTA function handles for functions that implement class,
 *                    missing functions must have value CTA_NULL
 * \param hmethcl  O  receives handle of new method class
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Meth_DefineClass(const char *name, const CTA_Func h_func[CTA_METH_NUMFUNC],
                         CTA_MethClass *hmethcl);

/** \brief Create an instance of a method.
 *
 * \param hmethcl   I  method class of new object
 * \param userdata  IO user data for creation (depends on class)
 * \param hmeth     O  receives handle of new method
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Meth_Create(CTA_Method hmethcl, CTA_Handle userdata, CTA_Method *hmeth);
                                  
/** \brief Run method.
 * \param hmeth     I  handle of method
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Meth_Run(CTA_Method hmeth);

/** \brief Free the method object.
 *
 * \Note hmeth=CTA_NULL is allowed
 *
 * \param hmeth  IO handle of method, replaced by CTA_NULL on return.
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Meth_Free(CTA_Method *hmeth);

#ifdef __cplusplus
}
#endif

#endif
