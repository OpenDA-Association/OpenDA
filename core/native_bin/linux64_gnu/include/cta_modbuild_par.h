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
\file  cta_modbuild_par.h

\brief Par-Modelbuilder.
*/

#ifndef CTA_MODBUILD_PAR_H
#define CTA_MODBUILD_PAR_H
#include "cta_handles.h"
#include "cta_datatypes.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create the model class of the Par-Modelbuilder and initilizes MPI
 *
 * \note This is not a user function. It is called at initialization of the
 *       COSTA environment.
 *
 *  \param modelcls  O  receives handle of the SP-modelbuilder class
 */
CTAEXPORT void CTA_Modbuild_par_CreateClass(CTA_ModelClass *modelcls);

/** \brief Stop the model class of the Par-Modelbuilder and finalize MPI
 *
 * \note This is not a user function. It is called at the finalization of the
 *       COSTA environment.
 *
 */
CTAEXPORT void CTA_Modbuild_par_Finalize();

#ifdef __cplusplus
}
#endif

#endif
