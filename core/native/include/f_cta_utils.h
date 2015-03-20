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
\file  f_cta_utils.h

\brief String utility functions for FORTRAN / C compatibility.
*/

#include <string.h>
#include "cta_system.h"
#include "cta_datatypes.h"
#include "cta_errors.h"

#ifndef F_CTA_UTILS_H
#define F_CTA_UTILS_H
#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create C string from FORTRAN string
 *
 * \param f_str I pointer to FORTRAN string
 * \param c_str O buffer for receiving C string
 * \param len_f I length of FORTRAN string
 *
 * \return error status: always CTA_OK (no internal error check)
 */
int CTA_fstr2cstr(char *f_str,char *c_str, int len_f);

/** \brief Create FORTRAN string from C string
 *
 * \param c_str I pointer to C string
 * \param f_str O buffer for receiving FORTRAN string
 * \param len_f I length of C string
 *
 * \return error status: always CTA_OK (no internal error check)
 */
int CTA_cstr2fstr(char *c_str,char *f_str, int len_f);


#ifdef __cplusplus
}
#endif
#endif
