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
\file  cta_file.h
\brief Functions for the implementation of CTA_FILE objects.
*/

#ifndef CTA_FILE_H
#define CTA_FILE_H

#include <stdio.h>
#include "cta_system.h"
#include "cta_string.h"
#include "cta_handles.h"
#include "cta_datatypes.h"

#ifdef __cplusplus
extern "C" {
#endif
/* Function Handle */
typedef CTA_Handle CTA_File;

/* functions */

/** \brief Create a new COSTA file
 *   for holding a C file descriptor of a FORTRAN file LUN
 *
 *  \note This call does not open a file. 
 *   No FORTRAN support in this version
 *
 *  \param hfile  O  receives handle of created file
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_Create(CTA_File *hfile);

/** \brief Free a new COSTA file-handle
 *
 *  \note The File is not closed (in this version)
 *
 *  \param hfile  IO  handle of COSTA file CTA_NULL on return
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_Free  (CTA_File *hfile);

/** \brief Get the C-file descriptor of the COSTA file
 *
 *  \note
 *
 *  \param hfile  I  handle of COSTA file-handle
 *  \param file   O  receives file descriptor
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_Get   (CTA_File hfile, FILE **file);

/** \brief Get the NETCDF file id of the COSTA file
 *
 *  \note
 *
 *  \param hfile  I  handle of COSTA file-handle
 *  \param ncid   O  receives NETCDF file id
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_GetNetcdf(CTA_File hfile,int *ncid);


/** \brief Set the C-file descriptor of the COSTA file
 *
 *  \note
 *
 *  \param hfile  IO  handle of COSTA file-handle
 *  \param file   I   file descriptor
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_Set   (CTA_File hfile, FILE *file);

/** \brief Open a C-file and set descriptor 
 *
 *  \note
 *
 *  \param hfile   I  provide a valid handle of COSTA file
 *  \param sname   I  file path
 *  \param smode   I  open-mode (see C fopen documentation)
 *                    if CTA_NULL is provided, the file will be 
 *                    opened with read/write access (file pointer at begin
 *                    of file)
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_Open  (CTA_File hfile, CTA_String sname, CTA_String smode);

/** \brief check whether file is a NETCDF file 
 *
 *  \param hfile    I  provide a valid handle of COSTA file
 *  \param isnetcdf O  CTA_TRUE if file is NETCDF file CTA_FALSE otherwise
 *
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_IsNetcdf(CTA_File hfile, int *isnetcdf);


/** \brief Write a string to file
 *
 *  \note
 *
 *  \param hfile   I  handle of COSTA file
 *  \param str     I  string that must be written to file
 *  \param eol     I  Add end of line, CTA_TRUE for adding end of line or CTA_FALSE otherwise
 *  \return CTA_OK if successful
 */
CTAEXPORT int CTA_File_WriteStr(CTA_File hfile, char *str, int eol);

#ifdef __cplusplus
}
#endif

#endif

