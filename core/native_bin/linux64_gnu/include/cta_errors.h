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
\file cta_errors.h
\brief Definitions of error return values used by COSTA.
*/


#ifndef CTA_ERRORS_H
#define CTA_ERRORS_H

// Common errors
/** No errors occured */
#define CTA_OK                      ( 0)
/** The handle of a COSTA component is not associated to any
    COSTA component nor is it the CTA_NULL handle. */
#define CTA_ILLEGAL_HANDLE          (-2)
/** A handle of a COSTA component instance corresponds to a 
    different component than expected */
#define CTA_INCOMPATIBLE_HANDLE     (-3)
/** A handle of a COSTA component cannot be found in administration */
#define CTA_HANDLE_NOT_FOUND        (-4)
/** Output array is too short to contain result */
#define CTA_ARRAY_TOO_SHORT         (10)
/** The datatype does not exist or is not supported */
#define CTA_ILLEGAL_DATATYPE        (11)
/** Dimensions of the component are different from input */
#define CTA_DIMENSION_ERROR         (12)
/** The operation is not (yet) supported for given combination of vectors-classes */
#define CTA_INCOMPATIBLE_VECTORS    (13)
/** The concatenation operation is not possible */
#define CTA_CONCAT_NOT_POSSIBLE     (14)
/** The setval operation is not possible */
#define CTA_SETVAL_NOT_POSSIBLE     (15)
/** Requested item not found */
#define CTA_ITEM_NOT_FOUND          (16)
/** Trying to access an non-initialised subtreevector */
#define CTA_UNINITIALISED_SUBTREEVECTOR (17)
/** Operation between two different treevectors is not possible because build-up/dimension is different */
#define CTA_TREEVECTORS_NOT_COMPATIBLE   (18)
/** The operation is not (yet) supported for given combination of matrix-classes */
#define CTA_INCOMPATIBLE_MATRICES   (19)
/** There is no implementation available for this method */
#define CTA_NOT_IMPLEMENTED         (20)
/** Illegal specification of userdata argument */
#define CTA_WRONG_USERDATA          (21)
/** Opening of file failed */
#define CTA_CANNOT_OPEN_FILE        (22)
/** Command is not valid */
#define CTA_INVALID_COMMAND         (23)
/** Error in external library */
#define CTA_EXTERNAL_ERROR          (24)
/** The matrix is singular */
#define CTA_SINGULAR_MATRIX         (25)
/** The buffer length is too small */
#define CTA_BUFFER_TOO_SMALL        (26)
/** The import/export format is not supported */
#define CTA_FORMAT_NOT_SUPPORTED    (27)
/** The metainfo of two treevectors is incompatible */ 
#define CTA_INCOMPATIBLE_METAINFO   (28)
/** The combined model is invalid  */ 
#define CTA_COMBINED_MODEL_ERROR   (29)
/** The announced observations do not fit into timespan of compute */
#define CTA_ANNOUNCED_OBS_INTERVAL_ERROR (30)

/** Operation is not (yet) implemented for given object */
#define CTA_INPUT_OBJECT_NOT_SUPPORTED (31)
/** Input arguments are of incompatible data type */
#define CTA_INPUT_OBJECTS_ARE_INCOMPATIBLE (32)
/** The relation tables cannot be combined */
#define CTA_RELTABLES_CANNOT_BE_COMBINED (33)
/** The matrix is not square              */
#define CTA_MATRIX_IS_NOT_SQUARE (34)

/** The input value is not correct */
#define CTA_ILLEGAL_INPUT_ARGUMENT (34)

/** Cannot find the process group */
#define CTA_CANNOT_FIND_PROCESS_GROUP (35)

/** Some JNI-interface error */
#define CTA_JNI_INTERFACING_ERROR (36)
#define CTA_RESULTWRITER_ERROR (37)

/** Hmm. you have found an internal error in the COSTA implementation */
#define CTA_INTERNAL_ERROR          (911)
/** You tried to do something that should work but has not been implemented yet in COSTA.
    Please implement and help improving COSTA*/
#define CTA_NOT_YET_SUPPORTED       (999)

#endif
