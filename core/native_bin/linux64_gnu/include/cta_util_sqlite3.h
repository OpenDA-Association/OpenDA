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
\file  cta_util_sqlite3.h
\brief Callback functions used for SQL access.
\note This is just a set of routines it does not define any COSTA component
*/

#ifndef CTA_UTIL_SQLITE3_H
#define CTA_UTIL_SQLITE3_H 
#include <sqlite3.h>
#include "cta_string.h"
#include "cta_datatypes.h"

typedef struct {
int index;
int dimension;
CTA_Datatype datatype;
void* values;
} CTAI_counter_vector;

typedef struct {
sqlite3 *db;
int     nusers;
char    *name;
} CTAI_util_sqlite3_database;

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Utility for returning values. Callback-function for sqlite3.
 *
 * \param vout         I  See SQLITE3 documentation
 * \param argc         I  See SQLITE3 documentation
 * \param argv         I  See SQLITE3 documentation
 * \param azColName    I  See SQLITE3 documentation
 * \return non-zero if an error occured (see SQLITE3 documentation)
 */
int CTAI_util_sqlite3_return_values(void *vout,
         int argc, char **argv, char **azColName);


/** \brief Utility for selecting values. Callback-function for sqlite3.
 *
 * \param out          O selected values
 * \param nout         I dimension of the output array 
 * \param datatype     I data type of the output array 
 * \param db           I the database 
 * \param selection    I the column-name which is returned  
 * \param condition    I the selection creterion 
 * \return non-zero if an error occured (see SQLITE3 documentation)
 */
int CTAI_util_sqlite3_select_values( void *out,
    int nout, CTA_Datatype datatype, sqlite3 *db,
    const char *selection, const char *condition);


/** \brief Utility for returning keys. Callback-function for sqlite3.
 *
 * \param n_keys       O  number of keys in table
 * \param Keys         O  Keys of table
 * \param db           I  pointer to sql database
 * \param condition    I  the selection creterion
 * \return  non-zero if an error occured (see SQLITE3 documentation)
 */
int CTAI_util_sqlite3_return_keys( int *n_keys, CTA_String **Keys,
    sqlite3 *db, const char *condition);

#ifdef __cplusplus
}
#endif

#endif
