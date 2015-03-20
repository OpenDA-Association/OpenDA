/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_time.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

OpenDA
Copyright (C) 2013  Nils van Velzen

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
 * OpenDA support the concept of resultwriters. The Data assimilation algorithm
 * will presents intermediate results and information on the computations to the
 * result writer. Each implementation of a resultwriter can then handle this data
 * in its own way.
 *
 * To implement an native resultwriter the following methods need to be implemented.
 * Note: for explanation on variables see resultwriter function methods. All strings
 *       have a len_... containing the result of strlen (used for using c-strings in Fortran)
 *
 * C:
 * 
 * void putmessage(int iDWriter, CTA_Tree config, char* workingDir, char* message, int *retval,
 *                 int len_workingDir, int len_message);
 *
 * void putvalue(int iDWriter, CTA_Tree config, char* workingDir, int id, int handle,
 *               int outputlevel, char* context, int iteration, int &retval,
 *               len_workingDir, len_id, len_context);
 *
 * void putiterationreport(int iDWriter, CTA_Tree config, char *workingDir, int iteration,
 *                          double cost, CTA_Vector handle, int *retval, int len_workingDir);
 *
 * void freewriter(int iDWriter);
 *
 * Fortran 2003:
 *
 * interface
 *    subroutine putmessage(iDWriter, config, workingDir, message, retval,         &
 *                          len_workingDir, len_message)                           &
 *       bind(C, NAME='putmessage')
 *       use iso_c_binding
 *       implicit none
 *       include 'cta_f90.inc'
 *
 *      integer(C_INT), VALUE  ::iDWriter, config, len_workingDir, len_message
 *      integer(C_INT)         ::retval
 *      character(kind=c_char) ::workingdir(*), message(*)
 *    end subroutine putmessage
 * end interface
 *
 *
 * interface
 *   subroutine putvalue(iDWriter, config, workingDir, id, handle,                 &
 *                       outputlevel, context, iteration, retval,                  &
 *                       len_workingDir, len_id, len_context)                      &
 *      bind(C, NAME='putvalue')
 *      use iso_c_binding
 *      implicit none
 *      include 'cta_f90.inc'
 *      integer(C_INT), VALUE  ::iDWriter, config, handle, outputlevel, iteration, &
 *                               len_workingDir, len_id, len_context
 *      integer(C_INT)         ::retval
 *      character(kind=c_char) ::workingdir(*), id(*), context(*)
 *   
 *    end subroutine putvalue
 * end interface
 *
 * interface
 *    subroutine putiterationreport(iDWriter, config, workingDir, iteration, cost,  &
 *                                  handle, retval, len_workingDir)                 &
 *       bind(C, NAME='putiterationreport')
 *       use iso_c_binding
 *       implicit none
 *       include 'cta_f90.inc'
 *       integer(C_INT), VALUE    ::iDWriter, config, iteration, handle, len_workingDir
 *       real(C_DOUBLE), VALUE    ::cost
 *       integer(C_INT)           ::retval
 *       character(kind=c_char)   ::workingdir(*)
 *    end subroutine putiterationreport
 * end interface
 *
 * interface
 *    subroutine freewriter(iDWriter)                                               &
 *       bind(C, NAME='freewriter')
 *       use iso_c_binding
 *       implicit none
 *       include 'cta_f90.inc'
 *       integer(C_INT), VALUE    ::iDWriter
 *    end subroutine freewriter
 * end interface

 * 
 * The resultwriter must have an xml-configuration file containing at least the following
 * (additional field may be used for private usage by the implementation)
 * Note name of library and funtion name must correspond to the user implementation.
 *
 * <?xml version="1.0" encoding="UTF-8"?>
 * <openda_native xmlns:xi="http://www.w3.org/2001/XInclude">
 *    <CTA_FUNCTION id="putvalue"               name="putvalue"              library="libresultwriter.so" function="putvalue"    />
 *    <CTA_FUNCTION id="putmessage"             name="putmessage"            library="libresultwriter.so" function="putmessage"    />
 *    <CTA_FUNCTION id="putiterationreport"     name="putiterationreport"    library="libresultwriter.so" function="putiterationreport"    />
 *    <CTA_FUNCTION id="freewriter"             name="freewriter"            library="libresultwriter.so" function="freewriter"    />
 * </openda_native>
 *
 * 
 * 
 * @author nils van velzen
 *
 */

#ifndef CTA_RESULTWRITER_H
#define CTA_RESULTWRITER_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"
/* Function Handle */
#ifdef __cplusplus
extern "C" {
#endif


/** \brief Handle a string message send to the resultwriter
 *
 * \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
 * \param config      I  Name of XML configuration file containting the function pointers and additional information
 * \param workingDir  I  Full path to working directory
 * \param message     I  Message send to resultwriter
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Resultwriter_putmessage(int idWriter, char *config, char *workingDir, char *message);

/** \brief Handle a string message send to the resultwriter
 *
 * \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
 * \param config      I  Name of XML configuration file containting the function pointers and additional information
 * \param workingDir  I  Full path to working directory
 * \param id          I  Name of the variable/array send to the resultwriter
 * \param handle      I  Handle (Vector or TreeVector) of variable
 * \param outputLevel I  Selected output level (see opendabridge for possible values)
 * \param context     I  Location from which the resultwriter was called
 * \param iteration   I  Iteration number from which the resultwriter was called
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Resultwriter_putvalue(int idWriter, char *config, char *workingDir, char *id, int handle, int outputLevel, char *context, int iteration);

/** \brief Handle a string message send to the resultwriter
 *
 * \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
 * \param config      I  Name of XML configuration file containting the function pointers and additional information
 * \param workingDir  I  Full path to working directory
 * \param iteration   I  Iteration number from which the resultwriter was called
 * \param cost        I  Value of cost function
 * \param handle      I  Handle (Vector or TreeVector) of the current parameters 
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Resultwriter_putiterationreport(int idWriter, char *config, char *workingDir, int iteration, double cost, int handle);

/** \brief Free a resultwriter (close output files etc).
 *
 * \param idWriter    I  ID of this resultwriter (Counter of number of native result writers)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Resultwriter_free(int idWriter);


#ifdef __cplusplus
}
#endif
#endif


