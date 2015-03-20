/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/openda_core/native/src/cta/cta_interface.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (di, 07 okt 2008) $

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

#ifndef CTA_MESSAGE_H
#define CTA_MESSAGE_H
#include "cta_functions.h"


#ifdef __cplusplus
extern "C" {
#endif

/** \brief Write a message
 *
 * \param className I name of class that writes the message
 * \param method    I name of the method that writes the message
 * \param message   I message
 * \param type      I type of message
 *                    -'M':message
 *                    -'I':Info
 *                    -'W':Warning
 *                    -'E':Error
 *                    -'F':Fatal error  (will terminate application)
 */
CTAEXPORT void CTA_Message_Write(const char *className, const char *method, const char *message, char type);

/** \brief Set an external writer for handling messages
 *
 *  An external writer must comply to the following C interface:
 *
 *  void my_writer(char *className, char *method, char *message, char type);
 *
 * \param externalWriter I External writer
 *
 *
 *  \note Fortran writers are not yet supported
 */
CTAEXPORT void CTA_Message_SetExternalWriter(CTA_Func externalWriter);


/** \brief Toggle message handler between quiet and normal mode.
 *  in the quiet mode no messages are send (not even to external writers)
 *
 * \param setting  I set message handler in quiet mode CTA_TRUE/CTA_FALSE
 *
 */
CTAEXPORT void CTA_Message_Quiet(int setting);


/** \brief Macro for writing errors
 *
 * The name of the class and the method name must be set by the defines CLSNAM  and METHOD
 *
 * \param message I Error message
 *
 */
#define CTA_WRITE_ERROR(message) CTA_Message_Write(CLASSNAME,METHOD,message,'E')

/** \brief Macro for writing info messages
 *
 * The name of the class and the method name must be set by the defines CLSNAM  and METHOD
 *
 * \param message I Info message
 *
 */
#define CTA_WRITE_INFO(message) CTA_Message_Write(CLASSNAME,METHOD,message,'I')

/** \brief Macro for writing warnings
 *
 * The name of the class and the method name must be set by the defines CLSNAM  and METHOD
 *
 * \param message I Warning message
 *
 */
#define CTA_WRITE_WARNING(message) CTA_Message_Write(CLASSNAME,METHOD,message,'W')

#ifdef __cplusplus
}
#endif
#endif

