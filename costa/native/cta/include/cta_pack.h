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
#ifndef CTA_PACK_H
#define CTA_PACK_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"

/**
\file  cta_pack.h

\brief The interface description of the COSTA pack component.

The pack component is used for storing non-sequential data before it is
saved to file communicated in a parallel environment.
The pack component contains a memory buffer that can be filled with data. The size of the buffer is
automatically increased when new data is added.

The pack component uses the FIFO princeple. Data that is added first can be retreved first.

*/

/* Function Handle */
typedef CTA_Handle CTA_Pack;

/*! Reset pack/unpack pointer of pack object    */
#define CTA_PACK_RESET (-1)

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a pack instance.
 *
 * \param initsize I  the initial size >=0 of the buffer
 * \param hpack    O  receives handle of new pack object
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Pack_Create(int initsize, CTA_Pack *hpack);

/** \brief Free a pack instance.
 *
 * \param hpack   IO handle of pack object, replaced by CTA_NULL on return
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Pack_Free(CTA_Pack *hpack);

/** \brief Add data to pack object.
 *
 * \param hpack    IO handle of pack object
 * \param data     I  data that must be packed
 * \param lendat   I  size of the data to be packed (chars)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Pack_Add(CTA_Pack hpack, void *data, int lendat);

/** \brief Unpack (get) data from pack object.
 *
 * \param hpack    IO handle of pack object
 * \param data     O  buffer that receives data that is unpacked from pack-buffer (buffer length must be >= lendat)
 * \param lendat   I  size of the data to be unpacked (chars)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Pack_Get(CTA_Pack hpack, void *data, int lendat);

/** \brief Get pointer to pack-buffer.
 *
 * \param hpack    I  handle of pack object
 *
 * \return pointer to buffer
 */
CTANOF90 CTAEXPORT char* CTA_Pack_GetPtr(CTA_Pack hpack);

/** \brief Get length of packed data in pack-buffer.
 *
 * \param hpack    I  handle of pack object
 *
 * \return length packed data
 */
CTAEXPORT int CTA_Pack_GetLen(CTA_Pack hpack);

/** \brief Only update administration for added elements
 *
 * This function can be used to update the administration after the
 * pack-buffer is filled externally (e.g. using an mpi_recv)
 *
 * \param hpack    I  handle of pack object
 * \param lendat   I  number of added elements (chars) 
 *
 * \return length packed data
 */
CTAEXPORT int CTA_Pack_AddCnt(CTA_Pack hpack, int lendat);

/** \brief Get the internal pack and unpack pointers
 *
 * This function can be used to save to pointers and 
 * reset the state of the pack component after unpacking or adding 
 * some data
 *
 * \param hpack    I  handle of pack object
 * \param ip1      O  unpack pointer
 * \param ip2      O  pack pointer
 *
 * \return length packed data
 */
CTAEXPORT int CTA_Pack_GetIndx(CTA_Pack hpack, int *ip1, int *ip2);

/** \brief Set the internal pack and unpack pointers
 *
 * This function can be used to restore the pointers and 
 * reset the state of the pack component after unpacking or adding 
 * some data
 *
 * \param hpack    I  handle of pack object
 * \param ip1      I  unpack pointer. In order to reset all unpackin 
 *                    set to CTA_PACK_RESET
 * \param ip2      I  pack pointer. In order to reset the whole pack object
 *                    set to CTA_PACK_RESET
 *
 * \return length packed data
 */


CTAEXPORT int CTA_Pack_SetIndx(CTA_Pack hpack, int ip1, int ip2);

#ifdef __cplusplus
}
#endif
#endif
