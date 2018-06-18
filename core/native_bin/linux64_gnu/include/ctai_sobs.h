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
\file  ctai_stoch_observer.h
\brief Utility for stochastic observer object.
*/
#ifndef NL_VORTECH_CTAI_SOBS_H__
#define NL_VORTECH_CTAI_SOBS_H__

#ifdef __cplusplus
extern "C" {
#endif
/** \brief Get user data of stochastic observer object
 *
 *  \param hsobs I  handle of stochastic observer object of which to get user data
 *  \param index I  index in user data array

 *  \return handle of object at given index in the user data array of the stochastic observer object
 */
CTA_Handle CTAI_SObs_GetUserData(CTA_StochObs hsobs, int index);
#ifdef __cplusplus
}
#endif                                  
#endif
