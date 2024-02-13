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
\file  cta_util_statistics.h
\brief Utility for calculating a realization of a standard normal distribution.
\note This is just a set of utility routines it does not define any COSTA component

*/

#ifndef CTA_UTIL_STATISTICS_H
#define CTA_UTIL_STATISTICS_H
#include "cta_system.h"

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Initialize the random generator.
 *  \note only initialize the random generator once.
 * \param seed     I some positive initial seed
 */
CTAEXPORT void CTA_rand_seed(int seed);


/** \brief Get an uniform random number from the interval [0 1].
 *
 * \param x     O  receives the random number
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_rand_u(double *x);
	

/** \brief Get a random number from a normal distribution whit mean 0 and
 * standard deviation 1.
 *
 * \param x     O  receives the random number
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_rand_n(double *x);

#ifdef __cplusplus
}
#endif
#endif

