/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2007  Johan Ansink

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
\file  cta_datetime.h
\brief Interface description of the default COSTA datetime component.

A datetime object describes a date and time object
*/

#ifndef CTA_DATETIME_H
#define CTA_DATETIME_H

#include <time.h>
#include "cta_handles.h"
#include "cta_datatypes.h"
#include "cta_system.h"

#define MJDREF   2400000.5 

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Julian day number from Gregorian date.
 *
 * \param year    I  Year
 * \param month   I  Month
 * \param day     I  Day
 * \param hour    I  Hour
 * \param minute  I  Minute
 * \param second  I  Second
 * \param jd      O  Julian day number
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_DateTime_GregorianToJulian(int year, int month, int day, int hour, int minute, int second, double *jd);

/** \brief Modified Julian day number from Gregorian date.
 *
 * \param year    I  Year
 * \param month   I  Month
 * \param day     I  Day
 * \param hour    I  Hour
 * \param minute  I  Minute
 * \param second  I  Second
 * \param mjd     O  Modified Julian day number
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_DateTime_GregorianToModifiedJulian(int year, int month, int day, int hour, int minute, int second, double *mjd);

/** \brief Convert days into hours, minutes, and seconds. 
 *
 * \param days    I  Year
 * \param hour    O  Hour
 * \param minute  O  Minute
 * \param second  O  Second
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_DateTime_DaysToHMS (double days, int *hour, int *minute, int *second);

/** \brief Julian day number from Modified Julian day number
 *
 * \param mjd     I  Modified Julian day number
 * \param jd      O  Julian day number
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_DateTime_ModifiedJulianToJulian(double mjd, double *jd);

/** \brief Gregorian calendar date from Julian day number
 *
 * \param jd      I  Julian day number
 * \param year    O  Year
 * \param month   O  Month
 * \param day     O  Day
 * \param hour    O  Hour
 * \param minute  O  Minute
 * \param second  O  Second
 * \param jd      O  Julian day number
 *
 * \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_DateTime_JulianToGregorian(double jd, int *year, int *month, int *day, int *hour, int *minute, int *second);

/** \brief Gregorian calendar date from Modified Julian day number
 *
 * \param mjd     I  Modified Julian day number
 * \param year    O  Year
 * \param month   O  Month
 * \param day     O  Day
 * \param hour    O  Hour
 * \param minute  O  Minute
 * \param second  O  Second
 *
 * \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_DateTime_ModifiedJulianToGregorian(double mjd, int *year, int *month, int *day, int *hour, int *minute, int *second);

#ifdef __cplusplus
}
#endif

#endif
