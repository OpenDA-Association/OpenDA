/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_datetime.c $
$Revision: 3407 $, $Date: 2012-08-17 13:50:50 +0200 (Fri, 17 Aug 2012) $

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

#include <math.h>

#include "f_cta_utils.h"
#include "ctai.h"
#include "cta_datetime.h"
#include "cta_errors.h"

#define CTA_DATETIME_MODIFIEDJULIANTOGREGORIAN_F77   F77_CALL(cta_datetime_modifiedjuliantogregorian,CTA_DATETIME_MODIFIEDJULIANTOGREGORIAN)
#define CTA_DATETIME_GREGORIANTOMODIFIEDJULIAN_F77   F77_CALL(cta_datetime_gregoriantomodifiedjulian,CTA_DATETIME_GREGORIANTOMODIFIEDJULIAN)

int CTA_DateTime_GregorianToJulian(int year, int month, int day, int hour, int minute, int second, double *jd){

   /*Julian day number from Gregorian date */
   double a, y, m;

   a = floor((14 - month)/12);
   y = year + 4800 - a;
   m = month + 12*a - 3;

   /* For a date in the Gregorian calendar */

   *jd = day + floor((153.*m + 2.)/5.) + y*365. + floor(y/4.) - floor(y/100.) + floor(y/400.)
         - 32045. + ( second + 60.*minute + 3600.*(hour - 12.) )/86400.0;

	return CTA_OK;
}

int CTA_DateTime_GregorianToModifiedJulian(int year, int month, int day, int hour, int minute, int second, double *mjd){
   double jd;

   CTA_DateTime_GregorianToJulian(year,month,day,hour,minute,second,&jd);
   *mjd = jd- MJDREF;
   return CTA_OK;
}

int CTA_DateTime_DaysToHMS (double days, int *h, int *m, int *s){
   int hour, minute, second;

   second = (int)(86400 * days);
   /* get number of hours */
   hour   = second/3600;
   /* remove the hours */
   second = second - 3600*hour;
   /* get number of minutes */
   minute = second/60;
   /* remove the minutes */
   second = second - 60*minute;

   *h = hour;
   *m = minute;
   *s = second;

   return CTA_OK;
}

int CTA_DateTime_ModifiedJulianToJulian(double mjd, double *jd){
   *jd = mjd + MJDREF;
   return CTA_OK;
}

int CTA_DateTime_JulianToGregorian(double jd, int *year, int *month, int *day, int *hour, int *minute, int *second){

   double ijd;          // integer part
   double fjd;          // fraction part
   double a,b,c,d,e,m;

   ijd = floor(jd + 0.5);

   fjd = jd - ijd + 0.5;
   CTA_DateTime_DaysToHMS(fjd, hour, minute, second);

   a = ijd + 32044;
   b = floor((4 * a + 3) / 146097);
   c = a - floor((b * 146097) / 4);

   d = floor((4 * c + 3) / 1461);
   e = c - floor((1461 * d) / 4);
   m = floor((5 * e + 2) / 153);

   *day   = (int)(e - floor((153 * m + 2) / 5) + 1);
   *month = (int)(m + 3 - 12 * floor(m / 10));
   *year  = (int)(b * 100 + d - 4800 + floor(m / 10));

   return CTA_OK;
}

int CTA_DateTime_ModifiedJulianToGregorian(double mjd, int *year, int *month, int *day, int *hour, int *minute, int *second){

   int    ierr;   // error code
   double jd;     // Julian date

   ierr=CTA_DateTime_ModifiedJulianToJulian(mjd, &jd);
   if (ierr == CTA_OK){
      ierr=CTA_DateTime_JulianToGregorian(jd, year, month, day, hour, minute, second);
   }
   return ierr;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_DATETIME_GREGORIANTOMODIFIEDJULIAN_F77(int *year, int *month, int *day, int *hour, int *minute, int *second, double *mjd, int *ierr){
   *ierr=CTA_DateTime_GregorianToModifiedJulian(*year, *month, *day, *hour, *minute, *second, mjd);
}

CTAEXPORT void CTA_DATETIME_MODIFIEDJULIANTOGREGORIAN_F77(double *mjd, int *year, int *month, int *day, int *hour, int *minute, int *second, int *ierr){
  *ierr=CTA_DateTime_ModifiedJulianToGregorian(*mjd, year, month, day, hour, minute, second);
}

