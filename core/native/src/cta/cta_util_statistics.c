/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_util_statistics.c $
$Revision: 4200 $, $Date: 2013-11-01 10:59:20 +0100 (Fri, 01 Nov 2013) $

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

#include <math.h>
#include <stdio.h>
#include "f_cta_utils.h"
#include "cta_datatypes.h"
#include "cta_errors.h"
#include "cta_util_statistics.h"

#define CTA_RAND_U_F77  F77_CALL(cta_rand_u,CTA_RAND_U)
#define CTA_RAND_N_F77  F77_CALL(cta_rand_n,CTA_RAND_N)

#define IM1 2147483563
#define IM2 2147483399
#define AM (1.0/IM1) 
#define IMM1 (IM1-1) 
#define IA1 40014
#define IA2 40692
#define IQ1 53668
#define IQ2 52774
#define IR1 12211
#define IR2 3791
#define NTAB 32
#define NDIV (1+IMM1/NTAB) 
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

static long CTAI_current_state_random=-21075;

float ran2(long *idum)
{
	int j;
	long k;
	static long idum2=123456789; static long iy=0;
	static long iv[NTAB];
	float temp;
	if (*idum <= 0) {
		if (-(*idum) < 1) *idum=1; else *idum = -(*idum); idum2=(*idum);
		for (j=NTAB+7;j>=0;j--) {
			k=(*idum)/IQ1;
			*idum=IA1*(*idum-k*IQ1)-k*IR1;
			if (*idum < 0) *idum += IM1; 
			if (j < NTAB) iv[j] = *idum;
		}
		iy=iv[0];
	 }
	k=(*idum)/IQ1;
	*idum=IA1*(*idum-k*IQ1)-k*IR1;
	if (*idum < 0) *idum += IM1;
	k=idum2/IQ2;
	idum2=IA2*(idum2-k*IQ2)-k*IR2;
	if (idum2 < 0) idum2 += IM2;
	j=iy/NDIV;
	iy=iv[j]-idum2;
	iv[j] = *idum;
	if (iy < 1) iy += IMM1;
	if ((temp= (float) AM*iy) > RNMX){
           return (float) RNMX;
        }
	else {
           return (float) temp;
        }
}


void CTA_rand_seed(int seed){
   printf("cta_util_statistics: Set initial seed: %ld\n",seed);
   if (seed=0) seed=-21075;
   if (seed>0) seed=-seed;
   CTAI_current_state_random=(long) seed;
   ran2(&CTAI_current_state_random);
}



int CTA_rand_u(double *x)
// Calculate a realization from a uniform [0 1] distribution
{
   //*x=(double) (random()/(double)(RAND_MAX));
   *x=(double) ran2(&CTAI_current_state_random);
   return CTA_OK;
}


int CTA_rand_n(double *x)
// Calculate a realization from a standard normal distribution
{
//    use the box-muller scheme, which calculates two standard
//    normal random numbers from two standard uniform random
//    numbers.

   // Remember the extra value in variable value2;
   //    remember whether an extra value is available in variable hebnog
   static BOOL hebnog=FALSE;
   static double value2;

   if (hebnog)
   {
   // extra value still available: return it
      hebnog = FALSE;
      *x = value2;
   }
   else
   {
   // no extra value available: calculate 2 normal rando:m numbers
   //   and return only one
     // double r1=(double) (random()+1.0)/(double) (RAND_MAX+1.0);
     // double r2=(double) (random()+1.0)/(double) (RAND_MAX+1.0);
     double r1= (double) ran2(&CTAI_current_state_random);
     double r2= (double) ran2(&CTAI_current_state_random);
      double hlp = sqrt(-2*log(r1));

      *x     = hlp * cos(2.0*M_PI*r2);
      value2 = hlp * sin(2.0*M_PI*r2);
      hebnog = TRUE;
   }
   return CTA_OK;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_RAND_U_F77(double *x, int *ierr){
   *ierr=CTA_rand_u(x);
}

CTAEXPORT void CTA_RAND_N_F77(double *x, int *ierr){
   *ierr=CTA_rand_n(x);
}



