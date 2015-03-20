/*
COSTA: Problem solving environment for data assimilation
Copyright (C) 2007  Nils van Velzen

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
\file  cta_system.h
\brief Macros for handing system dependent issues
*/

#ifndef CTA_SYSTEM_H
#define CTA_SYSTEM_H

#define CTANOEXPORT /* does not do anything yet, but it makes clear which routines are internal */
#define CTANOF90 /* does not do anything yet, but it makes clear if no fortran90 header is needed */
#ifdef WIN32

	#ifdef CTALIB
		#define CTAEXPORT  __declspec(dllexport)
	#else
		#define CTAEXPORT  __declspec(dllimport)
	#endif

    #ifdef FTN_CAPITAL	
       #define F77_FUNC(X,Y) Y
	   #define F77_FUNC_NOEXP(X,Y) Y
    #else
       #define F77_FUNC(X,Y) X
	   #define F77_FUNC_NOEXP(X,Y) X
    #endif

	#ifdef FTN_CAPITAL
		#define F77_CALL(X,Y) __cdecl Y
		#define CF77_CALL(X,Y) Y
	#else
		#define F77_CALL(X,Y) __cdecl X
		#define CF77_CALL(X,Y) X
	#endif


#else

	#define CTAEXPORT
	#define F77_CALL(X,Y) F77_FUNC(X,Y)
	#define CF77_CALL(X,Y) F77_FUNC(X,Y)

#endif

#endif

