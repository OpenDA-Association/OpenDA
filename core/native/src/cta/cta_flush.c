/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_flush.c $
$Revision: 3453 $, $Date: 2012-09-04 12:10:55 +0200 (Tue, 04 Sep 2012) $

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

#include "f_cta_utils.h"
#include "cta.h"

void CTA_Flush()
{
#ifndef WIN32
   fflush(stdout);
   fflush(stderr);
#endif
}
