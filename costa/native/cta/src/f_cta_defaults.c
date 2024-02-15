/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/f_cta_defaults.c $
$Revision: 1400 $, $Date: 2010-03-18 16:03:08 +0100 (Thu, 18 Mar 2010) $

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

#include "cta_defaults.h"


#define CTA_DEFAULT_VECTOR_F77   F77_CALL(cta_default_vector,CTA_DEFAULT_VECTOR_F)
#define CTA_DEFAULT_SOBS_F77     F77_CALL(cta_default_sobs,CTA_DEFAULT_SOBS_F)
#define CTA_OBSDESCR_TABLE_F77   F77_CALL(cta_obsdescr_table,CTA_OBSDESCR_TABLE_F)
#define CTA_DEFAULT_MATRIX_F77   F77_CALL(cta_default_matrix,CTA_DEFAULT_MATRIX_F)
#define CTA_MODBUILD_SP_F77      F77_CALL(cta_modbuild_sp,CTA_MODBUILD_SP_F)
#define CTA_MODBUILD_PAR_F77     F77_CALL(cta_modbuild_par,CTA_MODBUILD_PAR_F)
#define CTA_FILE_STDOUT_F77      F77_CALL(cta_file_stdout,CTA_FILE_STDOUT_F)
#define CTA_MODELCOMBINER_F77    F77_CALL(cta_modelcombiner,CTA_MODELCOMBINER_F)
#define CTA_MODBUILD_B3B_F77     F77_CALL(cta_modbuild_b3b,CTA_MODBUILD_B3B_F)

CTAEXPORT int CTA_DEFAULT_VECTOR_F77(){return CTA_DEFAULT_VECTOR;};
CTAEXPORT int CTA_DEFAULT_SOBS_F77()  {return CTA_DEFAULT_SOBS;};
CTAEXPORT int CTA_OBSDESCR_TABLE_F77(){return CTA_OBSDESCR_TABLE;};
CTAEXPORT int CTA_DEFAULT_MATRIX_F77(){return CTA_DEFAULT_MATRIX;};
CTAEXPORT int CTA_MODBUILD_SP_F77()   {return CTA_MODBUILD_SP;};
CTAEXPORT int CTA_MODBUILD_PAR_F77()  {return CTA_MODBUILD_PAR;};
CTAEXPORT int CTA_FILE_STDOUT_F77()   {return CTA_FILE_STDOUT;};
CTAEXPORT int CTA_MODELCOMBINER_F77() {return CTA_MODELCOMBINER;};
CTAEXPORT int CTA_MODBUILD_B3B_F77()  {return CTA_MODBUILD_B3B;};



