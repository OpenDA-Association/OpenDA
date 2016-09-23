/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_initialise.c $
$Revision: 4055 $, $Date: 2013-07-03 10:16:57 +0200 (Wed, 03 Jul 2013) $

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

#include <locale.h>
#include "f_cta_utils.h"
#include "cta.h"
#include "cta_obsdescr_table.h"
#include "cta_modbuild_b3b.h"
#include "cta_sobs_netcdf.h"
#include "cta_sobs_combine.h"
#include "cta_sobs_sqlite3.h"

#define CTA_INITIALISE_F77 F77_CALL(cta_core_initialise,CTA_CORE_INITIALISE)
#define CTA_FINALISE_F77 F77_CALL(cta_core_finalise,CTA_CORE_FINALISE)
#define CTA_SOBS_MAORI_INITIALISE_F77 F77_CALL(cta_sobs_maori_initialise, CTA_SOBS_MAORI_INITIALISE)


int is_initialised=0;

int CTA_Core_Initialise()
{

   //Fixes a very very nasty bug that results in non working atof (string -> double) conversions
   setlocale(LC_ALL, "C");

   CTA_INITIAL_RANDOM_SEED=16111970; //Bas ;-)
//   CTA_INITIAL_RANDOM_SEED=2101975;  //Nils ;-)

   // Default library name for user routines
   strcpy(userDefaultDynamicLibrary,"libuseropenda");

   printf("OpenDA Native initialize: Initial random seed %ld\n",CTA_INITIAL_RANDOM_SEED);
   CTA_rand_seed((long) CTA_INITIAL_RANDOM_SEED);
   if (! is_initialised)
   {
      is_initialised=1;
      CTA_File_Create(&CTA_FILE_STDOUT);
      CTA_File_Set(CTA_FILE_STDOUT, stdout);
      CTA_SObs_sqlite3_initialise(&CTA_DEFAULT_SOBS);

      CTA_SObs_netcdf_initialise(&CTA_NETCDF_SOBS);

      CTA_SObs_combine_initialise(&CTA_COMBINE_SOBS);

      CTA_ObsDescr_table_initialise(&CTA_OBSDESCR_TABLE);


      CTA_Vector_blas_initialise (&CTA_DEFAULT_VECTOR);

      CTA_Matrix_blas_initialise (&CTA_DEFAULT_MATRIX);
      CTA_Modbuild_sp_CreateClass(&CTA_MODBUILD_SP);

      CTA_Func_Create("CTAI_Treevector_Operation_ScaledRMS",
             &CTAI_Treevector_Operation_ScaledRMS, CTA_NULL,
             &CTA_OP_ROOT_RMS);

      CTA_Func_Create("CTAI_Treevector_Operation_Amax",
             &CTAI_Treevector_Operation_Amax, CTA_NULL,
             &CTA_OP_ROOT_AMAX);

      CTA_Func_Create("CTAI_Treevector_Operation_PrintEntry",
             &CTAI_Treevector_Operation_PrintEntry, CTA_NULL,
             &CTA_OP_ROOT_PRINTI);

      CTA_Func_Create("CTAI_Treevector_Operation_ScaledSSQ",
             &CTAI_Treevector_Operation_ScaledSSQ, CTA_NULL,
             &CTA_OP_ROOT_SSQ);

      CTA_Message_Quiet(1);
      // Additional observers:
      // 1) MAORI
      CTA_SObs_maori_initialize(&CTA_MAORI_SOBS);
      // 2) USER
      CTA_SObs_user_initialize(&CTA_USER_SOBS);
      CTA_Message_Quiet(0);

      CTA_MODBUILD_PAR=CTA_NULL;

      if (0) {
      printf("cta_initialise :CTA_MODBUILD_PAR  =%d\n",CTA_MODBUILD_PAR);
      printf("cta_initialise :CTA_FILE_STDOUT   =%d\n",CTA_FILE_STDOUT);
      printf("cta_initialise :CTA_DEFAULT_SOBS  =%d\n",CTA_DEFAULT_SOBS);
      printf("cta_initialise :CTA_NETCDF_SOBS   =%d\n",CTA_NETCDF_SOBS);
      printf("cta_initialise :CTA_COMBINE_SOBS  =%d\n",CTA_COMBINE_SOBS);
      printf("cta_initialise :CTA_OBSDESCR_TABLE=%d\n",CTA_OBSDESCR_TABLE);
      printf("cta_initialise :CTA_DEFAULT_VECTOR=%d\n",CTA_DEFAULT_VECTOR);
      printf("cta_initialise :CTA_DEFAULT_MATRIX=%d\n",CTA_DEFAULT_MATRIX);
      printf("cta_initialise :CTA_MODBUILD_SP   =%d\n",CTA_MODBUILD_SP);
      printf("cta_initialise :CTA_MODELCOMBINER =%d\n",CTA_MODELCOMBINER);
      printf("cta_initialise :CTA_MODBUILD_B3B  =%d\n",CTA_MODBUILD_B3B);
      printf("cta_initialise :CTA_MODBUILD_BB   =%d\n",CTA_MODBUILD_BB);
      printf("cta_initialise :CTA_OP_ROOT_RMS   =%d\n",CTA_OP_ROOT_RMS);
      printf("cta_initialise :CTA_OP_ROOT_AMAX  =%d\n",CTA_OP_ROOT_AMAX);
      printf("cta_initialise :CTA_OP_ROOT_PRINTI=%d\n",CTA_OP_ROOT_PRINTI);
      printf("cta_initialise :CTA_OP_ROOT_SSQ   =%d\n",CTA_OP_ROOT_SSQ);
      printf("cta_initialise :CTA_MAORI_SOBS    =%d\n",CTA_MAORI_SOBS);
      printf("cta_initialise :CTA_USER_SOBS     =%d\n",CTA_USER_SOBS);
      exit(-1);
      }
   };
   return CTA_OK;
}


int CTA_Core_Finalise(){
   printf("Calling CTA_Finalise\n");
   CTA_Modbuild_par_Finalize();
   return CTA_OK;
}


/* Interfacing with Fortran */

CTAEXPORT void CTA_INITIALISE_F77 (int *ierr){
   *ierr=CTA_Core_Initialise();
}

CTAEXPORT void CTA_FINALISE_F77 (int *ierr){
   *ierr=CTA_Core_Finalise();
}
