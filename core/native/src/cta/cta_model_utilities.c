/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_model_utilities.c $
$Revision: 2288 $, $Date: 2011-05-11 16:18:21 +0200 (Wed, 11 May 2011) $

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
#include <string.h>
#include <stdio.h>
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_model_utilities.h"
#include "cta_xml.h"
#include "cta_message.h"

#define CTA_MODEL_UTIL_INPUTTREE_F77  F77_CALL(cta_model_util_inputtree,CTA_MODEL_UTIL_INPUTTREE)

#define CLASSNAME "CTA_Model_Utilities" 
/* Several utility routines that can be of usage when creating COSTA model components */

#undef METHOD
#define METHOD "Util_Inputtree"
int CTA_Model_Util_InputTree(CTA_Handle hinput, CTA_Tree *tinput, int *cleanup){
  int ierr; /*Return value of a costa method */
  int retval;
  CTA_Handle dt;

  /* check handle it must be a tree or an input-filename */
  retval = CTA_Handle_GetDatatype(hinput, &dt);
  if (retval != CTA_OK) {return retval;
  } else {
      if (dt == CTA_TREE) {

        //printf("ik denk dat dit een boom is \n");
        /* It is a tree only copy handle of root of tree */
        *tinput=hinput;
        *cleanup=CTA_FALSE;
        return CTA_OK;
      } else if (dt == CTA_STRING) {
        /* It is a string. Parse input-file with this name */
        // printf("ik denk dat dit een cta-string is \n");
        ierr=CTA_XML_Read(hinput, tinput);
        if (ierr!=CTA_OK) {
          CTA_WRITE_ERROR("xml_read failed");
          return ierr;
        }
        *cleanup=CTA_TRUE;
        return CTA_OK;
      } else {
        /* It is nor a tree nor a string */
        char message[1024];
        sprintf(message,"Input is neither tree nor string! \n");
        CTA_WRITE_ERROR(message);
        return CTA_INPUT_OBJECT_NOT_SUPPORTED;
      }
  }
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_MODEL_UTIL_INPUTTREE_F77(int *hinput, int *tinput, int *cleanup, int *ierr){
  *ierr= CTA_Model_Util_InputTree((CTA_Handle) *hinput, (CTA_Tree*) tinput, cleanup);
}

