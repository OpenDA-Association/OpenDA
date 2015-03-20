/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_metainfo.c $
$Revision: 3407 $, $Date: 2012-08-17 13:50:50 +0200 (Fri, 17 Aug 2012) $

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

#include <stdio.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_sobs.h"
#include "cta_obsdescr.h"
#include "cta_metainfo.h"
#include "cta_errors.h"
#include "cta_handles.h"
#include "ctai.h"
#include "cta_file.h"
#include "cta_defaults.h"
#include "ctai_xml.h"
#include "cta_message.h"
#include "cta_pack.h"

#define CTA_METAINFO_DEFINEClASS_F77       F77_CALL(cta_metainfo_definecLass,CTA_METAINFO_DEFINECLASS)
#define CTA_METAINFO_CREATE_F77            F77_CALL(cta_metainfo_create,CTA_METAINFO_CREATE)
#define CTA_METAINFO_CHECK_SOBS_F77        F77_CALL(cta_metainfo_check_sobs,CTA_METAINFO_CHECK_SOBS)
#define CTA_METAINFO_GET_PROPERTIES_F77    F77_CALL(cta_metainfo_get_properties,CTA_METAINFO_GET_PROPERTIES)
#define CTA_METAINFO_GET_KEYS_F77          F77_CALL(cta_metainfo_get_keys,CTA_METAINFO_GET_KEYS)
#define CTA_METAINFO_PROPERTY_COUNT_F77    F77_CALL(cta_metainfo_property_count,CTA_METAINFO_PROPERTY_COUNT)
#define CTA_METAINFO_OBSERVATION_COUNT_F77 F77_CALL(cta_metainfo_observation_count,CTA_METAINFO_OBSERVATION_COUNT)
#define CTA_METAINFO_EXPORT_F77            F77_CALL(cta_metainfo_export,CTA_METAINFO_EXPORT)
#define CTA_METAINFO_IMPORT_F77            F77_CALL(cta_metainfo_import,CTA_METAINFO_IMPORT)
#define CTA_METAINFO_FREE_F77              F77_CALL(cta_metainfo_free,CTA_METAINFO_FREE)
#define CTA_METAINFO_SETTAG_F77            F77_CALL(cta_metainfo_settag,CTA_METAINFO_SETTAG)
#define CTA_METAINFO_SETBELONGSTO_F77      F77_CALL(cta_metainfo_setbelongsto,CTA_METAINFO_SETBELONGSTO)
#define CTA_METAINFO_SETGRID_F77      F77_CALL(cta_metainfo_setgrid,CTA_METAINFO_SETGRID)

#define CTA_METAINFO_SETREGGRID_F77        F77_CALL(cta_metainfo_setreggrid,CTA_METAINFO_SETREGGRID)
#define CTA_METAINFO_GETREGGRID_F77        F77_CALL(cta_metainfo_getreggrid,CTA_METAINFO_GETREGGRID)
#define CLASSNAME "CTA_Metainfo"

#define IDEBUG (0)


/* Struct holding all data associated to an COSTA Vector */
typedef struct {
CTA_Func functions[CTA_METAINFO_NUMFUNC];
} CTAI_MetainfoClass; // A MetainfoClass contains a list of the member-functions





typedef struct {
char unit[CTA_STRLEN_TAG];
char tag[CTA_STRLEN_TAG];
char belongs_to[CTA_STRLEN_TAG];
char *description;
int rest;
CTAI_Gridm *thisgrid;
CTA_Func functions[CTA_METAINFO_NUMFUNC]; // See cta_vector.h for a list of
} CTAI_Metainfo;




int CTAI_Grid_Init(
   CTAI_Gridm *hgrid,  /* Handle of the grid */
   int nx, int ny,int nz )
   {
   /* Local variables */
   int retval;             /* Return value of COSTA call   */

   /* copy */
   strcpy(hgrid->name,"gridname") ;
   hgrid->type = -99;   //undefined
   hgrid->nx = nx;    hgrid->ny = ny;    hgrid->nz = nz;
   hgrid->x_origin = 0.1;    hgrid->y_origin = 0.0;
   hgrid->z_origin = 0.0;
   hgrid->dx = 1.0;    hgrid->dy = 1.0;    hgrid->dz = 1.0;
   strcpy(hgrid->refdimp[1],"none");
   strcpy(hgrid->refdimp[2],"none");
   strcpy(hgrid->refdimp[3],"none");
   retval = 0;

   return retval;
};


int CTAI_Grid_IsEqual(
   CTAI_Gridm *hgrid1,  /* Handle of the grid */
   CTAI_Gridm *hgrid2 ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   int dimseq, chareq, realeq;
   double eps1=1.0E-4;
   double eps2=1.0E-6;
   /* compare */
   retval = CTA_DIMENSION_ERROR;
   chareq = (strcmp(hgrid1->name, hgrid2->name)==0);
   dimseq = ((hgrid1->type == hgrid2->type) &&   \
             hgrid1->nx == hgrid2->nx && hgrid1->ny == hgrid2->ny \
             &&  hgrid1->nz == hgrid2->nz );
   realeq = ((fabs(hgrid1->x_origin - hgrid2->x_origin) + \
             fabs(hgrid1->y_origin - hgrid2->y_origin) + \
             fabs(hgrid1->z_origin - hgrid2->z_origin) < eps1) && \
             (fabs(hgrid1->dx - hgrid2->dx) + \
             fabs(hgrid1->dy - hgrid2->dy) + \
              fabs(hgrid1->dz - hgrid2->dz) < eps2));
   if (dimseq && realeq) {
     if (!chareq) { printf("Grids are equal, but names differ \n");}
     retval = 0;}
   return retval;
};


int CTAI_Grid_Copy(
   CTAI_Gridm *hgrid1,  /* Handle of the sending grid */
   CTAI_Gridm *hgrid2   /* Handle of the receiving grid */
   ){

   /* Local variables */
   int retval=0;             /* Return value of COSTA call   */

   /* copy */
   //   printf("hgrid1->name=%s\n",hgrid1->name);
   if (
       (strcmp(hgrid1->name,"gridname") != 0) &
       (strcmp(hgrid1->name,"AR(1)-process") != 0)
       ) {// return -1234567;
}
   strcpy(hgrid2->name, hgrid1->name) ;
   hgrid2->type = hgrid1->type;
   hgrid2->nx = hgrid1->nx;    hgrid2->ny = hgrid1->ny;    hgrid2->nz = hgrid1->nz;
   hgrid2->x_origin = hgrid1->x_origin;    hgrid2->y_origin = hgrid1->y_origin;
   hgrid2->z_origin = hgrid1->z_origin;
   hgrid2->dx = hgrid1->dx;    hgrid2->dy = hgrid1->dy;    hgrid2->dz = hgrid1->dz;
   hgrid2->nsize = hgrid1->nsize;
   strcpy(hgrid2->refdimp[1],hgrid1->refdimp[1]);
   strcpy(hgrid2->refdimp[2],hgrid1->refdimp[2]);
   strcpy(hgrid2->refdimp[3],hgrid1->refdimp[3]);

   return retval;
};


/* ------------------------------------------------ */

int CTAI_Grid_Interpolate(CTAI_Gridm gridy, //desired grid
                          CTAI_Gridm gridx, // grid where vecx lives
                          CTA_Vector vecx,  // the vector to be interpolated
                                            // from gridx to gridy
                          CTA_Vector vecx_to_y ) // the resulting vector, living on gridy
{
  int retval, i,j,k,mi,mj,mk;
  int size_vecx;
  double px,py,pz;
  double xL, xR, yL, yR, zL, zR;
  int ixL, ixR, iyL, iyR, izL, izR;
  double fLLL, fRLL, fLRL, fRRL, fLLR, fRLR, fLRR, fRRR;
  double fyLL, fyRL, fyLR, fyRR, fzL, fzR, fint;


  if (CTAI_Grid_IsEqual(&gridx,&gridy) == CTA_OK) {
    // immediately finished; copy vecx_to_y to vecx
    if (IDEBUG>0) { printf("interpolating vectors on grid: grids are equal! \n");}
    retval = CTA_Vector_Copy(vecx, vecx_to_y);
    return retval;
  }


  CTA_Vector_GetSize(vecx, &size_vecx);
  if (size_vecx != gridx.nsize) return CTA_DIMENSION_ERROR;


  // dit werkt alleen goed voor uniform en rectilinear I en II,
  //        niet voor irregular/ curvilinear!
  // verder: eenheidsvectoren van grids moeten gelijk zijn! (niet gedraaid tov elkaar)
  // anders moet gridx sowieso eerst getransformeerd naar irregular grid
  //      met basisvectoren van gridy



  /* loop along grid points of desired gridy */
  for (i=1; i <= gridy.nx ;i++){
    for (j=1; j <= gridy.ny ;j++){
      for (k=1; k <= gridy.nz ;k++){
        px = gridy.x_origin + (i-1)*gridy.dx ; py = gridy.y_origin + (j-1)*gridy.dy ;
        pz = gridy.z_origin + (k-1)*gridy.dz ;


        /* search for grid points around px and values in x-direction  */
        if (gridx.nx ==1) {xL = gridx.x_origin; xR = xL + 1.0; ixL = 1; ixR=1;}
        else if (gridx.x_origin > px) {xL = gridx.x_origin; xR = xL +gridx.dx; ixL=1;ixR=2; }
        else if (gridx.x_origin+gridx.dx*(gridx.nx-1)< px) {
              xR = gridx.x_origin+gridx.dx*(gridx.nx-1); xL = xR -gridx.dx;ixL=gridx.nx-1;ixR=gridx.nx; }
        else {
          for (mi=1; mi <= gridx.nx-1; mi++) {
            if (gridx.x_origin + mi*gridx.dx > px ) {
              xR = gridx.x_origin+mi*gridx.dx ; xL = xR - gridx.dx; ixL=mi; ixR=mi+1;
              break;}
          }
        }
        /* same for y direction */
        if (gridx.ny ==1) {yL = gridx.y_origin; yR = yL + 1.0; iyL = 1; iyR=1;}
        else if (gridx.y_origin > py) {yL = gridx.y_origin; yR = yL +gridx.dy; iyL=1;iyR=2; }
        else if (gridx.y_origin+gridx.dy*(gridx.ny-1)< py) {
              yR = gridx.y_origin+gridx.dy*(gridx.ny-1); yL = yR -gridx.dy;iyL=gridx.ny-1;iyR=gridx.ny; }
        else {
          for (mj=1; mj <= gridx.ny-1; mj++) {
            if (gridx.y_origin + mj*gridx.dy > py ) {
              yR = gridx.y_origin+mj*gridx.dy ; yL = yR - gridx.dy; iyL=mj; iyR=mj+1;
              break;}
          }
        }
        /* same for z direction */
        if (gridx.nz ==1) {zL = gridx.z_origin; zR = zL + 1.0; izL = 1; izR=1;}
        else if (gridx.z_origin > pz) {zL = gridx.z_origin; zR = zL +gridx.dz; izL=1;izR=2; }
        else if (gridx.z_origin+gridx.dz*(gridx.nz-1)< pz) {
              zR = gridx.z_origin+gridx.dz*(gridx.nz-1); zL = zR -gridx.dz;izL=gridx.nz-1;izR=gridx.nz; }
        else {
          for (mk=1; mk <= gridx.nz-1; mk++) {
            if (gridx.z_origin + mk*gridx.dz > pz ) {
              zR = gridx.z_origin+mk*gridx.dz ; zL = zR - gridx.dz; izL=mk; izR=mk+1;
              break;}
          }
        }

      // is it always possible to convert to double?
        CTA_Vector_GetVal(vecx,ixL + (iyL-1)*gridx.nx +(izL-1)*gridx.nx*gridx.ny, &fLLL, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixR + (iyL-1)*gridx.nx +(izL-1)*gridx.nx*gridx.ny, &fRLL, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixL + (iyR-1)*gridx.nx +(izL-1)*gridx.nx*gridx.ny, &fLRL, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixR + (iyR-1)*gridx.nx +(izL-1)*gridx.nx*gridx.ny, &fRRL, CTA_DOUBLE);

        CTA_Vector_GetVal(vecx,ixL + (iyL-1)*gridx.nx +(izR-1)*gridx.nx*gridx.ny, &fLLR, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixR + (iyL-1)*gridx.nx +(izR-1)*gridx.nx*gridx.ny, &fRLR, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixL + (iyR-1)*gridx.nx +(izR-1)*gridx.nx*gridx.ny, &fLRR, CTA_DOUBLE);
        CTA_Vector_GetVal(vecx,ixR + (iyR-1)*gridx.nx +(izR-1)*gridx.nx*gridx.ny, &fRRR, CTA_DOUBLE);


        //  printf("distances %f %f %f %f %f %f  \n",xR-px, yR-py, zR-pz,xR-xL,yR-yL,zR-zL);


        // lower z- layer
        fyLL = ((xR-px)/(xR-xL))*fLLL + ((px-xL)/(xR-xL))*fRLL ;
        fyRL = ((xR-px)/(xR-xL))*fLRL + ((px-xL)/(xR-xL))*fRRL ;
        fzL = ((yR-py)/(yR-yL))*fyLL + ((py-yL)/(yR-yL))*fyRL ;

        // upper z-layer
        fyLR = ((xR-px)/(xR-xL))*fLLR + ((px-xL)/(xR-xL))*fRLR ;
        fyRR = ((xR-px)/(xR-xL))*fLRR + ((px-xL)/(xR-xL))*fRRR ;
        fzR = ((yR-py)/(yR-yL))*fyLR + ((py-yL)/(yR-yL))*fyRR ;

        fint = ((zR-pz)/(zR-zL))*fzL + ((pz-zL)/(zR-zL))*fzR;


        retval = CTA_Vector_SetVal(vecx_to_y,(i + (j-1)*gridy.nx), &fint, CTA_DOUBLE);

      };


    }
  }


  return CTA_OK;


};

/* ------------------------------------------------ */
int CTAI_XML_CreateGrid(xmlNode *cur_node, CTAI_Gridm *thisgrid) {
   xmlNode       *values_node = NULL;     /* values child node */
   xmlNode       *val_node = NULL;        /* value child node */
   xmlChar *idtmp;
   xmlChar *lentmp;
   xmlChar *axestmp;
   xmlChar *reftmp;

   BOOL isphysicalSpace = FALSE;
   int ndims_comp = 0;
   int ndims_phys = 0;
   char dimidc[10][CTA_STRLEN_TAG];   // names of comp. dimensions
   char dimidp[10][CTA_STRLEN_TAG];   // names of physical dimensions
   char axes[10][CTA_STRLEN_TAG];     // list of referenced comp. dimensions
   int dimlength[10];                 // number of elements  of comp. dimension


   /* fill some properties of the grid */
   thisgrid->type =  -1;          //  actual coordinates not yet read
   strcpy(thisgrid->name,"xml_grid");
   thisgrid->nsize = 1;

   for (values_node = cur_node->children; values_node; values_node = values_node->next) {
     if (0 == strcmp("computationalSpace", (char *) values_node->name)) {
       /* read dimensions */

       for (val_node = values_node->children; val_node; val_node = val_node->next) {
         if (0 == strcmp("dimension", (char *) val_node->name)) {
           ndims_comp = ndims_comp + 1;

           idtmp = xmlGetProp(val_node, CTAI_XML_ID);
           strcpy(dimidc[ndims_comp], (char *) idtmp);
           lentmp = xmlGetProp(val_node, CTAI_XML_LENGTH);
           dimlength[ndims_comp] = atoi((char *) lentmp);
           thisgrid->nsize = thisgrid->nsize*dimlength[ndims_comp];
           //           printf("grid comp dims: %d %d %s \n",ndims_comp,dimlength[ndims_comp],dimidc[ndims_comp]);
         }
       }
       if (ndims_comp < 1 || ndims_comp > 3) {printf("wrong number of computational dimensions\n");
         exit(-1);
       }
     }

     /* the physicalSpace part is not always present. In that case an integer grid can be formed using
        the computationalSpace. */

     if (0 == strcmp("physicalSpace", (char *) values_node->name)) {
       isphysicalSpace = TRUE;
       /* read dimensions */
       for (val_node = values_node->children; val_node; val_node = val_node->next) {
         if (0 == strcmp("dimension", (char *) val_node->name)) {
           ndims_phys = ndims_phys + 1;

           idtmp = xmlGetProp(val_node, CTAI_XML_ID);
           strcpy(dimidp[ndims_phys], (char *) idtmp);

           axestmp = xmlGetProp(val_node, (xmlChar *) "axes");
           strcpy(axes[ndims_phys], (char *) axestmp);

           reftmp = xmlGetProp(val_node, (xmlChar *) "ref");
           strcpy(thisgrid->refdimp[ndims_phys], (char *) reftmp);

           //   printf("grid phys dims: %d  |%s|%s|%s| \n",
           //     ndims_phys,dimidp[ndims_phys],axes[ndims_phys],thisgrid->refdimp[ndims_phys]);
         }
       }
     }
   }
   if (isphysicalSpace == FALSE) {    //  make an integer, regular  grid
     thisgrid->nx = dimlength[1];
     if (ndims_comp == 1) {
       thisgrid->type = 1; thisgrid->ny = 1;  //degenerated 2D
       thisgrid->x_origin = 0.0; thisgrid->y_origin = 0.0; thisgrid->z_origin = 0.0;
       thisgrid->dx = 1.0; thisgrid->dy = 1.0; thisgrid->dz = 1.0;
     };
     if (ndims_comp == 2 ) {
       thisgrid->type = 1; thisgrid->ny=dimlength[2];  // 2D
       thisgrid->x_origin = 1.0; thisgrid->y_origin = 1.0; thisgrid->z_origin = 0.0;
       thisgrid->dx = 1.0; thisgrid->dy = 1.0; thisgrid->dz = 1.0;
     };
     if (ndims_comp == 3 ) { thisgrid->type = 2;
       thisgrid->ny = dimlength[2];thisgrid->nz=dimlength[3];  // 3D
       thisgrid->x_origin = 1.0; thisgrid->y_origin = 1.0; thisgrid->z_origin = 1.0;
       thisgrid->dx = 1.0; thisgrid->dy = 1.0; thisgrid->dz = 1.0;
     }
   } else {     ;
     /* the reference to the coordinate vectors is contained in the grid.
        We have to wait until the whole tree has been read to be able to access it. */
     thisgrid->type = -ndims_comp;   // this denotes (minus) the number of refernces to coordinates
   }

   return CTA_OK;
  };

/* ------------------------------------------------ */

#undef METHOD
#define METHOD "DefineClass" 
int CTA_Metainfo_DefineClass(
   // INPUTS:
      const char *name,                // Name of the new stochobs class
      const CTA_Func h_func[CTA_METAINFO_NUMFUNC],  // function handles to
                                       // the implementations of the
                                       // stochobs-class' functions.
   // OUTPUTS:
      CTA_MetainfoClass  *hdescrcl)    // The (handle to) the new
                                       //   observation descriptor-class
{

   CTAI_MetainfoClass *data;
   int retval;

   /* Allocate new Metainfo object */
   data=CTA_Malloc(sizeof(CTAI_MetainfoClass));
   data->functions[CTA_METAINFO_CREATE_SIZE        ]=
                      h_func[CTA_METAINFO_CREATE_SIZE        ];
   data->functions[CTA_METAINFO_CREATE_INIT        ]=
                      h_func[CTA_METAINFO_CREATE_INIT        ];
   data->functions[I_CTA_METAINFO_FREE               ]=
                      h_func[I_CTA_METAINFO_FREE               ];
   data->functions[I_CTA_METAINFO_GET_REST     ]=
                      h_func[I_CTA_METAINFO_GET_REST     ];
   data->functions[I_CTA_METAINFO_GET_KEYS           ]=
                      h_func[I_CTA_METAINFO_GET_KEYS           ];
   data->functions[I_CTA_METAINFO_COUNT_OBSERVATIONS ]=
                      h_func[I_CTA_METAINFO_COUNT_OBSERVATIONS ];
   data->functions[I_CTA_METAINFO_COUNT_PROPERTIES   ]=
                      h_func[I_CTA_METAINFO_COUNT_PROPERTIES   ];
   data->functions[I_CTA_METAINFO_EXPORT             ]=
                      h_func[I_CTA_METAINFO_EXPORT             ];

   // Allocate new handle
   retval=CTA_Handle_Create(name,CTA_METAINFOCLASS,data,hdescrcl);
   retval=CTA_Handle_GetData((CTA_Handle) *hdescrcl,(void**) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   // return error when unsuccesfull
   return retval;
}

int CTA_Metainfo_Create(CTA_Metainfo *hmetainfo){

   CTAI_Metainfo *metainfo;
   CTAI_Gridm initgrid;
   int retval;

   /* allocate memory for new metainfo object */
   metainfo=CTA_Malloc(sizeof(CTAI_Metainfo));
   metainfo->rest=0;
   strcpy(metainfo->unit,"-");
   strcpy(metainfo->tag,"_");
   strcpy(metainfo->belongs_to,"NONE");
   metainfo->description = CTA_Malloc(9*sizeof(char));
   strcpy(metainfo->description,"none yet");

   retval=CTAI_Grid_Init(&initgrid,3,2,1);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot initialize Grid");   
	   return retval;
   }
   // printf("cta_metainfo_create 1: initgrid.nx %d \n ",initgrid.nx);
   //printf("cta_metainfo_create 1: initgrid.x_origin %f \n ",initgrid.x_origin);

   metainfo->thisgrid=CTA_Malloc(sizeof(CTAI_Gridm));

   retval=CTAI_Grid_Copy(&initgrid, metainfo->thisgrid);
   //   printf("cta_metainfo_create 2:  %d \n ",initgrid.nx);

   if (retval) {
	   CTA_WRITE_ERROR("Cannot copy grid");
	   return retval;
   }

   /* Allocate new handle and return error when unsuccesfull */
   retval=CTA_Handle_Create("metainfo",CTA_METAINFO,metainfo,hmetainfo);

   if (retval){
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
	   return retval;
   }

   return CTA_OK;
}




#undef METHOD
#define METHOD "Copy" 
int CTA_Metainfo_Copy(
   CTA_Metainfo hmeta_x,  /* Handle of the sending metainfo */
   CTA_Metainfo hmeta_y   /* Handle of the receiving metainfo */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Metainfo *data_x;      /* All data of vector hvec_x  */
   CTAI_Metainfo *data_y;      /* All data of vector hvec_y  */


   retval=CTA_Handle_Check((CTA_Handle) hmeta_x,CTA_METAINFO);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hmeta_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hmeta_y,CTA_METAINFO);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hmeta_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }


   /* copy */
   //  printf("!!!controle copy:|%s|%s|  \n", data_x->tag, data_y->tag);
   strcpy(data_y->unit, data_x->unit) ;
   strcpy(data_y->tag, data_x->tag) ;
   strcpy(data_y->belongs_to, data_x->belongs_to) ;
   data_y->rest = data_x->rest ;
   //  data_y->thisgrid=CTA_Malloc(sizeof(CTAI_Gridm));
   retval = CTAI_Grid_Copy(data_x->thisgrid, data_y->thisgrid);


   return retval;
};


#undef METHOD
#define METHOD "IsEqual"
int CTA_Metainfo_IsEqual(
   CTA_Metainfo hmeta_x,  /* Handle of the metainfo1 */
   CTA_Metainfo hmeta_y   /* Handle of the metainfo2 */
   ){


   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_Metainfo *data_x;      /* All data of vector hvec_x  */
   CTAI_Metainfo *data_y;      /* All data of vector hvec_y  */

   retval=CTA_Handle_Check((CTA_Handle) hmeta_x,CTA_METAINFO);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
	   return CTA_FALSE;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hmeta_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return CTA_FALSE;
   }

   retval=CTA_Handle_Check((CTA_Handle) hmeta_y,CTA_METAINFO);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
	   return CTA_FALSE;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hmeta_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return CTA_FALSE;
   }


   /* compare */
   retval = CTAI_Grid_IsEqual(data_x->thisgrid, data_y->thisgrid);
   if (retval) return CTA_FALSE;
   
   retval = (strcmp(data_x->unit, data_y->unit)==0);
   if (retval) return CTA_FALSE;

   retval = (strcmp(data_x->tag, data_y->tag)==0);
   if (retval) return CTA_FALSE;

   retval = (strcmp(data_x->belongs_to, data_y->belongs_to)==0);
   if (retval) return CTA_FALSE;

   retval = (data_x->rest == data_y->rest);
   if (retval) return CTA_TRUE;
   
   return CTA_FALSE;
};





#undef METHOD
#define METHOD "SetUnit"
int CTA_Metainfo_SetUnit(CTA_Metainfo hobsdescr,  char* nameofunit){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   //      printf("metainfo_setunit getdata %d \n", retval);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(metainfo->unit,nameofunit);
   return retval;
}

#undef METHOD
#define METHOD "SetTag"
int CTA_Metainfo_SetTag(CTA_Metainfo hobsdescr,  char* tagname){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(metainfo->tag,tagname);
   return retval;
}

#undef METHOD
#define METHOD "SetDescription"
int CTA_Metainfo_SetDescription(CTA_Metainfo hobsdescr,  char* description){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   if (metainfo->description !=NULL) {
       free(metainfo->description);
      }
   metainfo->description = CTA_Malloc((1+strlen(description))*sizeof(char));
   strcpy(metainfo->description,description);
   return retval;
}

#undef METHOD
#define METHOD "GetDescription"
int CTA_Metainfo_GetDescription(CTA_Metainfo hobsdescr,  char **description){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK){
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   /* Note: description is supposed not be allocated yet */
   if (*description!=NULL) {
          free(*description);
   }
   *description = CTA_Malloc((1+strlen(metainfo->description))*sizeof(char));
   strcpy(*description, metainfo->description);
   return retval;
}

#undef METHOD
#define METHOD "SetBelongsTo"
int CTA_Metainfo_SetBelongsTo(CTA_Metainfo hobsdescr,  char* tagname){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK){
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(metainfo->belongs_to,tagname);
   return retval;
}



#undef METHOD
#define METHOD "GetUnit"
int CTA_Metainfo_GetUnit(CTA_Metainfo hobsdescr,  char *nameofunit){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(nameofunit,metainfo->unit);
   return retval;
}

#undef METHOD
#define METHOD "GetTag"
int CTA_Metainfo_GetTag(CTA_Metainfo hobsdescr,  char *tagname){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(tagname,metainfo->tag);
   return retval;
}

#undef METHOD
#define METHOD "GetBelongsTo"
int CTA_Metainfo_GetBelongsTo(CTA_Metainfo hobsdescr,  char *tagname){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   strcpy(tagname,metainfo->belongs_to);
   return retval;
}


#undef METHOD
#define METHOD "GetRest"
int CTA_Metainfo_GetRest(CTA_Metainfo hobsdescr, int *rest){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */


   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   *rest = metainfo->rest;
   if (retval) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_GetRest");
	   return retval;
   }

   //   retval = CTAI_Metainfo_member_function(hobsdescr, CTA_METAINFO_GET_REST,
   //                                      &metainfo, &memfun);
   //if (retval) return retval;

   /* Call (user) implementation */
   //memfun(metainfo,rest,&retval);
   return retval;
}

#undef METHOD
#define METHOD "SetRest"
int CTA_Metainfo_SetRest(CTA_Metainfo hobsdescr, int *rest){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */


   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   metainfo->rest = *rest;
   if (retval) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_SetRest");
	   return retval;
   }
   return retval;
}


#undef METHOD
#define METHOD "GetGrid"
int CTA_Metainfo_GetGrid(CTA_Metainfo hobsdescr, CTAI_Gridm *hgrid){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_Check((CTA_Handle) hobsdescr,CTA_METAINFO);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Handle is not a cta_metainfo handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   //   printf("metainfo_getgrid%d \n", retval);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval = CTAI_Grid_Copy(metainfo->thisgrid, hgrid);
   //printf("metainfo_getgrid%d \n", retval);

   if (retval) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_GetGrid");
	   return retval;
   }
   return retval;
}

#undef METHOD
#define METHOD "SetGrid"
int CTA_Metainfo_SetGrid(CTA_Metainfo hobsdescr, CTAI_Gridm *hgrid){
   /* Local variables */
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *metainfo;    /* All data of observer hmetainfo  */

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hobsdescr,(void**) &metainfo);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval = CTAI_Grid_Copy(hgrid, metainfo->thisgrid);
   if (retval) {
	   CTA_WRITE_ERROR("Error in CTA_Metainfo_SetGrid");
	   return retval;
   }
   return retval;
}



int CTA_Metainfo_Export(CTA_Metainfo minfo, CTA_Handle usrdata){
  int retval;
  CTAI_Gridm thisgrid;
  char unitname[CTA_STRLEN_TAG];
  FILE *file;          //File pointer
  CTA_Datatype userDataType;

  retval = CTA_Metainfo_GetUnit(minfo,unitname);
  retval = CTA_Metainfo_GetGrid(minfo,&thisgrid);

  retval = CTA_Handle_GetDatatype(usrdata, &userDataType);
  if (retval != CTA_OK) return retval;

  if (userDataType == CTA_PACK) {
      if (IDEBUG>0) printf("Metainfo: packing:\n");
      retval=CTA_Pack_Add(usrdata,unitname,sizeof(char)*CTA_STRLEN_TAG);
      retval=CTA_Pack_Add(usrdata,&thisgrid,sizeof(CTAI_Gridm));
   }

   else if (userDataType == CTA_FILE) {
     retval=CTA_File_Get(usrdata,&file);
     if (thisgrid.type > -99 ) { // grid is defined
       fprintf(file,"         Metainfo: gridtype, size: %d %d\n",thisgrid.type, thisgrid.nsize);
       fprintf(file,"         Metainfo: phys. dimensions:  %d %d %d\n",thisgrid.nx, thisgrid.ny,thisgrid.nz);
       if (thisgrid.type < 0) {fprintf(file,"         Metainfo: coordinates-reference :  %s\n",
                                     thisgrid.refdimp[1]);}
       else {
         fprintf(file,"         Metainfo: grid dx,dy,dz: %f %f %f\n",thisgrid.dx, thisgrid.dy, thisgrid.dz);
         fprintf(file,"         Metainfo: grid origin: %f %f %f\n",
                        thisgrid.x_origin, thisgrid.y_origin, thisgrid.z_origin);
       }
     }
   }
   else {
      return retval=CTA_FORMAT_NOT_SUPPORTED;
   }



  return retval;

}



int CTA_Metainfo_Import(CTA_Metainfo minfo, CTA_Handle usrdata){
  int retval;
  CTAI_Gridm thisgrid;
  char unitname[CTA_STRLEN_TAG];


  if (CTA_Handle_Check(usrdata,CTA_PACK)==CTA_OK) {

    if (IDEBUG>0) printf("Metainfo: unpacking:\n");
    retval=CTA_Pack_Get(usrdata,unitname,sizeof(char)*CTA_STRLEN_TAG);
    retval=CTA_Pack_Get(usrdata,&thisgrid,sizeof(CTAI_Gridm));

    retval = CTA_Metainfo_SetUnit(minfo,unitname);
    retval = CTA_Metainfo_SetGrid(minfo,&thisgrid);

    // controle:
    if (IDEBUG> 4) {
      printf("CONTROLE: metainfo import:\n -------------------------- \n");
      retval = CTA_Metainfo_Export(minfo,CTA_FILE_STDOUT);
      printf("controle: end  -------------------------- \n");
    }

   }

  else {
      return CTA_FORMAT_NOT_SUPPORTED;
   }



  return retval;

}



#undef METHOD
#define METHOD "Free"
int CTA_Metainfo_Free(CTA_Metainfo *hdescr){
   int retval;                 /* Return value of COSTA call    */
   CTAI_Metainfo *descr;    /* All data of observer hmetainfo  */

   /* Check for quick return */
   if (*hdescr==CTA_NULL) return CTA_OK;

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) *hdescr,(void*) &descr);
   if (retval!=CTA_OK) { 
	   char message[1024];
	   sprintf(message,"metainfo_free: error %d \n",retval);
	   CTA_WRITE_ERROR(message);
	   return retval;
   }

   free(descr->thisgrid);
   free(descr);
   retval = CTA_Handle_Free(hdescr);
   if (retval!=CTA_OK) { 
	   char message[1024];
	   sprintf(message,"metainfo_free: handle_free : error %d \n",retval); 
	   CTA_WRITE_ERROR(message);
   }
   return retval;
}


#undef METHOD
#define METHOD "SetRegGrid"
int CTA_Metainfo_setRegGrid(CTA_Metainfo hdescr, char *name,
                            int nx, int ny, int nz,
                            double x_origin, double y_origin,
                            double z_origin,
                            double dx, double dy, double dz){

   int retval;              /* Return value of COSTA call    */
   CTAI_Metainfo* descr;    /* All data of observer hmetainfo  */
   CTAI_Gridm *grid;
   int typeGrid;


   /* Check for quick return */
   if (hdescr==CTA_NULL) return CTA_OK;

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hdescr,(void**) &descr);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Just set the grid info */
   grid=descr->thisgrid;
   strcpy(grid->name,name);

   if (ny==0){
      typeGrid = 1;
   } else if (nz==0) {
      typeGrid = 2;
   } else {
      typeGrid = 3;
   }




   grid->type     =typeGrid;
   grid->nx       =nx;
   grid->ny       =ny;
   grid->nz       =nz;
   grid->nsize    =nx*ny;
   grid->x_origin =x_origin;
   grid->y_origin =y_origin;
   grid->z_origin =z_origin;
   grid->dx       =dx;
   grid->dy       =dy;
   grid->dz       =dz;

   return CTA_OK;
 /*refdimp[10][80]*/
}

#undef METHOD
#define METHOD "getRegGrid"
int CTA_Metainfo_getRegGrid(CTA_Metainfo hdescr, char *name,
                            int *nx, int *ny, int *nz,
                            double *x_origin, double *y_origin,
                            double *z_origin,
                            double *dx, double *dy, double *dz){

   int retval;              /* Return value of COSTA call    */
   CTAI_Metainfo* descr;    /* All data of observer hmetainfo  */
   CTAI_Gridm *grid;


   /* Check for quick return */
   if (hdescr==CTA_NULL) return CTA_OK;

   /* Get pointer to struct with observation description data */
   retval=CTA_Handle_GetData((CTA_Handle) hdescr,(void**) &descr);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* Just set the grid info */
   grid=descr->thisgrid;
   strcpy(name,grid->name);
   *nx       = grid->nx;
   *ny       = grid->ny;
   *nz       = grid->nz;
   *x_origin = grid->x_origin;
   *y_origin = grid->y_origin;
   *z_origin = grid->z_origin;
   *dx       = grid->dx;
   *dy       = grid->dy;
   *dz       = grid->dz;

   return CTA_OK;
 /*refdimp[10][80]*/
}



/* Interfacing with Fortran */

CTAEXPORT void CTA_METAINFO_DEFINECLASS_F77( char *name, int *h_func, int *hobsdscrcl,int *ierr, int len_name){

   char  *c_name;
   // create a c-string equivalent to name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);

   *ierr=CTA_Metainfo_DefineClass(name, (CTA_Func*) h_func, (CTA_MetainfoClass*) hobsdscrcl);

   free(c_name);
}
/*
void CTA_METAINFO_CREATE_F77(int *hsobscl, int *usrdat,
              int *hmetainfo, int *ierr){

   *ierr=CTA_Metainfo_Create((CTA_MetainfoClass) *hsobscl, (CTA_Handle) *usrdat,
                             (CTA_Metainfo*) hmetainfo);
*/

CTAEXPORT void CTA_METAINFO_CREATE_F77(int *hmetainfo, int *ierr){

   *ierr=CTA_Metainfo_Create((CTA_Metainfo*) hmetainfo);
}


CTAEXPORT void CTA_METAINFO_SETTAG_F77(int *hobsdescr, char *tag, int *ierr, int len_tag) {
  char  *c_tag;
  // create a c-string equivalent to tag
  c_tag = CTA_Malloc((len_tag+1)*sizeof(char));
  CTA_fstr2cstr(tag,c_tag,len_tag);

   *ierr = CTA_Metainfo_SetTag((CTA_Metainfo) *hobsdescr,  c_tag);
   free(c_tag);
};

CTAEXPORT void CTA_METAINFO_SETBELONGSTO_F77(int *hobsdescr, char *tag, int *ierr, int len_tag) {
  char  *c_tag;
  // create a c-string equivalent to tag
  c_tag = CTA_Malloc((len_tag+1)*sizeof(char));
  CTA_fstr2cstr(tag,c_tag,len_tag);

   *ierr = CTA_Metainfo_SetBelongsTo((CTA_Metainfo) *hobsdescr,  c_tag);
   free(c_tag);
};


CTAEXPORT void CTA_METAINFO_SETGRID_F77(int *hobsdescr, int *hgrid, int *ierr) {
  *ierr= CTA_Metainfo_SetGrid((CTA_Metainfo) *hobsdescr, (CTAI_Gridm*) hgrid);
}


CTAEXPORT void CTA_METAINFO_EXPORT_F77(int *hdescr, int *usrdat, int *ierr){
   *ierr=CTA_Metainfo_Export((CTA_Metainfo) *hdescr, (CTA_Handle) *usrdat);
}

CTAEXPORT void CTA_METAINFO_IMPORT_F77(int *hdescr, int *usrdat, int *ierr){
   *ierr=CTA_Metainfo_Import((CTA_Metainfo) *hdescr, (CTA_Handle) *usrdat);
}


CTAEXPORT void CTA_METAINFO_FREE_F77(int *hobsdscr, int *ierr){
   *ierr=CTA_Metainfo_Free((CTA_Metainfo *) hobsdscr);
}


CTAEXPORT void CTA_METAINFO_GETREGGRID_F77(int *hdescr, char *name,
                            int *nx, int *ny, int *nz,
                            double *x_origin, double *y_origin,
                            double *z_origin,
                            double *dx, double *dy, double *dz, int *ierr, int lenstr){
   char  c_name[CTA_STRLEN_TAG];
   *ierr = CTA_Metainfo_getRegGrid((CTA_Metainfo) *hdescr, c_name, nx, ny, nz,
                            x_origin, y_origin, z_origin, dx, dy, dz);

   CTA_cstr2fstr(c_name,name,lenstr);
}

CTAEXPORT void CTA_METAINFO_SETREGGRID_F77(int *hdescr, char *name,
                            int *nx, int *ny, int *nz,
                            double *x_origin, double *y_origin,
                            double *z_origin,
                            double *dx, double *dy, double *dz, int *ierr, int strlen){

  char  *c_name;
  // create a c-string equivalent to name
  c_name=CTA_Malloc((strlen+1)*sizeof(char));
  CTA_fstr2cstr(name,c_name,strlen);

  *ierr=CTA_Metainfo_setRegGrid((CTA_Metainfo) *hdescr, c_name,
                            *nx, *ny, *nz,
                            *x_origin, *y_origin,
                            *z_origin,
                            *dx, *dy, *dz);
   free(c_name);

}



