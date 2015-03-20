/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_file.c $
$Revision: 3286 $, $Date: 2012-05-02 18:15:05 +0200 (Wed, 02 May 2012) $

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
#include <string.h>
#if HAVE_LIBNETCDF
#include <netcdf.h>
#endif
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_file.h"
#include "cta_errors.h"
#include "cta_message.h"

#define IDEBUG (0)

#define CTA_FILE_CREATE_F77   F77_CALL(cta_file_create,CTA_FILE_CREATE)
#define CTA_FILE_FREE_F77     F77_CALL(cta_file_free,CTA_FILE_FREE)
//#define CTA_FILE_GET_F77      F77_CALL(cta_file_get,CTA_FILE_GET)
//#define CTA_FILE_SET_F77      F77_CALL(cta_file_set,CTA_FILE_SET)
#define CTA_FILE_OPEN_F77     F77_CALL(cta_file_open,CTA_FILE_OPEN)
#define CTA_FILE_WRITESTR_F77 F77_CALL(cta_file_writestr,CTA_FILE_WRITESTR)

#define CTA_FILE_GETNETCDF_F77 F77_CALL(cta_file_getnetcdf,CTA_FILE_GETNETCDF)
#define CTA_File_ISNETCDF_F77  F77_CALL(cta_fILE_isnetcdf,CTA_FILE_ISNETCDF)
#define CLASSNAME "CTA_File"

typedef enum {text, netcdf} cta_filetype;


/* Struct holding all data associated to a COSTA file */

typedef struct {
cta_filetype fileType;
FILE *file;
int ncid;
} CTAI_File;



int CTA_File_Create(CTA_File *hfile){

   CTAI_File *cta_file;
   int retval;

   /* allocate memory for new File object */
   cta_file=CTA_Malloc(sizeof(CTAI_File));
   cta_file->fileType     = text;
   cta_file->file         = NULL;
   cta_file->ncid         = 0;

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("file",CTA_FILE,cta_file,hfile);
   return retval;
};


#undef METHOD
#define METHOD "Open"
int CTA_File_Open(CTA_File hfile, CTA_String spath, CTA_String smode){
   FILE *file;
   char *path;
   char *mode;
   int lenpath, lenmode, retval;
   int ncid;

   CTAI_File *cta_file;

   if (IDEBUG>0){printf("Start of CTA_File_Open\n");}
   path=NULL;
   mode=NULL;

   /* Chech handle and get data struct */
   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
      CTA_WRITE_ERROR("Handle is not a cta_time handle");
      return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   // handle path
   retval=CTA_String_GetLength(spath, &lenpath);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get length of string");
	   return retval;
   }
   path=CTA_Malloc((lenpath+1)*sizeof(char));
   retval=CTA_String_Get(spath, path);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get string");
	   return retval;
   }

   // determine the filetype
   if (strlen(path)>3){
      if (0==strncmp(path+strlen(path)-3,".nc",3)){
         cta_file->fileType=netcdf;
      }
   }
   if (cta_file->fileType==netcdf){
#if HAVE_LIBNETCDF
    if (IDEBUG>0){printf("Open netcdf file\n");}
{
   // handle mode
    if (smode==CTA_NULL){
        mode=CTA_Malloc(3*sizeof(char));
        strcpy(mode,"w");
    } else {
        retval=CTA_String_GetLength(smode, &lenmode);
        if (retval!=CTA_OK) {
           CTA_WRITE_ERROR("Cannot get length of string"); return retval;
        }
        mode=CTA_Malloc((lenmode+1)*sizeof(char));
        retval=CTA_String_Get(smode, mode);
        if (retval!=CTA_OK) {
          CTA_WRITE_ERROR("Cannot get string");
          return retval;
        }
    }
    // first check if file already exists;so we first try nc_open!!!
    if (0==strcmp(mode,"w"))  {
      retval = nc_create(path, NC_CLOBBER, &ncid);
      if (retval != NC_NOERR) {   // no success; so just create it
        char message[1024];
        sprintf(message,"Cannot create netCDF-file for no-append writing %s: %s\n", path, nc_strerror(retval));
        CTA_WRITE_ERROR(message);
      }
      if (IDEBUG>0){printf("Created new netcdf file\n");}
    } else {  //"r" or "a"
      retval = nc_open(path, NC_WRITE, &ncid);
      if (retval!=0){
        char message[1024];
        sprintf(message,"Cannot open netCDF-file %s for appending or reading: %s\n", path, nc_strerror(retval));
        CTA_WRITE_ERROR(message);
      }
      if (IDEBUG>0){printf("Open existing netcdf file\n");}
    }
    cta_file->ncid=ncid;
}

#else
      printf("CTA_File_Open: WARNING COSTA is compiled without NETCDF support\n");
      printf("               Will open file %s as normal text file \n",path);
      cta_file->fileType=text;
#endif
   }

   // Open normal text file
   if (cta_file->fileType==text){

      // handle mode
      if (smode==CTA_NULL){
        mode=CTA_Malloc(3*sizeof(char));
        strcpy(mode,"w+");
      } else {
         retval=CTA_String_GetLength(smode, &lenmode);
		 if (retval!=CTA_OK) {
			CTA_WRITE_ERROR("Cannot get length of string");
			return retval;
		 }
         mode=CTA_Malloc((lenmode+1)*sizeof(char));
         retval=CTA_String_Get(smode, mode);
		 if (retval!=CTA_OK) {
			 CTA_WRITE_ERROR("Cannot get string");	 
			 return retval;
		 }
      }

      // Open the file
      file=fopen(path,mode);
      CTA_File_Set(hfile,file);
   }
   if (path) free(path);
   if (mode) free(mode);
   if (IDEBUG>0){printf("End of CTA_File_Open");}
   return CTA_OK;
}

   #undef METHOD
   #define METHOD "WriteStr"
   int CTA_File_WriteStr(CTA_File hfile, char *str, int eol){
   CTAI_File *cta_file;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_time handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   fprintf(cta_file->file,"%s",str);
   if (eol==CTA_TRUE){
      fprintf(cta_file->file,"\n");
   }
   return CTA_OK;
}


#undef METHOD
#define METHOD "File_Free"
int CTA_File_Free(CTA_File *hfile)
{
   CTAI_File *cta_file;
   int retval;


   retval=CTA_Handle_Check((CTA_Handle) *hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_file handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   //Close netcdf file
   if (cta_file->fileType==netcdf){
#if HAVE_LIBNETCDF
      if (cta_file->ncid != 0) {
		  if ((retval = nc_close(cta_file->ncid))){
            printf("CTA_File_Free: cannot close netCDF-file: %s\n",
            nc_strerror(retval));
         }
      }
#endif
   }

   free(cta_file);

   retval=CTA_Handle_Free(hfile);

   return retval;
}

#undef METHOD
#define METHOD "IsNetcdf"
int CTA_File_IsNetcdf(CTA_File hfile, int *isnetcdf) {
   CTAI_File *cta_file;
   int retval;

   *isnetcdf=CTA_FALSE;

   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_file handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   if (cta_file->fileType==netcdf){
      *isnetcdf=CTA_TRUE;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "File_Set"
int CTA_File_Set(CTA_File hfile, FILE* file)
{
   CTAI_File *cta_file;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_file handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   cta_file->file = file;
   return CTA_OK;
}

#undef METHOD
#define METHOD "File_Get"
int CTA_File_Get(CTA_File hfile,FILE** file)
{
   CTAI_File *cta_file;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_time handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   *file = cta_file->file;
   return CTA_OK;
}

#undef METHOD
#define METHOD "File_GetNetcdf"
int CTA_File_GetNetcdf(CTA_File hfile,int *ncid)
{
   CTAI_File *cta_file;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hfile,CTA_FILE);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_time handle");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hfile,(void**) &cta_file);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   *ncid = cta_file->ncid;
   return CTA_OK;
}




/* Interfacing with Fortran */

CTAEXPORT void CTA_FILE_CREATE_F77(int *hfile, int *ierr){
   *ierr=CTA_File_Create((CTA_File*) hfile);
}

CTAEXPORT void CTA_FILE_FREE_F77(int *hfile, int *ierr){
   *ierr=CTA_File_Free  ((CTA_File*) hfile);
}

//NOT avaialble for Fortran:
//int CTA_File_Get   (CTA_File hfile, FILE **file);
//int CTA_File_Set   (CTA_File hfile, FILE *file);

CTAEXPORT void CTA_FILE_OPEN_F77(int *hfile, int *sname, int *smode, int *ierr){
   *ierr=CTA_File_Open((CTA_File) *hfile, (CTA_String) *sname,
                       (CTA_String) *smode);
}


CTAEXPORT void CTA_FILE_WRITESTR_F77(int *hfile, char *str, int *eol, int *ierr,
                       int len_str){

   char *c_str;
   /* create a c-string equivalent to name */
   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(str,c_str,len_str);

   *ierr=CTA_File_WriteStr((CTA_File) *hfile, c_str, *eol);

   free(c_str);
}


CTAEXPORT void CTA_FILE_GETNETCDF_F77(int *hfile,int *ncid, int *ierr){
   *ierr=CTA_File_GetNetcdf((CTA_File) *hfile, ncid);
}


CTAEXPORT void CTA_File_ISNETCDF_F77(int *hfile, int *isnetcdf, int *ierr) {
   *ierr=CTA_File_IsNetcdf((CTA_File) *hfile, isnetcdf);
}
