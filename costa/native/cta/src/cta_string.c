/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_string.c $
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
#include <string.h>
#include <stdio.h>
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_vector.h"
#include "cta_string.h"
#include "cta_errors.h"
#include "cta_pack.h"
#include "cta_message.h"

#define CTA_STRING_CREATE_F77     F77_CALL(cta_string_create,CTA_STRING_CREATE)
#define CTA_STRING_FREE_F77       F77_CALL(cta_string_free,CTA_STRING_FREE)
#define CTA_STRING_GETLENGTH_F77  F77_CALL(cta_string_getlength,CTA_STRING_GETLENGTH)
#define CTA_STRING_SET_F77        F77_CALL(cta_string_set,CTA_STRING_SET)
#define CTA_STRING_GET_F77        F77_CALL(cta_string_get,CTA_STRING_GET)
#define CTA_STRING_GETVALUE_F77   F77_CALL(cta_string_getvalue,CTA_STRING_GETVALUE)
#define CTA_STRING_CONC_F77       F77_CALL(cta_string_conc,CTA_STRING_CONC)
#define CTA_STRING_DUPLICATE_F77  F77_CALL(cta_string_duplicate,CTA_STRING_DUPLICATE)
#define CTA_STRING_EXPORT_F77     F77_CALL(cta_string_export,CTA_STRING_EXPORT)
#define CTA_STRING_IMPORT_F77     F77_CALL(cta_string_import,CTA_STRING_IMPORT)

#define CLASSNAME "CTA_String"

/* Struct holding all data associated to an COSTA String */

typedef struct {
int len;
char *str;
} CTAI_String;

int CTA_String_Create(CTA_String *hstring){

   CTAI_String *string;
   int retval;

   /* allocate memory for new string object */
   string=CTA_Malloc(sizeof(CTAI_String));
   string->len=0;
   string->str=CTA_Malloc(sizeof(char));
   string->str[0]=(char)0;

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("string",CTA_STRING,string,hstring);
   return retval;
}

#undef METHOD
#define METHOD "Free" 
int CTA_String_Free(CTA_String *hstring)
{
   CTAI_String *string;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) *hstring,CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *hstring,(void**) &string);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   free(string->str);
   free(string);

   retval=CTA_Handle_Free(hstring);
   return retval;
}

#undef METHOD
#define METHOD "Set"
int CTA_String_Set(CTA_String hstring, const char* str)
{
// Deallocate the current contents of a cta-string, then
// allocate a new string and fill it with the contents in the input
   CTAI_String *string;
   int retval;
   int len;

   retval=CTA_Handle_Check((CTA_Handle) hstring,CTA_STRING);

   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hstring,(void**) &string);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   } 

   len=(int) strlen(str);
   string->len=len;
   free(string->str);
   string->str=CTA_Malloc((len+1)*sizeof(char));

   strncpy(string->str,str,len+1);

   return CTA_OK;
}

#undef METHOD
#define METHOD "Get"
int CTA_String_Get(CTA_String hstring, char *str)
{
   CTAI_String *string;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hstring,CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hstring,(void**) &string);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   strncpy(str,string->str,string->len+1);

   return CTA_OK;
}

int CTA_String_Equals_Char(CTA_String hstring, const char *str0)
{
   char *str;
   int retval, len;

   retval = CTA_String_GetLength(hstring,&len);
   str=CTA_Malloc((len+1)*sizeof(char));
   retval = CTA_String_Get(hstring, str);
   retval= CTA_FALSE;
   if (strcmp(str,str0)==0) retval = CTA_TRUE;
   return retval;
}


int CTA_Strings_Equal(CTA_String hstring1, CTA_String hstring2)
{
   char *str1, *str2;
   int retval, len1, len2;

   retval = CTA_String_GetLength(hstring1,&len1);
   str1=CTA_Malloc((len1+1)*sizeof(char));
   retval = CTA_String_Get(hstring1, str1);
   retval = CTA_String_GetLength(hstring2,&len2);
   str2=CTA_Malloc((len2+1)*sizeof(char));
   retval = CTA_String_Get(hstring2, str2);
   retval= CTA_FALSE;
   if (strcmp(str1,str2)==0) retval = CTA_TRUE;
   return retval;
}




#undef METHOD
#define METHOD "GetValue"
int CTA_String_GetValue(CTA_String hstring, void *value, CTA_Datatype datatype)
{
   CTAI_String *string;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hstring, CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_time handle");
       return retval;
   }

   CTA_Handle_GetData((CTA_Handle) hstring,(void**) &string);
   switch (datatype) {
   case CTA_INTEGER:
      *(int*)value = atoi(string->str);
      return CTA_OK;
   case CTA_REAL:
      *(float*)value = (float)atof(string->str);
      return CTA_OK;
   case CTA_DOUBLE:
      *(double*)value = atof(string->str);
      return CTA_OK;
   case CTA_STRING:
      return CTA_String_Get(hstring, (char*)value);
   }
   return CTA_INCOMPATIBLE_HANDLE;
}

#undef METHOD
#define METHOD "Conc"
int CTA_String_Conc(CTA_String istring, CTA_String xstring)
{
   int retval, len1, len2, newlen;
   char *hlpstr;
   CTAI_String *string1;
   CTAI_String *string2;

   retval=CTA_Handle_Check((CTA_Handle) istring, CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) xstring, CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) istring, (void**) &string1);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) xstring, (void**) &string2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   len1=string1->len;
   len2=string2->len;
   newlen=len1+len2;

   hlpstr=CTA_Malloc((newlen+1)*sizeof(char));
   strncpy(hlpstr,     string1->str,len1);
   strncpy(hlpstr+len1,string2->str,len2+1);

   string1->len=newlen;
   free(string1->str);
   string1->str=hlpstr;

   return CTA_OK;
}

#undef METHOD
#define METHOD "GetLength"
int CTA_String_GetLength(CTA_String hstring, int *len)
{
   CTAI_String *string;
   int retval;

   retval=CTA_Handle_Check((CTA_Handle) hstring,CTA_STRING);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_string handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hstring,(void**) &string);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   *len = string->len;

   return CTA_OK;
}

/** \brief Get a pointer to the contents of the string (INTERNAL USE)
 *
 *  \param hstring  I handle of the string
 *  \return Pointer to the string contents
 */
char *CTAI_String_GetPtr(CTA_String hstring)
{
   CTAI_String *str;
   CTA_Handle_GetData((CTA_Handle) hstring,(void**) &str);
   return str->str;
}

/** \brief Allocate a string, created from a COSTA string (INTERNAL USE).
 *
 *  \param  hstr  I  COSTA string
 *  \return          C string allocated by this function
 */
char *CTAI_String_Allocate(CTA_String hstr) {
   int   len;
   char* str;

   str=NULL;
   if (CTA_OK == CTA_String_GetLength(hstr, &len)){
      str = (char *)CTA_Malloc((1 + len) * sizeof(char));
      CTA_String_Get(hstr, str);
   }
   return str;
}

/** \brief Create a duplication of a COSTA string
 *
 *  \param hfrom  I  handle of string to copy
 *  \param hto    O  handle of created string
 *  \return CTA_OK if successful
 */
#undef METHOD
#define METHOD "Duplicate"
int CTA_String_Duplicate(CTA_String hfrom, CTA_String *hto) {
   int retval;

   retval = CTA_String_Create(hto);
   if (retval != CTA_OK) {
       CTA_WRITE_ERROR("Cannot create string");
       return retval;
   }
   return CTA_String_Set(*hto, CTAI_String_GetPtr(hfrom));
}

#undef METHOD
#define METHOD "Export"
int CTA_String_Export(CTA_String hstring, CTA_Handle usrdata)
{
   int retval;
   BOOL packout;

   packout = (CTA_Handle_Check(usrdata,CTA_PACK)==CTA_OK);
   if (packout) {
      CTAI_String *str;
      CTA_Handle_GetData((CTA_Handle) hstring,(void**) &str);

      /* pack length and string itself */
      retval=CTA_Pack_Add(usrdata,&str->len,sizeof(int));
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Error in CTA_Pack_Add ");
         return retval;
      }

      retval=CTA_Pack_Add(usrdata,str->str,(str->len+1)*sizeof(char));
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Error in CTA_Pack_Add ");
         return retval;
      }

   } else {
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   return CTA_OK;
}

#undef METHOD
#define METHOD "Import"
int CTA_String_Import(CTA_String hstring, CTA_Handle usrdata)
{
   int retval;
   BOOL packout;

   packout = (CTA_Handle_Check(usrdata,CTA_PACK)==CTA_OK);
   if (packout) {
      CTAI_String *str;
      CTA_Handle_GetData((CTA_Handle) hstring,(void**) &str);

      /* unpack length and string itself */
      retval=CTA_Pack_Get(usrdata,&(str->len),sizeof(int));
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Error in CTA_Pack_Get ");
         return retval;
      }
      str->str=realloc(str->str,sizeof(int)*(str->len+1));
      retval=CTA_Pack_Get(usrdata,str->str,(str->len+1)*sizeof(char));
      if (retval!=CTA_OK) {
         CTA_WRITE_ERROR("Error in CTA_Pack_Get ");
         return retval;
      }

   } else {
      return CTA_FORMAT_NOT_SUPPORTED;
   }
   return CTA_OK;
}


/* Interfacing with Fortran */

CTAEXPORT void CTA_STRING_CREATE_F77(int *hstring, int *ierr){
   *ierr=CTA_String_Create((CTA_String*) hstring);
}

CTAEXPORT void CTA_STRING_FREE_F77(int *hstring, int *ierr){
   *ierr=CTA_String_Free((CTA_String*) hstring);
}

CTAEXPORT void CTA_STRING_GETLENGTH_F77(int *hstring,int *len, int *ierr){
   *ierr=CTA_String_GetLength((CTA_String) *hstring, len);
}

CTAEXPORT void CTA_STRING_SET_F77(int *hstring,char *str, int *ierr, int len_str){
   char *c_str;

   /* create a c-string equivalent to name */
   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   CTA_fstr2cstr(str,c_str,len_str);

   *ierr=CTA_String_Set((CTA_String) *hstring,c_str);

   free(c_str);
}


CTAEXPORT void CTA_STRING_GET_F77(int *hstring,char *str, int *ierr, int len_str){
   char *c_str;

   c_str=CTA_Malloc((len_str+1)*sizeof(char));
   *ierr=CTA_String_Get((CTA_String) *hstring,c_str);
   if (*ierr!=CTA_OK) return;
   *ierr=CTA_cstr2fstr(c_str,str, len_str);
   free(c_str);

}

CTAEXPORT void CTA_STRING_GETVALUE_F77(int *hstring,void *value,int *datatype, int *ierr){
   *ierr=CTA_String_GetValue((CTA_String) *hstring, value, *datatype);
}

CTAEXPORT void CTA_STRING_CONC_F77(int *istring, int *xstring, int *ierr){
   *ierr=CTA_String_Conc((CTA_String) *istring, (CTA_String) *xstring);
}


CTAEXPORT void CTA_STRING_EXPORT_F77(int *hstring, int *usrdata, int *ierr){
   *ierr=CTA_String_Export((CTA_String) *hstring, (CTA_Handle) *usrdata);
}

CTAEXPORT void CTA_STRING_IMPORT_F77(int *hstring, int *usrdata, int *ierr){
   *ierr=CTA_String_Import((CTA_String) *hstring, (CTA_Handle) *usrdata);
}

CTAEXPORT void CTA_STRING_DUPLICATE_F77(int *hfrom, int *hto, int *ierr){
   *ierr=CTA_String_Duplicate((CTA_String) *hfrom, (CTA_Handle*) hto);
}


