/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/f_cta_utils.c $
$Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $

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
#include "ctai.h"

/*! \brief Copies a Fortran-string into a zero terminated C-string

    Traling blanks in the Fortran-string are not copied.
    Note: it is the programmers responsibility that the C-string is long enough 
    to hold the copy of the fortran sting.
    \param f_str    I fortran string
    \param c_str    O c-string, copy of fstr (is allocated in this function)
    \param len_f    I length of Fortran string (hidden argument in Fortran)
*/
int CTA_fstr2cstr(char *f_str,char *c_str, int len_f){

   int len_c; //lengt of the c equivalent of f_str

   //determine "strlen"-length of fortran string
   for (len_c=len_f-1;len_c>=0 && f_str[len_c]==' ';len_c--){};
   //copy f-string into c-string
   if (len_c>=0) {
      strncpy(c_str,f_str,len_c+1);
   }
   //add "0"-character
   c_str[len_c+1]='\0';
   return CTA_OK;
};

int CTA_cstr2fstr(char *c_str,char *f_str, int len_f){
   int len_cpy; //numver of characters that must/can be copied
   int i;      // loopcouter over traling elements of f_str

   // avoid overflow by checking length
   len_cpy=MIN((int) strlen(c_str),len_f);
   strncpy(f_str,c_str,len_cpy);
   // put spaces in trailing elements of f_str
   for (i=len_cpy;i<len_f;i++){
      f_str[i]=' ';
   }
   return CTA_OK;
};
