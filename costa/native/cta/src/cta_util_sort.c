/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/cta/cta_time.c $
$Revision: 2751 $, $Date: 2011-09-09 08:58:46 +0200 (Fri, 09 Sep 2011) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2012  Nils van Velzen

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
#include <stdlib.h>
#include "cta_util_sort.h"
 
void swap(int *x,int *y){

   int temp;
   temp = *x;
   *x = *y;
   *y = temp;
}



int choose_pivot(int i,int j ){
  return((i+j) /2);
}



void CTAI_Util_IQSort2(int *list, int *indx, int m,int n){
 int key,i,j,k;
 if( m < n)
 {
    k = choose_pivot(m,n);
    swap(&list[m],&list[k]);
    swap(&indx[m],&indx[k]);
    key = list[m];
    i = m+1;
    j = n;
    while(i <= j)
    {
       while((i <= n) && (list[i] <= key))
              i++;
       while((j >= m) && (list[j] > key))
              j--;
       if( i < j){
              swap(&list[i],&list[j]);
              swap(&indx[i],&indx[j]);
       }
    }
    // swap two elements
    swap(&list[m],&list[j]);
    swap(&indx[m],&indx[j]);
    // recursively sort the lesser list
    CTAI_Util_IQSort2(list,indx,m,j-1);
    CTAI_Util_IQSort2(list,indx,j+1,n);
 }
}


void CTA_Util_IQSort2(int *list, int *indx, int n){
   CTAI_Util_IQSort2(list, indx, 0, n-1);
}



