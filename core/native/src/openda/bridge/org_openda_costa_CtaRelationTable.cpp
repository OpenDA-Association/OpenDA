/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaRelationTable.cpp $
$Revision: 2211 $, $Date: 2011-04-01 13:48:25 +0200 (Fri, 01 Apr 2011) $

OpenDA interface for COSTA.
Copyright (C) 2007  Stef Hummel / Nils van Velzen

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

#include "jni_cta_utils.h"
#include "cta_defaults.h"
#include "cta_vector.h"
#include "cta_errors.h"
#include "cta_reltable.h"
#include "org_openda_costa_CtaRelationTable.h"

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    ctaCreate
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaRelationTable_ctaCreate
(JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);

	CTA_RelTable ctaRelTab = CTA_NULL;
	int retVal = CTA_RelTable_Create( &ctaRelTab);
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not create relation table", retVal);
		return 0;
	}
	return ctaRelTab;
}

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    SetTableCombine
 * Signature: (Lorg/openda/interfaces/IRelationTable;ZLorg/openda/interfaces/IRelationTable;Z)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaRelationTable_SetTableCombine
(JNIEnv *env, jobject newtable, jobject reltab1, jboolean inv1, jobject reltab2, jboolean inv2){

   cta_jni_setJavaEnv(env);

   /* Get the COSTA handles */
   CTA_RelTable ctaRelTab = cta_jni_getCtaHandle(env, newtable);
   CTA_RelTable ctaRel1   = cta_jni_getCtaHandle(env, reltab1);
   CTA_RelTable ctaRel2   = cta_jni_getCtaHandle(env, reltab2);

   int retVal=CTA_RelTable_SetTableCombine (ctaRelTab, ctaRel1, inv1, ctaRel2, inv2);

	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not perform CTA_RelTable_SetTableCombine", retVal);
		return;
   }
}

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    SetSelect
 * Signature: (Lorg/openda/interfaces/IVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaRelationTable_SetSelect
(JNIEnv *env, jobject jRelTab, jobject jSelect){

   cta_jni_setJavaEnv(env);

   /* Get the COSTA handles */
   CTA_RelTable ctaRelTab = cta_jni_getCtaHandle(env, jRelTab);
   CTA_Vector ctaVecSelect = cta_jni_getCtaHandle(env, jSelect);

   /* Create Integervector */
   int n;
   CTA_Vector_GetSize(ctaVecSelect,&n);
   CTA_Vector ctaIVecSelect;
   CTA_Vector_Create(CTA_DEFAULT_VECTOR,n,CTA_INTEGER,CTA_NULL,&ctaIVecSelect);

   double *dval=(double *) malloc(n*sizeof(double));
   CTA_Vector_GetVals(ctaVecSelect,dval,n,CTA_DOUBLE);
   int *ival= (int *) malloc(n*sizeof(int));
   for (int i=0;i<n;i++){ival[i]=(int) dval[i];}

   CTA_Vector_SetVals(ctaIVecSelect,ival,n,CTA_INTEGER);

   int retVal=CTA_RelTable_SetSelect (ctaRelTab, ctaIVecSelect);

   CTA_Vector_Free(&ctaIVecSelect);
   free(ival);
   free(dval);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not perform CTA_RelTable_SetSelect", retVal);
		return;
   }

}

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    apply
 * Signature: (Lorg/openda/interfaces/IVector;Lorg/openda/interfaces/IVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaRelationTable_apply
(JNIEnv *env, jobject jRelTab, jobject jfrom, jobject jto){

   cta_jni_setJavaEnv(env);
   
   CTA_RelTable ctaRelTab = cta_jni_getCtaHandle(env, jRelTab);
   CTA_Vector ctaVecFrom  = cta_jni_getCtaHandle(env, jfrom);
   CTA_Vector ctaVecTo    = cta_jni_getCtaHandle(env, jto);

   int retVal =CTA_RelTable_Apply (ctaRelTab, ctaVecFrom, ctaVecTo);
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not perform CTA_RelTable_Apply", retVal);
		return;
   }

}

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    applyInv
 * Signature: (Lorg/openda/interfaces/IVector;Lorg/openda/interfaces/IVector;)V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaRelationTable_applyInv
(JNIEnv *env, jobject jRelTab, jobject jfrom, jobject jto){

   cta_jni_setJavaEnv(env);

   CTA_RelTable ctaRelTab = cta_jni_getCtaHandle(env, jRelTab);
   CTA_Vector ctaVecFrom  = cta_jni_getCtaHandle(env, jfrom);
   CTA_Vector ctaVecTo    = cta_jni_getCtaHandle(env, jto);

   int retVal =CTA_RelTable_ApplyInv (ctaRelTab, ctaVecFrom, ctaVecTo);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not perform CTA_RelTable_ApplyInv", retVal);
		return;
   }

}

/*
 * Class:     org_costa_CtaRelationTable
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_openda_costa_CtaRelationTable_free
(JNIEnv *env, jobject jRelTab){

   cta_jni_setJavaEnv(env);
   
   CTA_RelTable ctaRelTab = cta_jni_getCtaHandle(env, jRelTab);
   int retVal = CTA_RelTable_Free(&ctaRelTab);
   if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaRelationTable", "Could not perform CTA_RelTable_Free", retVal);
		return;
   }
}
