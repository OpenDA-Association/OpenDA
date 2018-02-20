/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/openda/bridge/org_openda_costa_CtaTreeVector.cpp $
$Revision: 2763 $, $Date: 2011-09-13 13:52:01 +0200 (Tue, 13 Sep 2011) $

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


#include "org_openda_costa_CtaTreeVector.h"
#include "cta_treevector.h"
#include "jni_cta_utils.h"
#include "cta_errors.h"
#include "cta_defaults.h"
#include "cta_metainfo.h"


/*
 * Class:     org_costa_CtaTreeVector
 * Method:    getId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_getId
(JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);

   char id[CTA_STRLEN_TAG];
   CTA_TreeVector ctaHandle = cta_jni_getCtaHandle(env, obj_this);

   int retVal = CTA_TreeVector_GetTag (ctaHandle, id);

	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not get ID", retVal);
		return NULL;
	}

   return env->NewStringUTF(id);

}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetNumSubTreeVectors
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetNumSubTreeVectors
(JNIEnv *env, jobject obj_this){
   cta_jni_setJavaEnv(env);
   CTA_TreeVector ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   int numSubTrees;
   CTA_TreeVector_GetNumSubTree(ctaHandle, &numSubTrees);
   return numSubTrees;
}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetSubTreeVector
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetSubTreeVector
(JNIEnv *env, jobject obj_this, jstring path){

   cta_jni_setJavaEnv(env);

   CTA_TreeVector ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   CTA_TreeVector subtreevec;
   const char *spath  = env->GetStringUTFChars(path, 0);

   int retVal=CTA_TreeVector_GetSubTreeVec (ctaHandle, spath, &subtreevec);
   env->ReleaseStringUTFChars(path, spath);
   
   
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaVector", "Could not get SubTreeVector", retVal);
		return 0;
	}
   
   /* Increase reference count to subtree-vector */
   CTA_TreeVector_IncRefCount(subtreevec);   
   
   return subtreevec;
}


/*
 * Class:     org_costa_ctatreevector
 * Method:    ctaSetRegGrid
 * Signature: ()V
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaSetRegGrid
  (JNIEnv * env, jobject obj_this, jint nx, jint ny, jint nz, jdouble x0, jdouble y0, jdouble z0,
                 jdouble dx, jdouble dy, jdouble dz)
{
  int retval;
  CTA_Metainfo minfo;
  CTA_TreeVector ctaTreeVec = cta_jni_getCtaHandle(env, obj_this);
  char cGrid[]="grid";
  
  CTA_Metainfo_Create(&minfo); 

  retval = CTA_Metainfo_setRegGrid(minfo,cGrid,nx,ny,nz,x0,y0,z0,dx,dy,dz);
  if (retval) return retval;
  retval = CTA_TreeVector_SetMetainfo(ctaTreeVec, minfo);

  return retval;
}


/*
 * Class:     org_costa_treevector
 * Method:    ctaNetcdfClose
 * Signature: ()V
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaNetcdfClose
  (JNIEnv * env, jobject obj_this, jint ctafilehandle)
{
  cta_jni_setJavaEnv(env);

  int ierr;
  int foutput;


  foutput = ctafilehandle;
  ierr = CTA_File_Free(&foutput);
  if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_export", "Could not close (netcdf) cta_file", ierr);
     return -1;

   }
   return ierr;
   
 
}

/*
 * Class:     org_resultwriters_NetcdfResultWriter
 * Method:    ctaNetcdfClose
 * Signature: ()V
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriter_ctaNetcdfClose
  (JNIEnv * env, jobject obj_this, jint ctafilehandle)
{
  cta_jni_setJavaEnv(env);

  int ierr;
  int foutput;


  foutput = ctafilehandle;
  ierr = CTA_File_Free(&foutput);
  if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_export", "Could not close (netcdf) cta_file", ierr);
     return -1;

   }
   return ierr;
}


/*
 * Class:     org_costa_treevector
 * Method:    ctaNetcdfInit
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaNetcdfInit
  (JNIEnv * env, jobject obj_this, jstring path, jstring action)
{
 cta_jni_setJavaEnv(env);

  int ierr;
  int foutput;
  int soutfile;
  int haction;

   const char *spath  = env->GetStringUTFChars(path, 0);
   const char *saction  = env->GetStringUTFChars(action, 0);

   // Maybe we should first check if filename in 'path' does exist and is opened?
   // In that case, we need to retrieve the file handle somehow.


   ierr = CTA_String_Create(&soutfile);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not create string", ierr);
     return -1;
   }
   ierr = CTA_String_Set(soutfile, spath);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not set filename string", ierr);
     return -1;
   }
   // Note: if file already exists, this is not OK.
   ierr = CTA_File_Create(&foutput);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not create file handle", ierr);
     return -1;
   }
   // Note: if file is already open, export should occur as 'append'
   // in netcdf case. 
   // Note further, that cta_file_open will delete the contents of an existing netcdf file!

   env->ReleaseStringUTFChars(path, spath);
   env->ReleaseStringUTFChars(action, saction);

   CTA_String_Create(&haction);
   CTA_String_Set(haction, saction);
   ierr = CTA_File_Open(foutput,soutfile,haction);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not open file", ierr);
     return -1;
   }

   CTA_String_Free(&haction);
   CTA_String_Free(&soutfile);
   
   return foutput;  //this is the cta filehandle!

}

/*
 * Class:     org_resultwriters_NetcdfResultWriter
 * Method:    ctaNetcdfInit
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_resultwriters_NetcdfResultWriter_ctaNetcdfInit
  (JNIEnv * env, jobject obj_this, jstring path, jstring action)
{
  cta_jni_setJavaEnv(env);

  int ierr;
  int foutput;
  int soutfile;
  int haction;

   const char *spath  = env->GetStringUTFChars(path, 0);
   const char *saction  = env->GetStringUTFChars(action, 0);

   // Maybe we should first check if filename in 'path' does exist and is opened?
   // In that case, we need to retrieve the file handle somehow.

 
   ierr = CTA_String_Create(&soutfile);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not create string", ierr);
     return -1;
   }
   ierr = CTA_String_Set(soutfile, spath);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not set filename string", ierr);
     return -1;
   }
   // Note: if file already exists, this is not OK.
   ierr = CTA_File_Create(&foutput);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not create file handle", ierr);
     return -1;
   }

   // Note: if file is already open, export should occur as 'append'
   // in netcdf case
   ierr = CTA_String_Create(&haction);
   if (ierr == CTA_OK) ierr = CTA_String_Set(haction, saction);
   if (ierr == CTA_OK) ierr = CTA_File_Open(foutput,soutfile,haction);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not open file", ierr);
     return -1;
   }
   env->ReleaseStringUTFChars(path, spath);
   env->ReleaseStringUTFChars(action, saction);
   ierr = CTA_String_Free(&haction);
   if (ierr == CTA_OK) ierr = CTA_String_Free(&soutfile);
   if ( ierr != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_netcdfinit", "Could not free cta_string", ierr);
     return -1;
   }

   return foutput;  //this is the cta filehandle!

}





/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaTreeExport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaExport
(JNIEnv *env, jobject obj_this, jint ctafilehandle){

   cta_jni_setJavaEnv(env);

  // int soutfile;
   int foutput;

   CTA_TreeVector ctaHandletv = cta_jni_getCtaHandle(env, obj_this);

   foutput = ctafilehandle;
 
   int retVal=CTA_TreeVector_Export (ctaHandletv, foutput);

   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_export", "Could not export Treevector", retVal);
     return 0;
   }

   return 0;
}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaImport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaImport
(JNIEnv *env, jobject obj_this, jint ctafilehandle){

   cta_jni_setJavaEnv(env);

   int finput;

   CTA_TreeVector ctaHandletv = cta_jni_getCtaHandle(env, obj_this);

   finput = ctafilehandle;
   int retVal=CTA_TreeVector_Import (ctaHandletv, finput);

   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_import", "Could not import Treevector", retVal);
     return 0;
   }

   return 0;
}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaVImport
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaVImport
(JNIEnv *env, jobject obj_this, jint ctafilehandle){

   cta_jni_setJavaEnv(env);

   int finput;

   CTA_TreeVector ctaHandletv = cta_jni_getCtaHandle(env, obj_this);

   finput = ctafilehandle;
   int retVal=CTA_TreeVector_VImport (ctaHandletv, finput);

   if ( retVal != CTA_OK ) {
     cta_jni_exception(env, "CtaTreeVector_Vimport", "Could not import Treevector", retVal);
     return 0;
   }

   return 0;
}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaGetSubTreeVectorId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_ctaGetSubTreeVectorId
(JNIEnv *env, jobject obj_this, jint index){
   char tag[CTA_STRLEN_TAG];
   cta_jni_setJavaEnv(env);
   CTA_TreeVector ctaTreeVec = cta_jni_getCtaHandle(env, obj_this);
   CTA_TreeVector_GetSubTreeVecId (ctaTreeVec, index, tag);
   return env->NewStringUTF(tag);
}


/*
 * Class:     org_costa_CtaTreeVector
 * Method:    getCaption
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_org_openda_costa_CtaTreeVector_getCaption
(JNIEnv *env, jobject obj_this){
   cta_jni_exception(env, "CtaTreeVector", "Get Caption is not implemented", 911);

   cta_jni_setJavaEnv(env);

   return NULL;

}

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaCreateFromVector
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaCreateFromVector
(JNIEnv *env , jobject obj_this, jstring str_id, jstring str_tag, jint ctavectorhandle){

   cta_jni_setJavaEnv(env);

   int handle = CTA_NULL;
   const char *sid  = env->GetStringUTFChars(str_id, 0);
   const char *stag  = env->GetStringUTFChars(str_tag, 0);
   int retVal = CTA_TreeVector_Create(sid,stag, &handle);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTreeVector I", "Could not create treevector", retVal);
      return 0;
   }
   env->ReleaseStringUTFChars(str_id, sid);
   env->ReleaseStringUTFChars(str_tag, stag);

   // now immediately fill the treevector!
    retVal = CTA_TreeVector_SetVec(handle,ctavectorhandle);
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaTreeVector I", "Could not fill the created treevector", retVal);
		return 0;
	}

   return handle;
};

/*
 * Class:     org_costa_CtaTreeVector
 * Method:    ctaCreateFromSubtreevectors
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_openda_costa_CtaTreeVector_ctaCreateFromSubtreevectors
(JNIEnv *env , jobject obj_this, jstring str_id, jstring str_tag, jint nsubtrees, jintArray jsubtreevectors){

   cta_jni_setJavaEnv(env);
   
   int handle = CTA_NULL;
   const char *sid  = env->GetStringUTFChars(str_id, 0);
   const char *stag  = env->GetStringUTFChars(str_tag, 0);
   int retVal = CTA_TreeVector_Create(sid,stag, &handle);
   env->ReleaseStringUTFChars(str_id, sid);
   env->ReleaseStringUTFChars(str_tag, stag);
   if ( retVal != CTA_OK ) {
	cta_jni_exception(env, "CtaTreeVector II", "Could not create treevector", retVal);
	return 0;
   }
   // now concatenate the  subtreevectors!
	int cCount = env->GetArrayLength(jsubtreevectors);

	jint * csubtreevectors = env->GetIntArrayElements(jsubtreevectors, 0);
	int *csub2 = new int[cCount] ;

	// a bit awkward, but it is not possible to retrieve csubtreevectors as int instead of jint?
	for (int i=0;i<cCount;i++){
      csub2[i] = csubtreevectors[i];
      CTA_TreeVector_IncRefCount(csub2[i]);
	}

    retVal = CTA_TreeVector_Conc(handle, csub2, nsubtrees);
	if ( retVal != CTA_OK ) {
		cta_jni_exception(env, "CtaTreeVector II", "Could not concatenate the created treevector", retVal);
		return 0;
	}
        delete [] csub2;

   return handle;
};


/*
 * Class:     org_costa_CtaTreeVector
 * Method:    clone
 * Signature: ()Lorg/openda/interfaces/ITreeVector;
 */
JNIEXPORT jobject JNICALL Java_org_openda_costa_CtaTreeVector_clone
  (JNIEnv *env, jobject obj_this){

   cta_jni_setJavaEnv(env);

   CTA_TreeVector ctaHandle = cta_jni_getCtaHandle(env, obj_this);
   CTA_TreeVector ctaDuplicate;
   int retVal=CTA_TreeVector_Duplicate(ctaHandle, &ctaDuplicate);
   if ( retVal != CTA_OK ) {
      cta_jni_exception(env, "CtaTreeVector", "Could not duplicate", retVal);
      return NULL;
   }

   /* Create a Java CtaTreeVector */
   jclass clsTreeVector = env->FindClass("org/openda/costa/CtaTreeVector");
   jmethodID constructorID = env->GetMethodID (clsTreeVector, "<init>", "()V");
   jobject jTreeVec = env->NewObject(clsTreeVector, constructorID);

   cta_jni_setCtaHandle(env, jTreeVec, ctaDuplicate);
   return jTreeVec;


  }





