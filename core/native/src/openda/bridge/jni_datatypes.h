/*
$URL: https://repos.deltares.nl/repos/openda/openda_1/public/trunk/core/native/src/openda/bridge/jni_cta_utils.h $
$Revision: 1257 $, $Date: 2009-12-21 13:42:01 +0100 (ma, 21 dec 2009) $

OpenDA interface for COSTA.
Copyright (C) 2010  Nils van Velzen

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


#include <jni.h>

typedef struct {
JNIEnv *env;           //Poiner to Java runtime environment;
jclass cls;            //JNI handle to the java class instance;
jobject obj;           //JNI object handle;
} sJni_Class;

