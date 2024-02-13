/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_matrix.c $
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
#include "cta_f77lapack.h"
#include "cta_flush.h"
#include "cta_matrix.h"
#include "cta_errors.h"
#include "ctai.h"
#include "cta_message.h"

#define CTA_MATRIX_DEFINECLASS_F77  F77_CALL(cta_matrix_defineclass,CTA_MATRIX_DEFINECLASS)
#define CTA_MATRIX_DUPLICATE_F77    F77_CALL(cta_matrix_duplicate,CTA_MATRIX_DUPLICATE)
#define CTA_MATRIX_CREATE_F77       F77_CALL(cta_matrix_create,CTA_MATRIX_CREATE)
#define CTA_MATRIX_GETSIZE_F77      F77_CALL(cta_matrix_getsize,CTA_MATRIX_GETSIZE)
#define CTA_MATRIX_GETDATATYPE_F77  F77_CALL(cta_matrix_getdatatype,CTA_MATRIX_GETDATATYPE)
#define CTA_MATRIX_GETVALS_F77      F77_CALL(cta_matrix_getvals,CTA_MATRIX_GETVALS)
#define CTA_MATRIX_GETVAL_F77       F77_CALL(cta_matrix_getval,CTA_MATRIX_GETVAL)
#define CTA_MATRIX_SETCONSTANT_F77  F77_CALL(cta_matrix_setconstant,CTA_MATRIX_SETCONSTANT)
#define CTA_MATRIX_SETCOL_F77       F77_CALL(cta_matrix_setcol,CTA_MATRIX_SETCOL)
#define CTA_MATRIX_SETVALS_F77      F77_CALL(cta_matrix_setvals,CTA_MATRIX_SETVALS)
#define CTA_MATRIX_SETVAL_F77       F77_CALL(cta_matrix_setval,CTA_MATRIX_SETVAL)
#define CTA_MATRIX_EXPORT_F77       F77_CALL(cta_matrix_export,CTA_MATRIX_EXPORT)
#define CTA_MATRIX_GER_F77          F77_CALL(cta_matrix_ger,CTA_MATRIX_GER)
#define CTA_MATRIX_GEMV_F77         F77_CALL(cta_matrix_gemv,CTA_MATRIX_GEMV)
#define CTA_MATRIX_GEMM_F77         F77_CALL(cta_matrix_gemm,CTA_MATRIX_GEMM)
#define CTA_MATRIX_INV_F77          F77_CALL(cta_matrix_inv,CTA_MATRIX_INV)
#define CTA_MATRIX_AXPY_F77         F77_CALL(cta_matrix_axpy,CTA_MATRIX_AXPY)
#define CTA_MATRIX_EIGVALS_F77      F77_CALL(cta_matrix_eigvals,CTA_MATRIX_EIGVALS)
#define CTA_MATRIX_FREE_F77         F77_CALL(cta_matrix_free,CTA_MATRIX_FREE)
#define CLASSNAME "CTA_Matrix"

/* Struct holding all data associated to an COSTA Vector */

typedef struct {
CTA_Func functions[CTA_MATRIX_NUMFUNC];
CTA_MatClass hmatcl;
int m;
int n;
CTA_Datatype datatype;
CTA_Handle userdata;
void *data;      /*implementation specific data */
} CTAI_Matrix;

typedef struct {
CTA_Func functions[CTA_MATRIX_NUMFUNC];
} CTAI_MatrixClass;


int CTA_Matrix_DefineClass(
   const char *name,
   const CTA_Func h_func[CTA_MATRIX_NUMFUNC],
   CTA_MatClass     *hmatcl
   ){

   CTAI_MatrixClass *data;
   int retval;

   /* Allocate new Vector object */
   data=CTA_Malloc(sizeof(CTAI_MatrixClass));

   data->functions[CTA_MATRIX_CREATE_SIZE]=h_func[CTA_MATRIX_CREATE_SIZE];
   data->functions[CTA_MATRIX_CREATE_INIT]=h_func[CTA_MATRIX_CREATE_INIT];
   data->functions[I_CTA_MATRIX_GETVALS    ]=h_func[I_CTA_MATRIX_GETVALS    ];
   data->functions[I_CTA_MATRIX_GETVAL     ]=h_func[I_CTA_MATRIX_GETVAL     ];
   data->functions[I_CTA_MATRIX_SETCOL     ]=h_func[I_CTA_MATRIX_SETCOL     ];
   data->functions[I_CTA_MATRIX_SETVALS    ]=h_func[I_CTA_MATRIX_SETVALS    ];
   data->functions[I_CTA_MATRIX_SETVAL     ]=h_func[I_CTA_MATRIX_SETVAL     ];
   data->functions[I_CTA_MATRIX_SETCONST   ]=h_func[I_CTA_MATRIX_SETCONST   ];
   data->functions[I_CTA_MATRIX_EXPORT     ]=h_func[I_CTA_MATRIX_EXPORT     ];
   data->functions[I_CTA_MATRIX_GER        ]=h_func[I_CTA_MATRIX_GER        ];
   data->functions[I_CTA_MATRIX_INV        ]=h_func[I_CTA_MATRIX_INV        ];
   data->functions[I_CTA_MATRIX_GEMV       ]=h_func[I_CTA_MATRIX_GEMV       ];
   data->functions[I_CTA_MATRIX_GEMM       ]=h_func[I_CTA_MATRIX_GEMM       ];
   data->functions[I_CTA_MATRIX_AXPY       ]=h_func[I_CTA_MATRIX_AXPY       ];
   data->functions[I_CTA_MATRIX_FREE       ]=h_func[I_CTA_MATRIX_FREE       ];

      // Allocate new handle and return eror when unsuccesfull
   retval=CTA_Handle_Create(name,CTA_MATRIXCLASS,data,hmatcl);
   return retval;
}

#undef METHOD
#define METHOD "Duplicate" 
int CTA_Matrix_Duplicate(CTA_Matrix hmatrix1, CTA_Matrix *hmatrix2){

   CTAI_Matrix *data;      /* All data of vector hvec      */
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmatrix1,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmatrix1,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Matrix_Create(data->hmatcl, data->m, data->n, data->datatype,
                            data->userdata, hmatrix2);

   return retval;
}

#undef METHOD
#define METHOD "Create"
int CTA_Matrix_Create(CTA_MatClass hmatcl, const int m, const int n,
                      CTA_Datatype datatype, CTA_Handle userdata,
                      CTA_Matrix *hmatrix){

   CTAI_Matrix *matrix;
   int memsize;
   int retval;
   CTAI_MatrixClass *clsdata;
   CTA_Function *function;
   int i;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hmatcl,CTA_MATRIXCLASS);
   if (retval!=CTA_OK){
	   CTA_WRITE_ERROR("Handle is not a cta_matrixclass handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmatcl,(void*) &clsdata);
   if (retval!=CTA_OK){
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* determine size of data object (CTA_MATRIX_CREATE_SIZE)*/
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_MATRIX_CREATE_SIZE],
                           &function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function CTA_MATRIX_CREATE_SIZE");
	   return retval;
   }

   (void) function(&m, &n,&datatype,userdata,&retval,&memsize);
   if (retval){
	   CTA_WRITE_ERROR("Error in function");   
	   return retval;
   }

   /* allocate memory for new matrix object */
   matrix=CTA_Malloc(sizeof(CTAI_Matrix));
   matrix->data=CTA_Malloc(memsize);

   /* copy function pointers */
   for (i=0;i<CTA_MATRIX_NUMFUNC;i++){
      matrix->functions[i]=clsdata->functions[i];
   }
   /* set other general information */
   matrix->hmatcl=hmatcl;
   matrix->m=m;
   matrix->n=n;
   matrix->datatype=datatype;
   matrix->userdata=userdata;

   /* Initilise and fill new vector */
   retval=CTA_Func_GetFunc(clsdata->functions[CTA_MATRIX_CREATE_INIT],
                           &function);
   if (retval!=CTA_OK){
	   CTA_WRITE_ERROR("Cannot get function CTA_MATRIX_CREATE_INIT");   
	   return retval;
   }
   (void) function(matrix->data, &m, &n, &datatype, userdata, &retval);
   if (retval) {
	   CTA_WRITE_ERROR("Error in function");   
	   return retval;
   }

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("vector",CTA_MATRIX,matrix,hmatrix);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create handle for CTA_MATRIX");   
	   return retval;
   }

   return CTA_OK;
}
#undef METHOD
#define METHOD "GetSize"
int CTA_Matrix_GetSize(
   CTA_Matrix hmat,      /* Handle of the matrix */
   int *m,              /* number of rows        */
   int *n               /* number of cols        */
   ){

   CTAI_Matrix *data;      /* All data of matrix hmat      */
   int retval;             /* Return value of COSTA call   */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");   
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   *m=data->m;
   *n=data->n;
   return CTA_OK;
};
#undef METHOD
#define METHOD "GetDatatype"
int CTA_Matrix_GetDatatype(
   CTA_Matrix hmat,         /* Handle of the matrix */
   CTA_Datatype *datatype   /* Returned data type   */
   ){

   CTAI_Matrix *data;      /* All data of matrix hmat     */
   int retval;             /* Return value of COSTA call   */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   *datatype=data->datatype;
   return CTA_OK;
};

#undef METHOD
#define METHOD "GetVals"
int CTA_Matrix_GetVals(
   CTA_Matrix hmat,      /* Handle of the matrix */
   void *vals,           /* Retured value        */
   int  m,               /* number of rows (fortran) matrix vals */
   int  n,               /* number of columns (fortran) matrix vals */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_GETVALS],&function);

   /* Call (user) implementation */
   function(data->data,vals,&m,&n,&datatype,&retval);
   return retval;
};



#undef METHOD
#define METHOD "GetVal"
int CTA_Matrix_GetVal(
   CTA_Matrix hmat,      /* Handle of the matrix */
   void *val,            /* Returned value        */
   int  m,               /* number of rows (fortran) matrix vals */
   int  n,               /* number of columns (fortran) matrix vals */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }
   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_GETVAL],&function);

   /* Call (user) implementation */
   function(data->data,val,&m,&n,&datatype,&retval);
   return retval;
};


#undef METHOD
#define METHOD "SetConstant"
int CTA_Matrix_SetConstant(
   CTA_Matrix hmat,  /* Handle of the matrix */
   void *val,        /* value that must be set */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_SETCONST],&function);

   /* Call (user) implementation */
   function(data->data,val,&datatype,&retval);
   return retval;
};

#undef METHOD
#define METHOD "Export"
int CTA_Matrix_Export(
   CTA_Matrix hmat,  /* Handle of the matrix */
   CTA_Handle userdata    /* user-data */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_EXPORT],&function);

   /* Call (user) implementation */
   function(data->data,userdata,&retval);
   return retval;
};


#undef METHOD
#define METHOD "SetCol"
int CTA_Matrix_SetCol(
   CTA_Matrix hmat,  /* Handle of the matrix  */
   int  n,           /* column number         */
   CTA_Vector hvec   /* new values for column */
   )
{
   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_SETCOL],&function);

   /* Call (user) implementation */
   function(data->data,&n,&hvec,&retval);
   return retval;
};

#undef METHOD
#define METHOD "SetVals"
int CTA_Matrix_SetVals(
   CTA_Matrix hmat,  /* Handle of the matrix */
   void *vals,       /* value that must be set */
   int  m,           /* number of rows        */
   int  n,           /* number of cols        */
   CTA_Datatype datatype /* Data type */
   ){
   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK)  {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_SETVALS],&function);

   /* Call (user) implementation */
   function(data->data,vals,&m, &n,&datatype,&retval);
   return retval;
};


#undef METHOD
#define METHOD "SetVal"
int CTA_Matrix_SetVal(
   CTA_Matrix hmat,  /* Handle of the matrix */
   void *vals,       /* value that must be set */
   int  m,           /* row index        */
   int  n,           /* col index        */
   CTA_Datatype datatype /* Data type */
   ){
   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_SETVAL],&function);

   /* Call (user) implementation */
   function(data->data,vals,&m, &n,&datatype,&retval);
   return retval;
};


#undef METHOD
#define METHOD "Ger"
int CTA_Matrix_Ger(CTA_Matrix hmat, double alpha, CTA_Vector vx,
                      CTA_Vector vy){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_GER],&function);

   /* Call (user) implementation */
   function(data->data,&alpha,&vx,&vy,&retval);
   return retval;
}

#undef METHOD
#define METHOD "Inv"
int CTA_Matrix_Inv(CTA_Matrix hmat){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_INV],&function);

   /* Call (user) implementation */
   function(data->data,&retval);
   return retval;
}

#undef METHOD
#define METHOD "Gemv"
int CTA_Matrix_Gemv(CTA_Matrix hmat, int trans, double alpha, CTA_Vector vx,
                    double beta,  CTA_Vector vy){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_GetData((CTA_Handle) hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_GEMV],&function);

   /* Call (user) implementation */
   function(data->data, &trans, &alpha,&vx,&beta,&vy,&retval);
   return retval;
}


#undef METHOD
#define METHOD "Gemm"
int CTA_Matrix_Gemm(CTA_Matrix mC, int transa, int transb, double alpha,
                    CTA_Matrix mA, CTA_Matrix mB, double beta){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *dataA;     /* All data of matrix A         */
   CTAI_Matrix *dataB;     /* All data of matrix B         */
   CTAI_Matrix *dataC;     /* All data of matrix C         */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) mA,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_Check((CTA_Handle) mB,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   
   retval=CTA_Handle_Check((CTA_Handle) mC,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }
   

   retval=CTA_Handle_GetData((CTA_Handle) mA, (void*) &dataA);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) mB, (void*) &dataB);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) mC, (void*) &dataC);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   if (dataA->functions[I_CTA_MATRIX_GEMV]!=
          dataB->functions[I_CTA_MATRIX_GEMV] ||
       dataA->functions[I_CTA_MATRIX_GEMV]!=
          dataC->functions[I_CTA_MATRIX_GEMV]){
      return CTA_INCOMPATIBLE_MATRICES;
   }

   CTA_Func_GetFunc(dataC->functions[I_CTA_MATRIX_GEMM],&function);

   /* Call (user) implementation */
   function(dataC->data, &transa, &transb, &alpha,  dataA->data,
            dataB->data, &beta, &retval);
   return retval;
}


#undef METHOD
#define METHOD "Free"
int CTA_Matrix_Free(
   CTA_Matrix *hmat  /* Handle of matrix  */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *data;      /* All data of matrix hmat  */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) *hmat,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) *hmat, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   CTA_Func_GetFunc(data->functions[I_CTA_MATRIX_FREE],&function);

   /* Call (user) implementation */
   function(data->data,&retval);
   free(data->data);
   free(data);
   retval=CTA_Handle_Free(hmat);

   return retval;
};

#undef METHOD
#define METHOD "Axpy"
int CTA_Matrix_Axpy(CTA_Matrix mY, double alpha, CTA_Matrix mX){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Matrix *dataX;     /* All data of matrix A         */
   CTAI_Matrix *dataY;     /* All data of matrix B         */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) mY,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) mX,CTA_MATRIX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Handle is not a cta_matrix handle");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) mY, (void*) &dataY);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) mX, (void*) &dataX);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retreive handle data");
	   return retval;
   }

   if (dataY->functions[I_CTA_MATRIX_AXPY]!=
          dataX->functions[I_CTA_MATRIX_AXPY]){
      return CTA_INCOMPATIBLE_MATRICES;
   }

   CTA_Func_GetFunc(dataY->functions[I_CTA_MATRIX_AXPY],&function);

   /* Call (user) implementation */
   function(dataY->data, &alpha, dataX->data, &retval);
   return retval;
}



int CTA_Matrix_EigVals(CTA_Matrix hmat, CTA_Vector eigvals,
                       CTA_Matrix Reigvecs){

char leigv, reigv;

int nrows, ncols, neig, n;
void *A, *VL, *VR, *eigR, *eigI, *work;
CTA_Datatype datatype;
int size, lwork, info;
CTA_Matrix Leigvecs=CTA_NULL;
double dlength;
float  slength;

   /* get dimensions and check them */
   CTA_Matrix_GetSize(hmat, &nrows, &ncols);
   if (nrows!=ncols) {
      return CTA_MATRIX_IS_NOT_SQUARE;
   }
   n=ncols;
   CTA_Matrix_GetDatatype(hmat,&datatype);
   CTA_SizeOf(datatype,&size);

   /* Check vector of eigen values */
   CTA_Vector_GetSize(eigvals,&neig);
   if (neig!=n){
      return CTA_DIMENSION_ERROR;
   }

   /* Check matrix of left eigenvectors */
   leigv='N';
   if (Leigvecs!=CTA_NULL){
      leigv='V';
      CTA_Matrix_GetSize(Leigvecs, &nrows, &ncols);
      if (ncols!=n || nrows!=n) {
         return CTA_DIMENSION_ERROR;
      }
   }

   /* Check matrix of right eigenvectors */
   reigv='N';
   if (Reigvecs!=CTA_NULL){
      reigv='V';
      CTA_Matrix_GetSize(Reigvecs, &nrows, &ncols);
      if (ncols!=n || nrows!=n) {
         return CTA_DIMENSION_ERROR;
      }
   }

   A=CTA_Malloc(size*n*n);
   VL=CTA_Malloc(size*n*n);
   VR=CTA_Malloc(size*n*n);
   CTA_Matrix_GetVals(hmat, A, n, n, datatype);

   eigR=CTA_Malloc(size*n);
   eigI=CTA_Malloc(size*n);

   switch (datatype){
   case CTA_REAL :
   /* Determine amount of work memory */
   lwork=-1;
   SGEEV_F77( &leigv, &reigv, &n, A, &n, eigR, eigI, VL, &n, VR, &n, &slength,
              &lwork, &info);

   /* Allocate work memory and start compute */
   lwork=(int) slength;
   work=CTA_Malloc(size*lwork);

   SGEEV_F77( &leigv, &reigv, &n, A, &n, eigR, eigI, VL, &n, VR, &n, work,
              &lwork, &info);
   break;
   case CTA_DOUBLE :
      lwork=-1;
      DGEEV_F77( &leigv, &reigv, &n, A, &n, eigR, eigI, VL, &n, VR, &n,
                 &dlength, &lwork, &info);

      /* Allocate work memory and start compute */
      lwork=(int) dlength;
      work=CTA_Malloc(size*lwork);

      DGEEV_F77( &leigv, &reigv, &n, A, &n, eigR, eigI, VL, &n, VR, &n, work,
                 &lwork, &info);

   break;
   }

   /* Set left eigenvalues */
   if (Leigvecs!=CTA_NULL){
      CTA_Matrix_SetVals(Leigvecs, VL, n, n, datatype);
   }

   /* Set right eigenvalues */
   if (Reigvecs!=CTA_NULL){
      CTA_Matrix_SetVals(Reigvecs, VR, n, n, datatype);
   }

   /* Set real-part eigenvalues */
   CTA_Vector_SetVals(eigvals, eigR, n, datatype);

   free(A);
   free(VL);
   free(VR);
   free(eigR);
   free(eigI);
   free(work);
   return CTA_OK;
}

/* Interfacing with Fortran */

CTAEXPORT void CTA_MATRIX_DEFINECLASS_F77(char *name, int *h_func,int *hmatcl, int *ierr,
                           int len_name){

   char  *c_name;
   // create a c-string equivalent to name
   c_name=CTA_Malloc((len_name+1)*sizeof(char));
   CTA_fstr2cstr(name,c_name,len_name);
   *ierr=CTA_Matrix_DefineClass(name, (CTA_Func*) h_func,
                                (CTA_MatClass*) hmatcl);
  free(c_name);
}

CTAEXPORT void CTA_MATRIX_DUPLICATE_F77(int *hmatrix1, int *hmatrix2, int *ierr){
   *ierr=CTA_Matrix_Duplicate((CTA_Matrix) *hmatrix1, (CTA_Matrix*) hmatrix2);
}

CTAEXPORT void CTA_MATRIX_CREATE_F77(int *hmatcl, int *m, int *n, int *datatype,
                         int *userdata, int *hmatrix, int *ierr){
   *ierr=CTA_Matrix_Create((CTA_MatClass) *hmatcl, *m, *n,
                      (CTA_Datatype) *datatype, (CTA_Handle) *userdata, (CTA_Matrix*) hmatrix);
}

CTAEXPORT void CTA_MATRIX_GETSIZE_F77(int *hmat, int *m, int *n, int *ierr){
   *ierr=CTA_Matrix_GetSize((CTA_Matrix) *hmat, m, n);
}

CTAEXPORT void CTA_MATRIX_GETDATATYPE_F77(int *hmat, int *datatype ,int *ierr){
   *ierr=CTA_Matrix_GetDatatype((CTA_Matrix) *hmat, (CTA_Datatype*) datatype);
}

CTAEXPORT void CTA_MATRIX_GETVALS_F77(int *hmat, void *vals, int  *m, int  *n,
                          int *datatype, int *ierr){
   *ierr=CTA_Matrix_GetVals((CTA_Matrix) *hmat, vals, *m, *n,
                            (CTA_Datatype) *datatype);
}

CTAEXPORT void CTA_MATRIX_GETVAL_F77(int *hmat, void *vals, int  *m, int  *n,
                          int *datatype, int *ierr){
   *ierr=CTA_Matrix_GetVal((CTA_Matrix) *hmat, vals, *m, *n,
                            (CTA_Datatype) *datatype);
}


CTAEXPORT void CTA_MATRIX_SETCONSTANT_F77(int *hmat, void *val, int *datatype, int *ierr){
   *ierr=CTA_Matrix_SetConstant((CTA_Matrix) *hmat, val,
                                (CTA_Datatype) *datatype);
}


CTAEXPORT void CTA_MATRIX_SETCOL_F77(int *hmat, int *n, int *hvec, int *ierr){
   *ierr=CTA_Matrix_SetCol((CTA_Matrix) *hmat, *n, (CTA_Vector) *hvec);
}

CTAEXPORT void CTA_MATRIX_SETVALS_F77(int *hmat, void *vals, int  *m, int *n,
                       int *datatype, int *ierr){
   *ierr=CTA_Matrix_SetVals((CTA_Matrix) *hmat, vals, *m, *n,
                            (CTA_Datatype) *datatype);
}

CTAEXPORT void CTA_MATRIX_SETVAL_F77(int *hmat, void *val, int  *m, int  *n,
                       int *datatype, int *ierr){
   *ierr=CTA_Matrix_SetVal((CTA_Matrix) *hmat, val, *m, *n,
                       (CTA_Datatype) *datatype);
}

CTAEXPORT void CTA_MATRIX_EXPORT_F77(int *hmat, int *usedoc, int *ierr){
   *ierr=CTA_Matrix_Export((CTA_Matrix) *hmat, (CTA_Handle) *usedoc);

}

CTAEXPORT void CTA_MATRIX_GER_F77(int *hmat, double *alpha, int *vx, int *vy, int *ierr){
   *ierr=CTA_Matrix_Ger((CTA_Matrix) *hmat, (double) *alpha, (CTA_Vector) *vx,
                        (CTA_Vector) *vy);
}

CTAEXPORT void CTA_MATRIX_INV_F77(int *hmat, int *ierr){
   *ierr=CTA_Matrix_Inv((CTA_Matrix) *hmat);
}

CTAEXPORT void CTA_MATRIX_GEMV_F77(int *hmat, int *trans, double *alpha, int *vx, double *beta, int *vy, int *ierr){
   *ierr=CTA_Matrix_Gemv((CTA_Matrix) *hmat, *trans, (double) *alpha, (CTA_Vector) *vx,
                        (double) *beta, (CTA_Vector) *vy);
}

CTAEXPORT void CTA_MATRIX_GEMM_F77(int *mC, int *transa, int *transb, double *alpha,
                    int *mA, int *mB, double *beta, int *ierr){
   *ierr=CTA_Matrix_Gemm((CTA_Matrix) *mC, *transa, *transb, (double) *alpha,
                    (CTA_Matrix) *mA, (CTA_Matrix) *mB, (double) *beta);
}



CTAEXPORT void CTA_MATRIX_AXPY_F77(int *mY, double *alpha, int *mX, int *ierr){
   *ierr=CTA_Matrix_Axpy((CTA_Matrix) *mY, *alpha, (CTA_Matrix) *mX);
}


CTAEXPORT void CTA_MATRIX_EIGVALS_F77(int *hmat, int *eigvals, int
*eigvecs, int *ierr){
   *ierr=CTA_Matrix_EigVals((CTA_Matrix) *hmat, (CTA_Vector) *eigvals, (CTA_Matrix) *eigvecs );
}




CTAEXPORT void CTA_MATRIX_FREE_F77(int *hmat, int *ierr){
   *ierr=CTA_Matrix_Free((CTA_Matrix*) hmat);
}


