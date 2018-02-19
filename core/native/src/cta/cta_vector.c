/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_vector.c $
$Revision: 3361 $, $Date: 2012-07-04 16:52:30 +0200 (Wed, 04 Jul 2012) $

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
#include "cta_mem.h"
#include "f_cta_utils.h"
#include "cta_errors.h"
#include "cta_datatypes.h"
#include "ctai_datatypes.h"
#include "cta_string.h"
#include "cta_vector.h"
#include "ctai.h"
#include "ctai_string.h"
#include "cta_pack.h"
#include "ctai_xml.h"
#include "cta_defaults.h"
#include "ctai_handles.h"
#include "cta_message.h"
#include "cta_metainfo.h"

#define CTA_VECTOR_DEFINECLASS_F77  F77_CALL(cta_vector_defineclass,CTA_VECTOR_DEFINECLASS)
#define CTA_VECTOR_DUPLICATE_F77    F77_CALL(cta_vector_duplicate,CTA_VECTOR_DUPLICATE)
#define CTA_VECTOR_CREATE_F77       F77_CALL(cta_vector_create,CTA_VECTOR_CREATE)
#define CTA_VECTOR_GETSIZE_F77      F77_CALL(cta_vector_getsize,CTA_VECTOR_GETSIZE)
#define CTA_VECTOR_GETVALS_F77      F77_CALL(cta_vector_getvals,CTA_VECTOR_GETVALS)
#define CTA_VECTOR_GETVAL_F77       F77_CALL(cta_vector_getval,CTA_VECTOR_GETVAL)
#define CTA_VECTOR_SETVALS_F77      F77_CALL(cta_vector_setvals,CTA_VECTOR_SETVALS)
#define CTA_VECTOR_SETVAL_F77       F77_CALL(cta_vector_setval,CTA_VECTOR_SETVAL)
#define CTA_VECTOR_SETCONSTANT_F77  F77_CALL(cta_vector_setconstant,CTA_VECTOR_SETCONSTANT)
#define CTA_VECTOR_SCAL_F77         F77_CALL(cta_vector_scal,CTA_VECTOR_SCAL)
#define CTA_VECTOR_COPY_F77         F77_CALL(cta_vector_copy,CTA_VECTOR_COPY)
#define CTA_VECTOR_AXPY_F77         F77_CALL(cta_vector_axpy,CTA_VECTOR_AXPY)
#define CTA_VECTOR_DOT_F77          F77_CALL(cta_vector_dot,CTA_VECTOR_DOT)
#define CTA_VECTOR_NRM2_F77         F77_CALL(cta_vector_nrm2,CTA_VECTOR_NRM2)
#define CTA_VECTOR_AMAX_F77         F77_CALL(cta_vector_amax,CTA_VECTOR_AMAX)
#define CTA_VECTOR_GETMAXLEN_F77    F77_CALL(cta_vector_getmaxlen,CTA_VECTOR_GETMAXLEN)
#define CTA_VECTOR_EXPORT_F77       F77_CALL(cta_vector_export,CTA_VECTOR_EXPORT)
#define CTA_VECTOR_IMPORT_F77       F77_CALL(cta_vector_import,CTA_VECTOR_IMPORT)
#define CTA_VECTOR_PRINT_TABLE_F77  F77_CALL(cta_vector_print_table,CTA_VECTOR_PRINT_TABLE)
#define CTA_VECTOR_FREE_F77         F77_CALL(cta_vector_free,CTA_VECTOR_FREE)
#define CTA_VECTOR_ELMDIV_F77       F77_CALL(cta_vector_elmdiv,CTA_VECTOR_ELMDIV)
#define CTA_VECTOR_ELMPROD_F77      F77_CALL(cta_vector_elmprod,CTA_VECTOR_ELMPROD)

#define CLASSNAME "CTA_Vector" 

/* Struct holding all data associated to an COSTA Vector */

static long CTAI_Vector_Memsize=0;


typedef struct {
CTA_Func functions[I_CTA_VECTOR_NUMFUNC]; // See cta_vector.h for a list of
                                        //     available vector functions
CTA_VecClass hveccl;                    // Vector class
int n;                                  // dimension
CTA_Datatype datatype;                  // data type of the vector elements
CTA_Handle usrdata;                     // ???
long memsize;                           // size of allocated memblock
void *data;                             // pointer to the data. The data are not only
                                        //   the vector values. In the case of a BLAS-vector
                                        //   for instance, data is a struct consisting of
                                        //   the fields datatype, values and n (dimension)
                                        //   Some information is stored twice (once directly in a CTA_Vector
                                        //   and once in its data.
} CTAI_Vector;



typedef struct {
CTA_Func functions[I_CTA_VECTOR_NUMFUNC];
} CTAI_VectorClass;                    // A VectorClass  contains a list of
                                       //   functions ???

/* Local interfaces */
int CTAI_XML_read_txt_node_size(char *txtstr);
int CTAI_XML_read_txt_node_content(char *txtstr, double *vals);

/* Global interface from a shared routine from cta_treevector.c */
void XML_newline(int spc, xmlTextWriter *writer);



int CTA_Vector_DefineClass(
   // INPUTS:
      const char *name,                // Name of the new vector class
      const CTA_Func h_func[I_CTA_VECTOR_NUMFUNC],  // function handles to
                                       // the implementations of the
                                       // vector class' functions.
   // OUTPUTS:
      CTA_VecClass     *hveccl)   // The (handle to) the new vector class
{

   CTAI_VectorClass *data;
   int retval;

   /* Allocate new Vector object */
   data=CTA_Malloc(sizeof(CTAI_VectorClass));

   data->functions[I_CTA_VECTOR_CREATE_SIZE]=h_func[I_CTA_VECTOR_CREATE_SIZE];
   data->functions[I_CTA_VECTOR_CREATE_INIT]=h_func[I_CTA_VECTOR_CREATE_INIT];
   data->functions[I_CTA_VECTOR_GETVALS]    =h_func[I_CTA_VECTOR_GETVALS    ];
   data->functions[I_CTA_VECTOR_GETVAL]     =h_func[I_CTA_VECTOR_GETVAL     ];
   data->functions[I_CTA_VECTOR_SETVALS]    =h_func[I_CTA_VECTOR_SETVALS    ];
   data->functions[I_CTA_VECTOR_SETVAL]     =h_func[I_CTA_VECTOR_SETVAL     ];
   data->functions[I_CTA_VECTOR_SETCONST]   =h_func[I_CTA_VECTOR_SETCONST   ];
   data->functions[I_CTA_VECTOR_SCAL   ]    =h_func[I_CTA_VECTOR_SCAL       ];
   data->functions[I_CTA_VECTOR_COPY   ]    =h_func[I_CTA_VECTOR_COPY       ];
   data->functions[I_CTA_VECTOR_AXPY   ]    =h_func[I_CTA_VECTOR_AXPY       ];
   data->functions[I_CTA_VECTOR_DOT    ]    =h_func[I_CTA_VECTOR_DOT        ];
   data->functions[I_CTA_VECTOR_NRM2   ]    =h_func[I_CTA_VECTOR_NRM2       ];
   data->functions[I_CTA_VECTOR_AMAX   ]    =h_func[I_CTA_VECTOR_AMAX       ];
   data->functions[I_CTA_VECTOR_GETMAXLEN]  =h_func[I_CTA_VECTOR_GETMAXLEN  ];
   data->functions[I_CTA_VECTOR_EXPORT ]    =h_func[I_CTA_VECTOR_EXPORT     ];
   data->functions[I_CTA_VECTOR_IMPORT ]    =h_func[I_CTA_VECTOR_IMPORT     ];
   data->functions[I_CTA_VECTOR_PRINT_TABLE]=h_func[I_CTA_VECTOR_PRINT_TABLE];
   data->functions[I_CTA_VECTOR_FREE   ]    =h_func[I_CTA_VECTOR_FREE       ];
   data->functions[I_CTA_VECTOR_APPENDVAL]  =h_func[I_CTA_VECTOR_APPENDVAL  ];
   data->functions[I_CTA_VECTOR_ELMDIV]     =h_func[I_CTA_VECTOR_ELMDIV     ];
   data->functions[I_CTA_VECTOR_ELMPROD]    =h_func[I_CTA_VECTOR_ELMPROD    ];
   data->functions[I_CTA_VECTOR_ELMSQRT]    =h_func[I_CTA_VECTOR_ELMSQRT    ];


   // Allocate new handle
   retval=CTA_Handle_Create(name,CTA_VECTORCLASS,data,hveccl);

   // return error when unsuccesfull
   return retval;
}


#undef METHOD
#define METHOD "Duplicate"
int CTA_Vector_Duplicate(CTA_Vector hvector1, CTA_Vector *hvector2)
{
   CTAI_Vector *data;      /* All data of vector hvec      */
   int retval;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hvector1,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_treevector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvector1,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Vector_Create(data->hveccl, data->n, data->datatype,
                            data->usrdata, hvector2);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot create vector");
	   return retval;
   }
   retval=CTA_Vector_Copy(hvector1,*hvector2);
   return retval;
}




#undef METHOD
#define METHOD "Create"
int CTA_Vector_Create(
    // INPUTS:
       CTA_VecClass hveccl,
       const int n,            // dimension
       CTA_Datatype datatype,  // datatype of the vector elements
       CTA_Handle usrdata,     // ???
    // OUTPUTS
       CTA_Vector *hvector)    // The new COSTA-function (handle)
{
   CTAI_Vector *vector;
   int memsize;
   int retval;
   CTAI_VectorClass *clsdata;
   CTA_Function *my_Create_Size, *my_Create_Init;
   int i;
   int elSize;

   /* Get class data containing all function pointers */
   retval=CTA_Handle_Check((CTA_Handle) hveccl,CTA_VECTORCLASS);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vectorclass handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hveccl,(void*) &clsdata);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   /* determine size of data object (CTA_VECTOR_CREATE_SIZE)*/
   retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_VECTOR_CREATE_SIZE],&my_Create_Size);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function");
	   return retval;
   }
   (void) my_Create_Size(&n,&datatype,&usrdata,&retval,&memsize);
   if (retval) {
	   CTA_WRITE_ERROR("Error using my_Create_Size");
	   return retval;
   }

   /* allocate memory for new vector object */
   vector=CTA_Malloc(sizeof(CTAI_Vector));
   vector->data=CTA_Malloc(memsize);

   /* Count allocated memory local and global */
   CTA_SizeOf(datatype, &elSize);
   vector->memsize=sizeof(CTAI_Vector)+memsize+n*elSize;
   CTAI_Vector_Memsize+=vector->memsize;

   /* copy function pointers */
   for (i=0;i< I_CTA_VECTOR_NUMFUNC;i++){
      vector->functions[i]=clsdata->functions[i];
   }
   /* set other general information */
   vector->hveccl=hveccl;
   vector->n=n;
   vector->datatype=datatype;

   vector->usrdata=usrdata;

   /* Initialise and fill new vector */
   retval=CTA_Func_GetFunc(clsdata->functions[I_CTA_VECTOR_CREATE_INIT],&my_Create_Init);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function");
	   return retval;
   }
   (void) my_Create_Init(vector->data, &n, &datatype, &usrdata, &retval);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Error using my_Create_Init");
	   return retval;
   }

   /* Allocate new handle and return eror when unsuccesfull */
   retval=CTA_Handle_Create("vector",CTA_VECTOR,vector,hvector);
   if (retval) {
	   CTA_WRITE_ERROR("Cannot create handle");
	   return retval;
   }

   return CTA_OK;


}


#undef METHOD
#define METHOD "GetSize"
int CTA_Vector_GetSize(
   CTA_Vector hvec,      /* Handle of the vector */
   int *n               /* size of vector */
   ){

   CTAI_Vector *data;      /* All data of vector hvec      */
   int retval;             /* Return value of COSTA call   */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
      char message[1024];
	  sprintf(message, "handle check in cta_vector_getsize failed");
	  CTA_WRITE_ERROR(message);
      return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec,(void*) &data);
   if (retval!=CTA_OK) {
      char message[1024];
      sprintf(message, "handle _getdata in cta_vector_getsize failed");
      CTA_WRITE_ERROR(message);
	  return retval;
   }

   *n=data->n;
   return CTA_OK;
};

#undef METHOD
#define METHOD "GetDatatype"
int CTA_Vector_GetDatatype(
   CTA_Vector hvec,         /* Handle of the vector */
   CTA_Datatype *datatype   /* Returned data type   */
   ){

   CTAI_Vector *data;      /* All data of vector hvec      */
   int retval;             /* Return value of COSTA call   */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vectorclass handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   *datatype=data->datatype;
   return CTA_OK;
};




#undef METHOD
#define METHOD "GetVals"
int CTA_Vector_GetVals(
   CTA_Vector hvec,      /* Handle of the vector */
   void *vals,           /* Returned value        */
   int  n,               /* length of array vals  */
   CTA_Datatype datatype /* Data type */
   )
{

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK)
   {
      char message[1024];
      sprintf(message,"cta_vector: Handle %d is not a vector handle", hvec);
	  CTA_WRITE_ERROR(message);
      return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec,(void*) &data);
   if (retval!=CTA_OK)
   {
     char message[1024];
     sprintf(message,"cta_vector: Handle %d has no data", hvec);
     CTA_WRITE_ERROR(message);
	 return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_GETVALS],&function);
   if (retval!=CTA_OK)
   {
       char message[1024];
       sprintf(message,"cta_vector: getvals function %d, handle %d is not valid",
           I_CTA_VECTOR_GETVALS,data->functions[I_CTA_VECTOR_GETVALS]);
       CTA_WRITE_ERROR(message);
       return retval;
   }

   /* Call (user) implementation */
   function(data->data,vals,&n,&datatype,&retval);
   return retval;
};



#undef METHOD
#define METHOD "GetVal"
int CTA_Vector_GetVal(
   CTA_Vector hvec,      /* Handle of the vector */
   int  i,               /* position of value    */
   void *val,            /* Returned value        */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec,(void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_GETVAL],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_GETVAL");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,&i,val,&datatype,&retval);

   return retval;
};


/* Get value as string  c; free pointer after use */
const char *CTAI_Vector_GetStringVal(CTA_Vector hvec, int  i)
{
   char          *buf = NULL;
   double         dval;
   float          fval;
   int            ival;
   int            len;
   CTA_String     sval;
   int            retval;
   CTA_Datatype   dt;

   retval = CTA_Vector_GetDatatype(hvec, &dt);
   if (retval == CTA_OK) {
      switch (dt) {
      case CTA_DOUBLE:
         retval = CTA_Vector_GetVal(hvec, i, &dval, dt);
         if (retval == CTA_OK) {
            buf = (char*)CTA_Malloc(64);
            sprintf(buf, "%lf", dval);
         }
         break;
      case CTA_REAL:
         retval = CTA_Vector_GetVal(hvec, i, &fval, dt);
         if (retval == CTA_OK) {
            buf = (char*)CTA_Malloc(64);
            sprintf(buf, "%g", fval);
         }
         break;
      case CTA_INTEGER:
      case CTA_HANDLE:
         retval = CTA_Vector_GetVal(hvec, i, &ival, dt);
         if (retval == CTA_OK) {
            buf = (char*)CTA_Malloc(64);
            sprintf(buf, "%d", ival);
         }
         break;
      case CTA_STRING:
         retval = CTA_String_Create(&sval);
         if (retval != CTA_OK) break;
         retval = CTA_Vector_GetVal(hvec, i, &sval, dt);
         if (retval == CTA_OK) {
            CTA_String_GetLength(sval, &len);
            buf = (char*)CTA_Malloc(len + 1);
            strcpy(buf, CTAI_String_GetPtr(sval));
            buf[len] = '\0';
         }
         CTA_String_Free(&sval);
         break;
      }
   }
   return buf;
};


#undef METHOD
#define METHOD "SetConstant"
int CTA_Vector_SetConstant(
   CTA_Vector hvec,  /* Handle of the vector */
   void *val,        /* value that must be set */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_SETCONST],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_SETCONST");
	   return retval;
   }
   /* Call (user) implementation */
   function(data->data,val,&datatype,&retval);
   return retval;
};





#undef METHOD
#define METHOD "Export"
int CTA_Vector_Export(
   CTA_Vector hvec, /* Handle of the vector */
   CTA_Handle usrdata
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */


   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_EXPORT],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_EXPORT");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,&usrdata,&retval);
   return retval;
};

#undef METHOD
#define METHOD "Import"
int CTA_Vector_Import(
   CTA_Vector hvec,    /* Handle of the vector */
   CTA_Handle usrdata
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */


   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_IMPORT],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_IMPORT");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,&usrdata,&retval);
   return retval;
};


#undef METHOD
#define METHOD "Print_Table"
int CTA_Vector_Print_Table(
   CTA_Vector* vtable, /* Handle of the vector */
   int ncolumns,
   CTA_Vector vformats
   )
{

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   void *formats;          /* data of vector formats       */
   CTA_Function *function; /* Function that must be called */
   void **table =          /* All data of vectors in table */
      CTA_Malloc(sizeof(CTAI_Vector *)*ncolumns) ;

   int i; /* loop counter */

   /* Get pointer to implementation of this function */
   for (i=0; i<ncolumns; i++)
   {
      retval=CTA_Handle_Check((CTA_Handle) vtable[i], CTA_VECTOR);
      if (retval!=CTA_OK)
      {
		  char message[1024];
		  sprintf(message, "ERROR in CTA_Vector_Print_Table: element %d of first argument must be a CTA_Vector",i);
          CTA_WRITE_ERROR(message);
		  return retval;
      }

      retval=CTA_Handle_GetData((CTA_Handle) vtable[i], (void*) &data);
      if (retval!=CTA_OK)
      {
          char message[1024];
		  sprintf(message, "ERROR in CTA_Vector_Print_Table: element %d of first argument would not return its data",i);
          CTA_WRITE_ERROR(message);
		  return retval;
      }

      table[i] = data->data;
   }

   retval=CTA_Handle_Check((CTA_Handle) vformats, CTA_VECTOR);
   if (retval!=CTA_OK)
   {
       char message[1024];
       sprintf(message, "ERROR in CTA_Vector_Print_Table: last argument is not a CTA_Vector");
	   CTA_WRITE_ERROR(message);
       return retval;
   }

   retval=CTA_Handle_GetData((CTA_Handle) vformats, (void*) &data);
   if (retval!=CTA_OK){
       CTA_WRITE_ERROR("ERROR in CTA_Vector_Print_Table: last argument would not return its data");
       return retval;
   }

   formats = data->data;

   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_PRINT_TABLE],&function);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("ERROR in CTA_Vector_Print_Table: could not obtain a PRINT_TABLE function for last argument");
       return retval;
   }

   /* Call (user) implementation */
   function(table,&ncolumns,formats,&retval);
   if (retval!=CTA_OK)
   {
       CTA_WRITE_ERROR("ERROR in CTA_Vector_Print_Table: PRINT_TABLE function returned an error code");
   }


   free(table);
   return retval;
};


#undef METHOD
#define METHOD "SetVals"
int CTA_Vector_SetVals(
   CTA_Vector hvec,  /* Handle of the vector */
   void *vals,       /* value that must be set */
   int  n,           /* Length of array vals */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_SETVALS],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_SETVALS");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,vals,&n,&datatype,&retval);
   return retval;
};

#undef METHOD
#define METHOD "SetVal"
int CTA_Vector_SetVal(
   CTA_Vector hvec,  /* Handle of the vector */
   int  i,           /* position of value that must be set */
   void *val,        /* value that must be set */
   CTA_Datatype datatype /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_SETVAL],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_SETVAL");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,&i,val,&datatype,&retval);
   return retval;
};


/* Set value from string */
int CTAI_Vector_SetStringVal(CTA_Vector hvec, int  i, const char *val, CTA_Datatype datatype) {
   void *buf = NULL;     /* Buffer to store the converted value */
   int  size = 0;        /* Buffer size */
   int  retval;

   if (CTA_OK == CTA_SizeOf(datatype, &size)) {
      buf = CTA_Malloc(size);
      switch (datatype) {
         case CTA_REAL:
            *((float *)buf) = (float)atof(val);
            break;
         case CTA_DOUBLE:
            *((double *)buf) = atof(val);
            break;
         case CTA_INTEGER:
         case CTA_HANDLE:
            *((int *)buf) = atoi(val);
            break;
         case CTA_STRING:
            retval = CTA_String_Create((CTA_String *)buf);
            if (retval == CTA_OK) {
               CTA_String_Set(*((CTA_String *)buf), val);
               break;
            }
            free(buf);
            return retval;
         default:
            free(buf);
            return CTA_ILLEGAL_DATATYPE;
      }
      /* Set the actual value */
      retval =  CTA_Vector_SetVal(hvec, i, buf, datatype);
      free(buf);
      return retval;
   }
   return CTA_ILLEGAL_DATATYPE;
};


#undef METHOD
#define METHOD "AppendVal"
int CTA_Vector_AppendVal(
   CTA_Vector hvec,        /* Handle of the vector */
   const void *val,        /* value that must be set */
   CTA_Datatype datatype   /* Data type */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec, CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_APPENDVAL],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_APPENDVAL");
	   return retval;
   }

   /* Update vector length */
   ++data->n;

   /* Call (user) implementation */
   function(data->data,val,&datatype,&retval);
   return retval;
};



#undef METHOD
#define METHOD "Scal"
int CTA_Vector_Scal(
   CTA_Vector hvec,  /* Handle of the vector */
   double alpha     /* scaling factor        */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data;      /* All data of vector hvec      */
   CTA_Function *function; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec, (void*) &data);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data->functions[I_CTA_VECTOR_SCAL],&function);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_SCAL");
	   return retval;
   }

   /* Call (user) implementation */
   function(data->data,&alpha,&retval);
   return retval;
};

#undef METHOD
#define METHOD "Copy"
int CTA_Vector_Copy(
   CTA_Vector hvec_x,  /* Handle of the sending vector */
   CTA_Vector hvec_y   /* Handle of the receiving vector */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTAI_Vector *data_y;      /* All data of vector hvec_y  */
   CTA_Function *function_x;   /* Function that must be called */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_COPY],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_COPY");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_COPY],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_COPY");
	   return retval;
   }

   /* Check whether implementions match  */
   if (function_x==function_y){

      /* Call (user) implementation */
      function_x(data_x->data,data_y->data,&retval);
      return retval;
   }
   else {
      return CTA_INCOMPATIBLE_VECTORS;
   }
};

#undef METHOD
#define METHOD "ElmSqrt"
int CTA_Vector_ElmSqrt(
   CTA_Vector hvec_y  /* Handle of the receiving vector */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_y;      /* All data of vector hvec_y  */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_ELMSQRT],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_ELMSQRT");
	   return retval;
   }

   /* Call (user) implementation */
   function_y(data_y->data,&retval);
   return retval;
};


#undef METHOD
#define METHOD "ElmDiv"
int CTA_Vector_ElmDiv(
   CTA_Vector hvec_y,  /* Handle of the receiving vector */
   CTA_Vector hvec_x  /* Handle of the sending vector */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTAI_Vector *data_y;      /* All data of vector hvec_y  */
   CTA_Function *function_x;   /* Function that must be called */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_ELMDIV],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_ELMDIV");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_ELMDIV],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_ELMDIV");
	   return retval;
   }
   /* Check whether implementions match  */
   if (function_x==function_y){

      /* Call (user) implementation */
      function_x(data_y->data,data_x->data,&retval);
      return retval;
   }
   else {
      return CTA_INCOMPATIBLE_VECTORS;
   }
};


#undef METHOD
#define METHOD "ElmProd"
int CTA_Vector_ElmProd(
   CTA_Vector hvec_y,  /* Handle of the receiving vector */
   CTA_Vector hvec_x  /* Handle of the sending vector */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTAI_Vector *data_y;      /* All data of vector hvec_y  */
   CTA_Function *function_x;   /* Function that must be called */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_ELMPROD],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_ELMPROD");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_ELMPROD],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_ELMPROD");
	   return retval;
   }
   /* Check whether implementions match  */
   if (function_x==function_y){

      /* Call (user) implementation */
      function_x(data_y->data,data_x->data,&retval);
      return retval;
   }
   else {
      return CTA_INCOMPATIBLE_VECTORS;
   }
};


#undef METHOD
#define METHOD "Axpy"
int CTA_Vector_Axpy(
   CTA_Vector hvec_y,  /* Handle of the receiving vector */
   double alpha,       /* value of alpha */
   CTA_Vector hvec_x  /* Handle of the sending vector */
   ){

   /* Local variables */
   int retval;                 /* Return value of COSTA call   */
   CTAI_Vector *data_x;        /* All data of vector hvec_x    */
   CTAI_Vector *data_y;        /* All data of vector hvec_y    */
   CTA_Function *function_x;   /* Function that must be called */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_AXPY],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_AXPY");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_AXPY],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_AXPY");
	   return retval;
   }

   /* Check whether implementions match  */
   if (function_x==function_y){

      /* Call (user) implementation */
      function_x(data_y->data,&alpha,data_x->data,&retval);
      return retval;
   }
   else {
      return CTA_INCOMPATIBLE_VECTORS;
   }
};


#undef METHOD
#define METHOD "Dot"
int CTA_Vector_Dot(
   CTA_Vector hvec_x,  /* Handle of the first vector  */
   CTA_Vector hvec_y,  /* Handle of the second vector */
   double *dotprod
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTAI_Vector *data_y;      /* All data of vector hvec_y  */
   CTA_Function *function_x;   /* Function that must be called */
   CTA_Function *function_y;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Handle_Check((CTA_Handle) hvec_y,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_y, (void*) &data_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_DOT],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_DOT");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_y->functions[I_CTA_VECTOR_DOT],&function_y);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_DOT");
	   return retval;
   }

   /* Check whether implementions match  */
   if (function_x==function_y){

      /* Call (user) implementation */
      function_x(data_x->data,data_y->data,dotprod,&retval);
      return retval;
   }
   else {
      return CTA_INCOMPATIBLE_VECTORS;
   }
};

#undef METHOD
#define METHOD "Dot"
int CTA_Vector_Nrm2(
   CTA_Vector hvec_x,  /* Handle of the vector  */
   double *norm2
   ){

   /* Local variables */
   int retval;               /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTA_Function *function_x; /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_NRM2],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_NRM2");
	   return retval;
   }

   /* Call (user) implementation */
   function_x(data_x->data,norm2,&retval);
   return retval;
};

#undef METHOD
#define METHOD "Amax"
int CTA_Vector_Amax(
   CTA_Vector hvec_x,  /* Handle of the first vector  */
   int *iloc
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTA_Function *function_x;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_AMAX],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_AMAX");
	   return retval;
   }
   /* Call (user) implementation */
   function_x(data_x->data,iloc, &retval);
   return retval;
};

#undef METHOD
#define METHOD "GetMaxLen"
int CTA_Vector_GetMaxLen(
   CTA_Vector hvec_x,  /* Handle of the first vector  */
   int *maxlen
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTA_Function *function_x;   /* Function that must be called */

   /* Get pointer to implementation of this function */
   retval=CTA_Handle_Check((CTA_Handle) hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }
   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_GETMAXLEN],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_GETMAXLEN");
	   return retval;
   }

   /* Call (user) implementation */
   function_x(data_x->data, maxlen, &retval);
   return retval;
};

#undef METHOD
#define METHOD "Free"
int CTA_Vector_Free(
   CTA_Vector *hvec_x  /* Handle of vector  */
   ){

   /* Local variables */
   int retval;             /* Return value of COSTA call   */
   CTAI_Vector *data_x;      /* All data of vector hvec_x  */
   CTA_Function *function_x;   /* Function that must be called */

   /* Get pointer to implementation of this function */

   if (*hvec_x==CTA_NULL) return CTA_OK;
   retval=CTA_Handle_Check((CTA_Handle) *hvec_x,CTA_VECTOR);
   if (retval!=CTA_OK) {
       CTA_WRITE_ERROR("Handle is not a cta_vector handle");
       return retval;
   }
   retval=CTA_Handle_GetData((CTA_Handle) *hvec_x, (void*) &data_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot retrieve handle data");
	   return retval;
   }

   retval=CTA_Func_GetFunc(data_x->functions[I_CTA_VECTOR_FREE],&function_x);
   if (retval!=CTA_OK) {
	   CTA_WRITE_ERROR("Cannot get function I_CTA_VECTOR_FREE");
	   return retval;
   }


   /* Count allocated memory local and global */
   CTAI_Vector_Memsize-=data_x->memsize;

   /* Call (user) implementation */
   function_x(data_x->data,&retval);
   free(data_x->data);
   free(data_x);
   retval=CTA_Handle_Free(hvec_x);

   return retval;
};


/** \brief Generate XML from one COSTA vector
*
*  \param hvec   I  handle of a COSTA vector
*  \param writer I  the XML text writer
*/

void CTAI_XML_WriteVector(CTA_Vector hvec, const char *id, const char *caption, CTA_Metainfo minfo, int level, xmlTextWriter *writer) {
   const char   *text;
   int          dimension;
   int          i;
   char unitname[CTA_STRLEN_TAG];
   int misval=0;
   CTAI_Gridm thisgrid;
   char misvaltxt[20];
   char valtotxt[10];

    if (minfo) {
      // note: description according to xml-scheme should be at a state node, so it is not read here.
      CTA_Metainfo_GetUnit(minfo,unitname);
      CTA_Metainfo_GetRest(minfo,&misval);
      CTA_Metainfo_GetGrid(minfo,&thisgrid);
    }


   /* Start an element the the name of the tree handle */
   xmlTextWriterStartElement(writer, (xmlChar *) CTAI_Type2String(CTA_VECTOR));

   /* write id (COSTA: tag of parent state) */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_ID,  (xmlChar *) id);

   /* Write caption (COSTA: name of parent state */
   xmlTextWriterWriteAttribute(writer, CTAI_XML_CAPTION, (xmlChar *) caption);
   XML_newline(level,writer);

   /* Write unit */
   xmlTextWriterStartElement(writer, (xmlChar *) "unit");
   xmlTextWriterWriteString(writer, (xmlChar *) unitname);
   xmlTextWriterEndElement(writer); XML_newline(level,writer);
   /* Write missingvalue */
   xmlTextWriterStartElement(writer, (xmlChar *) "missingValue");
   sprintf(misvaltxt,"%d",misval);
   xmlTextWriterWriteString(writer, (xmlChar *) misvaltxt);
   xmlTextWriterEndElement(writer); XML_newline(level,writer);

   if (thisgrid.type > -99) {
     /* write grid; always computational dimensions */
     xmlTextWriterStartElement(writer, (xmlChar *) "grid");      level=level+3;XML_newline(level,writer);
     xmlTextWriterStartElement(writer, (xmlChar *) "computationalSpace");    level=level+3;XML_newline(level,writer);

   xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
   xmlTextWriterWriteAttribute(writer, CTAI_XML_ID,  (xmlChar *) "xi");
   sprintf(valtotxt,"%d",thisgrid.nx);
   xmlTextWriterWriteAttribute(writer, (xmlChar *) "length", (xmlChar *) valtotxt);
   xmlTextWriterEndElement(writer);
   if (thisgrid.type > 1) {
     XML_newline(level,writer);
     xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
     xmlTextWriterWriteAttribute(writer, CTAI_XML_ID,  (xmlChar *) "eta");
     sprintf(valtotxt,"%d",thisgrid.ny);
     xmlTextWriterWriteAttribute(writer, (xmlChar *) "length", (xmlChar *) valtotxt);
   xmlTextWriterEndElement(writer);
   }
   if (thisgrid.type > 2) {
     XML_newline(level,writer);
     xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
     xmlTextWriterWriteAttribute(writer, CTAI_XML_ID,  (xmlChar *) "psi");
     sprintf(valtotxt,"%d",thisgrid.nz);
     xmlTextWriterWriteAttribute(writer, (xmlChar *) "length", (xmlChar *) valtotxt);
     xmlTextWriterEndElement(writer);
   }
   level=level-3; XML_newline(level,writer);
   xmlTextWriterEndElement(writer); XML_newline(level,writer);  //computationalSpace

   if (0==strcmp(thisgrid.refdimp[1],"none")) {
     printf("no reference for physical space given.\n");
   } else {
     xmlTextWriterStartElement(writer, (xmlChar *) "physicalSpace"); level=level+3; XML_newline(level,writer);

     xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
     xmlTextWriterWriteAttribute(writer, (xmlChar *) "id", (xmlChar *) "x");
     xmlTextWriterWriteAttribute(writer, (xmlChar *) "axes", (xmlChar *) "xi");
     xmlTextWriterWriteAttribute(writer, (xmlChar *) "ref", (xmlChar *) thisgrid.refdimp[1]);
     xmlTextWriterEndElement(writer);

     if (thisgrid.type > 1) {
       XML_newline(level,writer);
       xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "id", (xmlChar *) "y");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "axes",(xmlChar *) "eta");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "ref", (xmlChar *) thisgrid.refdimp[2]);
       xmlTextWriterEndElement(writer);
     }
     if (thisgrid.type > 2) {
       XML_newline(level,writer);
       xmlTextWriterStartElement(writer, (xmlChar *) "dimension");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "id", (xmlChar *) "z");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "axes",(xmlChar *) "psi");
       xmlTextWriterWriteAttribute(writer, (xmlChar *) "ref",(xmlChar *)  thisgrid.refdimp[3]);
       xmlTextWriterEndElement(writer);
     }
     level=level-3;XML_newline(level,writer);

     xmlTextWriterEndElement(writer); level=level-3;XML_newline(level,writer);  //physicalSpace
   }



   xmlTextWriterEndElement(writer); XML_newline(level,writer);  //grid
   }


   /* Get dimension */
   CTA_Vector_GetSize(hvec, &dimension);

   /* Write values */
   xmlTextWriterStartElement(writer, CTAI_XML_VECTOR);

   /* Write each value */
   for (i = 1; i <= dimension; ++i) {

      text = CTAI_Vector_GetStringVal(hvec, i);
      xmlTextWriterWriteString(writer, (xmlChar *) text);
      xmlTextWriterWriteString(writer, (xmlChar *) " ");
      free((char *)text);

   }

   /* End the tree level elements */
   xmlTextWriterEndElement(writer);
   level=level-3; XML_newline(level,writer);
   xmlTextWriterEndElement(writer);
   XML_newline(level,writer);
}

/* -------------------------------------------------------------------- */

/** \brief Create a COSTA vector from NEW xml-input
*
*  \param cur_node  I  Current XML node
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTA_Vector CTAI_XML_CreateVector_New(xmlNode *cur_node, CTA_Metainfo minfo) {
   xmlNode       *values_node = NULL;     /* values child node */
   xmlNode       *txt_node = NULL;        /* text node */
   CTA_Vector    hnew = CTA_NULL;         /* The new COSTA handle */
   xmlChar       *val;                    /* element/property value */
   CTA_Datatype  datatype = CTA_DOUBLE;   /* datatype of the vector */
   CTA_VecClass  vecclass = CTA_DEFAULT_VECTOR;
   int           dim = 0;                 /* vector dim */
   char          *teststr, *teststr2;
   int           misval;
   double        *vals;
   CTAI_Gridm    thisgrid;


   /* Parse this node's attributes */

   /* Get datatype */
   val = xmlGetProp(cur_node, CTAI_XML_DATATYPE);
   if (val) {
      datatype = CTAI_String2Type((char *) val);
      xmlFree(val);
      if (datatype != CTA_DOUBLE && datatype != CTA_REAL &&
          datatype != CTA_INTEGER && datatype != CTA_STRING) return CTA_NULL;
   } else {
     /* default: */
     datatype = CTA_DOUBLE ;
   }


   /* Get dimension */
   dim = 0;
   val = xmlGetProp(cur_node, CTAI_XML_DIMENSION);
   if (val) {
      dim = atoi((char *) val);
      xmlFree(val);
   }

   /* First: get all modes other than the 'vector' containing the values */
   for (values_node = cur_node->children; values_node; values_node = values_node->next) {
     if (0 == strcmp((char *) CTAI_XML_UNIT, (char *) values_node->name)) {
       for (txt_node = values_node->children; txt_node; txt_node = txt_node->next) {
         if (txt_node->type == XML_TEXT_NODE) {
           //  printf("unit: |%s| %d \n",txt_node->content,strlen(txt_node->content));
           CTA_Metainfo_SetUnit(minfo,(char *) txt_node->content);
         }
       }
     }
     if (0 == strcmp((char *) CTAI_XML_MISSINGVALUE, (char *) values_node->name)) {
       for (txt_node = values_node->children; txt_node; txt_node = txt_node->next) {
         if (txt_node->type == XML_TEXT_NODE) {
           //        printf("missingvalue: |%s| %d \n",txt_node->content,strlen(txt_node->content));
           if (0==strcmp((char *) txt_node->content,"NaN")) {misval=-9999;}
           else {misval=atoi((char *) txt_node->content);}
           CTA_Metainfo_SetRest(minfo,&misval);
         }
       }
     }
     if (0 == strcmp((char *) CTAI_XML_GRID, (char *) values_node->name)) {
       //       printf("entering ctai_xml_grid ...\n");
       CTAI_XML_CreateGrid(values_node, &thisgrid);
       //      printf("... done; retval; type: %d %d \n",retval,thisgrid.type);
       CTA_Metainfo_SetGrid(minfo, &thisgrid);
     }

   }


   /* TODO: read vector class when it can be something else than CTA_DEFAULT_VECTOR */


   /* Get values node. NOTE: it is denoted as 'vector'*/
   values_node = NULL;
   for (values_node = cur_node->children; values_node; values_node = values_node->next) {
      if (0 == strcmp((char *) CTAI_XML_VECTOR, (char *) values_node->name)) break;
   }

   if (values_node) {

     for (txt_node = values_node->children; txt_node; txt_node = txt_node->next) {
       if (txt_node->type == XML_TEXT_NODE) {
         if (txt_node->content) {


           teststr = CTA_Malloc((strlen((char *) txt_node->content)+1)*sizeof(char));
           teststr2 = CTA_Malloc((strlen((char *) txt_node->content)+1)*sizeof(char));
           strcpy(teststr,(char *) txt_node->content);
           strcpy(teststr2,teststr);

           dim = CTAI_XML_read_txt_node_size(teststr);


           if (dim > 0) {
             /* Create vector */
             CTA_Vector_Create(vecclass, dim, datatype, CTA_NULL, &hnew);
             vals = CTA_Malloc((dim)*sizeof(double));
             dim = CTAI_XML_read_txt_node_content(teststr2,vals);
             //  printf("vals: %f %f %f\n",vals[0],vals[1],vals[2]);

             free(teststr);
             free(teststr2);
             CTA_Vector_SetVals(hnew, vals,dim, datatype);

             return hnew;
           }

         }
       }
     }
   }

   return CTA_NULL;
}


/* ---------------------- */
int CTAI_XML_read_txt_node_size(char *txtstr){
  int i;
  // char *saveptr;
  char *txtpart;

  //for (i=0; txtpart != NULL; i++)
  for (i=0; ; i++,txtstr=NULL)
    {
      txtpart = strtok(txtstr," ");
      if (txtpart == NULL) break;
    }
  return i ;
}

int CTAI_XML_read_txt_node_content(char *txtstr, double *vals){
  int i;
  //  char *saveptr;
  char *txtpart;

  //for (i=0; txtpart != NULL; i++)
  for (i=0; ; i++,txtstr=NULL)
    {
      txtpart = strtok(txtstr," ");
      //      txtpart = strtok_r(txtstr," ",&saveptr);
      if (txtpart == NULL) break;
      //      printf(" %d: %s -> %f\n",i,txtpart, atof(txtpart));
      vals[i]=atof(txtpart);
    }
  return i ;
}


long CTAI_Vector_GetMemsize(){
   return CTAI_Vector_Memsize;
}




/* ------------------------------------------------- */

/* Interfacing with Fortran */
CTAEXPORT void CTA_VECTOR_DEFINECLASS_F77(char *name,int *h_func,int *hveccl,
                             int *ierr, int len_name){
  char  *c_name;
  // create a c-string equivalent to name
  c_name=CTA_Malloc((len_name+1)*sizeof(char));
  CTA_fstr2cstr(name,c_name,len_name);

  *ierr=CTA_Vector_DefineClass(c_name, (CTA_Func*) h_func,
                               (CTA_VecClass*) hveccl);
  free(c_name);
};

CTAEXPORT void CTA_VECTOR_DUPLICATE_F77(int *hvector1, int *hvector2, int *ierr){
   *ierr=CTA_Vector_Duplicate(*hvector1, hvector2);
};
CTAEXPORT void CTA_VECTOR_CREATE_F77(int *hveccl, const int *n, int *datatype,
                         int *usrdata, int *hvector, int *ierr){

 *ierr=CTA_Vector_Create((CTA_VecClass) *hveccl, *n, (CTA_Datatype) *datatype,
                      (CTA_Handle) *usrdata, (CTA_Vector*) hvector);

};

CTAEXPORT void CTA_VECTOR_GETSIZE_F77 (int *hvec, int *n, int *ierr){

   *ierr= CTA_Vector_GetSize(*hvec, n);
};

CTAEXPORT void CTA_VECTOR_GETVALS_F77(int *hvec, void *vals, int  *n, int *datatype,
                       int *ierr){

   *ierr=CTA_Vector_GetVals(*hvec, vals, *n, *datatype);
};

CTAEXPORT void CTA_VECTOR_GETVAL_F77(int *hvec, int *i, void *val, int *datatype,
                         int *ierr){

   *ierr=CTA_Vector_GetVal(*hvec, *i, val, *datatype);
};

CTAEXPORT void CTA_VECTOR_SETVALS_F77(int *hvec, void *vals, int  *n, int *datatype,
                        int *ierr){

   *ierr=CTA_Vector_SetVals(*hvec, vals, *n, *datatype);
};


CTAEXPORT void CTA_VECTOR_SETVAL_F77(int *hvec, int  *i, void *val, int *datatype,
                       int *ierr){

   *ierr =CTA_Vector_SetVal((CTA_Vector) *hvec, *i, val,
                            (CTA_Datatype) *datatype);
}

CTAEXPORT void CTA_VECTOR_SETCONSTANT_F77(int *hvec, void *val, int *datatype,
                        int *ierr){

   *ierr=CTA_Vector_SetConstant(*hvec, val, *datatype);
};

CTAEXPORT void CTA_VECTOR_SCAL_F77(int *hvec, double *alpha, int *ierr){

   *ierr=CTA_Vector_Scal((CTA_Vector) *hvec,*alpha);
};

CTAEXPORT void CTA_VECTOR_COPY_F77( int *hvec_x, int *hvec_y, int *ierr){
   *ierr=CTA_Vector_Copy( (CTA_Vector) *hvec_x, (CTA_Vector) *hvec_y);
};

CTAEXPORT void CTA_VECTOR_AXPY_F77( int *hvec_y, double *alpha, int *hvec_x, int *ierr){
   *ierr=CTA_Vector_Axpy( (CTA_Vector) *hvec_y, *alpha, (CTA_Vector) *hvec_x);
};

CTAEXPORT void CTA_VECTOR_DOT_F77( int *hvec_x, int *hvec_y, double *dotprod, int *ierr){
   *ierr=CTA_Vector_Dot( (CTA_Vector) *hvec_x, (CTA_Vector) *hvec_y,
                         dotprod);
};

CTAEXPORT void CTA_VECTOR_NRM2_F77( int *hvec_x,double *norm2, int *ierr){
   *ierr=CTA_Vector_Nrm2( (CTA_Vector) *hvec_x, norm2);
};

CTAEXPORT void CTA_VECTOR_AMAX_F77( int *hvec_x, int *iloc, int *ierr){
   *ierr=CTA_Vector_Amax( (CTA_Vector) *hvec_x, iloc);
};

CTAEXPORT void CTA_VECTOR_GETMAXLEN_F77( int *hvec_x, int *maxlen, int *ierr){
   *ierr=CTA_Vector_GetMaxLen( (CTA_Vector) *hvec_x, maxlen);
};

CTAEXPORT void CTA_VECTOR_EXPORT_F77(int *hvec_x, int *usrdata, int *ierr){
   *ierr=CTA_Vector_Export((CTA_Vector) *hvec_x, (CTA_Handle) *usrdata);
};

CTAEXPORT void CTA_VECTOR_IMPORT_F77(int *hvec_x, int *usrdata, int *ierr){
   *ierr=CTA_Vector_Import((CTA_Vector) *hvec_x, (CTA_Handle) *usrdata);
};



CTAEXPORT void CTA_VECTOR_PRINT_TABLE_F77(int *table, int *ncolumns, int *vformats, int *ierr){
   *ierr=CTA_Vector_Print_Table((CTA_Vector *) table, *ncolumns, (CTA_Vector) *vformats);
};

CTAEXPORT void CTA_VECTOR_FREE_F77(int *hvec_x, int *ierr){
   *ierr=CTA_Vector_Free((CTA_Vector*) hvec_x);
};

CTAEXPORT void CTA_VECTOR_ELMDIV_F77( int *hvec_y, int *hvec_x, int *ierr){
   *ierr=CTA_Vector_ElmDiv( (CTA_Vector) *hvec_y, (CTA_Vector) *hvec_x);
};

CTAEXPORT void CTA_VECTOR_ELMPROD_F77( int *hvec_y, int *hvec_x, int *ierr){
   *ierr=CTA_Vector_ElmProd( (CTA_Vector) *hvec_y, (CTA_Vector) *hvec_x);
};



