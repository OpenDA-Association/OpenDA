/*
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
#ifndef CTA_TREEVECTOR_H
#define CTA_TREEVECTOR_H
#include "cta_system.h"
#include "cta_datatypes.h"
#include "cta_handles.h"
#include "cta_vector.h"
#include "cta_matrix.h"
#include "cta_metainfo.h"

/**
\file  cta_treevector.h
\brief Interface description of the COSTA default tree-vector component.

The tree-vector is an extension of a vector component. A tree-vector
either contains a single vector or is a concatenation of a number of
tree-vectors, called sub-tree-vectors in this context. The usage of
sub-tree-vectors makes is possible to concatenate models or extend models
as is done when a deterministic model is extended into a stochastic model.
The sub-tree-vectors are also very useful inside the model source code where the
whole state of the model is not represented by a single vector. 
The default tree-vector component uses COSTA vector-components for storing the values.

Each (sub-)tree-vector has a tag. Tree-vectors having the same tag are considered
to be the same, meaning they have the same buildup in sub-tree-vectors, length and
datatypes.
*/


/**
  Type definition of a handle to a COSTA tree-vector instance
*/
typedef CTA_Handle CTA_TreeVector;

#ifdef __cplusplus
extern "C" {
#endif

/** \brief Create a tree-vector.
 *
 * \param name    I  name of the tree-vector, this is a human readable
 *                   string used for (debug) output and not by the algorithms
 *                   itself
 * \param tag     I  tag of this tree-vector
 * \param treevec  O new tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Create(const char *name, const char *tag, CTA_TreeVector *treevec);

/** \brief Duplicate a tree-vector.
 *
 * \note Duplication means that a new tree-vector is created that is identical to
 * the originating tree-vector. All data in the original tree-vector is also copied.
 *
 * \param treevec1  I  handle of treevector to be duplicated
 * \param treevec2  O  receives handle to duplicate
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Duplicate(CTA_TreeVector treevec1, CTA_TreeVector *treevec2 );


/** \brief Define a tree-vector to be a concatination of other tree-vectors.
 *
 * \note The concatenation is done by reference (handle). The sub-tree-vectors that
 *       are concatenated are not copied.
 *
 * \param treevec1  I  tree-vector that will be concatenation of the sub-tree-vectors provided in parameter treevecs
 * \param treevecs  I  array of the sub-tree-vectors
 * \param ntreevecs I  number of sub-tree-vectors in treevecs
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Conc(CTA_TreeVector treevec1, CTA_TreeVector *treevecs, int ntreevecs);

/** \brief Get the handle of a sub-tree-vectors using its tag.
 *
 * \note This is done by reference (handle). The handle of the
 *       returned sub-tree-vector is not a copy
 *
 * \param treevec   I  Tree-vector
 * \param tag       I  tag of the requested sub-tree-vector
 * \param subtreevec O  receives handle of the requested sub-tree-vectors, this is by reference, not a copy
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetSubTreeVec(CTA_TreeVector treevec, const char *tag, CTA_TreeVector *subtreevec);

/** \brief Get the tag of a sub-tree-vector using its index (starting with 0).
 *
 * \param treevec   I  Tree-vector
 * \param index     I  index of the requested sub-tree-vector
 * \param tag       O  String of standard length containnig the tag
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetSubTreeVecId(CTA_TreeVector treevec, int index, char tag[CTA_STRLEN_TAG]);


/** \brief Get the handle of a first-layer sub-tree-vector using its index.
 *
 * \note The concatination is done by reference (handle). The handle of the
 *       returned sub-tree-vector is not a copy
 *
 * \param treevec   I  Tree-vector
 * \param index     I  index of requested sub-tree-vector. Note that the first sub-tree-vector has index 1.
 * \param subtreevec O  receives handle of the requested sub-tree-vector, this is by reference, not a copy
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetSubTreeVecIndex(CTA_TreeVector treevec, int index,
                                CTA_TreeVector *subtreevec);

/** \brief Get number of sub-treevectors
 *
 * \note In case of a leaf 0 is returned
 *
 * \param treevec      I  Tree-vector
 * \param numSubTrees  O  Number of sub-treevectors
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetNumSubTree(CTA_TreeVector treevec, int* numSubTrees);


/** \brief Get the tag of the tree-vector.
 *
 * Note tag should be large enough to hold the result
 *      length of CTA_STRLEN_TAG is always save (no internal protection)
 *
 * \param treevec   I  Tree-vector
 * \param tag       O  receives the tag of the requested sub-tree-vector (see note)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetTag(CTA_TreeVector treevec, char *tag);


/** \brief Set the values of the tree-vector
 *
 * \note This operation is only possible when all data elements in the tree-vector
 *       are of the same type and the size of the tree-vector corresponds to the
 *       size of the input vector.
 *
 * \param treevec   IO TreeVector
 * \param hvec      I  handle of the vector containing new values (see note)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_SetVec(CTA_TreeVector treevec, CTA_Vector hvec);

/** \brief Get the values of the tree-vector.
 *
 * \note This operation is only possible when all data elements in the tree-vector
 *       are of the same type and the size of the tree-vector corresponds to the
 *       vector size.
 *
 * \param treevec   I  Tree-vector
 * \param hvec      O  Vector that is receiving the values; must exist before calling
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetVec(CTA_TreeVector treevec, CTA_Vector hvec);

/** \brief Axpy operation between two tree-vectors.
 *
 * \note  Axpy: y=alpha*x+y. Add alpha times tree-vector x to
 *              this tree-vector (y).
 *
 * \param y         IO Tree-vector (y)
 * \param alpha     I  scalar
 * \param x         I  Tree-vector (x)
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Axpy(CTA_TreeVector y, double alpha, CTA_TreeVector x);

/** \brief Compute dot product of two tree-vectors.
 *
 * \note  dotprod = sum[all i]  (treevec1_i * treevec2_i)                           
 *
 * \param treevec1  I  first tree-vector
 * \param treevec2  I  second tree-vector
 * \param dotprod   O  receives the dot product
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Dot(CTA_TreeVector treevec1, CTA_TreeVector treevec2, double *dotprod);

/** \brief Compute the 2-norm of a tree-vector.
 *
 * \param treevec1  I  Tree-vector
 * \param nrm2      O  receives the 2-norm
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Nrm2(CTA_TreeVector treevec1, double *nrm2);


/** \brief Copy a tree-vector
 *
 * \note  The two tree-vectors must be compatible: same structure and datatypes.
 *
 * \param treevec1   I  sending tree-vector
 * \param treevec2   O  receiving tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Copy(CTA_TreeVector treevec1, CTA_TreeVector treevec2);

/** \brief Set whole tree-vector equal to a constant value.
 *
 * \note  This method can only be used if all elements of the tree-vector 
 *        have the same data type.
 *
 * \param treevec  IO TreeVector
 * \param val      I  value to set
 * \param datatype I  data type of val, must be same as data type of tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_SetConstant(CTA_TreeVector treevec, void *val,CTA_Datatype datatype);

/** \brief Scale tree-vector.
 *
 * \param treevec   IO handle of tree-vector
 * \param alpha    I  scalar
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Scal(CTA_TreeVector treevec, double alpha);

/** \brief Set all values of the tree-vector.
 *
 * \note  This method can only be used if all elements of the tree-vector 
 *        are of the same data type.
 *
 * \param treevec  IO Tree-vector
 * \param val      I  values to be set
 * \param nval     I  number of elements in val
 * \param datatype I  data type of *val, must be the same as data type of elements in tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_SetVals(CTA_TreeVector treevec, void *val,int nval, CTA_Datatype datatype);


/** \brief Get all values of the tree-vector.
 *
 * \note  This method can only be used if all elements of the tree-vector
 *        are of the same data type.
 *
 * \param treevec  I  Tree-vector
 * \param val      O  receives the values
 * \param nval     I  number of elements in val
 * \param datatype I  data type of *val, must be the same as data type of elements in tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetVals(CTA_TreeVector treevec, void *val,int nval,CTA_Datatype datatype);

/** \brief Set single value of the tree-vector.
 *
 * \param treevec  IO Tree-Vector
 * \param i        I  index of value in tree-vector
 * \param val      I  value to be set
 * \param datatype I  data type of *val, must be the same as data type of element in tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_SetVal(CTA_TreeVector treevec, int i, void *val, CTA_Datatype datatype);


/** \brief Get single value of the tree-vector.
 *
 * \param treevec  I  Tree-vector
 * \param i        I  index in value in tree-vector
 * \param val      O  returned value
 * \param datatype I  data type of *val, must be the same as data type of element in tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetVal(CTA_TreeVector treevec, int i, void *val,CTA_Datatype datatype);

/** \brief Get size of tree-vector.
 *
 * \param treevec  I  Tree-vector
 * \param n        O  receives size of tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_GetSize(CTA_TreeVector treevec, int *n);

/** \brief Export tree-vector.
 *
 * Can export tree-vector to file or pack object.\n
 * usrdata must contain a handle of the file or pack object to be used.\n
 * Dependency: CTA_Vector_Export()
 *
 *
 * \param treevec  I  Tree-vector
 * \param usrdata  I  export properties
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Export(CTA_TreeVector treevec, CTA_Handle usrdata);

/** \brief Import Tree-vector.
 *
 * Can import tree-vector from file or pack object.\n
 * usrdata must contain a handle of the file or pack object to be used.\n
 * Dependency: CTA_Vector_Import()
 *
 *
 * \param treevec  I  Tree-vector
 * \param usrdata  I  import properties
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Import(CTA_TreeVector treevec, CTA_Handle usrdata);

/** \brief Import Tree-vector as flat vector.
 *
 * Can import tree-vector from netcdf file.\n
 * usrdata must contain a handle of the file .\n
 * Dependency: CTA_Vector_VImport()
 *
 *
 * \param treevec  I  Tree-vector
 * \param usrdata  I  import properties
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_VImport(CTA_TreeVector treevec, CTA_Handle usrdata);


/** \brief Free Tree-vector.
 *
 * \param treevec     I  handle of tree-vector
 * \param recursive  I  also free all sub-tree-vectors, yes: CTA_TRUE or no: CTA_FALSE
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Free(CTA_TreeVector *treevec, int recursive);

/** \brief Print tree-vector information.
 *
 * Gives following information:\n\n
 *  Tree-vector information:\n
 *  tag: [tag]\n
 *  nsubtreevecs: [number of sub-tree-vectors]\n
 *
 * If nsubtreevecs > 0: recursively prints all sub-tree-vectors
 * Else prints:\n
 * leaf: yes\n
 * tree-vector size (leaf)
 *
 * \param treevec     I  tree-vector
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Info(CTA_TreeVector treevec);

/** \brief Perform the matrix multiplication C:=alpha*op(A)*op(B)+beta*C
    where op(X)=X, X^T. However C and A are matrices of wich the columns are 
    tree-vectors
 *
 *  \param sC     IO array of tree-vector (matrix C)
 *  \param nc     I  number of columns of C (dimension of sC)
 *  \param transa I  transpose flag CTA_TRUE/CTA_FALSE for matrix A (not supported)
 *  \param transb I  transpose flag CTA_TRUE/CTA_FALSE for matrix B
 *  \param alpha  I  scalar
 *  \param sA     I  handle of matrix A
 *  \param na     I  number of columns of A (dimension of sA)
 *  \param mB     I  handle of matrix B
 *  \param beta   I  scalar
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_TreeVector_Gemm(CTA_TreeVector *sC, int nc, int transa, int transb, double alpha, CTA_TreeVector *sA, int na,
                   CTA_Matrix mB, double beta);


/** \brief Generate XML from one COSTA tree-vector
*
*  \param treevec   I  handle of a COSTA tree-vector
*  \param writer I  the XML text writer
*/
CTAEXPORT void CTAI_XML_WriteTreeVec(CTA_TreeVector treevec, xmlTextWriter *writer);

/** \brief Create a COSTA tree-vector from XML. 
*
*  \param cur_node  I  Current XML node 
*  \return             Handle to create or CTA_NULL in case of an error.
*/
CTAEXPORT CTA_TreeVector CTAI_XML_CreateTreeVec(xmlNode *cur_node);


/** \brief Perform given operation on all leafs of the treevector
*
*  \param treevec1 I  handle of first COSTA tree-vector
*  \param treevec2 I  handle of second COSTA tree-vector
*  \param treevec  I  handle of a COSTA tree-vector
*  \param op       I  operation to perform on the leafs
*  \param arg      I  additional argument of operation
 * \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_TreeVector_OpOnLeafs(CTA_TreeVector treevec1, CTA_TreeVector treevec2, CTA_Func op, CTA_Handle arg);

/** \brief Elementwise division of two vectors
*  \note y:=y./x
*
*  \param y       I  handle of a COSTA tree-vector (y)
*  \param x       I  handle of a COSTA tree-vector (y)
 * \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_TreeVector_ElmDiv(CTA_TreeVector y, CTA_TreeVector x);

/** \brief Elementwise multiplication of two vectors
*  \note y:=y.*x
*
*  \param y       I  handle of a COSTA tree-vector (y)
*  \param x       I  handle of a COSTA tree-vector (y)
*  \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_TreeVector_ElmProd(CTA_TreeVector y, CTA_TreeVector x);

/** \brief Elementwise sqare root 
*  \note y:=sqrt(y)
*
*  \param y       I  handle of a COSTA tree-vector (y)
*  \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_TreeVector_ElmSqrt(CTA_TreeVector y);


/** \brief Set nocompute flag of a sub-tree vector 
*
*  When this flag is set, the values of the sub-treevector will
*  be ignored in all basic vector operations (including asking the 
*  total length of the tree-vector). This propertie is used for
*  additionally adding some meta information
*
* \note the nocompute flag is set at the level of the parent!
* so the "isolated" sub-treevector can be used in basic vector
* operations.
*
*  \param x       I  handle of a COSTA tree-vector (y)
*  \param tag     I  tag of sub-treevector 
*  \return error status: CTA_OK if successful
*/
CTAEXPORT int CTA_TreeVector_SetSubTreeNocompute(CTA_TreeVector x, const char *tag);

/** \brief Increase the reference count of a treevector and all subtrevectors
 *
 * \param treevec  I handle of a COSTA tree-vector
*  \return error status: CTA_OK if successful
 *
 */
CTAEXPORT int CTA_TreeVector_IncRefCount(CTA_TreeVector treevec);
   
   

CTAEXPORT int CTA_TreeVector_SetMetainfo(CTA_TreeVector treevec, CTA_Metainfo minfo);
CTAEXPORT int CTA_TreeVector_GetMetainfo(CTA_TreeVector treevec, CTA_Metainfo minfo);


CTAEXPORT int CTAI_TreeVec_GetVecNumHandles(CTA_TreeVector treevec);

CTAEXPORT int CTAI_TreeVec_List(CTA_TreeVector treevec, CTA_Vector taglist, int *indx);

CTAEXPORT int CTA_TreeVector_List(CTA_TreeVector treevec, CTA_Vector taglist );

CTAEXPORT int CTA_TreeVector_GetVecNumHandles(CTA_TreeVector treevec);

CTAEXPORT void CTAI_Treevector_Operation_ScaledRMS(char *tag, CTA_Vector v1, 
                             CTA_Vector vscal, CTA_Handle hdum, int *retval);

CTAEXPORT void CTAI_Treevector_Operation_Amax(char *tag, CTA_Vector v1, 
                             CTA_Vector * v2, CTA_Handle hdum, int *retval);

CTAEXPORT void CTAI_Treevector_Operation_PrintEntry(char *tag, CTA_Vector v1, 
                             CTA_Vector v2, CTA_Handle hdum, int *retval);

CTAEXPORT void CTAI_Treevector_Operation_ScaledSSQ(char *tag, CTA_Vector v1, 
                             CTA_Vector vscal, CTA_Handle hdum, int *retval);

CTAEXPORT void CTAI_Treevector_Operation_MaxAbs(char *tag, CTA_Vector v1, 
                             CTA_Vector , CTA_Handle hdum, int *retval);

#ifdef __cplusplus
}
#endif
#endif

