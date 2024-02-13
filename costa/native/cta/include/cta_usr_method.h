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

/**
\file  cta_usr_method.h

\brief In this file a description is given of the interface of user method functions.
When creating your own user method class use the following as template.

The Usr_Meth to which is being referred in this template can be substituted by your own user method object.

<b>Step 1</b>: for creating your own user method class call the function CTA_Meth_DefineClass().

Example:

\code
typedef CTA_Handle CTA_MethClass;

CTA_Func h_func[CTA_METH_NUMFUNC];
CTA_MethClass my_own_meth_class;

ierr=CTA_Func_Create(" ",&usr_meth_create_size, hintf, &h_func[CTA_METH_CREATE_SIZE]);
//...for all implementation functions...

CTA_Meth_DefineClass("classname", h_func, &my_own_meth_class);\endcode

Making a user method class involves the implementation of the following functions:

CTA_METH_CREATE_SIZE  \n
CTA_METH_CREATE_INIT  \n
CTA_METH_RUN          \n
CTA_METH_FREE         \n

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined method class call CTA_Meth_Create() in the
same way as creating a CTA_Meth object but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_Meth usrmeth; //user method object
CTA_Handle userdata = CTA_NULL;
CTA_Meth_Create(my_own_meth_class, &userdata, &usrmeth);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents themselves.\n\n
*/


//#define CTA_METH_CREATE_SIZE  ( 1)
/** \brief Implementation that forms part of the create process.
 * 
 * Must give the memory size of a new method user object.
 *
 * Example:
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_METH;

//user implementation:
void usr_meth_create_size(...){
   *memsize = sizeof(USR_METH);
   *retval = CTA_OK;
}
 \endcode
 *
 * \note At index CTA_METH_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param memsize  O must receive the number of bytes which are necessary to store one
                     user method class, with a pointer to the contents (data), but without the
                     contents themselves
 * \param retval   O must receive return value of user implementation function
 * \return no return value
 */
void usr_meth_create_size(int *memsize, int *retval);



//#define CTA_METH_CREATE_INIT  ( 2)
/** \brief Implementation that forms part of the create process.
 *
 * The user method object needs to be made ready for use.
 *
 * \note At index CTA_METH_CREATE_INIT in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to user method object data
 * \param userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_meth_create_init(Usr_Meth *objectdata, void *userdata, int *retval);



//#define CTA_METH_RUN          ( 3)
/** \brief Implementation for running user method.
 *
 * \note At index CTA_METH_RUN in the function list of the class descriptor.
 *
 * \param objectdata I pointer to user method object data
 * \param retval     O must receive return value of user implementation function
 * \return no return value
 */
void usr_meth_run(Usr_Meth *objectdata, int *retval);



//#define CTA_METH_FREE         ( 4)
/** \brief Implementation for freeing the object data and associated resources.
 *
 * \note At index CTA_METH_FREE in the function list of the class descriptor.
 *
 * \param objectdata I pointer to user method object data
 * \param retval     O must receive return value of user implementation function
 * \return no return value
 */
void usr_meth_free(Usr_Meth *objectdata, int *retval);














