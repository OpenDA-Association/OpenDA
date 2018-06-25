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
\file  cta_usr_obs_desc.h

\brief In this file a description is given of the interface of user observation descriptor functions.
When creating your own user observation descriptor class use the following as template.

The CTA_Usr_ObsDescr to which is being referred in this template can be substituted by your own user observation descriptor object.

<b>Step 1</b>: for creating your own user model class call the function CTA_Model_DefineClass().

Example:

\code
typedef CTA_Handle CTA_ObsDescrClass;

CTA_Func h_func[CTA_OBSDESCR_NUMFUNC];
CTA_ObsDescrClass my_own_obsdescr_class;

ierr=CTA_Func_Create(" ",&usr_obsdescr_create_size, hintf, &h_func[CTA_OBSDESCR_CREATE_SIZE]);
//...for all implementation functions...

CTA_ObsDescr_DefineClass("classname", h_func, &my_own_obsdescr_class);\endcode

Making a new observation descriptor class involves the implementation of the following functions:

CTA_OBSDESCR_CREATE_SIZE         \n
CTA_OBSDESCR_CREATE_INIT         \n
CTA_OBSDESCR_FREE                \n
CTA_OBSDESCR_GET_PROPERTIES      \n
CTA_OBSDESCR_GET_KEYS            \n
CTA_OBSDESCR_COUNT_OBSERVATIONS  \n
CTA_OBSDESCR_COUNT_PROPERTIES    \n
CTA_OBSDESCR_EXPORT

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined observation descriptor class call CTA_ObsDescr_Create() in the
same way as creating a CTA_ObsDescr object but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_ObsDescr usrobsdescr; //user observation descriptor object
CTA_Handle userdata = CTA_NULL;
CTA_ObsDescr_Create(my_own_obsdescr_class, &userdata, &usrobsdescr);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents of the observation descriptor.\n\n
*/



//#define CTA_OBSDESCR_CREATE_SIZE         ( 1)
/** \brief Implementation that forms part of the create process.
 * 
 * Must give the memory size of a new object.
 *
 * Example:
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_OBSDESCR;

//user implementation:
void usr_obsdescr_create_size(...){
   *memsize = sizeof(USR_OBSDESCR);
   *retval = CTA_OK;
}
 \endcode
 *
 * \note At index CTA_OBSDESCR_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param memsize  O  must receive the number of bytes which are necessary to store one
                      user stochastic observer class, with a pointer to the contents (data), but without the
                      contents themselves
 * \param retval   O  must receive return value of user implementation function

 * \return no return value
 */
void usr_obsdescr_create_size(int *memsize, int *retval);


//#define CTA_OBSDESCR_CREATE_INIT         ( 2)
/** \brief Implementation that forms part of the create process.
 *
 * The user observation descriptor object needs to be made ready for use.
 *
 * \note At index CTA_OBSDESCR_CREATE_INIT in the function list of the class descriptor.
 *
 * \param myhandle        I  Handle assigned by COSTA
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_create_init(CTA_ObsDescr myhandle, Usr_ObsDescr *objectdata,
                              void *userdata, int *retval);


//#define CTA_OBSDESCR_FREE                ( 3)
/** \brief Implementation for freeing the object data and associated resources.
 *
 * \note At index CTA_OBSDESCR_FREE in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_free(Usr_ObsDescr *objectdata, int *retval);


//#define CTA_OBSDESCR_GET_PROPERTIES      ( 4)
/** \brief Implementation for gettings properties associated with given key.
 *
 * \note At index CTA_OBSDESCR_GET_PROPERTIES in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param key        I  description of property key
 * \param properties O  vector that must receive properties associated with given key; must exist before calling
 * \param datatype   I  data type of elements in properties vector
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_get_properties(Usr_ObsDescr *objectdata,
          const char *key, CTA_Vector *properties, CTA_Datatype *datatype, int *retval);


//#define CTA_OBSDESCR_GET_KEYS            ( 5)
/** \brief Implementation for getting all key names of user observation descriptor.
 *
 * \note At index CTA_OBSDESCR_GET_KEYS in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param keys       O  handle vector that must receive key descriptions; must exist before calling
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_get_keys(Usr_ObsDescr *objectdata, CTA_Vector *keys, int *retval);


//#define CTA_OBSDESCR_COUNT_OBSERVATIONS  ( 6)
/** \brief Implementation for counting number of observations.
 *
 * \note At index CTA_OBSDESCR_COUNT_OBSERVATIONS in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param nobs       O  must receive number of observations
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_count_observations(Usr_ObsDescr *objectdata, int* nobs, int *retval);


//#define CTA_OBSDESCR_COUNT_PROPERTIES    ( 7)
/** \brief Implementation for counting number of properties.
 *
 * \note At index CTA_OBSDESCR_COUNT_PROPERTIES in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param nkeys      O  must receive number of property keys
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_count_properties(Usr_ObsDescr *objectdata, int* nkeys, int *retval);


//#define CTA_OBSDESCR_EXPORT              ( 8)
/** \brief Implementation for exporting user observation descriptor.
 *
 * \note At index CTA_OBSDESCR_EXPORT in the function list of the class descriptor.
 *
 * \param objectdata I  pointer to object data of user stochastic observer
 * \param userdata   IO user data
 * \param retval     O  must receive return value of user implementation function
 * \return no return value
 */
void usr_obsdescr_export(Usr_ObsDescr *objectdata, void *userdata, int *retval);


//#define CTA_OBSDESCR_SELECTION           ( 9)
/** \brief Create a new observation description that is subset of existing
 *         observation description.
 *
 * \param objectdata      I  object data of observation description of the
 *                           initial observation description insntance
 * \param selection       I  selection criterion (subset of SQL)
 * \param reltab          O  Relation table specifying the relation between
 *                           the original and new observation description
 *                           component. Note no relation table is created when 
 *                           reltab==CTA_NULL on enty
 * \param objectdata_out  O  new observation description created subset
 * \param retval          O  receives return value
 */
void usr_obsdescr_createsel(Usr_ObsDescr *descr,
          CTA_String *selection, CTA_RelTable reltab, 
          CTA_ObsDescr myhandle_out,
          Usr_ObsDescr *descrout, int *retval);


