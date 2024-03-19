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
\file  cta_usr_stoch_observer.h

\brief In this file a description is given of the interface of user stochastic observer functions.
When creating your own user stochastic observer class use the following as template.

The Usr_SObs to which is being referred in this template can be substituted by your own user stochastic observer object.

<b>Step 1</b>: for creating your own user stochastic observer class call the function CTA_SObs_DefineClass().

Example:

\code
typedef CTA_Handle CTA_SObsClass;

CTA_Func h_func[CTA_SOBS_NUMFUNC];
CTA_SObsClass my_own_sobs_class;
CTA_ObsDescrClass obsdescrclass; //observation descriptor class that is being used by stochastic observer

ierr=CTA_Func_Create(" ",&usr_sobs_create_size, hintf, &h_func[CTA_SOBS_CREATE_SIZE]);
//...for all implementation functions...

CTA_SObs_DefineClass("classname", h_func, obsdescrclass, &my_own_sobs_class);\endcode



Making a user stochastic observer class involves the implementation of the following functions:

CTA_SOBS_CREATE_SIZE         \n
CTA_SOBS_CREATE_INIT         \n
CTA_SOBS_FREE                \n
CTA_SOBS_CREATE_SELECTION    \n
CTA_SOBS_COUNT               \n
CTA_SOBS_GET_OBS_DESCRIPTION \n
CTA_SOBS_GET_VALUES          \n
CTA_SOBS_GET_REALISATION     \n
CTA_SOBS_GET_EXPECTATION     \n
CTA_SOBS_EVALUATE_PDF        \n
CTA_SOBS_GET_COV_MATRIX      \n
CTA_SOBS_GET_VARIANCE        \n
CTA_SOBS_EXPORT              \n
CTA_SOBS_GET_TIMES           \n

For creating an implementation function see documentation of CTA_Func_Create().

<b>Step 2</b>: to create an object of the newly defined stochastic observer class call CTA_SObs_Create() in the
same way as creating a CTA_SObs object but with a different class handle, i.e. the user class handle from step 1 above.

Example:

\code
Usr_SObs usrsobs; //user stochastic observer object
CTA_Handle userdata = CTA_NULL;
CTA_SObs_Create(my_own_sobs_class, &userdata, &usrsobs);
\endcode
\n
<b>Note 1: </b> with object data is meant only the object itself including pointer(s) to its contents, but
not the contents of the stochastic observer.\n\n
*/



//#define CTA_SOBS_CREATE_SIZE         ( 1)
/** \brief Implementation that forms part of the create process.
 * 
 * Must give the memory size of a new user stochastic observer object.
 * 
 * Example:
 *  \code
//in header file:
typedef struct {
   //your own user object data goes here...
}USR_SOBS;

//user implementation:
void usr_sobs_create_size(...){
   *memsize = sizeof(USR_SOBS);
   *retval = CTA_OK;
}
 \endcode
 *
 * \note At index CTA_SOBS_CREATE_SIZE in the function list of the class descriptor.
 *
 * \param memsize  O  must receive the number of bytes which are necessary to store one
                      user stochastic observer class, with a pointer to the contents (data), but without the
                      contents themselves
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_create_size(int *memsize, int *retval);


//#define CTA_SOBS_CREATE_INIT         ( 2)
/** \brief Implementation that forms part of the create process.
 *
 * The user stochastic observer object needs to be made ready for use.
 *
 * \note At index CTA_SOBS_CREATE_INIT in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_create_init(Usr_SObs *sobsdata, void *userdata, int *retval);


//#define CTA_SOBS_FREE                ( 3)
/** \brief Implementation for freeing the object data and associated resources.
 *
 * \note At index CTA_SOBS_FREE in the function list of the class descriptor.
 *
 * \param sobsdata IO pointer to user object data
 * \param userdata IO user data
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_free(Usr_SObs *sobsdata, void *userdata, int *retval);


//#define CTA_SOBS_CREATE_SELECTION    ( 4)
/** \brief Implementation for creating a new stochastic observer that is subset of existing stochastic observer.
 *
 * \note At index CTA_SOBS_CREATE_SELECTION in the function list of the class descriptor.
 * \note CTA_SObs_CreateTimSel() calls this implementation with userdata a handle of a time selection string.
 *
 * \param sobsdatain  I  pointer to object data of in-object
 * \param userdata    IO user data
 * \param sobsdataout IO pointer to object data of out-object
 * \param retval      O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_create_selection(Usr_SObs *sobsdatain, void *userdata, Usr_SObs *sobsdataout, int *retval);


//#define CTA_SOBS_COUNT               ( 5)
/** \brief Implementation for counting the number of measures 
in user stochastic observer object.
 *
 * \note At index CTA_SOBS_COUNT in the function list of the class descriptor.
 *
 * \param sobsdata IO pointer to user object data
 * \param nmeasr   O  must receive number of measures in object
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_count(Usr_SObs *sobsdata, int *nmeasr, int *retval);


//#define CTA_SOBS_GET_OBS_DESCRIPTION ( 6)
/** \brief Implementation for creating the observation description corresponding to the stochastic observer.
 *
 * \note At index CTA_SOBS_GET_DESCRIPTION in the function list of the class descriptor.
 * \note Caller is responsible for freeing the here created observation description
 * \note THIS IMPLEMENTATION FUNCTION IS NOT SUPPORTED
 *
 * \param sobsdata IO pointer to user object data
 * \return no return value
 */
void usr_sobs_get_description(Usr_SObs *sobsdata);


//#define CTA_SOBS_GET_VALUES          ( 7)
/** \brief Implementation for getting all values of user stochastic observer object.
 *
 * \note At index CTA_SOBS_GET_VALUES in the function list of the class descriptor.
 *
 * \param sobsdata IO pointer to user object data
 * \param hvec     O  handle of vector that must receive the values; must exist before calling;
                      must be of appropriate data type
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_get_values(Usr_SObs *sobsdata, CTA_Vector *hvec, int *retval);


//#define CTA_SOBS_GET_REALISATION     ( 8)
/** \brief Implementation for calculating stochastic realizations for all the 
           measurements in a user stochastic observer.
 *
 * \note At index CTA_SOBS_GET_REALISATION in the function list of the class descriptor.
 *
 * \param sobsdata I pointer to user object data
 * \param hvec     O handle of vector that must receive realisation; must exist before calling
 * \param retval   O must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_get_realisation(Usr_SObs *sobsdata, CTA_Vector *hvec, int *retval);


//#define CTA_SOBS_GET_EXPECTATION     ( 9)
/** \brief Implementation for getting the expectation of the probability density function of the mesurements.
 *
 * \note At index CTA_SOBS_GET_EXPECTATION in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param hvec     O  handle of vector that must receive expectation; must exist before calling
 * \param retval   O  must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_get_expectation(Usr_SObs *sobsdata, CTA_Vector *hvec, int *retval);


//#define CTA_SOBS_EVALUATE_PDF        (10)
/** \brief Implementation for getting the value of the probability density function of the mesurements at given location.
 *
 * \note At index CTA_SOBS_EVALUATE_PDF in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param location I  handle of vector with location for evaluating pdf
 * \param pdfvalue O  handle of vector that must receive result of evaluation; must exist before calling
 * \param retval   O  must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_evaluate_pdf(Usr_SObs *sobsdata, CTA_Vector *location, CTA_Vector *pdfvalue, int *retval);


//#define CTA_SOBS_GET_COV_MATRIX      (11)
/** \brief Implementation for getting all the variances of the measurements in a
           user stochastic observer.
 *
 * \note At index CTA_SOBS_GET_COV_MATRIX in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param hmatrix  O  handle of matrix object that must receive the covariance matrix; must exist before calling
 * \param retval   O  must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_get_cov_matrix(Usr_SObs *sobsdata, CTA_Matrix *hmatrix, int *retval);


//#define CTA_SOBS_GET_VARIANCE        (12)
/** \brief Implementation for calculating all variances or standard deviations. 
 *
 * \note At index CTA_SOBS_GET_VARIANCE in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param variance O  handle of vector that must receive variances / standard
                      deviations; must exist before calling
 * \param varflag  I  variance flag: CTA_TRUE for calculating variances,
                      CTA_FALSE for standard deviations
 * \param retval   O  must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_get_variance(Usr_SObs *sobsdata, CTA_Vector *variance, int *varflag, int *retval);


//#define CTA_SOBS_EXPORT              (13)
/** \brief Implementation for exporting user stochastic observer object.
 *
 * \note At index CTA_SOBS_EXPORT in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param userdata IO pointer to user data
 * \param retval   O  must receive return value of implementation function
 * \return no return value
 */
void usr_sobs_export(Usr_SObs *sobsdata, void *userdata, int *retval);


//#define CTA_SOBS_GET_TIMES           (14)
/** \brief Implementation for getting all times associated with the measurements.
 *
 * \note At index CTA_SOBS_GET_TIMES in the function list of the class descriptor.
 *
 * \param sobsdata I  pointer to user object data
 * \param times    O  handle of vector that must receive times; must exist before calling
 * \param retval   O  must receive return value of user implementation function
 * \return no return value
 */
void usr_sobs_get_times(Usr_SObs *sobsdata, CTA_Vector *times, int *retval);




