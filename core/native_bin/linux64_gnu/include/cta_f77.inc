!
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2005  Nils van Velzen
! 
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
! 
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
 
!     FORTRAN file unit for standard output
      integer FCTA_STDOUT
      parameter (FCTA_STDOUT = 6)

!     handle to a COSTA object
      integer CTA_HANDLE
      parameter (CTA_HANDLE=-1) 
!     A handle of a COSTA interface object
      integer CTA_INTERFACE
      parameter (CTA_INTERFACE=-2) 
!     A handle of a COSTA function object
      integer CTA_FUNCTION
      parameter (CTA_FUNCTION=-3)
!     A handle of a COSTA vector object
      integer CTA_VECTOR
      parameter (CTA_VECTOR=-4)
!     A handle of a COSTA Vector class
      integer CTA_VECTORCLASS
      parameter (CTA_VECTORCLASS=-5)
!     A handle of a COSTA (sub) state
      integer CTA_STATE
      parameter (CTA_STATE=-6)
!/*! A handle of a COSTA Matrix class     */
      integer CTA_MATRIXCLASS
      parameter (CTA_MATRIXCLASS=-7)
!/*! A handle of a COSTA Matrix      */
      integer CTA_MATRIX
      parameter (CTA_MATRIX=-8)
!/*!     A COSTA model class  */
      integer CTA_MODELCLASS
      parameter (CTA_MODELCLASS=-16)

      integer CTA_VOID
      parameter (CTA_VOID        =-100)
      integer CTA_INTEGER
      parameter (CTA_INTEGER     =-101)
      integer CTA_REAL
      parameter (CTA_REAL        =-102)
      integer CTA_DOUBLE
      parameter (CTA_DOUBLE      =-103)
      integer CTA_FSTRING
      parameter (CTA_FSTRING     =-104)
      integer CTA_CSTRING
      parameter (CTA_CSTRING     =-105)
      integer CTA_STRING
      parameter (CTA_STRING      =-28)
      integer CTA_TIME
      parameter (CTA_TIME        = -15)
      integer CTA_ARRAY
      parameter (CTA_ARRAY        = -30)

      integer CTA_1DINTEGER
      parameter (CTA_1DINTEGER   =-201)
      integer CTA_1DREAL
      parameter (CTA_1DREAL      =-202)
      integer CTA_1DDOUBLE
      parameter (CTA_1DDOUBLE    =-203)
      integer CTA_1DFSTRING
      parameter (CTA_1DFSTRING   =-204)
      integer CTA_1DCSTRING
      parameter (CTA_1DCSTRING   =-205)

!     Common errors
      integer CTA_OK
      parameter (CTA_OK=0)
      integer CTA_ARRAY_TOO_SHORT
      parameter (CTA_ARRAY_TOO_SHORT=10)
      integer CTA_ILLEGAL_DATATYPE
      parameter (CTA_ILLEGAL_DATATYPE=11)
      integer CTA_DIMENSION_ERROR
      parameter (CTA_DIMENSION_ERROR=12)
      integer CTA_INCOMPATIBLE_VECTORS
      parameter (CTA_INCOMPATIBLE_VECTORS=13)
      integer CTA_CONCAT_NOT_POSSIBLE
      parameter (CTA_CONCAT_NOT_POSSIBLE=14)
      integer CTA_SETVAL_NOT_POSSIBLE
      parameter (CTA_SETVAL_NOT_POSSIBLE=15)
      integer CTA_ITEM_NOT_FOUND
      parameter (CTA_ITEM_NOT_FOUND=16)
      integer CTA_UNINITIALISED_SUBSTATES 
      parameter (CTA_UNINITIALISED_SUBSTATES=17)
      integer CTA_STATES_NOT_COMPATIBLE   
      parameter (CTA_STATES_NOT_COMPATIBLE  =18)
      integer CTA_INCOMPATIBLE_MATRICES   
      parameter (CTA_INCOMPATIBLE_MATRICES  =19)
      integer CTA_NOT_IMPLEMENTED         
      parameter (CTA_NOT_IMPLEMENTED        =20)

!     Common Constants
      integer CTA_DEFAULT
      parameter (CTA_DEFAULT=0)
      integer CTA_NULL
      parameter (CTA_NULL=0)
      integer CTA_TRUE
      parameter(CTA_TRUE=1)
      integer CTA_FALSE
      parameter(CTA_FALSE=0)

      integer CTA_STRLEN_NAME, CTA_STRLEN_TAG
      parameter (CTA_STRLEN_NAME=80, CTA_STRLEN_TAG=80)

      integer CTA_ASSIMOBS, CTA_VALIDATEOBS, CTA_ALLOBS
      parameter(CTA_ASSIMOBS=1, CTA_VALIDATEOBS=2, CTA_ALLOBS=3)

!     parameters from cta_pack.h

!     Reset pack/unpack pointer of pack object
      integer, parameter ::CTA_PACK_RESET=-1

!     parameters from cta_model.h

      integer, parameter ::FCTA_MODEL_CREATE_SIZE        =1
      integer, parameter ::FCTA_MODEL_CREATE_INIT        =2
      integer, parameter ::FCTA_MODEL_FREE               =3
      integer, parameter ::FCTA_MODEL_COMPUTE            =4
      integer, parameter ::FCTA_MODEL_SET_STATE          =5
      integer, parameter ::FCTA_MODEL_GET_STATE          =6
      integer, parameter ::FCTA_MODEL_AXPY_STATE         =7
      integer, parameter ::FCTA_MODEL_AXPY_MODEL         =8
      integer, parameter ::FCTA_MODEL_SET_FORC           =9
      integer, parameter ::FCTA_MODEL_GET_FORC           =10
      integer, parameter ::FCTA_MODEL_AXPY_FORC          =11
      integer, parameter ::FCTA_MODEL_SET_PARAM          =12
      integer, parameter ::FCTA_MODEL_GET_PARAM          =13
      integer, parameter ::FCTA_MODEL_AXPY_PARAM         =14
      integer, parameter ::FCTA_MODEL_GET_STATESCALING   =15
      integer, parameter ::FCTA_MODEL_GET_TIMEHORIZON    =16
      integer, parameter ::FCTA_MODEL_GET_CURRENTTIME    =17


      integer, parameter ::FCTA_MODEL_GET_NOISE_COUNT    =18
      integer, parameter ::FCTA_MODEL_GET_NOISE_COVAR    =19
      integer, parameter ::FCTA_MODEL_GET_OBSVALUES      =20
      integer, parameter ::FCTA_MODEL_GET_OBSSELECT      =21
      integer, parameter ::FCTA_MODEL_ANNOUNCE_OBSVALUES =22
      integer, parameter ::FCTA_MODEL_ADD_NOISE          =23
      integer, parameter ::FCTA_MODEL_EXPORT             =24
      integer, parameter ::FCTA_MODEL_IMPORT             =25
      integer, parameter ::FCTA_MODEL_ADJ_SET_FORC       =26
      integer, parameter ::FCTA_MODEL_ADJ_COMPUTE        =27
      integer, parameter ::FCTA_MODEL_ADJ_PREPARE        =28
      integer, parameter ::FCTA_MODEL_NUMFUNC            =28

!     Observation description parameters
      integer, parameter ::FCTA_OBSDESCR_CREATE_SIZE        =  1+1
      integer, parameter ::FCTA_OBSDESCR_CREATE_INIT        =  2+1
      integer, parameter ::FCTA_OBSDESCR_FREE               =  3+1
      integer, parameter ::FCTA_OBSDESCR_GET_PROPERTIES     =  4+1
      integer, parameter ::FCTA_OBSDESCR_GET_KEYS           =  5+1
      integer, parameter ::FCTA_OBSDESCR_COUNT_OBSERVATIONS =  6+1
      integer, parameter ::FCTA_OBSDESCR_COUNT_PROPERTIES   =  7+1
      integer, parameter ::FCTA_OBSDESCR_EXPORT             =  8+1
      integer, parameter ::FCTA_OBSDESCR_SELECTION          =  9+1
      integer, parameter ::FCTA_OBSDESCR_NUMFUNC            = 10+1

      integer, parameter ::FCTA_SOBS_CREATE_SIZE        =  1+1
      integer, parameter ::FCTA_SOBS_CREATE_INIT        =  2+1
      integer, parameter ::FCTA_SOBS_FREE               =  3+1
      integer, parameter ::FCTA_SOBS_CREATE_SELECTION   =  4+1
      integer, parameter ::FCTA_SOBS_COUNT              =  5+1
      integer, parameter ::FCTA_SOBS_GET_OBS_DESCRIPTION=  6+1
      integer, parameter ::FCTA_SOBS_GET_VALUES         =  7+1
      integer, parameter ::FCTA_SOBS_GET_REALISATION    =  8+1
      integer, parameter ::FCTA_SOBS_GET_EXPECTATION    =  9+1
      integer, parameter ::FCTA_SOBS_EVALUATE_PDF       = 10+1
      integer, parameter ::FCTA_SOBS_GET_COV_MATRIX     = 11+1
      integer, parameter ::FCTA_SOBS_GET_VARIANCE       = 12+1
      integer, parameter ::FCTA_SOBS_EXPORT             = 13+1
      integer, parameter ::FCTA_SOBS_GET_TIMES          = 14+1
      integer, parameter ::FCTA_SOBS_NUMFUNC            = 15+1


!     Common block holding "constants" that are set at initialisation

!NOT HAPPY BUT HAVE SOME PROBLEMS WITH LOADING OF SO-FILES UNDER LINUX
!WHERE WE HAVE 2 INSTANCES OF COMMON BLOCK JUST SET VALUES!!!!

      integer, parameter :: CTA_MODBUILD_PAR   =  0
      integer, parameter :: CTA_FILE_STDOUT    =  1
      integer, parameter :: CTA_DEFAULT_SOBS   = 23
      integer, parameter :: CTA_COMBINE_SOBS   = 45
      integer, parameter :: CTA_NETCDF_SOBS    = 67
      integer, parameter :: CTA_MAORI_SOBS     = -173
      integer, parameter :: CTA_OBSDESCR_TABLE = 77
      integer, parameter :: CTA_DEFAULT_VECTOR = 100
      integer, parameter :: CTA_DEFAULT_MATRIX = 116
      integer, parameter :: CTA_MODBUILD_SP    = 147
      integer, parameter :: CTA_MODELCOMBINER  = 0
      integer, parameter :: CTA_MODBUILD_B3B   = 0
      integer, parameter :: CTA_MODBUILD_BB    = 0
      integer, parameter :: CTA_OP_ROOT_RMS    = 148
      integer, parameter :: CTA_OP_ROOT_AMAX   = 149 
      integer, parameter :: CTA_OP_ROOT_PRINTI = 150
      integer, parameter :: CTA_OP_ROOT_SSQ    = 151

!      external CTA_DEFAULT_VECTOR, CTA_DEFAULT_MATRIX,                    &
!     &         CTA_DEFAULT_SOBS, CTA_MODBUILD_SP,                         &
!     &         CTA_MODBUILD_PAR, CTA_OBSDESCR_TABLE,                      &
!     &         CTA_FILE_STDOUT, CTA_MODELCOMBINER,                        &
!     &         CTA_MODBUILD_B3B



!!!DEC$ IF DEFINED(CTALIB)
!!!DEC$ ATTRIBUTES DLLEXPORT:: /ctaf77/
!!!DEC$ ELSE
!!!DEC$ ATTRIBUTES DLLIMPORT:: /ctaf77/
!!!DEC$ ENDIF
!!      common /ctaf77/ CTA_DEFAULT_VECTOR, CTA_DEFAULT_MATRIX,             &
!!     &                CTA_DEFAULT_SOBS, CTA_MODBUILD_SP,                  &
!!     &                CTA_MODBUILD_PAR, CTA_OBSDESCR_TABLE,               &
!!     &                CTA_FILE_STDOUT,  CTA_MODELCOMBINER,                &
!!     &                CTA_MODBUILD_B3B





