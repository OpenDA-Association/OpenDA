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
\file  cta_modbuild_b3.h
\brief Description of the COSTA blackbox component
*/

#ifndef CTA_MODBUILD_B3B_H
#define CTA_MODBUILD_B3B_H

#include <string.h>
#ifdef WIN32
#include <direct.h>
#endif
#include "cta.h"
#include "cta_system.h"
#include "f_cta_utils.h"
#include "cta_datatypes.h"
#include "cta_model_utilities.h" 
#include "cta_handles.h"
#include "cta_datetime.h"

#define STRING_MAX		1024                 /* maximum length of string */

#define B3B_ENCODING  ("utf-8")              /* XML encodiing */


typedef struct B3B_Variable B3B_Variable;
typedef B3B_Variable * B3B_VariablePntr;

struct B3B_Variable {
   char    *name;                            /* variable name */
   double   value;                           /* variable value */
};

typedef struct B3B_Station B3B_Station;
typedef B3B_Station * B3B_StationPntr;

struct B3B_Station {
   char             *name;                   /* station name */
   char             *location;               /* station location */
   char             *filename;               /* station filename */
   int               nvariables;             /* station number of variables */
   B3B_VariablePntr *variables;              /* station variables */
};

typedef struct B3B_Forcing B3B_Forcing;
typedef B3B_Forcing * B3B_ForcingPntr;

struct B3B_Forcing {
   char    *name;                            /* forcing name */
   char    *file;                            /* forcing file */
   char    *element;                         /* forcing element */
   char    *property;                        /* forcing property */
   char    *item;                            /* forcing item */
   double   value;                           /* forcing value */
};

typedef struct B3B_Parameter B3B_Parameter;
typedef B3B_Parameter * B3B_ParameterPntr;

struct B3B_Parameter {
   char    *name;                            /* parameter name */
   char    *file;                            /* parameter file */
   char    *element;                         /* parameter element */
   char    *property;                        /* parameter property */
   char    *item;                            /* parameter item */
   double   value;                           /* parameter value */
};

typedef struct B3B_StateExchange B3B_StateExchange;
typedef B3B_StateExchange * B3B_StateExchangePntr;

struct B3B_StateExchange {
   char    *executable;                      /* name of blackbox executable */
   char    *state2model;                     /* name of input file */
   char    *model2state;                     /* name of output file */
   char    *outputSteps;                     /* output steps 'last' or 'all' */
};

typedef struct B3B_Model B3B_Model;
typedef B3B_Model * B3B_ModelPntr;

struct B3B_Model{
   char                   *type;             /* model type */
   char                   *description;      /* model description */
   double                  timestep;         /* simulation timestep in seconds */
   int                     simulationNumber; /* simulation number */
   char                   *workingdir;       /* working map of simulation */
   char                   *templatedir;      /* map with simulation template */
   char                   *simulationMap;    /* map with simulation results */
   int                     stateLength;      /* length of the state vector */
   int                     nstations;        /* number of stations */
   B3B_StateExchangePntr   stateexchange;    /* pointer to the state exchange variables */
   B3B_StationPntr        *stations;         /* pointer to the list of stations */
   int                     nparameters;      /* number of parameters */
   B3B_ParameterPntr      *parameters;       /* pointer to the list of parameters */
   int                     nforcings;        /* number of forcings */
   B3B_ForcingPntr        *forcings;         /* pointer to the list of forcings */
};

#define B3B_INDEX_THIS              ( 0) /* Handle of instance               */
#define B3B_INDEX_TIME              ( 1) /* Time instance of model (state)   */
#define B3B_INDEX_STATE             ( 2) /* State vector of model            */
#define B3B_INDEX_FORCINGS          ( 3) /* Tree vector containing the forcings of the model */
#define B3B_INDEX_PARAMETERS        ( 4) /* Tree vector of model parameters */
#define B3B_INDEX_USERDATA          ( 5) /* Userdata */

#define B3B_SIZE_DATABLK            ( 6)


/** \brief Create the model class of the B3B Black-box builder
 *
 * \note This is not a user function. It is called at initialization of the
 *       COSTA environment.
 *
 *  \param modelcls  O  receives handle of the B3B-modelbuilder class
 */

CTANOEXPORT void CTA_Modbuild_b3b_CreateClass(CTA_ModelClass *modelcls);

#endif

