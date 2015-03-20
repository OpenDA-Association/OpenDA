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

#ifndef CTA_MODBUILD_BB_H
#define CTA_MODBUILD_BB_H

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

#define BB_ENCODING  ("utf-8")              /* XML encodiing */


typedef struct BB_Variable BB_Variable;
typedef BB_Variable * BB_VariablePntr;

struct BB_Variable {
   char    *name;                            /* variable name */
   double   value;                           /* variable value */
};

typedef struct BB_Station BB_Station;
typedef BB_Station * BB_StationPntr;

struct BB_Station {
   char             *name;                   /* station name */
   char             *location;               /* station location */
   char             *filename;               /* station filename */
   int               nvariables;             /* station number of variables */
   BB_VariablePntr *variables;              /* station variables */
};

typedef struct BB_Forcing BB_Forcing;
typedef BB_Forcing * BB_ForcingPntr;

struct BB_Forcing {
   char    *name;                            /* forcing name */
   char    *file;                            /* forcing file */
   char    *element;                         /* forcing element */
   char    *property;                        /* forcing property */
   char    *item;                            /* forcing item */
   double   value;                           /* forcing value */
};

typedef struct BB_Parameter BB_Parameter;
typedef BB_Parameter * BB_ParameterPntr;

struct BB_Parameter {
   char    *name;                            /* parameter name */
   char    *file;                            /* parameter file */
   char    *element;                         /* parameter element */
   char    *property;                        /* parameter property */
   char    *item;                            /* parameter item */
   double   value;                           /* parameter value */
};

typedef struct BB_StateExchange BB_StateExchange;
typedef BB_StateExchange * BB_StateExchangePntr;

struct BB_StateExchange {
   char    *executable;                      /* name of blackbox executable */
   char    *state2model;                     /* name of input file */
   char    *model2state;                     /* name of output file */
   char    *outputSteps;                     /* output steps 'last' or 'all' */
   char    *state2model_file;                /* filename to write state */
   char    *model2state_file;                /* filename to read state */
   char    *initial_state_file;              /* filename containing initial state */
   CTA_Func state2model_func;                /* Handle to user function for writing state 2 model */
   CTA_Func model2state_func;                /* Handle to user function for writing model 2 state */
   CTA_Func initial_state_func;                /* Handle to user function for reading initial state */
  int      nofquantities;                    /* number of quantities == number of substates */
  int     quantsize;                          /* length of quantityvector */
  int nofdimensions;                         /* number of co-ordinate dimensions */
  CTA_String     *hquantid;                 /* id of each quantity */ 
  CTA_String     *hdimid;                 /* id of each dimension */ 
  int *dimlength;                        /* length of each dimension */
};

typedef struct BB_Model BB_Model;
typedef BB_Model * BB_ModelPntr;

struct BB_Model{
   char                   *type;             /* model type */
   char                   *description;      /* model description */
   double                  timestep;         /* simulation timestep in seconds */
   int                     simulationNumber; /* simulation number */
   char                   *workingdir;       /* working map of simulation */
   char                   *templatedir;      /* map with simulation template */
   char                   *simulationMap;    /* map with simulation results */
   int                     stateLength;      /* length of the state vector */
   int                     nstations;        /* number of stations */
   BB_StateExchangePntr   stateexchange;    /* pointer to the state exchange variables */
   BB_StationPntr        *stations;         /* pointer to the list of stations */
   int                     nparameters;      /* number of parameters */
   BB_ParameterPntr      *parameters;       /* pointer to the list of parameters */
   int                     nforcings;        /* number of forcings */
   BB_ForcingPntr        *forcings;         /* pointer to the list of forcings */
};

static int BB_simulationNumber=0;             // Number of the simulation         

#define BB_INDEX_THIS              ( 0) /* Handle of instance               */
#define BB_INDEX_TIME              ( 1) /* Time instance of model (state)   */
#define BB_INDEX_STATE             ( 2) /* State vector of model            */
#define BB_INDEX_FORCINGS          ( 3) /* Tree vector containing the forcings of the model */
#define BB_INDEX_PARAMETERS        ( 4) /* Tree vector of model parameters */
#define BB_INDEX_USERDATA          ( 5) /* Userdata */

#define BB_SIZE_DATABLK            ( 6)


/** \brief Create the model class of the BB Black-box builder
 *
 * \note This is not a user function. It is called at initialization of the
 *       COSTA environment.
 *
 *  \param modelcls  O  receives handle of the BB-modelbuilder class
 */
CTANOEXPORT void CTA_Modbuild_b3b_CreateClass(CTA_ModelClass *modelcls);

#endif

