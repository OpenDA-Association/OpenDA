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
#ifndef CTA_PAR_H
#define CTA_PAR_H
#include "cta_system.h"
#include "cta_handles.h"
#include "cta_datatypes.h"

#ifdef USE_MPI
#include "mpi.h"
#endif

/**
\file  cta_par.h

\brief Interface for creating parallel applications with COSTA */

/** Type of processes */
enum CTA_ParProcType {CTA_ParMaster, CTA_ParWorker, CTA_ParOther};


#ifdef __cplusplus
extern "C" {
#endif

/** Flag to indicate whether this is a filter process or model/worker process */
CTAEXPORT extern int CTA_FILTER_PROCESS;


/** Flag CTA_TRUE/CTA_FALSE to indicate whether run is in a parallel
 *  environment */
CTAEXPORT extern int CTA_IS_PARALLEL;

/** Rank in world of all processes Note this variable is only
 * used for debugging and output. */
CTAEXPORT extern int CTA_PAR_MY_RANK;

/** Kind of this process */
CTAEXPORT extern enum CTA_ParProcType CTA_MY_PROC_TYPE;

#ifdef USE_MPI
/** The whole communication universe              */
CTAEXPORT extern MPI_Comm CTA_COMM_WORLD;

/** My own communication group                    */
CTAEXPORT extern MPI_Comm CTA_COMM_MYWORLD;

/** Group consisting of master process and all worker processes the master
 *  communicates with */
CTAEXPORT extern MPI_Comm CTA_COMM_MASTER_WORKER;        

#endif

/** \brief Initialises parallel environment for a process that spawned
 *  The executable is spawned using MPI_COMM_SPAWN or MPI_COMM_SPAWN_MULTIPLE
 *
 *  It will set up the communication groups and optionally starts the parallel
 *  model builder
 *
 * \param StartPar  I  CTA_TRUE/CTA_FALSE start parallel model builder
 *
 * Note when a worker process is part of a Master-Worker model and it does
 * not implement the COSTA model interface it should not start the parallel model builder
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Par_WorkerSpawn(int StartPar);


/** \brief Initialises parallel environment and create process groups
 *
 * \param parConfig I  configuration input from XML-file
 * \param StartPar  I  CTA_TRUE/CTA_FALSE start parallel model builder
 *
 * Note when a worker process is part of a Master-Worker model and it does
 * not implement the COSTA model interface it should not start the parallel model builder
 *
 * \return error status: CTA_OK if successful
 */
CTAEXPORT int CTA_Par_CreateGroups(int parConfig, int StartPar);

#ifdef USE_MPI
/** \brief Get the Fortran (integer handles) of the communicators 
 *
 * \param cta_comm_world         O  Fortran communcator CTA_COMM_WORLD
 * \param cta_comm_myworld       O  Fortran communcator CTA_COMM_MYWORLD
 * \param cta_comm_master_worker O  Fortran communcator CTA_COMM_MASTER_WORKER
 *
 */
CTANOF90 CTAEXPORT int CTA_Par_CreateNewCreateGetComm(CTA_ModelClass modelCls, MPI_Comm *comm);
#endif

#ifdef USE_MPI
/** \brief Get a communicator for a process group by index 
 *
 * \param indx I  indx of communicator
 * \param comm O  MPI communicator
 * \return CTA_OK when succesfull. The value is CTA_CANNOT_FIND_PROCESS_GROUP is
 *         returned when the communicator cannot be found
 *
 */
CTANOF90 CTAEXPORT int CTA_Par_GetAllCommByIndex(int indx, MPI_Comm *comm);
#endif

/** \brief Get the global number of COSTA process group and index of this process
 *
 * \param itime  O The index of this process in the group
 * \param iGroup O The group number of this process belongs to (1..nGroups)
 *
 * \note If this function is called by the COSTA master process or in a sequential run it will return 0 for iGroup
 *
 */
CTAEXPORT void CTA_Par_GetGroupInfo(int *iGroup, int *itime);



#ifdef __cplusplus
}
#endif
#endif


