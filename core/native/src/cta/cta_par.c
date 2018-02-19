/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_par.c $
$Revision: 4073 $, $Date: 2013-07-31 15:43:55 +0200 (Wed, 31 Jul 2013) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2008  Nils van Velzen

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

#include "cta_mem.h"
#include "cta.h"
#include <string.h>
#ifdef USE_MPI
//#include "cta_util_mpi.h"
#include "mpi.h"
#endif
#include "cta_par.h"

#define CLASSNAME "CTA_Par" 

#define CTA_PAR_WORKERSPAWN_F77   F77_CALL(cta_par_workerspawn,CTA_PAR_WORKERSPAWN)
#define CTA_PAR_GETFCOMM_F77      F77_CALL(cta_par_getfcomm,CTA_PAR_GETFCOMM)
#define CTA_PAR_CREATEGROUPS_F77  F77_CALL(cta_par_creategroups,CTA_PAR_CREATEGROUPS)
#define CTA_PAR_GETGROUPINFO_F77  F77_CALL(cta_par_getgroupinfo,CTA_PAR_GETGROUPINFO)

#define IDEBUG (0)
enum CTAI_ParType  {WorkerWorker,MasterWorker};
enum CTA_ParProcType CTA_MY_PROC_TYPE=CTA_ParMaster;
int  CTA_IS_PARALLEL=CTA_FALSE;
int  CTA_PAR_MY_RANK=0;
#ifdef USE_MPI

typedef struct {
int spawn_workers;            // spawn worker processes 0 (false) 1 (true)
char *name;                  // Name of group
char *forModel;              // Model (tag) to use this model for
int nProc;                   // Number of processes in instance of group
int nTimes;                  // Number of instances of this group
int iTime;                   // Index in communicators used for providing new groups when models are created
enum CTAI_ParType  parType;  // Type of parallel model
MPI_Comm *CommNoMaster;
MPI_Comm *CommWithMaster;
int nDumProcs;
int *dumProcs;
} CTAI_Group;

/* Public variables */

MPI_Comm CTA_COMM_WORLD=MPI_COMM_WORLD;          // Communication group of all processes in the COSTA universe.
MPI_Comm CTA_COMM_MYWORLD=MPI_COMM_WORLD;        // Private world of this process (excluding COSTA master).
MPI_Comm CTA_COMM_MASTER_WORKER=MPI_COMM_WORLD;  // Communcication group between the master and this worker.
int CTA_FILTER_PROCESS=0;

int myRankInWorld=-1;

int CTAI_iGroup=0;                            // Group number I belong to (not valid for the master)
int CTAI_nGroups=0;                              // Global number of groups

CTAI_Group *CTAI_AllGroups;

/* Local interfaces */
void CTAI_Par_FatalError(int ierr, char* msg);



#undef METHOD
#define METHOD "CTAI_Par_NumGroups"
void CTAI_Par_NumGroups(int parConfig, int *ngroups1, int *ngroups2){
   int ierr;
   CTA_Handle hitem;
   int i, ncount, datatype;
   char *implements, *parallel_type, *spawn_workers, *nproc, *ntimes,  *dumProcs;

   *ngroups1=0;
   *ngroups2=0;

   /* first count the groups in parallel configuration */
   ierr=CTA_Tree_CountHandlesStr(parConfig,"parallel/process_groups/group",
                                 ngroups1);
   if (ierr!=CTA_OK){
      *ngroups1=0;
   }

   /* Second look for all model classes in configuration */
   ierr=CTA_Tree_CountItems (parConfig, &ncount);
   for (i=1;i<=ncount;i++){
      CTA_Tree_GetItem (parConfig, i, &hitem);
      CTA_Handle_GetDatatype(hitem, &datatype);
      /* check whether it is a model class */
      if (datatype==CTA_MODELCLASS){
         /* Get paralel information */
         CTAI_ModelFac_GetParallelData(hitem, &implements, &parallel_type, &spawn_workers, &nproc, &ntimes, &dumProcs);
         /* increase number of parallel groups when parallel information is available */
         if (implements && parallel_type && nproc && ntimes) (*ngroups2)++;
      }
   }
}

#undef METHOD
#define METHOD "CTAI_Par_GetGroupConfig2"
void CTAI_Par_GetGroupConfig2(CTA_Tree parConfig, int iGroup, CTA_ModelClass *groupConfig){
   int icount,i;
   int ncount;
   int datatype;
   CTA_Handle hitem;
   char *implements, *parallel_type, *spawn_workers, *nproc, *ntimes, *dumProcs;

   icount=0;
   *groupConfig=CTA_NULL;


   /* Second look for all model clases in configuration */
   CTA_Tree_CountItems (parConfig, &ncount);
   for (i=1;i<=ncount;i++){
      CTA_Tree_GetItem (parConfig, i, &hitem);
      CTA_Handle_GetDatatype(hitem, &datatype);
      /* check whether it is a model class */
      if (datatype==CTA_MODELCLASS){
         /* Get paralel information */
         CTAI_ModelFac_GetParallelData(hitem, &implements, &parallel_type, &spawn_workers, &nproc, &ntimes, &dumProcs);
         /* increase number of parallel groups when parallel information is available */
         if (implements && parallel_type && nproc && ntimes) {
            if (iGroup==icount) {
               *groupConfig=hitem;
               return;
            }
            icount++;
         }
      }
   }
}



#undef METHOD
#define METHOD "CTAI_Par_GetGroupConfig"
void CTAI_Par_GetGroupConfig(CTA_Tree parConfig, int iGroup, CTA_Tree *groupConfig){
   int ierr;
   CTA_Tree tGroups;

   ierr=CTA_Tree_GetHandleStr(parConfig,"parallel/process_groups", &tGroups);
   if (ierr==CTA_OK) {
      ierr=CTA_Tree_GetItem(tGroups, iGroup+1, groupConfig);
   }

   if (ierr!=CTA_OK){
      *groupConfig=CTA_NULL;
   }
}

/* Distribute the groups over the available processes */
/* We set field nTimes for groups with free multiplicity  */
#undef METHOD
#define METHOD "CTAI_Par_SetNtimes"
void CTAI_Par_SetNtimes(CTAI_Group *allGroups, int nGroups, int nProcWorld){
   int nodes_left; /* Number of processes that are left */
   BOOL didAddGroup; /* Assigned group to nodes in last cycle */
   BOOL first;       /* Flag indicating first time in loop distributing processes to groups */
   int iLoop;        /* Assigment loop counter */
   int nProcGroup;   /* Number of processes in a group */
   int nTimes;       /* multiplicity of group <=0 yet unknown number */
   int iGroup;       /* Counter over all groups */
   int myRank;
   int mySize;
   char msg[256]; /* String for writing messages */

   nodes_left=nProcWorld-1;
   didAddGroup=TRUE;
   
   MPI_Comm_rank(CTA_COMM_WORLD, &myRank);
   MPI_Comm_size(CTA_COMM_WORLD, &mySize);
   if (IDEBUG) {
      printf("#%d DEBUG: CTAI_Par_SetNtimes Size of CTA_COMM_WORLD =%d:\n",myRank, mySize);
      MPI_Comm_size(MPI_COMM_WORLD, &mySize);
      printf("#%d DEBUG: CTAI_Par_SetNtimes Size of MPI_COMM_WORLD =%d:\n",myRank, mySize);
      printf("#%d DEBUG: CTAI_Par_SetNtimes nProcWorld=%d:\n",myRank, nProcWorld);
      printf("#%d DEBUG: CTAI_Par_SetNtimes nGroups=%d:\n",nGroups, nProcWorld);
      for (iGroup=0; iGroup<nGroups; iGroup++){
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].spawn_workers=%d\n",myRank, iGroup, allGroups[iGroup].spawn_workers);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].name         =%s\n",myRank, iGroup,  allGroups[iGroup].name );
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].forModel     =%s\n",myRank, iGroup,  allGroups[iGroup].forModel);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].nProc        =%d\n",myRank, iGroup,  allGroups[iGroup].nProc);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].nTimes       =%d\n",myRank, iGroup,  allGroups[iGroup].nTimes);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].iTime        =%d\n",myRank, iGroup,  allGroups[iGroup].iTime);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].nDumProcs    =%d\n",myRank, iGroup,  allGroups[iGroup].nDumProcs);
         printf("#%d DEBUG: CTAI_Par_SetNtimes allGroups[%d].dumProcs     =%p\n",myRank, iGroup,  allGroups[iGroup].dumProcs );
      }
   }

   /* iLoop assign all processes fource the first pass*/
   first=TRUE;
   for (iLoop=0;((didAddGroup & nodes_left) >0) || first;iLoop++){
      first=FALSE;
      didAddGroup=FALSE;
      /* Loop over all groups */
      for (iGroup=0;iGroup<nGroups;iGroup++){
         nProcGroup=allGroups[iGroup].nProc;
         nTimes=allGroups[iGroup].nTimes;
         if (allGroups[iGroup].spawn_workers){
           /* When workers are spawned:
              1) do not count the processes (since they do not exist yet)
              2) nTimes must be set since we cannot determine it
            */
            if (nTimes<1){
               sprintf(msg,"Group %d has option 'spawn_workers=Yes' but option nTimes is not set\n",
                       iGroup);
               CTA_WRITE_ERROR(msg);
               exit(-1);
            }
            didAddGroup=TRUE;
         } 
         else {
            if (nTimes>0) {
               if (iLoop==0) {
                  nodes_left=nodes_left-nProcGroup*nTimes;
                  didAddGroup=TRUE;
               }
            }
            else {
               if ((nodes_left>=nProcGroup) | (iLoop==0)){
                  nodes_left=nodes_left-nProcGroup;
                  allGroups[iGroup].nTimes=nTimes-1;
                  didAddGroup=TRUE;
               }
            }
         }
      }
   }

   /* Check for more nodes requested than available */
   if (nodes_left<0){
      sprintf(msg,"%d spawned processes too less. Please spawn more processes or cange parallel configuration\n",
              nProcWorld-nodes_left);
      CTA_WRITE_ERROR(msg);
      exit(-1);
   }

   /* Processes have been spawned that cannot be assigned
      Probably, the user has made an error so we give an error as well.
      Note: we even cannot continue since we will not be able to terminate
     this process in a correct way at the end of the program
   */
   if (nodes_left>0){
      sprintf(msg,"%d spawned processes cannot be assigned please spawn less processes or change parallel configuration.\n",
              nodes_left);
      CTA_WRITE_ERROR(msg);
      exit(-1);
   }

   /* set nTimes field positive for all groups */
   if (IDEBUG) printf("#%d CTAI_Par_SetNtimes nGroups is %d\n",myRank,nGroups);
   for (iGroup=0;iGroup<nGroups;iGroup++){
      nTimes=allGroups[iGroup].nTimes;
      if (nTimes<0) {allGroups[iGroup].nTimes=-nTimes;}
      if (nTimes==0) {
         sprintf(msg,"There are not enough processes to hold a single instance of group %d.\n", iGroup);
         CTA_WRITE_ERROR(msg);
      }
      if (IDEBUG) printf("#%d CTAI_Par_SetNtimes allGroups[%d].nTimesnGroups is %d\n",myRank,iGroup,allGroups[iGroup].nTimes);
   }
}

#undef METHOD
#define METHOD "CTAI_Par_CreateMPIGroups"
void CTAI_Par_CreateMPIGroups(CTAI_Group *allGroups, int nGroups){
   MPI_Group group_world;
   MPI_Comm  subCommNoMaster, subCommWithMaster;
   MPI_Group subGroupNoMaster, subGroupWithMaster;
   int itime;  /* Counter over multiplicity of group */
   int nProc;  /* Number of processes in one instance of the group */
   int iProc;  /* Loop counter over processes in group */
   int *ranks; /* list of ranks in group */
   int nTimes;
   int myRank;
   int iOff;   /* offset deleting processes from list */

   int nProcWorld, nWork;
   int iGroup;
   int proc_free;
   BOOL isMyGroup;

   /* Get number of processes */
   MPI_Comm_size(CTA_COMM_WORLD, &nProcWorld);
   MPI_Comm_rank(CTA_COMM_WORLD, &myRank);
   nWork=nProcWorld-1;

   if (CTA_FILTER_PROCESS) {
      if (IDEBUG) printf("#%d CTAI_Par_CreateMPIGroups: nProcWorld=%d\n",myRank,nProcWorld);
      if (IDEBUG) printf("#%d CTAI_Par_CreateMPIGroups: nWork=%d\n",myRank, nWork);
   }

   /* Check whether we have enough processes and set nTimes fields */
   CTAI_Par_SetNtimes(allGroups, nGroups, nProcWorld);

   /* Make process groups and communicators */
   MPI_Comm_group(CTA_COMM_WORLD,&group_world);
   ranks=CTA_Malloc(sizeof(int)*nProcWorld);
   ranks[0]=0; /* Set master process */

   proc_free=1;
   for (iGroup=0;iGroup<nGroups;iGroup++){
      isMyGroup=FALSE;
      nTimes=allGroups[iGroup].nTimes;
      if (CTA_FILTER_PROCESS) {
         if (IDEBUG) printf("#%d iGroup=%d nTimes=%d\n",myRank,iGroup+1, nTimes);
      }
      /* Allocate fields for storing communicators and init to MPI_COMM_NULL */
      allGroups[iGroup].CommNoMaster  =CTA_Malloc(sizeof(MPI_Comm)*nTimes);
      allGroups[iGroup].CommWithMaster=CTA_Malloc(sizeof(MPI_Comm)*nTimes);
      for (itime=0;itime<nTimes;itime++){
         allGroups[iGroup].CommNoMaster[itime]  =MPI_COMM_NULL;
         allGroups[iGroup].CommWithMaster[itime]=MPI_COMM_NULL;
      }
      if (! allGroups[iGroup].spawn_workers){
         /* For the number of times we have assigned this group */
         for (itime=0;itime<nTimes;itime++) {
            isMyGroup=FALSE;
            if (CTA_FILTER_PROCESS) {
               if (IDEBUG) printf("CTAI_Par_CreateMPIGroups Create Group[%d,%d]\n",iGroup+1,
                       itime);
            }
            nProc=allGroups[iGroup].nProc;
            if (CTA_FILTER_PROCESS) {
               if (IDEBUG) printf("CTAI_Par_CreateMPIGroups nProc for Group %d =%d\n",iGroup+1, nProc);
            }
            /* Set array of ranks of processes */
            for (iProc=0;iProc<nProc;iProc++){
                ranks[iProc+1]=proc_free+iProc;
                /* Check if this process is in group */
                if (ranks[iProc+1]==myRankInWorld){
                   isMyGroup=TRUE;
                }
            }
            if (isMyGroup) {
               if (IDEBUG) printf("#%d I'm a member of group[%d,%d]\n",myRank, iGroup+1, itime);
               CTAI_iGroup=iGroup+1;
               allGroups[iGroup].iTime=itime;
            } else {
               if (IDEBUG) printf("#%d I'm not a member of group[%d,%d]\n",myRank, iGroup+1, itime);
            }

            proc_free=proc_free+nProc;

            if (CTA_FILTER_PROCESS) {
               if (IDEBUG) {
                  printf("CTAI_Par_CreateMPIGroups processes in group [%d,%d] are:\n",iGroup+1, itime);
                  for (iProc=0;iProc<nProc-1;iProc++){
                     printf("%d, ",ranks[iProc+1]);
                  }
                  printf("%d\n",ranks[nProc]);
               }
            }

            /* Create group without master */
            MPI_Group_incl(group_world,nProc,ranks+1,&subGroupNoMaster);

            /* Create group with master */
            if (allGroups[iGroup].parType==WorkerWorker) {
               if (CTA_FILTER_PROCESS) {
                  if (IDEBUG) printf("CTAI_Par_CreateMPIGroups Group %d is of WorkerWorker type:\n",iGroup+1);
               }
               if (allGroups[iGroup].nDumProcs>0) {
                  /* This is the complicated case were there are a number of dummy processes
                   * that do not communicate with the COSTA master */
                  iOff=0;
                  for (iProc=0;iProc<nProc;iProc++){
                     if (iOff<allGroups[iGroup].nDumProcs &&  iProc==allGroups[iGroup].dumProcs[iOff]){
                        iOff++;
                     }
                     else  {
                        ranks[iProc+1-iOff]=ranks[iProc+1];
                     }
                  }
                  /* Decrease number of processes */
                  nProc=nProc-allGroups[iGroup].nDumProcs;
               }
               if (CTA_FILTER_PROCESS) {
                  if (IDEBUG) {
                     printf("CTAI_Par_CreateMPIGroups processes in group min dum [%d,%d] are:\n",iGroup+1, itime);
                     for (iProc=0;iProc<nProc-1;iProc++){
                        printf("%d, ",ranks[iProc+1]);
                     }
                     printf("%d\n",ranks[nProc]);
                  }
               }

               MPI_Group_incl(group_world,nProc+1,ranks,&subGroupWithMaster);
            }
            else  {
               if (CTA_FILTER_PROCESS) {
                  if (IDEBUG) printf("CTAI_Par_CreateMPIGroups Group %d is of MasterWorker type:\n",iGroup+1);
               }
               MPI_Group_incl(group_world,2,ranks,&subGroupWithMaster);
            }

            /* Create communicators */
            MPI_Comm_create(CTA_COMM_WORLD,subGroupNoMaster,
                                                &subCommNoMaster);
            MPI_Comm_create(CTA_COMM_WORLD,subGroupWithMaster,
                                                &subCommWithMaster);
            allGroups[iGroup].CommNoMaster[itime]= subCommNoMaster;
            allGroups[iGroup].CommWithMaster[itime]=subCommWithMaster;

            /* Set global communicators */
            if (isMyGroup) {
               if (IDEBUG) printf("#%d I'm in group [%d,%d]\n",      myRankInWorld, iGroup+1,itime);
               if (IDEBUG) printf("#%d CTA_COMM_MYWORLD       =%p\n",myRankInWorld, subCommNoMaster);
               if (IDEBUG) printf("#%d CTA_COMM_MASTER_WORKER =%p\n",myRankInWorld, subCommWithMaster);

               CTA_COMM_MYWORLD       = subCommNoMaster;
               CTA_COMM_MASTER_WORKER = subCommWithMaster;
            }
         }
      }
   }
   free(ranks);
}

#undef METHOD
#define METHOD "CTAI_Par_SetGroupInfo"
int CTAI_Par_SetGroupInfo(CTA_Handle parConfig, CTAI_Group *group){

   CTA_String sName, sUseFor, sParType, sSpawn_workers, sDumProcs;
   int ierr;
   int myRank;
   int len;
   int datatype;
   char *implements, *parallel_type, *nproc, *ntimes, *dumProcs, *spawn_workers;
   int i;
   char *txtpart, *txtstr, *s;

   /* Get rank for output */
   ierr=MPI_Comm_rank(CTA_COMM_WORLD, &myRank);


   CTA_Handle_GetDatatype(parConfig, &datatype);
   /* check whether it is a model class */

   // Set dummy processes
   group->nDumProcs=0;
   group->dumProcs=NULL;


   if (datatype==CTA_MODELCLASS){
      /* get information from the modelcls */
      CTAI_ModelFac_GetParallelData(parConfig, &implements, &parallel_type, &spawn_workers, &nproc,  &ntimes, &dumProcs);

      // NAME (using implements)
      group->name=CTA_Malloc((strlen(implements)+1)*sizeof(char));
      strcpy(group->name,implements);

      // NPROC
      group->nProc = atoi(nproc);

      // SPAWN WORKERS
      group->spawn_workers = (spawn_workers && (spawn_workers[1]=='Y' || spawn_workers[1]=='y'));

      //FORMODEL (implements)
      group->forModel=CTA_Malloc((strlen(implements)+1)*sizeof(char));
      strcpy(group->forModel,implements);

      // PARALLEL_TYPE
      if (strcmp(parallel_type,"Worker-Worker")==0){
         group->parType=WorkerWorker;
      } else if (strcmp(parallel_type,"Master-Worker")==0){
         group->parType=MasterWorker;
      } else {
         CTAI_Par_FatalError(CTA_ILLEGAL_INPUT_ARGUMENT,
            "parallel_type must have value 'Worker-Worker' or  'Master-Worker'");
      }
      // NTIMES
      group->nTimes = atoi(ntimes);

      // ITIMES
      group->iTime=0;



   } else {

      // NAME
      ierr=CTA_Tree_GetHandleStr(parConfig,"group/name", &sName);
      CTAI_Par_FatalError(ierr, "cannot find  group/name in input");
      ierr=CTA_String_GetLength(sName, &len);
      group->name=CTA_Malloc((len+1)*sizeof(char));
      ierr=CTA_String_Get(sName, group->name);

      //NPROC
      ierr=CTA_Tree_GetValueStr(parConfig,"group/nproc", &(group->nProc), CTA_INTEGER);
      CTAI_Par_FatalError(ierr, "cannot find  group/nproc in input");

      //FORMODEL
      ierr=CTA_Tree_GetHandleStr(parConfig,"group/use_for_model", &sUseFor);
      CTAI_Par_FatalError(ierr, "cannot find  group/use_for_model in input");
      CTA_String_GetLength(sUseFor, &len);
      group->forModel=CTA_Malloc((len+1)*sizeof(char));
      CTA_String_Get(sUseFor, group->forModel);

      // PARALLEL_TYPE
      ierr=CTA_Tree_GetHandleStr(parConfig,"group/parallel_type", &sParType);
      CTAI_Par_FatalError(ierr, "cannot find  group/parallel_type in input");
      if (strcmp(CTAI_String_GetPtr(sParType),"Worker-Worker")==0){
         group->parType=WorkerWorker;
      } else if (strcmp(CTAI_String_GetPtr(sParType),"Master-Worker")==0){
         group->parType=MasterWorker;
      } else {
         CTAI_Par_FatalError(CTA_ILLEGAL_INPUT_ARGUMENT,
            "group/parallel_type must have value 'Worker-Worker' or  'Master-Worker'");
      }

      // SPAWN_WORKER
      group->spawn_workers=FALSE;
      ierr=CTA_Tree_GetHandleStr(parConfig,"group/spawn_workers", &sSpawn_workers);
      if (ierr==CTA_OK){
         spawn_workers=CTAI_String_GetPtr(sSpawn_workers);
         group->spawn_workers=(spawn_workers[0]=='Y' || spawn_workers[0]=='y');
      }

      // NTIMES
      ierr=CTA_Tree_GetValueStr(parConfig,"group/ntimes", &(group->nTimes), CTA_INTEGER);
      if (ierr!=CTA_OK){
         group->nTimes=0;
      }

      // ITIMES
      group->iTime=0;


      // Set dummy processes (only in case of Worker-Worker processes
      if ( group->parType==WorkerWorker){
         // Set message writer in quiet mode since it is ok if this info
         // does not exist
         CTA_Message_Quiet(CTA_TRUE);
         ierr=CTA_Tree_GetHandleStr(parConfig,"group/dumproc", &sDumProcs);
         CTA_Message_Quiet(CTA_FALSE);
         if (ierr==CTA_OK){
             txtstr=CTAI_String_GetPtr(sDumProcs);
             s = txtstr;
             for (i=0; s[i]; s[i]==',' ? i++ : *s++);
             group->nDumProcs = strlen(txtstr) == 0 ? 0 :  i+1;
             if (group->nDumProcs > 0) {
                group->dumProcs  = CTA_Malloc(sizeof(int)*group->nDumProcs);
                for (i=0; ; i++,txtstr=NULL) {
                   txtpart = strtok(txtstr,",");
                   if (txtpart == NULL) break;
                   group->dumProcs[i]=atoi(txtpart);
                }
             }
             if (IDEBUG) printf("#%d Dummy process is selected in Worker-Worker concept\n", myRank);
         }
      }
   }
   if (CTA_FILTER_PROCESS) {
      printf("----- OpenDA communication/model group information ------\n");
      printf("Name of group           =%s\n",group->name);
      printf("number of proccesses    =%d\n",group->nProc);
      printf("Used for model          =%s\n",group->forModel);
      printf("Number of replications  =%d\n",group->nTimes);
      printf("Kind of parallelization =");
      if (group->parType==WorkerWorker){
         printf("WorkerWorker\n");
      } else {
         printf("MasterWorker\n");
      }
      printf("Spawn workers           =");
      if (group->spawn_workers){
         printf("Yes\n");
      } else {
         printf("No\n");
      }
   }
   return CTA_OK;
}




#undef METHOD
#define METHOD "CTAI_Par_FatalError"
void CTAI_Par_FatalError(int ierr, char* msg){
   if (ierr!=CTA_OK){
      printf("FATAL ERROR:\n");
      printf("%s\n", msg);
      exit(-1);
   }
}

#endif

#undef METHOD
#define METHOD "CTA_Par_WorkerSpawn"
int CTA_Par_WorkerSpawn(int StartPar){
#ifdef USE_MPI
   int retval;      /** return value of MPI-call */
   int comm_size;   /** Size of communcation group/number of spawned processes */

   if (IDEBUG) printf("Debug: start of %s\n",METHOD);
   /* initialise MPI. */
   retval=MPI_Init(NULL,NULL);
   retval=MPI_Comm_size(CTA_COMM_WORLD, &comm_size);
   retval=MPI_Comm_rank(CTA_COMM_WORLD, &myRankInWorld);

   if (myRankInWorld==0) printf("%s Number of processes that are spawned :%d\n",METHOD, comm_size);

   /* Set rank of this process, note: this is not in the whole world! */
   CTA_PAR_MY_RANK=myRankInWorld;

   /* By definition: we are running parallel */
   CTA_IS_PARALLEL=TRUE;

   /* By definition: this process is a worker process */
   CTA_FILTER_PROCESS=FALSE;
   CTA_MY_PROC_TYPE=CTA_ParWorker;
   
   /* Set communicators */
   CTA_COMM_MYWORLD       = MPI_COMM_WORLD;
   retval=MPI_Comm_get_parent(&CTA_COMM_MASTER_WORKER);
   if (retval){
      printf("Error in MPI_Comm_get_parent\n");
      exit(-1);
   }

   /* Do we need to start the modelbuilder */
   if (StartPar==CTA_TRUE) {
      printf("=================================================\n");
      printf("Starting the modelbuilder\n");
      printf("Inter communicator is %p\n",CTA_COMM_MASTER_WORKER);
      printf("=================================================\n");


      CTA_Modbuild_par_CreateClass(&CTA_MODBUILD_PAR);
   }
#endif
   return CTA_OK;

   }


#undef METHOD
#define METHOD "CTA_Par_CreateGroups"
int CTA_Par_CreateGroups(int parConfig, int StartPar){

#ifdef USE_MPI

   int comm_size;
   int iGroup;
   CTA_Tree groupConfig;
   int nGroups1, nGroups2;

   /* initialise MPI. We now assume that COSTA is the whole world.
    * This might change in the future */
   MPI_Init(NULL,NULL);
   MPI_Comm_size(CTA_COMM_WORLD, &comm_size);

   /* When this function is called we will always run in parallel */
   CTA_IS_PARALLEL=CTA_TRUE;

   if (myRankInWorld==0) printf("CTA_Par_CreateGroups: Size of CTA_COMM_WORLD is %d\n",comm_size);

   /* Get the rank of this process */
   MPI_Comm_rank(CTA_COMM_WORLD, &myRankInWorld);
   CTA_PAR_MY_RANK=myRankInWorld;

   CTA_FILTER_PROCESS=(myRankInWorld==0);

   /* Set master and worker */
   if (CTA_FILTER_PROCESS) {
      CTA_MY_PROC_TYPE=CTA_ParMaster;
   } else {
      CTA_MY_PROC_TYPE=CTA_ParWorker;
   }

   /* Read configuration */
   if (CTA_FILTER_PROCESS) printf("CTA_Par_CreateGroups: number of processes is %d\n",comm_size);

   /* Count number of process groups */
   CTAI_Par_NumGroups(parConfig, &nGroups1, &nGroups2);
   CTAI_nGroups=nGroups1+nGroups2;
   if (CTA_FILTER_PROCESS) printf("CTA_Par_CreateGroups: number groups defined globally  =%d\n",nGroups1);
   if (CTA_FILTER_PROCESS) printf("CTA_Par_CreateGroups: number groups defined locally   =%d\n",nGroups2);
   if (CTA_FILTER_PROCESS) printf("CTA_Par_CreateGroups: total number of defined ngroups =%d\n",CTAI_nGroups);

   /* Get properties of the groups and store them in CTAI_AllGroups */
   CTAI_AllGroups=NULL;
   if (CTAI_nGroups>0) {
      CTAI_AllGroups=CTA_Malloc(CTAI_nGroups*sizeof(CTAI_Group));

      for (iGroup=0;iGroup<nGroups1; iGroup++){
         CTAI_Par_GetGroupConfig(parConfig, iGroup, &groupConfig);
         CTAI_Par_SetGroupInfo(groupConfig, &(CTAI_AllGroups[iGroup]));
      }

      for (iGroup=0;iGroup<nGroups2; iGroup++){
         CTAI_Par_GetGroupConfig2(parConfig, iGroup, &groupConfig);
         CTAI_Par_SetGroupInfo(groupConfig, &(CTAI_AllGroups[nGroups1+iGroup]));
      }
   }
   /* Create process groups and communicators */
   CTAI_Par_CreateMPIGroups(CTAI_AllGroups, CTAI_nGroups);

   /* Start parallel model builder for workers */
   /* OR start worker for Master-Worker processes */

   if (StartPar==CTA_TRUE) {
      CTA_Modbuild_par_CreateClass(&CTA_MODBUILD_PAR);
   }
#endif
   return CTA_OK;

   }


#ifdef USE_MPI
#undef METHOD
#define METHOD "CTA_Par_GetAllCommByIndex"
int CTA_Par_GetAllCommByIndex(int indx, MPI_Comm *comm){
   int iGroup; /* Counter over process groups */
   int jndx;   /* Total counter over communicators */
   int nTimes; /* Number of times an process group exists */
   int iTime;  /* Counter over NTimes */

   *comm=MPI_COMM_NULL;
   if (IDEBUG) printf("CTA_Par_GetAllCommByIndex requesting comm with index %d\n",indx);

   jndx=-1;
   if (IDEBUG) printf("Number of groups is %d\n",CTAI_nGroups);
   for (iGroup=0;iGroup<CTAI_nGroups;iGroup++){
      if (IDEBUG) printf("CTA_Par_GetAllCommByIndex iGroup=%d\n",iGroup);
      nTimes=CTAI_AllGroups[iGroup].nTimes;
      if (IDEBUG) printf("CTA_Par_GetAllCommByIndex nTimes=%d\n",nTimes);
      for (iTime=0;iTime<nTimes;iTime++){
         jndx++;
         if (jndx==indx) {
            *comm=CTAI_AllGroups[iGroup].CommWithMaster[iTime];
            return CTA_OK;
         }
      }
   }
   return CTA_CANNOT_FIND_PROCESS_GROUP;

}
#endif



#ifdef USE_MPI
#undef METHOD
#define METHOD "CTA_Par_CreateNewCreateGetComm"
int CTA_Par_CreateNewCreateGetComm(CTA_ModelClass modelCls, MPI_Comm *comm){

int iGroup;  /* Counter over all groups */
int iTime;   /* Loop counter over number of instances of the group */
int nTimes;  /* Number of instances of the group */

char *command; /* Startup command of processes */
int *errCodes; /* Startup error codes for MPI_Comm_spawn */
char **argv;   /* Startup arguments of processes in MPI_Comm_spawn */
int nProc;     /* Number of processes that must be spawned */
MPI_Comm intercomm; /* inter communicator between spawning process and worker processes */



const char *implements;

  *comm=MPI_COMM_NULL;
  if (IDEBUG) printf("CTA_Par_CreateNewCreateGetComm: CTAI_nGroups=%d\n",CTAI_nGroups );

  /* What model is implemented by this modelclass? */
  implements=CTAI_ModelFac_GetImplements(modelCls);
  if (IDEBUG) printf("CTA_Par_CreateNewCreateGetComm: implements=%s\n",implements);

  /* Search for the corresponding process group */
  for (iGroup=0;iGroup<CTAI_nGroups;iGroup++){

     if (IDEBUG) {
        printf("Exisiting group is:%s\n",CTAI_AllGroups[iGroup].forModel);
        printf("We are looking for:%s\n",implements);
     }
     if (strcmp(implements,CTAI_AllGroups[iGroup].forModel)==0){
        /* return communicator */
        iTime =CTAI_AllGroups[iGroup].iTime;
        nTimes=CTAI_AllGroups[iGroup].nTimes;
        /* spawn worker processes when needed */
        if (CTAI_AllGroups[iGroup].spawn_workers){
           if(CTAI_AllGroups[iGroup].CommWithMaster[iTime]==MPI_COMM_NULL){


              command="pollute2d_ww";
              argv=CTA_Malloc(sizeof(char*));
              argv[0]=NULL;
              nProc=CTAI_AllGroups[iGroup].nProc;
              errCodes=CTA_Malloc(sizeof(int)*nProc);
              if (IDEBUG) {
                 printf("Spawning processes\n");
                 printf("command            :%s\n",command);
                 printf("Number of processes:%d\n",nProc);
              }

              MPI_Comm_spawn(command,argv,nProc,MPI_INFO_NULL,
                             0,CTA_COMM_MYWORLD, 
                             &intercomm,
                             errCodes);
              free(errCodes);
              free(argv);
              CTAI_AllGroups[iGroup].CommWithMaster[iTime]=intercomm;
           }
        }
        *comm=CTAI_AllGroups[iGroup].CommWithMaster[iTime];
        CTAI_AllGroups[iGroup].iTime=(iTime+1)%nTimes;
        break;
     }
  }
  if (iGroup==CTAI_nGroups) {
     return CTA_CANNOT_FIND_PROCESS_GROUP;
  }
  else {
     return CTA_OK;
  }
}



#undef METHOD
#define METHOD "CTA_Par_GetGroupInfo"
void CTA_Par_GetGroupInfo(int *myGroup, int *itime){
*myGroup = CTAI_iGroup;
*itime =   CTAI_AllGroups[CTAI_iGroup-1].iTime + 1;
// printf("**************** cta_par_getgroupinfo: %d %d \n",CTAI_iGroup, *itime);
}





#undef METHOD
#define METHOD "CTA_Par_GetFComm"
void CTA_Par_GetFComm(int *cta_comm_world, int *cta_comm_myworld, int *cta_comm_master_worker){
   int isInit;
   MPI_Initialized(&isInit);
   if (isInit){
      *cta_comm_world          = MPI_Comm_c2f(CTA_COMM_WORLD);
      *cta_comm_myworld        = MPI_Comm_c2f(CTA_COMM_MYWORLD);
      *cta_comm_master_worker  = MPI_Comm_c2f(CTA_COMM_MASTER_WORKER);
   }
   else {
      *cta_comm_world          = 0;
      *cta_comm_myworld        = 0;
      *cta_comm_master_worker  = 0;      
   }
}


#else

#undef METHOD
#define METHOD "CTA_Par_GetFComm"
void CTA_Par_GetFComm(int *cta_comm_world, int *cta_comm_myworld, int *cta_comm_master_worker){

*cta_comm_world          = -911;
*cta_comm_myworld        = -911;
*cta_comm_master_worker  = -911;

}
#endif

CTAEXPORT void CTA_PAR_WORKERSPAWN_F77 (int *StartPar, int *ierr) {
   *ierr=CTA_Par_WorkerSpawn(*StartPar);
}

CTAEXPORT void CTA_PAR_GETFCOMM_F77(int *cta_comm_world, int *cta_comm_myworld, int *cta_comm_master_worker){
   CTA_Par_GetFComm(cta_comm_world, cta_comm_myworld, cta_comm_master_worker);
}

CTAEXPORT void CTA_PAR_CREATEGROUPS_F77 (int *parConfig, int *StartPar, int *ierr) {
   *ierr=CTA_Par_CreateGroups(*parConfig, *StartPar);
}


#if USE_MPI
CTAEXPORT void CTA_PAR_GETGROUPINFO_F77 (int *myGroup, int *itime){
   CTA_Par_GetGroupInfo(myGroup, itime);
}
#endif
