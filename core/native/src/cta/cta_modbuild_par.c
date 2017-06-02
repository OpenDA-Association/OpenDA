/*
$URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_modbuild_par.c $
$Revision: 4056 $, $Date: 2013-07-03 14:19:55 +0200 (Wed, 03 Jul 2013) $

COSTA: Problem solving environment for data assimilation
Copyright (C) 2006  Nils van Velzen

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
#include "f_cta_utils.h"
#include "cta_model_utilities.h"

#ifdef USE_MPI
#include "mpi.h"
#include "cta_par.h"

#define IDEBUG (0)
#define EXITONERROR (1)
#define ALL_WORKERS (-1)
#define MASTER (0)
#define NOOSOBS (1)

#define CTA_PACK_DEFAULT_SIZE (1000000)


double MISVAL=-1E20;

enum TypeState {STATE, FORC, PARAM, SCAL};


typedef struct {
CTA_Model mthis;        /* Handles of this model (local handle)         */
int       *mremote;     /* Remote handle numbers of the model for each process 1..nworker */
MPI_Comm  comm;         /* Communicator to use for this model instance  */
int       is_intercomm; /* Flag indicating whether comm is an intercommunication group */
int       nworker;      /* Number of workers to adress to               */
CTA_String *SelectObs;  /* Selection for observations for all models    */
} CTAI_Modbuild_par;


int ctai_rank_ind   = 1;  /* Rank of new worker to be created */
int ctai_rank       =-1;  /* My rank */
int ctai_rank_master= 0;  /* rank of master */
int ctai_comm_size  =-1;  /* Total number of workers          */
int hmodel_scratch  =CTA_NULL; /* handle of extra model instance on worker */

CTA_ObsDescr last_hdescr=CTA_NULL;
CTA_ObsDescr last_hdescr_tab=CTA_NULL;

CTA_Model *createdModels   =NULL;
int        numCreatedModels=0;

void modbuild_models_add(CTA_Model hmodel){
   int iModel;
   CTA_Model *newList;

   printf("Adding model %d\n",hmodel);
   numCreatedModels++;
   newList= (CTA_Model*) CTA_Malloc(numCreatedModels*sizeof(CTA_Model));
   for (iModel=0; iModel<numCreatedModels-1; iModel++){
      newList[iModel]=createdModels[iModel];
   }
   newList[numCreatedModels-1]=hmodel;
   if (createdModels) free(createdModels);
   createdModels=newList;
}

void modbuild_models_delete(CTA_Model hmodel){
   int iPlek=-1;
   int iModel;

   for (iModel=0; iModel<numCreatedModels; iModel++){
      if (createdModels[iModel]==hmodel) iPlek=iModel;
   }
   if (iPlek>=0){
      numCreatedModels--;
      if (numCreatedModels>0){
        CTA_Model *newList= (CTA_Model*) CTA_Malloc(numCreatedModels*sizeof(CTA_Model));
        for (iModel=0;iModel<iPlek; iModel++){newList[iModel]=createdModels[iModel];}
        for (iModel=iPlek; iModel<numCreatedModels; iModel++){newList[iModel]=createdModels[iModel+1];}
        free(createdModels);
        createdModels=newList;
      } 
      else {
        free(createdModels);
        createdModels=NULL;
      }
   }
}

void modbuild_models_delete_all_models(){
   int iModel;

   for (iModel=0; iModel<numCreatedModels; iModel++){
      CTA_Model_Free(&(createdModels[iModel]));
   }
}



void modbuild_par_sendtaskandhandle(char *task, int rank, CTAI_Modbuild_par *data,
                                    int tag){
  int lentask;
  int irank;
  int jrank;

  if (IDEBUG>0){
     printf("modbuild_par_sendtaskandhandle\n");
     printf("task=%s rank=%d, \n",task,rank);
     printf("nworker=%d\n",data->nworker);
  }


  /* Loop over all worker processes of this model */
  for (irank=1;irank<=data->nworker;irank++){
     if (irank==rank || rank<0) {
        if (data->is_intercomm){
           jrank=irank-1;
        }
        else {
           jrank=irank;
        }


        /* Send Task */
        lentask=strlen(task)+1;
        MPI_Send(task,lentask, MPI_CHAR, jrank, tag, data->comm);

        /* Send remote model handle */
        if (IDEBUG>0) printf("# %d modbuild_par_sendtaskandhandle: send task (%s) and handle (%d) to worker %d\n",
                        CTA_PAR_MY_RANK, task, data->mremote[irank],irank);
        MPI_Send((void*) &data->mremote[irank], 1, MPI_INT, jrank, tag, data->comm);
     }
  }
}


/* Send a state-vector */
void modbuild_par_sendstate(CTA_TreeVector state, CTAI_Modbuild_par *data, int tag){
   int lentask;
   CTA_Pack hpack;
   char *pack;
   CTA_TreeVector state_sub;
   MPI_Comm comm;
   int irank,jrank, nworker;

   /* Handle to send to master if data==NULL) */
   if (data) {
      nworker=data->nworker;
      comm=data->comm;
   } else {
      nworker=1;
      comm=CTA_COMM_MASTER_WORKER;
   }


   /* Loop over all worker processes of this model */
   for (irank=1;irank<=nworker;irank++){
      /* Get substate (when necessary) */
      if (nworker>1){
         CTA_TreeVector_GetSubTreeVecIndex (state, irank, &state_sub);
      } else {
         state_sub=state;
      }

      CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);

      CTA_TreeVector_Export(state_sub,hpack);
      lentask= CTA_Pack_GetLen(hpack);
      pack   = CTA_Pack_GetPtr(hpack);

      /* Handle to send to master if data==NULL) */
      if (data){ jrank=irank;} else {jrank=0;}

      if (data && data->is_intercomm) jrank--;


      /* Send length of packed state */
      MPI_Send(&lentask, 1, MPI_INT, jrank, tag, comm);

      /* Send packed state */
      MPI_Send(pack, lentask, MPI_CHAR, jrank, tag, comm);

      /* Free pack-buffer */
      CTA_Pack_Free(&hpack);
   }
}

/* Receive a state-vector */
void modbuild_par_recvstate(CTA_TreeVector *state, CTAI_Modbuild_par *data, int tag){
   MPI_Status status;
   int retval;
   int lenpack;
   CTA_Pack hpack;
   char *pack;
   char sNum[20];
   char rootwrk[30];
   MPI_Comm comm;

   CTA_TreeVector *state_subs, state_sub;
   int nworker, irank, jrank;

   /* Handle to send to master if data==NULL) */
   if (data) {
      nworker=data->nworker;
      comm=data->comm;
   } else {
      nworker=1;
      comm=CTA_COMM_MASTER_WORKER;
   }

   /* Create state including sub-states when needed */
   if (*state==CTA_NULL) {
      CTA_TreeVector_Create("MASTERroot","MASTERroot",state);

      /* Create sub-states as well when we have multiple workers */
      if (nworker>1) {
         state_subs=(CTA_Vector*) CTA_Malloc(sizeof(CTA_TreeVector)*nworker);
 
    
         for (irank=1;irank<=nworker;irank++){

            sprintf(sNum,"%d",irank);
            strcpy(rootwrk,"rtwrk"); 
            strcat(rootwrk,sNum); 
            CTA_TreeVector_Create("root_worker",rootwrk,&(state_subs[irank-1]));
         }
         CTA_TreeVector_Conc (*state, state_subs, nworker);
         free(state_subs);
      }
      //printf("modbuild_par_recstate: state did not exist yet; created \n");
   }

   /* Receive all states from worker */
   for (irank=1;irank<=nworker;irank++){
      if (nworker>1){
         retval=CTA_TreeVector_GetSubTreeVecIndex (*state, irank, &state_sub);
      } else {
         state_sub=*state;
      }

      /* Handle receive from master if data==NULL) */
      if (data){ jrank=irank;} else {jrank=0;}

      if (data && data->is_intercomm) jrank--;

      /* Receive state-vector */
      retval=MPI_Recv(&lenpack, 1, MPI_INT, jrank,1234, comm, &status);
      retval=CTA_Pack_Create(lenpack,&hpack);
      pack  =CTA_Pack_GetPtr(hpack);

      retval=MPI_Recv(pack, lenpack, MPI_CHAR, jrank,1234, comm, &status);
      retval=CTA_Pack_AddCnt(hpack,lenpack);

      /* unpack the received-state */
      retval=CTA_TreeVector_Import(state_sub,hpack);
      if (retval!=CTA_OK) {
         printf("modbuild_par_recvstate: error (%d) in CTA_TreeVector_Import\n",retval);
         exit(-1);
      }

      /* Free pack-buffer */
      retval=CTA_Pack_Free(&hpack);
     
   }
     if (IDEBUG>0) {
        printf("---------------------------------modbuild_par_recstate: whole state DONE \n"); 
        printf("................................ whole state is: \n");
        retval = CTA_TreeVector_Info(*state);
     }
}


void modbuild_par_sendobsdescr(CTAI_Modbuild_par *data, int krank, CTA_Time ttime,
                               CTA_ObsDescr hdescr, int tag, int *ierr){

   double span[2];
   int lentask;
   CTA_Pack hpack;
   char *pack;
   int irank, jrank;


   hpack=CTA_NULL;
   /* handle an empty desciption */
   if (hdescr==CTA_NULL){
         hpack=CTA_NULL;
         lentask= 0;
   }
   else {
      if (IDEBUG>0) printf("modbuild_par_sendobsdescr :Start\n");
      /* First delete an existing obsdescr */
      if (! last_hdescr_tab == CTA_NULL) {
         if (IDEBUG>0) printf("modbuild_par_sendobsdescr: MEMLEAK FIXED\n");
         CTA_ObsDescr_Free(&last_hdescr_tab);
      }
      /* Create a new table obsdescr */
      *ierr=CTA_ObsDescr_Create(CTA_OBSDESCR_TABLE,hdescr, &last_hdescr_tab);
      if (IDEBUG>0) printf("Created table version ierr=%d\n",*ierr);
      if (*ierr!=CTA_OK) return;

      /* Pack hdescr_tab */
      *ierr=CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
      if (*ierr!=CTA_OK) return;
      *ierr=CTA_ObsDescr_Export(last_hdescr_tab, hpack);
      if (*ierr!=CTA_OK) return;

      /* Send hdescr_tab */
      lentask= CTA_Pack_GetLen(hpack);
      pack   = CTA_Pack_GetPtr(hpack);
   }

   /* Send to all workers */
   for (irank=1;irank<=data->nworker;irank++){
      if (irank==krank || krank<0){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;


         /* Send timespan */
         *ierr=CTA_Time_GetSpan(ttime, &span[0], &span[1]);

         if (*ierr!=CTA_OK) return;
         if (IDEBUG>0) {printf("#%d Sending timespan to %d\n",CTA_PAR_MY_RANK,irank);}
         *ierr=MPI_Send((void*) span, 2, MPI_DOUBLE, jrank, tag, data->comm);
         if (*ierr!=CTA_OK) return;
         if (IDEBUG>0) {printf("#%d Sending done\n",CTA_PAR_MY_RANK);}


         if (IDEBUG>0) {printf("#%d Sending lenpack to %d\n",CTA_PAR_MY_RANK,irank);}
         /* Send length of packed obsdescr */
         MPI_Send(&lentask, 1, MPI_INT, jrank, tag,
                         data->comm);
         if (lentask>0){
            if (IDEBUG>0) {printf("#%d Sending packed obsdescr to %d\n",CTA_PAR_MY_RANK,irank);}
            /* Send packed obsdescr */
            MPI_Send(pack, lentask, MPI_CHAR, jrank, tag, data->comm);
         }
       }
   }

   /* Free vars we do not need */
   *ierr=CTA_Pack_Free(&hpack);
   if (*ierr!=CTA_OK) return;

   *ierr=CTA_OK;
   if (IDEBUG>0) printf("modbuild_par_sendobsdescr :end\n");
}




void modbuild_par_recvobsdescr(CTA_Time *timespan, CTA_ObsDescr *hdescr_tab,
                               int tag){

   MPI_Status status;
   CTA_Pack hpack;
   double span[2];
   int lenpack, retval;
   char *pack;

   /* Receive the simulation timespan */
   retval=CTA_Time_Create(timespan);

   if (IDEBUG>0) {printf("#%d Receiving timespan from master\n",CTA_PAR_MY_RANK);}
   retval=MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   retval=CTA_Time_SetSpan(*timespan, span[0], span[1]);

   /* Receive the observation description */
   if (IDEBUG>0) {printf("#%d Receiving lenpack from master\n",CTA_PAR_MY_RANK);}
   retval=MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   if (IDEBUG>0) {printf("#%d lenpack =%d\n",CTA_PAR_MY_RANK, lenpack);}

   if (lenpack==0){
      CTA_ObsDescr_Free(hdescr_tab);
      *hdescr_tab=CTA_NULL;
   }
   else {
      retval=CTA_Pack_Create(lenpack,&hpack);
      pack  =CTA_Pack_GetPtr(hpack);

      if (IDEBUG>0) {printf("#%d Receiving packed obsdescr from master\n",CTA_PAR_MY_RANK);}
      retval=MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                      CTA_COMM_MASTER_WORKER, &status);
      if (IDEBUG>0) {printf("#%d Done Receiving packed obsdescr from master\n",CTA_PAR_MY_RANK); }

      retval=CTA_Pack_AddCnt(hpack,lenpack);

      /* Create the observation description */
      if (! *hdescr_tab == CTA_NULL){
         printf("modbuild_par_sendobsdescr :MEMLEAK (2) FIXED\n");
         CTA_ObsDescr_Free(hdescr_tab);
      }
      retval=CTA_ObsDescr_Create(CTA_OBSDESCR_TABLE,hpack, hdescr_tab);
      if (IDEBUG>0) {printf("#%d Done Creating obsdescr %d\n",CTA_PAR_MY_RANK, retval);}
      retval=CTA_Pack_Free(&hpack);
   }
   if (IDEBUG>0) {printf("#%d End of modbuild_par_recvobsdescr\n",CTA_PAR_MY_RANK);}
}

/* Determine the size of the datablock of one model instance */
void modbuild_par_create_size(CTA_Handle *userdata, int *memsize,
                          int *ierr){
   *memsize=(int) sizeof(CTAI_Modbuild_par);
   *ierr=CTA_OK;
}

/* Create a model-instance */
void modbuild_par_create_init(CTA_Handle *this, CTAI_Modbuild_par *data ,
                              CTA_Handle *hinput, int *ierr){

   MPI_Status status;
   CTA_String sclsnam;
   CTA_ModelClass modcls;
   CTA_Handle tmodel;
   const char *clsnam, *modinp;
   int lennamcls, lenmodel;
   int rank, jrank;
   int tag, lentask;
   int isintercomm;
   char *task;

   int cleanup; /*Flag indicating whether tinput must be freed */
   CTA_Tree tinput;

   /* Convert input into a COSTA-tree if it is not done already */
   *ierr=CTA_Model_Util_InputTree(*hinput, &tinput, &cleanup);
   if (*ierr!=CTA_OK) return;

   /* Handle input :*/

   /* Determine name of model class to worker process */
   *ierr=CTA_Tree_GetHandleStr(tinput,"modelclass", &modcls);
   if (*ierr!=CTA_OK) return;

   /* get name of model class */
   CTA_String_Create(&sclsnam);
   CTA_Handle_GetName(modcls,sclsnam);
   clsnam=CTAI_String_GetPtr(sclsnam);

   /* Get parallel information */
   *ierr=CTA_Par_CreateNewCreateGetComm(modcls, &(data->comm));
   if (*ierr!=CTA_OK){
      printf("FATAL ERROR CTA_Par_CreateNewCreateGetComm -failed \n");
      exit(-1);
   }

   /* Test whether this is an inter communicator */
   /* Test whether this is an inter communicator */
   MPI_Comm_test_inter(data->comm, &(data->is_intercomm));

   /* Determine the number of workers */
   if (data->is_intercomm){
      MPI_Comm_remote_size(data->comm,&(data->nworker));
   }
   else {
      MPI_Comm_size(data->comm,&(data->nworker));
      //printf("DE GROEP BESTAAT UIT %d processes \n", data->nworker);
      data->nworker--;
      //printf("DE GROEP BESTAAT UIT %d workers \n", data->nworker);
   }

   /* Allocate array mremote holding remote handle numbers create one
    * larger in order to avoind 0-indexing */
   data->mremote=CTA_Malloc((data->nworker+1)*sizeof(int));

   /* Determine the model input name of model class to worker process */
   *ierr=CTA_Tree_GetHandleStr(tinput,"model", &tmodel);
   if (*ierr!=CTA_OK) return;

   /* We cannot pack and send whole costa trees.
      We only support a single string */
   *ierr=CTA_String_GetLength(tmodel, &lenmodel);
   if (*ierr==CTA_OK) {
      modinp=CTAI_String_GetPtr(tmodel);
   } else {
      printf("modbuild_par_create_init: input /modelbuild_par/model ");
      printf("must be a string.\n");
      printf("packing and sending of COSTA-tree objects is not yet ");
      printf("supported.\n");
      exit(-1);
   }
      /* Send data to worker (s)*/
      tag=1234;
      task="cta_model_create";
      lentask=strlen(task)+1;


      /* Communicate to all workers Note rank 0 is master! */
      for (rank=1;rank<=data->nworker;rank++){
         if (IDEBUG>0) printf("#%d Creating model instance on worker %d\n",CTA_PAR_MY_RANK, rank);

         jrank=rank;
         if (data && data->is_intercomm) jrank--;

         /* Send task */

         MPI_Comm_test_inter(data->comm,&isintercomm);

         MPI_Send("cta_model_create",lentask, MPI_CHAR, jrank,tag,
                         data->comm);

         /* Send name of model class */
         lennamcls=strlen(clsnam);
         MPI_Send((void*) clsnam, lennamcls+1, MPI_CHAR, jrank,tag,
                         data->comm);
         /* Send name of input */
         lenmodel=strlen(modinp);
         MPI_Send((void*) modinp, lenmodel+1, MPI_CHAR, jrank,tag,
                         data->comm);
      }
      /* Receive local handle if new model-instances */
      for (rank=1;rank<=data->nworker;rank++){

         jrank=rank;
         if (data && data->is_intercomm) jrank--;

         MPI_Recv(&(data->mremote[rank]),1,MPI_INT, jrank, 1234,
                         data->comm,&status);
         if (IDEBUG>0) printf("#%d Done creating model instance on worker\n",CTA_PAR_MY_RANK);
      }
      /* Set rest of data */
      data->mthis=*this;

      /* Set selections */
      data->SelectObs=NULL;

      *ierr=CTA_OK;

   CTA_String_Free(&sclsnam);
   /* Clean-up input tree */
   if (cleanup==CTA_TRUE){
      CTA_Tree_Free(&tinput);
   }
}

int receive_pack_object( int rank, int tag, BOOL add_length, CTA_Pack hpack, MPI_Comm comm)
{
   int retval, lenpack;
   char* buffer;
   MPI_Status status;

   if (IDEBUG>0) printf("receive_pack_object: ervoor \n");

   retval=MPI_Recv(&lenpack,1,MPI_INT,rank,tag,comm,&status);

   if (IDEBUG>0) printf("receive_pack_object: lenpack %d\n", lenpack);

   buffer= (char*) CTA_Malloc(lenpack*sizeof(char));
   retval=MPI_Recv(buffer,lenpack,MPI_CHAR,rank,tag,comm,&status);

   /* Additional add length in pack object */
   if (add_length) {
      retval=CTA_Pack_Add(hpack,&lenpack,sizeof(int));
   }

   /* Add packed model state */
   retval=CTA_Pack_Add(hpack,buffer,lenpack);
   free(buffer);
   return retval;
}

void modbuild_par_export(
  CTAI_Modbuild_par *data,
  CTA_Handle *hpack,    /* export object (e.q. handle to file or pack instance) */
  int *ierr)
{
   int tag;
   int irank, jrank;

   if (IDEBUG>0) printf("modbuild_par_export\n");

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_export", ALL_WORKERS, data, tag);

   for (irank=1;irank<=data->nworker;irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /* Send receiving rank */

      if (IDEBUG>0) printf("modbuild_par_export: send rank of receiving process (%d) to process %d\n",ctai_rank_master, irank);
      MPI_Send(&ctai_rank_master, 1, MPI_INT, jrank,tag, data->comm);
   }

   for (irank=1;irank<=data->nworker;irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;

//      Hmmmmmmm what is the global rank???
      /* Receive the export (currently only supporting COSTA pack object) */
      *ierr=receive_pack_object(jrank,tag, TRUE, *hpack, data->comm);

   }




}

void CTA_Modbuild_par_worker_export(){

   MPI_Status status;
   CTA_Model hmodel;
   int lentask, retval, tag, rank;
   CTA_Pack hpack;
   char *pack;

   if (IDEBUG>0) printf("# %d modbuild_par_worker_export\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,CTA_COMM_MASTER_WORKER,&status);
   if (IDEBUG>0) printf("# %d modbuild_par_worker_export: receiving model handle %d\n",CTA_PAR_MY_RANK, hmodel);
   /* Receive receiving rank */
   retval=MPI_Recv(&rank,   1, MPI_INT, ctai_rank_master,tag,CTA_COMM_MASTER_WORKER,&status);

   if (IDEBUG>0) printf("# %d modbuild_par_worker_export: receiving rank %d\n",CTA_PAR_MY_RANK, rank);

   /* Create local pack object */
   retval  = CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   retval  = CTA_Model_Export(hmodel,hpack);
   if (retval!=CTA_OK) {
      printf("#%d modbuild_par_worker_exort: FATAL ERROR in CTA_Model_Export ierr=%d\n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }

   lentask = CTA_Pack_GetLen(hpack);
   pack    = CTA_Pack_GetPtr(hpack);

   /* Send length of packed state */
   retval=MPI_Send(&lentask, 1, MPI_INT, rank, tag, CTA_COMM_MASTER_WORKER);
   /* Send packed state */
   retval=MPI_Send(pack, lentask, MPI_CHAR, rank, tag, CTA_COMM_MASTER_WORKER);
   /* Free pack-buffer */
   retval=CTA_Pack_Free(&hpack);
}


void modbuild_par_import(
  CTAI_Modbuild_par *data,
  CTA_Handle *himport,    /* import object (e.q. handle to file or pack instance) */
  int *ierr)
{

   int tag, lentask;
   char *pack;
   int irank, jrank, ip1, ip2;


   if (IDEBUG>0) printf("modbuild_par_import\n");

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_import", ALL_WORKERS, data, tag);
   for (irank=1;irank<=data->nworker;irank++){
      
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /* Unpack length of packed data for this worker */
      *ierr=CTA_Pack_Get(*himport, &lentask,  sizeof(int));
      if (*ierr!=CTA_OK) return;

      /* Get pointer to data */
      pack    = CTA_Pack_GetPtr(*himport);

      /* Send length of data to worker */
      *ierr=MPI_Send(&lentask, 1, MPI_INT, jrank,tag,data->comm);
      if (*ierr!=CTA_OK) return;

      /* Send packed state */
      *ierr=MPI_Send(pack, lentask, MPI_CHAR, jrank,tag,data->comm);
      if (*ierr!=CTA_OK) return;

      /* Set pointer in packed data */
      *ierr=CTA_Pack_GetIndx(*himport, &ip1, &ip2);
      if (*ierr!=CTA_OK) return;
      *ierr=CTA_Pack_SetIndx(*himport, ip1+lentask, ip2);
      if (*ierr!=CTA_OK) return;
   }
}

void CTA_Modbuild_par_worker_import()
{
   CTA_Pack himport;    /* import object (e.q. handle to file or pack instance)*/
   CTA_Model hmodel;
   MPI_Status status;
   int retval, tag;

   if (IDEBUG>0) printf("#%d modbuild_par_worker_import\n",CTA_PAR_MY_RANK );

   tag=1234;

   /* Receive the model handle */
   retval = MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,CTA_COMM_MASTER_WORKER,&status);

   retval = CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&himport);
   if (retval!=CTA_OK) {
      printf("#%d modbuild_par_worker_import: FATAL ERROR in COSTA METHOD ierr=%d\n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }

   retval = receive_pack_object(ctai_rank_master,tag,FALSE,himport,CTA_COMM_MASTER_WORKER);
   retval = CTA_Model_Import(hmodel,himport);
   if (retval!=CTA_OK) {
      printf("#%d modbuild_par_worker_import: FATAL ERROR in CTA_Model_Import ierr=%d\n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }
   retval = CTA_Pack_Free(&himport);
}


void modbuild_par_free(CTAI_Modbuild_par *data ,int *ierr){
   int tag;

   tag=1234;

   if (IDEBUG>0) printf("#%d modbuild_par_free\n",CTA_PAR_MY_RANK);

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_free", ALL_WORKERS, data, tag);

   *ierr=CTA_OK;
}

void modbuild_par_compute(
          CTAI_Modbuild_par *data,
          CTA_Time *timespan, /* current time, "now" */
          int *ierr)
{
   double span[2];
   int tag;
   int irank, jrank;

   if (IDEBUG>0) printf("#%d modbuild_par_compute\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_compute", ALL_WORKERS, data, tag);

   /* Send timespan */
   CTA_Time_GetSpan(*timespan, &span[0], &span[1]);
   for (irank=1;irank<=data->nworker;irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      MPI_Send((void*) span, 2, MPI_DOUBLE,jrank,tag,data->comm);
   }
   *ierr=CTA_OK;
}


void modbuild_par_setstate(CTAI_Modbuild_par *data, CTA_TreeVector *state, int *ierr){

   int tag;

   if (IDEBUG>0) printf("#%d modbuild_par_setstate\n",CTA_PAR_MY_RANK);
   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_setstate", ALL_WORKERS, data, tag);

   /* Send the state */
   modbuild_par_sendstate(*state, data, 1234);

   *ierr=CTA_OK;
}

void modbuild_par_getstate(CTAI_Modbuild_par *data, CTA_TreeVector *state, int *ierr){

   int tag;

   if (IDEBUG>0) printf("#%d modbuild_par_getstate\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_getstate", ALL_WORKERS, data, tag);

   /* Receive the state */
   modbuild_par_recvstate(state, data, tag);

   *ierr=CTA_OK;

}

void modbuild_par_getstatescaling(CTAI_Modbuild_par *data, CTA_TreeVector *state, int *ierr){

   int tag;

   if (IDEBUG>0) printf("#%d modbuild_par_getstatescaling\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_getstatescaling", ALL_WORKERS, data, tag);

   /* Receive the state */
   modbuild_par_recvstate(state, data, tag);

   *ierr=CTA_OK;

}


 void modbuild_par_axpymodel(CTAI_Modbuild_par *datay,double *alpha,
                             CTAI_Modbuild_par *datax, int *ierr)
 {
   int tag;
   int OneWorker;
   int irank, jrank;

//   printf("AXPYMODEL NOT IMPLEMENTED\n");
//   exit(-1);

//   if (ctai_comm_size>1) {
      tag=1234;

      /* Send Task and model handle */
      modbuild_par_sendtaskandhandle("cta_model_axpymodel", ALL_WORKERS, datay, tag);
      /* Send alpha */
      for (irank=1;irank<=datay->nworker;irank++){
         jrank=irank;
         if (datay && datay->is_intercomm) jrank--;

         MPI_Send(alpha, 1, MPI_DOUBLE, jrank,tag, datay->comm);
      }

      /* Check whether the two models are on the same worker */
      OneWorker=datay->comm==datax->comm;

      /* Send flag  "Oneworker" */
      for (irank=1;irank<=datay->nworker;irank++){
         jrank=irank;
         if (datay && datay->is_intercomm) jrank--;

         MPI_Send(&OneWorker, 1, MPI_INT, jrank,tag, datay->comm);
      }






      if (OneWorker) {
         /* Both models are on the same worker, the worker can handle it by
          * itself
          */

         /* Send model handle of x */
         for (irank=1;irank<=datay->nworker;irank++){

            jrank=irank;
            if (datay && datay->is_intercomm) jrank--;

            MPI_Send(&datax->mremote[irank], 1, MPI_INT, jrank,tag, datay->comm);
         }
     } else {
        printf("AXPYMODEL NOT FOR MODELS IN DIFFERENT WORKER GROUPS IS NOT (YET) IMPLEMENTED\n");
        exit(-1);

//        /* Send sending rank */
//        retval=MPI_Send(&datax->rank, 1, MPI_INT, datay->rank,tag,
//                        CTA_COMM_WORKER);
//
//        /* Ask second worker to export to worker that holds model y */
//        modbuild_par_sendtaskandhandle("cta_model_export", datax, tag);
//
//        /* Send receiving rank */
//        retval=MPI_Send(&datay->rank, 1, MPI_INT, datax->rank,tag,
//                        CTA_COMM_WORKER);

     }
//   } else {
//     *ierr=CTA_Model_AxpyState(datay->mremote,*alpha,datax->mremote);
//   }

}


void modbuild_par_axpystate(CTAI_Modbuild_par *data, double *alpha,
                            CTA_TreeVector *statex, int *ierr){

   int tag, irank, jrank;

   if (IDEBUG>0) printf("#%d modbuild_par_axpystate\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_axpystate", ALL_WORKERS, data, tag);

   /* Send alpha */
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      MPI_Send(alpha, 1, MPI_DOUBLE, jrank, tag,
                      data->comm);
   }
   /* Send the state */
   modbuild_par_sendstate(*statex, data, 1234);

   *ierr=CTA_OK;
}

void modbuild_par_setforc(CTAI_Modbuild_par *data, CTA_Time *timespan,
                          CTA_TreeVector *state, int *ierr){

   int tag, irank, jrank;
   double span[2];

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_setstate", ALL_WORKERS, data, tag);

   /* Send timespan */
   CTA_Time_GetSpan(*timespan, &span[0], &span[1]);
   for (irank=1;irank<=data->nworker;irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      MPI_Send((void*) span, 2, MPI_DOUBLE, jrank,tag,data->comm);
   }
   /* Send the state */
   modbuild_par_sendstate(*state, data, 1234);

   *ierr=CTA_OK;

}

void modbuild_par_getforc(CTAI_Modbuild_par *data, CTA_Time *timespan,
                          CTA_TreeVector *state, int *ierr){
   int tag, irank, jrank;
   double span[2];

      tag=1234;

      /* Send Task and model handle */
      modbuild_par_sendtaskandhandle("cta_model_getforc", ALL_WORKERS, data, tag);

      /* Send timespan */
      CTA_Time_GetSpan(*timespan, &span[0], &span[1]);
      for (irank=1;irank<=data->nworker;irank++){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;

         MPI_Send((void*) span, 2, MPI_DOUBLE,jrank,tag,data->comm);
      }

      /* Receive the state */
      modbuild_par_recvstate(state, data, 1234);

      *ierr=CTA_OK;

}

void modbuild_par_axpyforc(CTAI_Modbuild_par *data, CTA_Time *timespan,
                           double *alpha, CTA_TreeVector *statex, int *ierr){
   int tag, irank, jrank;
   double span[2];

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_axpyforc", ALL_WORKERS, data, tag);

   /* Send timespan */
   CTA_Time_GetSpan(*timespan, &span[0], &span[1]);

   for (irank=1;irank<=data->nworker; irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      MPI_Send((void*) span, 2, MPI_DOUBLE, jrank,tag,data->comm);

      /* Send alpha */
      MPI_Send(alpha, 1, MPI_DOUBLE, jrank,tag, data->comm);
   }

   /* Send the state */
   modbuild_par_sendstate(*statex, data, 1234);

   *ierr=CTA_OK;
}

void modbuild_par_setparam(CTAI_Modbuild_par *data, CTA_TreeVector *state,
                           int *ierr){
   int tag;

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_setparam", ALL_WORKERS, data, tag);

   /* Send the state */
   modbuild_par_sendstate(*state, data, 1234);

   *ierr=CTA_OK;

}

void modbuild_par_getparam(CTAI_Modbuild_par *data, CTA_TreeVector *state,
                           int *ierr){
   int tag;

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_getparam", ALL_WORKERS, data, tag);

    /* Create root of state-vector */
   if (*state==CTA_NULL) CTA_TreeVector_Create("root","root",state);

   /* Receive the state */
   modbuild_par_recvstate(state, data, 1234);

   *ierr=CTA_OK;
}

void modbuild_par_axpyparam(CTAI_Modbuild_par *data, double *alpha,
                            CTA_TreeVector *statex, int *ierr){
   int tag, irank, jrank;

      tag=1234;

      /* Send Task and model handle */
      modbuild_par_sendtaskandhandle("cta_model_axpyparam", ALL_WORKERS, data, tag);

      /* Send alpha */
      for (irank=1;irank<=data->nworker;irank++){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;

         MPI_Send(alpha, 1, MPI_DOUBLE, jrank,tag, data->comm);
      }

      /* Send the state */
      modbuild_par_sendstate(*statex, data, 1234);

      *ierr=CTA_OK;
}

void modbuild_par_getnoisecount(CTAI_Modbuild_par *data, int* nnoise, int* ierr) {

   MPI_Status status;
   int tag;
   int nnoise_sub, irank, jrank;

   *nnoise=0;

   if (IDEBUG>0) printf("#%d modbuild_par_getnoisecount\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_getnoisecount", ALL_WORKERS, data, tag);

   for (irank=1;irank<=data->nworker;irank++){

      jrank=irank;
      if (data && data->is_intercomm) jrank--;


      /* Receive number of noise parameters */
      MPI_Recv(&nnoise_sub,1,MPI_INT, jrank, 1234, data->comm,&status);
      if (irank==1) {
         *nnoise=nnoise_sub;
      } else {
         if (*nnoise!=nnoise_sub) {
            printf("Error number of noise parameters of models is not the same\n");
            printf("#%d rank 1 has %d parameters\n", CTA_PAR_MY_RANK, *nnoise);
            printf("#%d rank %d has %d parameters\n",CTA_PAR_MY_RANK, irank, nnoise_sub);
            exit(-1);
         }
      }
   }

   if (IDEBUG>0) printf("#%d Number of noise parameters %d\n",CTA_PAR_MY_RANK,*nnoise);
   *ierr=CTA_OK;
}

void modbuild_par_getnoisecovar(CTAI_Modbuild_par *data, CTA_TreeVector *colsvar, int* ierr){


   MPI_Status status;
   int tag, nnoise, icol;
   int irank, jrank;

   if (IDEBUG>0) printf("#%d modbuild_par_getnoisecovar\n",CTA_PAR_MY_RANK);

      tag=1234;

      /* Send Task and model handle */
      modbuild_par_sendtaskandhandle("cta_model_getnoisecovar", ALL_WORKERS,
                                     data, tag);

      /* Receive number of noise parameters from all workers
       * actually one worker is enough but for the time beeing they all send
       * it */
      for (irank=1;irank<=data->nworker;irank++){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;

         /* Receive number of noise parameters */
         MPI_Recv(&nnoise,1,MPI_INT, jrank, 1234,
                         data->comm,&status);

      }

      /* Receive the columns of the covariance matrix one-by-one */
      for (icol=0;icol<nnoise;icol++){
         /* Create root of state-vector */
//         printf("#%d icol=%d\n",CTA_PAR_MY_RANK, icol);
         modbuild_par_recvstate(&colsvar[icol], data, tag);
//         if (IDEBUG>0) {printf("Reveived column of covar");
//            retval=CTA_TreeVector_Export(colsvar[icol],CTA_FILE_STDOUT);
//         }
      }
      *ierr=CTA_OK;
}

void modbuild_par_getobsselect_communicate(CTAI_Modbuild_par *data, int krank, CTA_Time *ttime, CTA_ObsDescr *hdescr,
                                    CTA_String *sselect, int* ierr){

   MPI_Status status;
   int tag;
   char select[1024];
   CTA_String sselect_sub, sor;

   int irank, jrank;
   int retval_method;
   int retval_all;
   CTA_String_Create(&sselect_sub);
   CTA_String_Create(&sor);
   CTA_String_Set(sor," OR ");

   strcpy(select,"( ");

      tag=1234;

      if (IDEBUG>0) printf("#%d modbuild_par_getobsselect: krank=%d\n",CTA_PAR_MY_RANK, krank);
      if (IDEBUG>0) printf("#%d modbuild_par_getobsselect: ttime=%d\n",CTA_PAR_MY_RANK, *ttime);

      /* Send Task and model handle */

      modbuild_par_sendtaskandhandle("cta_model_getobsselect", krank, data, tag);
      /* Send time and obsdescr (to all workers) */
      if (IDEBUG>0) printf("#%d Send timespan to krank=%d\n",CTA_PAR_MY_RANK, krank);
      modbuild_par_sendobsdescr(data, krank,  *ttime, *hdescr, tag, ierr);
      if (*ierr!=CTA_OK){
         printf("ERROR in modbuild_par_getobsselect: modbuild_par_sendobsdescr returned with error %d\n",*ierr);                if (EXITONERROR) exit(-1);
         return;
      }
      retval_all=CTA_OK;
      for (irank=1;irank<=data->nworker;irank++){
         if (irank==krank || krank<0){

            jrank=irank;
            if (data && data->is_intercomm) jrank--;

            /* Receive the error string */
            MPI_Recv(&retval_method,1,MPI_INT, jrank, 1234, data->comm,&status);
            if (retval_all==CTA_OK && retval_method!=CTA_OK) retval_all=retval_method;

            /* Receive the select string */
            MPI_Recv(select+2,1022,MPI_CHAR, jrank, 1234,
                            data->comm,&status);
            strcpy(select+strlen(select)," )");


            if (IDEBUG>0) printf("#%d modbuild_par_getobsselect: selection rank %d is %s\n",CTA_PAR_MY_RANK,
                            irank, select);

            if (irank==1 || krank>0){
               CTA_String_Set(*sselect,select);

            } else {
               CTA_String_Set(sselect_sub,select);
               CTA_String_Conc(*sselect,sor);
               CTA_String_Conc(*sselect,sselect_sub);
            }
         }
         if (IDEBUG>0) printf("#%d modbuild_par_getobsselect: selection string is %s\n",CTA_PAR_MY_RANK, CTAI_String_GetPtr(*sselect));
         *ierr=retval_all;

         if (IDEBUG>0) printf("#%d end of modbuild_par_getobsselect\n",CTA_PAR_MY_RANK);
      }
      CTA_String_Free(&sselect_sub);
      CTA_String_Free(&sor);
}


void modbuild_par_announceobsvalues(CTAI_Modbuild_par *data, CTA_ObsDescr *hdescr, int* ierr){

   CTA_Time timedum;
   int retval;
   int tag;
   int irank, jrank;
   MPI_Status status;
   CTA_ObsDescr *subObsDescr;
   CTA_RelTable *reltab;
   int nobs, nobs_sub;

      CTA_ObsDescr_Observation_Count (*hdescr,&nobs);
      if (IDEBUG>0) printf("#%d modbuild_par_announceobsvalues; nobs tot:%d \n",CTA_PAR_MY_RANK, nobs);

      tag=1234;
      *ierr=CTA_OK;

      // Create dummy time
      CTA_Time_Create(&timedum);

      if (NOOSOBS < 1) {
      // We need to know what observations the various processes can provide
      //
         if (IDEBUG>0) printf("#%d modbuild_par_getobsvalues: asking for getobsselect to processes \n",CTA_PAR_MY_RANK);
         if (data->nworker>1 && !data->SelectObs){
           data->SelectObs= (CTA_ObsDescr*) CTA_Malloc((data->nworker+1)*sizeof(CTA_ObsDescr));
           for (irank=1;irank<=data->nworker;irank++){
             CTA_String_Create(&(data->SelectObs[irank]));
        //      printf("calling  modbuild_par_getobsselect_communicate... \n");
              modbuild_par_getobsselect_communicate(data, irank, &timedum, hdescr, &(data->SelectObs[irank]), &retval);
           }
         }
      }
      /* Create stochastic observers for different process */
      subObsDescr= (CTA_ObsDescr*) CTA_Malloc(sizeof(CTA_ObsDescr)*(data->nworker+1));

      if (data->nworker==1){
         subObsDescr[1]=*hdescr;
      } else {
         if (NOOSOBS < 1) {

            reltab=CTA_Malloc(sizeof(CTA_RelTable)*(data->nworker+1));
            for (irank=1;irank<=data->nworker;irank++){
               if (IDEBUG>0) printf("#%d Creating Obs selection for process %d\n",CTA_PAR_MY_RANK, irank);
               if (IDEBUG>0) printf("#%d Creating Obs selection='%s'\n",CTA_PAR_MY_RANK, CTAI_String_GetPtr(data->SelectObs[irank]));
               CTA_RelTable_Create(&(reltab[irank]));
               retval=CTA_ObsDescr_CreateSel(*hdescr, data->SelectObs[irank], reltab[irank], &subObsDescr[irank]);
               if (retval!=CTA_OK) {
                  printf("#%d Error creating selection for worker %d process error code is %d\n",CTA_PAR_MY_RANK, irank, retval);
                  exit(-1);
               }

            }
         } else {
             for (irank=1;irank<=data->nworker;irank++){
               subObsDescr[irank]=*hdescr;
             }
            // reltab not defined
         }
      }

      /* Send observation descriptions of the various sub processes */
      for (irank=1;irank<=data->nworker;irank++){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;

         if (IDEBUG>0) printf("#%d Prepare to send Obsdescr to worker %d\n",CTA_PAR_MY_RANK, irank);

         CTA_ObsDescr_Observation_Count (subObsDescr[irank],&nobs_sub);
         if (IDEBUG>0) printf("#%d Number of observations in obsdescr for worker %d is %d\n",CTA_PAR_MY_RANK, irank, nobs_sub);
         if (nobs_sub>0) {

            /* Send Task and model handle */
            modbuild_par_sendtaskandhandle("cta_model_announceobsvalues", irank, data, tag);
            if (IDEBUG>0) printf("#%d done sending task and handle \n",CTA_PAR_MY_RANK);

            /* Send time and obsdescr */
            if (IDEBUG>0) printf("#%d Sending observation description to worker %d\n",CTA_PAR_MY_RANK, irank);
            modbuild_par_sendobsdescr(data, irank, timedum, subObsDescr[irank], tag, ierr);
            if (*ierr!=CTA_OK) return;

         }
      }
      /* Free obsdescr of sub-domains */
      if (data->nworker>1){
         if (NOOSOBS < 1) {
            for (irank=1;irank<=data->nworker;irank++){
               CTA_ObsDescr_Free(&(subObsDescr[irank]));
               CTA_RelTable_Free(&(reltab[irank]));
            }
            free(reltab);
         } else { // do nothing

         }  
      }

      CTA_Time_Free(&timedum);
      free(subObsDescr);
      *ierr=CTA_OK;
}



void modbuild_par_getobsvalues(CTAI_Modbuild_par *data, CTA_Time *ttime,
                               CTA_ObsDescr *hdescr, CTA_Vector *vval,
                               int* ierr){

   int retval;
   int tag;
   int lenpack;
   int irank, jrank;
   CTA_Pack hpack;
   char *pack;
   MPI_Status status;
   CTA_ObsDescr *subObsDescr;
   CTA_RelTable *reltab;
   CTA_Vector vsub;
   int nobs, nobs_sub;
   double dmisval;
   CTA_Datatype datatype;
   int size_type;
   double value;
   int iobs;

   dmisval = -1E-20;



      CTA_ObsDescr_Observation_Count (*hdescr,&nobs);
      CTA_Vector_SetConstant(*vval,&dmisval,CTA_DOUBLE);
      if (IDEBUG>0) printf("#%d modbuild_par_getobsvalues; nobs tot:%d \n",CTA_PAR_MY_RANK, nobs);

      tag=1234;
      *ierr=CTA_OK;

      if (NOOSOBS < 1) {
      // We need to know what observations the various processes can provide
      //
         if (IDEBUG>0) printf("#%d modbuild_par_getobsvalues: asking for getobsselect to processes \n",CTA_PAR_MY_RANK);
         if (data->nworker>1 && !data->SelectObs){
           data->SelectObs=CTA_Malloc((data->nworker+1)*sizeof(CTA_ObsDescr));
           for (irank=1;irank<=data->nworker;irank++){
             CTA_String_Create(&(data->SelectObs[irank]));
        //      printf("calling  modbuild_par_getobsselect_communicate... \n");
              modbuild_par_getobsselect_communicate(data, irank, ttime, hdescr, &(data->SelectObs[irank]), &retval);
           }
         }
      }
      /* Create stochastic observers for different process */
      subObsDescr=CTA_Malloc(sizeof(CTA_ObsDescr)*(data->nworker+1));

      if (data->nworker==1){
         subObsDescr[1]=*hdescr;
      } else {
         if (NOOSOBS < 1) {

            reltab=CTA_Malloc(sizeof(CTA_RelTable)*(data->nworker+1));
            for (irank=1;irank<=data->nworker;irank++){
               if (IDEBUG>0) printf("#%d Creating Obs selection for process %d\n",CTA_PAR_MY_RANK, irank);
               if (IDEBUG>0) printf("#%d Creating Obs selection='%s'\n",CTA_PAR_MY_RANK, CTAI_String_GetPtr(data->SelectObs[irank]));
               CTA_RelTable_Create(&(reltab[irank]));
               retval=CTA_ObsDescr_CreateSel(*hdescr, data->SelectObs[irank], reltab[irank], &subObsDescr[irank]);
               if (retval!=CTA_OK) {
                  printf("#%d Error creating selection for worker %d process error code is %d\n",CTA_PAR_MY_RANK, irank, retval);
                  exit(-1);
               }

            }
         } else {
             for (irank=1;irank<=data->nworker;irank++){
               subObsDescr[irank]=*hdescr;
             }
            // reltab not defined
         }
      }

      /* Send observation descriptions of the various sub processes */
      for (irank=1;irank<=data->nworker;irank++){

         jrank=irank;
         if (data && data->is_intercomm) jrank--;

         if (IDEBUG>0) printf("#%d Prepare to send Obsdescr to worker %d\n",CTA_PAR_MY_RANK, irank);

         CTA_ObsDescr_Observation_Count (subObsDescr[irank],&nobs_sub);
         if (IDEBUG>0) printf("#%d Number of observations in obsdescr for worker %d is %d\n",CTA_PAR_MY_RANK, irank, nobs_sub);
         if (nobs_sub>0) {

            /* Send Task and model handle */
            modbuild_par_sendtaskandhandle("cta_model_getobsvalues", irank, data, tag);
            if (IDEBUG>0) printf("#%d done sending task and handle \n",CTA_PAR_MY_RANK);

            /* create vector for individual processes */
            if (data->nworker>1) {
               if (NOOSOBS < 1) {  // probably unneccessary since nobs_sub = nobs;
                  CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs_sub,CTA_DOUBLE,CTA_NULL,&vsub);
               } else {
                  CTA_Vector_Create(CTA_DEFAULT_VECTOR,nobs,CTA_DOUBLE,CTA_NULL,&vsub);
               } 
            } else {
               vsub=*vval;
            }

            /* Send time and obsdescr */
            if (IDEBUG>0) printf("#%d Sending observation description to worker %d\n",CTA_PAR_MY_RANK, irank);
            modbuild_par_sendobsdescr(data, irank, *ttime, subObsDescr[irank], tag, ierr);
            if (*ierr!=CTA_OK) return;

            /* Receive vector */
            if (IDEBUG>0) printf("#%d Waiting for a vector form worker %d\n",CTA_PAR_MY_RANK, irank);
            retval=MPI_Recv(&lenpack, 1, MPI_INT, jrank,tag,
                         data->comm, &status);
            if (IDEBUG>0) printf("lenpack %d\n", lenpack);
            retval=CTA_Pack_Create(lenpack,&hpack);
            pack  =CTA_Pack_GetPtr(hpack);

            retval=MPI_Recv(pack, lenpack, MPI_CHAR, jrank,tag,
                         data->comm, &status);
            *ierr=CTA_Pack_AddCnt(hpack,lenpack);

            if (IDEBUG>0) printf("#%d Received vector from worker %d\n",CTA_PAR_MY_RANK, irank);
            if (*ierr!=CTA_OK) return;

            if (IDEBUG>0) printf("modbuild_par_getobsvalues: receiving vectors done\n");
            *ierr=CTA_Vector_Import(vsub,hpack);
            if (*ierr!=CTA_OK) return;

            /* Free workvars */
            *ierr=CTA_Pack_Free(&hpack);
            if (*ierr!=CTA_OK) return;

            /* if noosobs, then vsub has the same size as vval (all observations) but contains MISVALS.
               These MISVALS have to be filtered out. */

            /* Copy observations of domain to global vector */
            if (data->nworker>1) {
               if (NOOSOBS < 1) {
                  retval=CTA_RelTable_ApplyInv(reltab[irank], vsub, *vval);
                  if (*ierr!=CTA_OK) {
                     printf("modbuild_par_getobsvalues ERROR in CTA_RelTable_ApplyInv\n");
                     return;
                  }
               } else {

                  *ierr=CTA_Vector_GetDatatype(vsub,&datatype);
                  *ierr=CTA_SizeOf(datatype,&size_type);
                 
                  for ( iobs = 0; iobs < nobs; iobs++) {
                    *ierr = CTA_Vector_GetVal(vsub,iobs+1, &value,datatype);
                     if (  value > MISVAL+1.0) {
                     *ierr = CTA_Vector_SetVal(*vval, iobs+1, &value,datatype);
                     if (*ierr!=CTA_OK) {
                        printf("modbuild_par_getobsvalues ERROR %d in filling vvals from vsub\n",*ierr);
                        return;
                        } 
                     }
                  }
             
               }

               CTA_Vector_Free(&vsub);
            }
         }
         else {
            if (IDEBUG>0) printf("#%d No need to ask observations since there are none \n",CTA_PAR_MY_RANK);
         }
      }
      /* Free obsdescr of sub-domains */
      if (data->nworker>1){
         if (NOOSOBS < 1) {
            for (irank=1;irank<=data->nworker;irank++){
               CTA_ObsDescr_Free(&(subObsDescr[irank]));
               CTA_RelTable_Free(&(reltab[irank]));
            }
            free(reltab);
         } else { // do nothing

         }  
      }
      free(subObsDescr);
       //  printf("@@ getobsvalues end; final array of predictions:\n");
       //CTA_Vector_Export(*vval, CTA_FILE_STDOUT);

      *ierr=CTA_OK;
}


void modbuild_par_getobslocalization(CTAI_Modbuild_par *data, CTA_ObsDescr *hdescr, 
                   double *distance, CTA_Vector *locVecs, int* ierr) {
   //NB  locvecs (size: nobs) is vector containing handles

   int tag;
   int lenpack;
   int irank, jrank, lentask;
   CTA_Pack hpack;
   CTA_Time ttime;
   char *pack;
   CTA_ObsDescr *subObsDescr;
   int nobs, iobs; 
   CTA_TreeVector *locstate;
   int locIDEBUG = 0;

      if (locIDEBUG>0) { printf("modbuild_par_getobslocalization START\n");}

      tag = 1234;

      CTA_ObsDescr_Observation_Count (*hdescr,&nobs);

      if (nobs>0){
         /* Create stochastic observers for different process */
         subObsDescr=CTA_Malloc(sizeof(CTA_ObsDescr)*(data->nworker+1));

         if (data->nworker==1){
            subObsDescr[1]=*hdescr;
         } else {
                for (irank=1;irank<=data->nworker;irank++){
                  subObsDescr[irank]=*hdescr;
                }
         }

         /* Pack the vector containing handles*/
         lenpack=nobs*sizeof(int)+100;
         CTA_Pack_Create(lenpack,&hpack);
         CTA_Vector_Export(*locVecs, hpack);

         /* Send vector to workers */
         lentask= CTA_Pack_GetLen(hpack);
         pack   = CTA_Pack_GetPtr(hpack);

         /* Send observation descriptions of the various sub processes */
         for (irank=1;irank<=data->nworker;irank++){

            jrank=irank;
            if (data && data->is_intercomm) jrank--;

            if (locIDEBUG>0) printf("#%d Prepare to send Obsdescr to worker %d\n",CTA_PAR_MY_RANK, irank);

            /* Send Task and model handle */
            modbuild_par_sendtaskandhandle("cta_model_getobslocalization", irank, data, tag);
            if (locIDEBUG>0) printf("#%d done sending task and handle \n",CTA_PAR_MY_RANK);

            /* Send time and obsdescr */
            /* note: time is not needed but we want to use the  sendobsdescr function */
            CTA_Time_Create(&ttime);
            CTA_Time_SetSpan(ttime, 0.0,0.0);
           
            if (locIDEBUG>0) printf("#%d Sending observation description to worker %d\n",CTA_PAR_MY_RANK, irank);
            modbuild_par_sendobsdescr(data, irank, ttime, subObsDescr[irank], tag, ierr);
            if (*ierr!=CTA_OK) return;

            CTA_Time_Free(&ttime);

            /* send characteristic distance */  
            MPI_Send(distance, 1, MPI_DOUBLE, jrank, tag,
                      data->comm);

            /* Send length of packed vector */
            if (locIDEBUG>0) printf("#%d Send vector to worker (lentask)\n", irank);
            MPI_Send(&lentask, 1, MPI_INT, jrank, tag,
                      data->comm);

            /* Send packed vector */
            MPI_Send(pack, lentask, MPI_CHAR, jrank, tag,
                      data->comm);
            if (locIDEBUG>0) printf("#%d Done sending vector\n",CTA_PAR_MY_RANK);


         }     

         locstate=CTA_Malloc(sizeof(CTA_TreeVector) *nobs);
    
         /* receive for each observation the localization state from all workers */        
         for ( iobs = 0; iobs < nobs; iobs++) {
            if (locIDEBUG>0) printf("modbuild_par_getobslocalization: receiving state of observation %d \n",iobs);
            locstate[iobs] = CTA_NULL; 
            modbuild_par_recvstate(&(locstate[iobs]), data, 1234);

            /* put the handles in the cta-vector */
            CTA_Vector_SetVal(*locVecs,iobs+1,&(locstate[iobs]),CTA_HANDLE);  
         }

           
         if (locIDEBUG>0) {printf("modbuild_par_getobslocalization: returning vector of handles:  \n");
                      CTA_Vector_Export(*locVecs, CTA_FILE_STDOUT);
                     }      

         /* Free work variables/objects */
         CTA_Pack_Free(&hpack);
         free(subObsDescr);
         free(locstate);
   }
}


void modbuild_par_getobsselect(CTAI_Modbuild_par *data, CTA_Time *ttime, CTA_ObsDescr *hdescr,
                                CTA_String *sselect, int* ierr){

   CTA_ObsDescr hdescr_null=CTA_NULL;

   if (IDEBUG>0) printf("#%d modbuild_par_getobsselect hdescr=%d\n",CTA_PAR_MY_RANK, *hdescr);

   /* First try do not send the observation description */
   modbuild_par_getobsselect_communicate(data, ALL_WORKERS, ttime, &hdescr_null, sselect, ierr);
   if (ierr!=CTA_OK) {
      /* Second try and send the observation description */
      modbuild_par_getobsselect_communicate(data, ALL_WORKERS, ttime, hdescr, sselect, ierr);
   }
   if (IDEBUG>0) printf("#%d END if modbuild_par_getobsselect ierr=%d\n",CTA_PAR_MY_RANK, *ierr);
}


void modbuild_par_gettime(CTAI_Modbuild_par *data, CTA_Time *timespan, char* task, int* ierr){

  double span[2];
  int tag;
  int irank, jrank;
  MPI_Status status;

  tag=1234;

  /* Send Task and model handle */
  modbuild_par_sendtaskandhandle(task, ALL_WORKERS, data, tag);

  /* Send timespan */
  for (irank=1;irank<=data->nworker;irank++){

     jrank=irank;
     if (data && data->is_intercomm) jrank--;

     MPI_Recv(span, 2, MPI_DOUBLE, jrank,1234, data->comm, &status);
     CTA_Time_SetSpan(*timespan, span[0], span[1]);
  }

  *ierr=CTA_OK;
}


void modbuild_par_getcurrenttime(CTAI_Modbuild_par *data, CTA_Time *timespan, int* ierr){

  if (IDEBUG>0) printf("#%d modbuild_par_getcurrenttime\n",CTA_PAR_MY_RANK);
  modbuild_par_gettime(data, timespan, "cta_model_getcurrenttime", ierr);

}

void modbuild_par_gettimehorizon(CTAI_Modbuild_par *data, CTA_Time *timespan, int* ierr){

  if (IDEBUG>0) printf("#%d modbuild_par_gettimehorizon\n",CTA_PAR_MY_RANK);
  modbuild_par_gettime(data, timespan, "cta_model_gettimehorizon", ierr);

}


void modbuild_par_addnoise(CTAI_Modbuild_par *data, CTA_Time *timespan, int* ierr){

  double span[2];
  int tag;
  int irank, jrank;

  if (IDEBUG>0) printf("#%d modbuild_par_addnoise\n",CTA_PAR_MY_RANK);

  tag=1234;

  /* Send Task and model handle */
  modbuild_par_sendtaskandhandle("cta_model_addnoise", ALL_WORKERS, data, tag);

  /* Send timespan */
  CTA_Time_GetSpan(*timespan, &span[0], &span[1]);
  for (irank=1;irank<=data->nworker;irank++){
     jrank=irank;
     if (data && data->is_intercomm) jrank--;

     MPI_Send((void*) span, 2, MPI_DOUBLE,jrank,tag,data->comm);
  }

  *ierr=CTA_OK;
}

void modbuild_par_savepersistentstate(CTAI_Modbuild_par *data, CTA_String *filename, CTA_String *instanceID, int *ierr){
   MPI_Status status;
   CTA_Pack hpack;
   int irank, jrank;
   int tag, lenpack, dumval;
   char str1[256], str2[256],fname[256];
   char *pack;

   if (IDEBUG>0) printf("#%d modbuild_par_savepersistentstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_savepersistentstate", ALL_WORKERS, data, tag);

   /* Create pack object for instance ID */
   CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   CTA_String_Export(*instanceID,hpack);
   tag=7321;
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /*Send packed instanceID to each worker process */
      lenpack= CTA_Pack_GetLen(hpack);
      pack=CTA_Pack_GetPtr(hpack);
      MPI_Send(&lenpack, 1, MPI_INT, jrank, tag,
                      data->comm);
      MPI_Send(pack, lenpack, MPI_CHAR, jrank , tag,
                      data->comm);
   }
   /* Free packed object */
   CTA_Pack_Free(&hpack);

   /* Check extension NetCDF and temporarily remove it */
   CTA_String_Get(*filename,str1);
   if (strlen(str1)>3){
     if (0==strcmp( ".nc",(str1+strlen(str1)-3))){
        str1[strlen(str1)-3]='\0';
      } else {
         printf("#%d Error: only export to NetCDF possible '%s'\n",
               CTA_PAR_MY_RANK, str1);
         exit(-1);
      }
   }
   tag=3217;
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /*Add rank of worker process to string containing filename*/
      strcpy(fname,str1);
      sprintf(str2,"_part%04d",jrank);
      strcat(fname,str2);
      strcat(fname,".nc");
      if (IDEBUG>0) printf("send filename '%s' to process #%d \n",fname, jrank);
      CTA_String_Set(*filename,fname);

      /* send packed filename to each worker process */
      CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
      CTA_String_Export(*filename,hpack);

      lenpack= CTA_Pack_GetLen(hpack);
      pack=CTA_Pack_GetPtr(hpack);
      MPI_Send(&lenpack, 1, MPI_INT, jrank, tag,
                      data->comm);
      MPI_Send(pack, lenpack, MPI_CHAR, jrank , tag,
                      data->comm);
      CTA_Pack_Free(&hpack);
   }

   /* Receive CTA_OK to prevent the master from collecting zip files
      before they are written */
   tag = 2173;
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      MPI_Recv(&dumval, 1, MPI_INT, jrank, tag, data->comm, &status);
   }
   *ierr=CTA_OK;
}

void modbuild_par_loadpersistentstate(CTAI_Modbuild_par *data, CTA_String *filename, CTA_String *instanceID, int *ierr){

   MPI_Status status;
   CTA_Pack hpack;
   CTA_String hID;
   int irank, jrank;
   int tag, lenpack;
   char str1[256], str2[256],fname[256];
   char *pack;

   if (IDEBUG>0) printf("#%d modbuild_par_loadpersistentstate\n",CTA_PAR_MY_RANK);

   tag=1234;

   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_loadpersistentstate", ALL_WORKERS, data, tag);

   /* Check extension NetCDF and temporarily remove it */
   CTA_String_Get(*filename,str1);
   if (strlen(str1)>3){
     if (0==strcmp( ".nc",(str1+strlen(str1)-3))){
        str1[strlen(str1)-3]='\0';
      } else {
         printf("#%d Error: only export to NetCDF possible '%s'\n",
               CTA_PAR_MY_RANK, str1);
         exit(-1);
      }
   }
   tag=3218;
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /*Add rank of worker process to string containing filename*/
      strcpy(fname,str1);
      sprintf(str2,"_part%04d",jrank);
      strcat(fname,str2);
      strcat(fname,".nc");
      if (IDEBUG>0) printf("send filename '%s' to process #%d \n",fname, jrank);
      CTA_String_Set(*filename,fname);

      /* send packed filename to each worker process */
      CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
      CTA_String_Export(*filename,hpack);

      lenpack= CTA_Pack_GetLen(hpack);
      pack=CTA_Pack_GetPtr(hpack);
      MPI_Send(&lenpack, 1, MPI_INT, jrank, tag,
                      data->comm);
      MPI_Send(pack, lenpack, MPI_CHAR, jrank , tag,
                      data->comm);
      CTA_Pack_Free(&hpack);
   }

   /* Receive instanceID from each worker process */
   CTA_String_Create(&hID);
   tag=8321;
   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /* Receive string: TODO: teach CTA_Pack this simple and repetitive task:
         CTA_Pack hpack = CTA_Pack_MPI_Receive(jrank,tag,comm);
      */
      MPI_Recv(&lenpack, 1, MPI_INT, jrank,tag, data->comm, &status);
      CTA_Pack_Create(lenpack,&hpack);
      pack  =CTA_Pack_GetPtr(hpack);
      MPI_Recv(pack, lenpack, MPI_CHAR, jrank, tag, data->comm, &status);
      CTA_Pack_AddCnt(hpack,lenpack);

      /* Get string from pack object*/
      CTA_String_Import(hID,hpack);
      CTA_Pack_Free(&hpack);

      if (irank==1) {
         *instanceID=hID;
      }
      if (IDEBUG>0) {
         char *tmpstr = CTAI_String_GetPtr(hID);
         printf("# %d LPS: received instanceID '%s'\n",CTA_PAR_MY_RANK, tmpstr);
      }
   }
   *ierr=CTA_OK;
}

void modbuild_par_saveinternalstate(CTAI_Modbuild_par *data, CTA_String *instanceID, int *ierr){

  MPI_Status status;
  CTA_String hID;
  CTA_Pack hpack;
  int tag, lenpack;
  int irank, jrank;
  char *pack;

  if (IDEBUG>0) printf("#%d modbuild_par_saveinternalstate\n",CTA_PAR_MY_RANK);

  tag=1234;
  /* Send Task and model handle */
  modbuild_par_sendtaskandhandle("cta_model_saveinternalstate", ALL_WORKERS, data, tag);

  /* Receive instanceID from each worker process */
  CTA_String_Create(&hID);
  tag=6321;
  for (irank=1;irank<=data->nworker;irank++){
     jrank=irank;
     if (data && data->is_intercomm) jrank--;

     /* Receive string: TODO: teach CTA_Pack this simple and repetitive task:
        CTA_Pack hpack = CTA_Pack_MPI_Receive(jrank,tag,comm);
     */
     MPI_Recv(&lenpack, 1, MPI_INT, jrank,tag, data->comm, &status);
     CTA_Pack_Create(lenpack,&hpack);
     pack  =CTA_Pack_GetPtr(hpack);
     MPI_Recv(pack, lenpack, MPI_CHAR, jrank, tag, data->comm, &status);
     CTA_Pack_AddCnt(hpack,lenpack);

     /* Get string from pack object*/
     CTA_String_Import(hID,hpack);
     CTA_Pack_Free(&hpack);

     if (irank==1) {
        *instanceID=hID;
     }
     if (IDEBUG>0) {
        char *tmpstr = CTAI_String_GetPtr(hID);
        printf("# %d SIS: received instanceID '%s'\n",CTA_PAR_MY_RANK, tmpstr);
     }
  }
  *ierr=CTA_OK;
}

void modbuild_par_restoreinternalstate(CTAI_Modbuild_par *data, CTA_String *instanceID, int *ierr){

   int irank, jrank;
   int tag, lenpack;
   CTA_Pack hpack;
   char *pack;

  if (IDEBUG>0) printf("#%d modbuild_par_restoreinternalstate\n",CTA_PAR_MY_RANK);

  tag=1234;
  /* Send Task and model handle */
  modbuild_par_sendtaskandhandle("cta_model_restoreinternalstate", ALL_WORKERS, data, tag);

   tag=5321;
   /* Create pack object for instance ID */
   CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   CTA_String_Export(*instanceID,hpack);

   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /*Send packed instanceID to each worker process */
      lenpack= CTA_Pack_GetLen(hpack);
      pack=CTA_Pack_GetPtr(hpack);
      MPI_Send(&lenpack, 1, MPI_INT, jrank, tag,
                      data->comm);
      MPI_Send(pack, lenpack, MPI_CHAR, jrank , tag,
                      data->comm);
   }
   /* Free local objects */
   CTA_Pack_Free(&hpack);

   *ierr=CTA_OK;
}

void modbuild_par_releaseinternalstate(CTAI_Modbuild_par *data, CTA_String *instanceID, int *ierr){

   int irank, jrank;
   int tag, lenpack;
   CTA_Pack hpack;
   char *pack;

   if (IDEBUG>0) printf("#%d modbuild_par_releaseinternalstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Send Task and model handle */
   modbuild_par_sendtaskandhandle("cta_model_releaseinternalstate", ALL_WORKERS, data, tag);

   tag=9321;
   /* Create pack object for instance ID */
   CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   CTA_String_Export(*instanceID,hpack);

   for (irank=1;irank<=data->nworker;irank++){
      jrank=irank;
      if (data && data->is_intercomm) jrank--;

      /*Send packed instanceID to each worker process */
      lenpack= CTA_Pack_GetLen(hpack);
      pack=CTA_Pack_GetPtr(hpack);
      MPI_Send(&lenpack, 1, MPI_INT, jrank, tag,
                      data->comm);
      MPI_Send(pack, lenpack, MPI_CHAR, jrank , tag,
                      data->comm);
   }
   /* Free local objects */
   CTA_Pack_Free(&hpack);

  *ierr=CTA_OK;
}

void CTA_Modbuild_par_worker_create(){
   MPI_Status status;
   int retval;
   CTA_Model hmodel;
   /* cta_model_create */
   char clsnam[256], modinp[256];
   CTA_String sclsnam, smodinp;
   CTA_ModelClass hmodcl;

   /* Create some work-variables */
   CTA_String_Create(&sclsnam);
   CTA_String_Create(&smodinp);
   /* Receive the name of the modelclass */
   retval=MPI_Recv(clsnam,256,MPI_CHAR, ctai_rank_master, 1234,
                   CTA_COMM_MASTER_WORKER,&status);
   CTA_String_Set(sclsnam,clsnam);
   if (IDEBUG>0) printf("#%d name of model-class is '%s'\n",CTA_PAR_MY_RANK,clsnam);
   /* Receive the name of the models input-file */
   retval=MPI_Recv(modinp,256,MPI_CHAR, ctai_rank_master, 1234,
                   CTA_COMM_MASTER_WORKER,&status);
   CTA_String_Set(smodinp,modinp);
   if (IDEBUG>0) printf("#%d model-input is '%s'\n",CTA_PAR_MY_RANK, modinp);

   /* Find the handle of the modelclass */
   retval=CTA_Handle_Find(sclsnam, CTA_MODELCLASS, &hmodcl);
   if (retval!=CTA_OK) {
      printf("#%d Cannot find model-class with name '%s'\n",
              CTA_PAR_MY_RANK, CTAI_String_GetPtr(sclsnam));
      exit(-1);
   }
   /* Create the model */
   if (IDEBUG>0) printf("#%d Calling model-create for modell class %d\n",CTA_PAR_MY_RANK, hmodcl);

   retval=CTA_Model_Create(hmodcl,smodinp,&hmodel);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker:Error creating model instance ",CTA_PAR_MY_RANK);
      printf("ierr='%d'\n",retval);
      exit(-1);
   }

   /* add new model to list */
   modbuild_models_add(hmodel);


   /* Do we need to create our sratch model? */
   if (hmodel_scratch==CTA_NULL) {
      printf("WARNING NOT CREATING SCRATCH MODEL\n");
//      retval=CTA_Model_Create(hmodcl,smodinp,&hmodel_scratch);
//      if (retval!=CTA_OK) {
//         printf("#%d Failed to create scratch model\n",CTA_PAR_MY_RANK);
//         printf("#%d CTA_Modbuild_par_worker:Error creating model instance ",CTA_PAR_MY_RANK);
//         printf("ierr='%d'\n",retval);
//         exit(-1);
//      }
   }


   /* Send the handle of new model instance to master */
   retval=MPI_Send(&hmodel,1, MPI_INT,ctai_rank_master,1234,CTA_COMM_MASTER_WORKER);

   /* Free work variables */
   CTA_String_Free(&sclsnam);
   CTA_String_Free(&smodinp);
}

void CTA_Modbuild_par_worker_compute(){
   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   double span[2];


   if (IDEBUG>0) printf("#%d start CTA_Modbuild_par_worker_compute\n",CTA_PAR_MY_RANK);

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,1234,
                  CTA_COMM_MASTER_WORKER, &status);

  /* Receive the simulation timespan */
  CTA_Time_Create(&timespan);
  MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,1234,
                  CTA_COMM_MASTER_WORKER, &status);
  CTA_Time_SetSpan(timespan, span[0], span[1]);

  /* Perform the compute */
  CTA_Model_Compute(hmodel,timespan);

  /* Free work variables */
   CTA_Time_Free(&timespan);

   if (IDEBUG>0) printf("#%d end CTA_Modbuild_par_worker_compute\n",CTA_PAR_MY_RANK);

}


void CTA_Modbuild_par_worker_setstate(int typeset){
   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   double span[2];
   CTA_TreeVector state;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER, &status);

  /* Receive the simulation timespan (Forcings) */
  timespan=CTA_NULL;
  if (typeset==FORC) {
     CTA_Time_Create(&timespan);
     MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,1234,
                     CTA_COMM_MASTER_WORKER, &status);
     CTA_Time_SetSpan(timespan, span[0], span[1]);
   }

   /* Receive the state */
   state=CTA_NULL;
   modbuild_par_recvstate(&state, NULL, 1234);

   /* Perform the setstate */
   if (typeset==STATE){
      CTA_Model_SetState(hmodel,state);
   } else if (typeset==FORC){
      CTA_Model_SetForc(hmodel,timespan,state);
   } else if  (typeset==PARAM){
      CTA_Model_SetParam(hmodel,state);
   } else {
      printf("Internal error in CTA_Modbuild_par_worker_setstate\n");
      exit(-1);
   }
   CTA_Time_Free(&timespan);

   /* Free work variables */
   CTA_TreeVector_Free(&state,CTA_TRUE);
}

void CTA_Modbuild_par_worker_getstate(int typeget){
   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   double span[2];
   int retval;
   CTA_TreeVector state;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER, &status);

  /* Receive the simulation timespan (Forcings) */
  timespan=CTA_NULL;
  if (typeget==FORC) {
     retval=CTA_Time_Create(&timespan);
     retval=MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,1234,
                     CTA_COMM_MASTER_WORKER, &status);
     retval=CTA_Time_SetSpan(timespan, span[0], span[1]);
   }

   /* Perform the getstate */
   state=CTA_NULL;
   if (typeget==STATE) {
      if (IDEBUG>0) printf("#%d Calling CTA_Model_GetState\n",CTA_PAR_MY_RANK);
      retval=CTA_Model_GetState(hmodel,&state);
      if (retval!=CTA_OK) {
         printf("#%d CTA_Modbuild_par_worker_getstate: error (%d) calling CTA_Model_GetState\n",CTA_PAR_MY_RANK,retval );
         exit(-1);
      }
   } else if (typeget==FORC){
      retval=CTA_Model_GetForc(hmodel,timespan, &state);
   } else if (typeget==PARAM){
      retval=CTA_Model_GetParam(hmodel,&state);
   } else if (typeget==SCAL){
      retval=CTA_Model_GetStateScaling(hmodel,&state);
   } else {
      printf("#%d Internal error in CTA_Modbuild_par_worker_getstate\n",CTA_PAR_MY_RANK);
      exit(-1);
   }

   /* Send the state */

   modbuild_par_sendstate(state, NULL, 1234);

   /* Free work variables */
   retval=CTA_TreeVector_Free(&state,CTA_TRUE);
   retval=CTA_Time_Free(&timespan);
}

void CTA_Modbuild_par_worker_axpymodel(){
   MPI_Status status;
   double alpha;
   int OneWorker;
   int retval;
   CTA_Model hmodely, hmodelx;
   int rank;
   CTA_Pack himport;
   int tag;

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodely, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Receive alpha */
   retval=MPI_Recv(&alpha, 1, MPI_DOUBLE, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Receive Oneworker option */
   retval=MPI_Recv(&OneWorker, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   if (OneWorker) {
      /* Receive handle of other model */
      retval=MPI_Recv(&hmodelx, 1, MPI_INT, ctai_rank_master,tag,
                      CTA_COMM_MASTER_WORKER, &status);

   } else {
      /* Receive rank of worker that will send the export */
      retval=MPI_Recv(&rank, 1, MPI_INT, ctai_rank_master,tag,
                      CTA_COMM_MASTER_WORKER, &status);

      /* Receive export */
      printf("ERROR SCRATCH MODEL DOES NOT WORK!\n");
      exit(-1);
      hmodelx=hmodel_scratch;
      retval = CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&himport);
      retval = receive_pack_object(rank,tag,FALSE, himport, CTA_COMM_MASTER_WORKER);
      retval = CTA_Model_Import(hmodelx,himport);
      retval = CTA_Pack_Free(&himport);
   }

   retval=CTA_Model_AxpyState(hmodely,alpha,hmodelx);

   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_axpymodel: error (%d) calling CTA_Model_Axpy\n",CTA_PAR_MY_RANK,retval );
         exit(-1);
      }
}

void CTA_Modbuild_par_worker_axpy(int typeaxpy){
   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   CTA_TreeVector state;
   double alpha;
   double span[2];

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER, &status);

  /* Receive the simulation timespan (Forcings) */
  timespan=CTA_NULL;
  if (typeaxpy==FORC) {
     CTA_Time_Create(&timespan);
     MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,1234,
                     CTA_COMM_MASTER_WORKER, &status);
     CTA_Time_SetSpan(timespan, span[0], span[1]);
   }

   /* Receive alpha */
   MPI_Recv(&alpha, 1, MPI_DOUBLE, ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Receive the state */
   state=CTA_NULL;
   modbuild_par_recvstate(&state, NULL, 1234);

   /* Perform the axpy */
   if (typeaxpy==STATE) {
      CTA_Model_AxpyState(hmodel,alpha,state);
   } else if (typeaxpy==FORC) {
      CTA_Model_AxpyForc(hmodel,timespan,alpha,state);
   } else if (typeaxpy==PARAM) {
      CTA_Model_AxpyParam(hmodel,alpha,state);
   } else {
      printf("#%d Internal error in CTA_Modbuild_par_worker_axpy\n",CTA_PAR_MY_RANK);
      exit(-1);
   }

   /* Free work variables */
   CTA_TreeVector_Free(&state,CTA_TRUE);
   CTA_Time_Free(&timespan);
}


void CTA_Modbuild_par_worker_gettime(char *task){
   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   double span[2];
   int tag;

   tag=1234;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   timespan=CTA_NULL;
   CTA_Time_Create(&timespan);

   if (strcmp(task, "cta_model_getcurrenttime") ==0) {
      CTA_Model_GetCurrentTime(hmodel,timespan);
   } else if (strcmp(task, "cta_model_gettimehorizon") ==0) {
      CTA_Model_GetTimeHorizon(hmodel,timespan);
   } else {
     printf("#%d CTA_Modbuild_par_worker_gettime: Unkown task '%s'",CTA_PAR_MY_RANK,task);
     exit(-1);
   }

   /* Sent timespan */
   CTA_Time_GetSpan(timespan, &span[0], &span[1]);

   MPI_Send(span, 2, MPI_DOUBLE, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER);

   /* Free workvars */
   CTA_Time_Free(&timespan);
}


void CTA_Modbuild_par_worker_addnoise(){

   MPI_Status status;
   CTA_Time timespan;
   CTA_Model hmodel;
   double span[2];
   int tag;

   tag=1234;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Receive the simulation timespan */
   timespan=CTA_NULL;
   CTA_Time_Create(&timespan);
   MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Time_SetSpan(timespan, span[0], span[1]);

   /* Perform the interpolation */
   CTA_Model_AddNoise(hmodel, timespan);

   /* Free workvars */
   CTA_Time_Free(&timespan);
}

void CTA_Modbuild_par_worker_savepersistentstate(){

   MPI_Status status;
   CTA_Model hmodel;
   CTA_Pack hpacki, hpackf;
   CTA_String hID, fname;
   int tag, lenpack, dumval;
   char *pack;

   if (IDEBUG>0) printf("#%d start CTA_Modbuild_par_worker_savepersistentstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                    CTA_COMM_MASTER_WORKER, &status);

   /* Receive instanceID */
   tag = 7321;
   MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_Create(lenpack,&hpacki);
   pack  =CTA_Pack_GetPtr(hpacki);
   MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_AddCnt(hpacki,lenpack);

   /* Get string from pack object*/
   CTA_String_Create(&hID);
   CTA_String_Import(hID,hpacki);
   CTA_Pack_Free(&hpacki);
   if (IDEBUG>0) {
      char *ID = CTAI_String_GetPtr(hID);
      printf("#%d worker SPS received instance ID = '%s'\n",CTA_PAR_MY_RANK,ID);
   }

   tag = 3217;
   /* Receive filename */
   MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_Create(lenpack,&hpackf);
   pack  =CTA_Pack_GetPtr(hpackf);
   MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_AddCnt(hpackf,lenpack);

   /* Get string from pack object*/
   CTA_String_Create(&fname);
   CTA_String_Import(fname,hpackf);
   CTA_Pack_Free(&hpackf);
   if (IDEBUG>0) {
      char *fstr = CTAI_String_GetPtr(fname);
      printf("#%d worker SPS received filename = '%s'\n",CTA_PAR_MY_RANK,fstr);
   }

   /*  Write state to file for own domain*/
   CTA_Model_SavePersistentState(hmodel, fname, hID);

   /* Return CTA_OK for each worker process to prevent the master to start
      zipping files before they are written */
   tag = 2173;
   dumval = CTA_OK;
   MPI_Send(&dumval, 1, MPI_INT, ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);
}

void CTA_Modbuild_par_worker_loadpersistentstate(){

   MPI_Status status;
   CTA_Model hmodel;
   CTA_Pack hpack;
   CTA_Pack hpackf;
   CTA_String hID, fname;
   int tag, lenpack;
   char *pack;

   tag=1234;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                    CTA_COMM_MASTER_WORKER, &status);

   if (IDEBUG>0) {
     printf("#%d LPS received model handle = '%d'\n",CTA_PAR_MY_RANK,hmodel);
   }

   tag = 3218;
   /* Receive filename */
   MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_Create(lenpack,&hpackf);
   pack  =CTA_Pack_GetPtr(hpackf);
   MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_AddCnt(hpackf,lenpack);

   /* Get string from pack object*/
   CTA_String_Create(&fname);
   CTA_String_Import(fname,hpackf);
   CTA_Pack_Free(&hpackf);
   if (IDEBUG>0) {
      char *fstr = CTAI_String_GetPtr(fname);
      printf("#%d worker LPS received filename = '%s'\n",CTA_PAR_MY_RANK,fstr);
   }

   /*  Read state from file for own domain*/
   CTA_String_Create(&hID);
   CTA_Model_LoadPersistentState(hmodel, fname, &hID);

   /* Print the ID string for debugging reasons*/
   if (IDEBUG>0) {
      char *select = CTAI_String_GetPtr(hID);
      printf("#%d: Debugging: my instanceID is '%s'\n",CTA_PAR_MY_RANK, select);
   }

   /* Send instance ID */
   CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   CTA_String_Export(hID,hpack);

   /* Send package: TODO: teach CTA_Pack this simple repetitive task */
   tag = 8321;
   lenpack= CTA_Pack_GetLen(hpack);
   pack=CTA_Pack_GetPtr(hpack);
   MPI_Send(&lenpack, 1, MPI_INT, ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);
   MPI_Send(pack, lenpack, MPI_CHAR,ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);

   /* Free local objects */
   CTA_Pack_Free(&hpack);
}

void CTA_Modbuild_par_worker_saveinternalstate(){

   MPI_Status status;
   CTA_Model hmodel;
   CTA_Pack hpack;
   CTA_String hID;
   int tag, lenpack;
   char *pack;

   if (IDEBUG>0) printf("#%d start CTA_Modbuild_par_worker_saveinternalstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                     CTA_COMM_MASTER_WORKER, &status);
   if (IDEBUG>0) printf("# %d modbuild_par_worker_saveinternalstate: receiving model handle %d\n",CTA_PAR_MY_RANK, hmodel);

   /* Save internal state for each worker process and get ID back*/
   CTA_String_Create(&hID);
   CTA_Model_SaveInternalState(hmodel,&hID);

   /* Print the ID string for debugging reasons*/
   if (IDEBUG>0) {
      char *select = CTAI_String_GetPtr(hID);
      printf("#%d: Debugging: my instanceID is '%s'\n",CTA_PAR_MY_RANK, select);
   }

   /* Send instance ID */
   CTA_Pack_Create(CTA_PACK_DEFAULT_SIZE,&hpack);
   CTA_String_Export(hID,hpack);

   /* Send package: TODO: teach CTA_Pack this simple repetitive task */
   tag = 6321;
   lenpack= CTA_Pack_GetLen(hpack);
   pack=CTA_Pack_GetPtr(hpack);
   MPI_Send(&lenpack, 1, MPI_INT, ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);
   MPI_Send(pack, lenpack, MPI_CHAR,ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);

   /* Free local objects */
   CTA_Pack_Free(&hpack);
 }

void CTA_Modbuild_par_worker_restoreinternalstate(){

   MPI_Status status;
   CTA_Model hmodel;
   CTA_String hID;
   CTA_Pack hpack;
   int tag, lenpack;
   char *pack;

   if (IDEBUG>0) printf("#%d start CTA_Modbuild_par_worker_restoreinternalstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Receive instanceID to be restored */
   tag = 5321;
   MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_Create(lenpack,&hpack);
   pack  =CTA_Pack_GetPtr(hpack);
   MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_AddCnt(hpack,lenpack);

   /* Get string from pack object*/
   CTA_String_Create(&hID);
   CTA_String_Import(hID,hpack);
   if (IDEBUG>0) {
      char *ID = CTAI_String_GetPtr(hID);
      printf("#%d worker RIS received instance ID = '%s'\n",CTA_PAR_MY_RANK, ID);
   }
   /* Release the internal state */
   CTA_Model_RestoreInternalState(hmodel, hID);

   /* Free local objects */
   CTA_Pack_Free(&hpack);
   CTA_String_Free(&hID);
}

void CTA_Modbuild_par_worker_releaseinternalstate(){

   MPI_Status status;
   CTA_Model hmodel;
   CTA_String hID;
   CTA_Pack hpack;
   int tag, lenpack;
   char *pack;

   if (IDEBUG>0) printf("#%d start CTA_Modbuild_par_worker_releaseinternalstate\n",CTA_PAR_MY_RANK);

   tag=1234;
   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                    CTA_COMM_MASTER_WORKER, &status);

   /* Receive instanceID to be released */
   tag = 9321;
   MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_Create(lenpack,&hpack);
   pack  =CTA_Pack_GetPtr(hpack);
   MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   CTA_Pack_AddCnt(hpack,lenpack);

   /* Get string from pack object*/
   CTA_String_Create(&hID);
   CTA_String_Import(hID,hpack);
   if (IDEBUG>0) {
      char *ID = CTAI_String_GetPtr(hID);
      printf("#%d worker RLS received instance ID = '%s'\n",CTA_PAR_MY_RANK, ID);
   }
   /* Release the internal state */
   CTA_Model_ReleaseInternalState(hmodel, hID);

   /* Free local objects */
   CTA_Pack_Free(&hpack);
   CTA_String_Free(&hID);
}

void CTA_Modbuild_par_worker_free(){

   MPI_Status status;
   CTA_Model hmodel;
   int tag;

   tag=1234;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Free model */
   modbuild_models_delete(hmodel);
   CTA_Model_Free(&hmodel);
}

void CTA_Modbuild_par_worker_getnoisecount(){
   MPI_Status status;
   CTA_Model hmodel;
   int tag, retval, nnoise;

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Get number of noise parameters */
   retval=CTA_Model_GetNoiseCount(hmodel, &nnoise);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_getnoisecount: error (%d) calling CTA_Model_GetNoiseCount\n",CTA_PAR_MY_RANK,retval );
         exit(-1);
      }

   /* Send number of noise parameters */
   retval=MPI_Send(&nnoise,1, MPI_INT,ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER);

}

void CTA_Modbuild_par_worker_getnoisecovar(){
   MPI_Status status;
   CTA_Model hmodel;
   int tag, retval, nnoise, inoise;
   CTA_TreeVector *hcovar;
   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   /* Get number of noise parameters */
   retval=CTA_Model_GetNoiseCount(hmodel, &nnoise);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_getnoisecount: error (%d) calling CTA_Model_GetNoiseCount\n",
         CTA_PAR_MY_RANK,retval );
      exit(-1);
   }

   /* Send number of noise parameters */
   retval=MPI_Send(&nnoise,1, MPI_INT,ctai_rank_master,1234,
                   CTA_COMM_MASTER_WORKER);

   if (IDEBUG>0){printf ("#%d Number of noise parameters is %d",CTA_PAR_MY_RANK,nnoise);}

   /* Allocate workspace */
   hcovar=CTA_Malloc(nnoise*sizeof(CTA_TreeVector));
   for (inoise=0;inoise<nnoise;inoise++){
      hcovar[inoise]=CTA_NULL;
   }

   /* Get the noise covariance for own domain */
   retval=CTA_Model_GetNoiseCovar(hmodel, hcovar);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_getnoisecount: error (%d) calling CTA_Model_GetNoiseCovar\n",CTA_PAR_MY_RANK,retval );
         exit(-1);
      }

   /* Send columns of covariance matrix */
   for (inoise=0;inoise<nnoise;inoise++){
      modbuild_par_sendstate(hcovar[inoise], NULL, 1234);

   }

   /* Free workspace */
   for (inoise=0;inoise<nnoise;inoise++){
      retval=CTA_TreeVector_Free(&hcovar[inoise],CTA_TRUE);
   }
   free(hcovar);

}


void CTA_Modbuild_par_worker_announceobsvalues(){

   MPI_Status status;
   CTA_Time timespan;
   CTA_ObsDescr hdescr_tab;
   CTA_Model hmodel;
   int tag, retval;

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);


   timespan=CTA_NULL;
   hdescr_tab=CTA_NULL;
   modbuild_par_recvobsdescr(&timespan, &hdescr_tab, tag);

   /* Perform the interpolation */
   retval=CTA_Model_AnnounceObsValues(hmodel, hdescr_tab);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_announceobsvalues Error (%d) calling CTA_Model_AnnounceObsValues \n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }

   /* Free workvars */
   retval=CTA_Time_Free(&timespan);
   retval=CTA_ObsDescr_Free(&hdescr_tab);


}


void CTA_Modbuild_par_worker_getobsvalues(){

   MPI_Status status;
   CTA_Time timespan;
   CTA_Pack hpack;
   CTA_ObsDescr hdescr_tab;
   CTA_Model hmodel;
   CTA_Vector values;
   int lenpack, lentask, retval, nmeasr, tag;
   char *pack;

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);


   timespan=CTA_NULL;
   hdescr_tab=CTA_NULL;
   modbuild_par_recvobsdescr(&timespan, &hdescr_tab, tag);

   /* Receive the simulation timespan */
//   retval=CTA_Time_Create(&timespan);
//   retval=MPI_Recv(span, 2, MPI_DOUBLE, ctai_rank_master,tag,
//                   CTA_COMM_WORKER, &status);
//   retval=CTA_Time_SetSpan(timespan, span[0], span[1]);

   /* Receive the observation description */
//   retval=MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
//                   CTA_COMM_WORKER, &status);
//   retval=CTA_Pack_Create(lenpack,&hpack);
//   pack  =CTA_Pack_GetPtr(hpack);
//
//   retval=MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
//                   CTA_COMM_WORKER, &status);
//   retval=CTA_Pack_AddCnt(hpack,lenpack);
//   if (IDEBUG>0) printf("CREATE THE CTA_OBSDESCR_TABLE1\n");

   /* Create the observation description */
//   retval=CTA_ObsDescr_Create(CTA_OBSDESCR_TABLE,hpack, &hdescr_tab);
//   retval=CTA_Pack_Free(&hpack);
//   if (IDEBUG>0) printf("Send vector to master\n");

   /* Create vector for holding values */
   retval=CTA_ObsDescr_Observation_Count(hdescr_tab, &nmeasr);
   if (IDEBUG>0) printf("#%d Number of observations %d\n",CTA_PAR_MY_RANK,nmeasr );
   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr,  CTA_DOUBLE, CTA_NULL, &values);

   /* Perform the interpolation */
   retval=CTA_Model_GetObsValues(hmodel, timespan, hdescr_tab, values);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_getobsvalues Error (%d) calling CTA_Model_GetObsValues \n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }
   /* Pack the vector */
   lenpack=nmeasr*sizeof(double)+100;
   retval=CTA_Pack_Create(lenpack,&hpack);
   retval=CTA_Vector_Export(values, hpack);

   /* Send vector to master */
   lentask= CTA_Pack_GetLen(hpack);
   pack   = CTA_Pack_GetPtr(hpack);

   /* Send length of packed state */
   if (IDEBUG>0) printf("#%d Send vector to master (lentask)\n",CTA_PAR_MY_RANK);
   retval=MPI_Send(&lentask, 1, MPI_INT, ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);
   if (IDEBUG>0) printf("#%d Send vector to master (packed vector)\n",CTA_PAR_MY_RANK);

   /* Send packed state */
   retval=MPI_Send(pack, lentask, MPI_CHAR, ctai_rank_master, tag,
                   CTA_COMM_MASTER_WORKER);
   if (IDEBUG>0) printf("#%d Done sending vector\n",CTA_PAR_MY_RANK);


   /* Free workvars */
   retval=CTA_Pack_Free(&hpack);
   retval=CTA_Time_Free(&timespan);
   retval=CTA_Vector_Free(&values);
   retval=CTA_ObsDescr_Free(&hdescr_tab);


}

/* ----------------------------------------- */


void CTA_Modbuild_par_worker_getobslocalization(){

   MPI_Status status;
   CTA_Pack hpack;
   CTA_ObsDescr hdescr_tab;
   CTA_Model hmodel;
   CTA_Vector values;
   int lenpack, retval, nmeasr, tag;
   char *pack;
   CTA_Time timespan; 
   int iobs;
   CTA_Handle hstate;
   int locIDEBUG = 0;
   double distance;

   tag=1234;

   /* Receive the model handle */
   retval=MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   timespan=CTA_NULL;
   hdescr_tab=CTA_NULL;
   modbuild_par_recvobsdescr(&timespan, &hdescr_tab, tag);
   if (locIDEBUG>0) printf("#%d received obsdescr  %d\n",CTA_PAR_MY_RANK,hdescr_tab );

   /* Create vector for holding values */
   retval=CTA_ObsDescr_Observation_Count(hdescr_tab, &nmeasr);
   if (locIDEBUG>0) printf("#%d Number of observations %d\n",CTA_PAR_MY_RANK,nmeasr ); 

   retval=CTA_Vector_Create(CTA_DEFAULT_VECTOR, nmeasr,  CTA_HANDLE, CTA_NULL, &values);

   /* receive distance */
   if (IDEBUG>0) {printf("#%d Receiving characteristic distance from master\n",CTA_PAR_MY_RANK);}
   retval=MPI_Recv(&distance, 1, MPI_DOUBLE, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);


   /* Receive the vector with handles */
   if (locIDEBUG>0) {printf("#%d Receiving lenpack from master\n",CTA_PAR_MY_RANK);}
   retval=MPI_Recv(&lenpack, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);
   if (locIDEBUG>0) {printf("#%d vector locvals lenpack =%d\n",CTA_PAR_MY_RANK, lenpack);}

   if (lenpack==0){
      // free something? 
   }
   else {
      retval=CTA_Pack_Create(lenpack,&hpack);
      pack  =CTA_Pack_GetPtr(hpack);

      if (locIDEBUG>0) {printf("#%d Receiving packed locvals from master\n",CTA_PAR_MY_RANK);}
      retval=MPI_Recv(pack, lenpack, MPI_CHAR, ctai_rank_master,tag,
                      CTA_COMM_MASTER_WORKER, &status);
      if (IDEBUG>0) {printf("#%d Done Receiving packed locvals from master\n",CTA_PAR_MY_RANK); }

      retval=CTA_Pack_AddCnt(hpack,lenpack);

      /* Create the vector */
       retval = CTA_Vector_Import(values,hpack);
      if (IDEBUG>0) {printf("#%d Done Creating vector %d\n",CTA_PAR_MY_RANK, retval);}
      retval=CTA_Pack_Free(&hpack);
   }

   if (locIDEBUG>0) printf("#%d now obtained null vector from master  %d\n",CTA_PAR_MY_RANK,values );

   /* Perform the interpolation */
   retval=CTA_Model_GetObsLocalization(hmodel, hdescr_tab, distance, values);
   if (retval!=CTA_OK) {
      printf("#%d CTA_Modbuild_par_worker_getobslocalization Error (%d) calling CTA_Model_GetOblocalization \n",CTA_PAR_MY_RANK, retval);
      exit(-1);
   }
   
   /* The vector cannot be packed since the state handles are only known locally. Therefore,
    we have to pack and send the corresponding states. We do this for each observation.*/
   for (iobs=0;iobs<nmeasr;iobs++) {
      retval = CTA_Vector_GetVal(values,iobs+1, &hstate, CTA_HANDLE);

      if (locIDEBUG>0) printf("#%d sending localization tv (handle %d) of obs  %d\n",CTA_PAR_MY_RANK,hstate,iobs+1 );
      modbuild_par_sendstate(hstate, NULL, 1234);
   }  
     
  
   /* Free workvars */
   retval=CTA_Vector_Free(&values);
   retval=CTA_ObsDescr_Free(&hdescr_tab);
   retval=CTA_Time_Free(&timespan);
}


void CTA_Modbuild_par_worker_getobsselect(){

   MPI_Status status;
   CTA_Time timespan;
   CTA_String sselect;
   CTA_ObsDescr hdescr_tab;
   CTA_Model hmodel;
   int lenstr, tag;
   int retval_method;
   char *select;

   tag=1234;

   /* Receive the model handle */
   MPI_Recv(&hmodel, 1, MPI_INT, ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER, &status);

   timespan=CTA_NULL;
   hdescr_tab=CTA_NULL;
   modbuild_par_recvobsdescr(&timespan, &hdescr_tab, tag);

   /* Call obsselect */
   CTA_String_Create(&sselect);
   retval_method=CTA_Model_GetObsSelect(hmodel,timespan,hdescr_tab,sselect);

   /* Send the error code back to the master */
   MPI_Send(&retval_method,1, MPI_INT,ctai_rank_master,tag, CTA_COMM_MASTER_WORKER);

   /* Send the select string */
   select=CTAI_String_GetPtr(sselect);
   if (IDEBUG>0) printf("#%d CTA_Modbuild_par_worker_getobsselect: My @@ selection is '%s'\n",CTA_PAR_MY_RANK, select);
   lenstr=strlen(select)+1;
   MPI_Send(select,lenstr, MPI_CHAR,ctai_rank_master,tag,
                   CTA_COMM_MASTER_WORKER);

   /* Free workvars */
   CTA_String_Free(&sselect);
   CTA_ObsDescr_Free(&hdescr_tab);
   CTA_Time_Free(&timespan);
}

void CTA_Modbuild_par_worker(){
   MPI_Status status;
   char task[256];

   /* Note we have to set our own random-seed */
   CTA_INITIAL_RANDOM_SEED=CTA_INITIAL_RANDOM_SEED+ctai_rank;
   if (IDEBUG>0) printf("#%d Set initial random seed:  %ld\n", CTA_PAR_MY_RANK, CTA_INITIAL_RANDOM_SEED);
   CTA_rand_seed((long) CTA_INITIAL_RANDOM_SEED);

   if (IDEBUG>0) printf("#%d I am the listening process of worker %d\n", CTA_PAR_MY_RANK, ctai_rank);
   for (;1;){
      if (IDEBUG>0) printf("#%d waiting for message of master...\n",CTA_PAR_MY_RANK);

      MPI_Recv(task,256,MPI_CHAR, ctai_rank_master, 1234,
                     CTA_COMM_MASTER_WORKER ,&status);


      if (IDEBUG>0) {printf("%d My task is: %s \n",CTA_PAR_MY_RANK,task);}

      if        (strcmp(task, "cta_model_create")         ==0) {
         CTA_Modbuild_par_worker_create();
      } else if (strcmp(task, "cta_model_compute")        ==0) {
         CTA_Modbuild_par_worker_compute();
      } else if (strcmp(task, "cta_model_setstate")       ==0) {
         CTA_Modbuild_par_worker_setstate(STATE);
      } else if (strcmp(task, "cta_model_getstate")       ==0) {
         CTA_Modbuild_par_worker_getstate(STATE);
      } else if (strcmp(task, "cta_model_axpystate")      ==0) {
         CTA_Modbuild_par_worker_axpy(STATE);
      } else if (strcmp(task, "cta_model_axpymodel")      ==0) {
         CTA_Modbuild_par_worker_axpymodel();
      } else if (strcmp(task, "cta_model_setforc")        ==0) {
         CTA_Modbuild_par_worker_setstate(FORC);
      } else if (strcmp(task, "cta_model_getforc")        ==0) {
         CTA_Modbuild_par_worker_getstate(FORC);
      } else if (strcmp(task, "cta_model_getstatescaling")==0) {
         CTA_Modbuild_par_worker_getstate(SCAL);
      } else if (strcmp(task, "cta_model_axpyforc")       ==0) {
         CTA_Modbuild_par_worker_axpy(FORC);
      } else if (strcmp(task, "cta_model_getnoisecount")  ==0) {
         CTA_Modbuild_par_worker_getnoisecount();
      } else if (strcmp(task, "cta_model_getnoisecovar")  ==0) {
         CTA_Modbuild_par_worker_getnoisecovar();
      } else if (strcmp(task, "cta_model_getobsselect")   ==0) {
         CTA_Modbuild_par_worker_getobsselect();
      } else if (strcmp(task, "cta_model_addnoise")       ==0) {
         CTA_Modbuild_par_worker_addnoise();
      } else if (strcmp(task, "cta_model_export")         ==0) {
         CTA_Modbuild_par_worker_export();
      } else if (strcmp(task, "cta_model_import")         ==0) {
         CTA_Modbuild_par_worker_import();
      } else if (strcmp(task, "cta_model_getobsvalues")   ==0) {
         CTA_Modbuild_par_worker_getobsvalues();
      } else if (strcmp(task, "cta_model_getobslocalization")   ==0) {
        CTA_Modbuild_par_worker_getobslocalization(); 
      } else if (strcmp(task, "cta_model_getcurrenttime") ==0) {
         CTA_Modbuild_par_worker_gettime(task);
      } else if (strcmp(task, "cta_model_gettimehorizon") ==0) {
         CTA_Modbuild_par_worker_gettime(task);
      } else if (strcmp(task, "cta_model_free")           ==0) {
         CTA_Modbuild_par_worker_free();
      } else if (strcmp(task, "cta_model_savepersistentstate") ==0) {
         CTA_Modbuild_par_worker_savepersistentstate();
      } else if (strcmp(task, "cta_model_loadpersistentstate") ==0) {
         CTA_Modbuild_par_worker_loadpersistentstate();
      } else if (strcmp(task, "cta_model_saveinternalstate") ==0) {
        CTA_Modbuild_par_worker_saveinternalstate();
      } else if (strcmp(task, "cta_model_restoreinternalstate") ==0) {
         CTA_Modbuild_par_worker_restoreinternalstate();
      } else if (strcmp(task, "cta_model_releaseinternalstate") ==0) {
         CTA_Modbuild_par_worker_releaseinternalstate();
      } else if (strcmp(task, "cta_model_announceobsvalues") ==0) {
         CTA_Modbuild_par_worker_announceobsvalues();
      } else if (strcmp(task, "finalize")                 ==0) {
         if (IDEBUG>0) printf("Task is finalize\n");
         // Free all models (in order to finalize workers as well)
         modbuild_models_delete_all_models();
         MPI_Finalize();
         exit(0);
      } else {
         printf("#%d Unkown task '%s'",CTA_PAR_MY_RANK,task);
         exit(-1);
      }
   }
   printf("#%d CTA_Modbuild_par_worker: Internal error\n",CTA_PAR_MY_RANK);
   exit(-1);
}

#endif
void CTA_Modbuild_par_Finalize(){
#if USE_MPI

   /* Send Task */
   char *task;
   int lentask, irank;
   int tag=1234;
   int comSize;
   MPI_Comm comm;
   int is_intercomm;
   int indx;
   int isInit;
   int rank_first;


   if (IDEBUG>0) printf("#%d CTA_Modbuild_par_Finalize\n",CTA_PAR_MY_RANK);
   // Loop over all communicators
   task="finalize";
   lentask=strlen(task)+1;

   comm=MPI_COMM_WORLD;
   for (indx=0; comm!=MPI_COMM_NULL; indx++){
      CTA_Par_GetAllCommByIndex(indx, &comm);
      if (comm!=MPI_COMM_NULL){
         /* Determine processes to send finalize task
            But first we have to check the kind of communicater 
          */
         MPI_Comm_test_inter(comm, &is_intercomm);
         if (is_intercomm){
            rank_first=0;
            MPI_Comm_remote_size(comm,&comSize);
         }
         else {
            rank_first=1;
            MPI_Comm_size(comm,&comSize);
         }

         if (IDEBUG>0) printf("Size of communication group :%d\n", comSize);
         if (IDEBUG>0) printf("Communicator is inter comm  :%d\n",is_intercomm);
         for (irank=rank_first;irank<comSize; irank++){
            if (IDEBUG>0) printf("Sending finalize to process %d\n", irank);
            MPI_Send(task,lentask, MPI_CHAR, irank,tag,
                           comm); 
         }
      }
   }

   // Only finalize when mpi is initialized
   if (IDEBUG>0) printf("#%d CTA_Modbuild_par_Finalize calling MPI_Finalize\n",CTA_PAR_MY_RANK);
   MPI_Initialized(&isInit);
   if (isInit){
     MPI_Finalize();
   }
   exit(0);
#endif
}



void CTA_Modbuild_par_CreateClass(CTA_ModelClass *modelcls){

#ifdef USE_MPI

   int ierr; //COSTA return value
   int aierr[CTA_MODEL_NUMFUNC];
   int i;
   CTA_Func hfunc[CTA_MODEL_NUMFUNC];
   CTA_Intf intf;


   /* Calculate number of nodes */
   MPI_Comm_size(CTA_COMM_WORLD,&ctai_comm_size);

   /* Determine whether you are master or worker */
   MPI_Comm_rank(CTA_COMM_WORLD,&ctai_rank);

   if (CTA_FILTER_PROCESS) {
      if (IDEBUG>0) printf("#%d I'm a filter process; my rank is %d\n",CTA_PAR_MY_RANK, ctai_rank);
   } else {
      /* I am a worker starting to wait for new tasks */
      if (IDEBUG>0) printf("#%d I'm a worker my rank is %d\n",CTA_PAR_MY_RANK, ctai_rank);
      CTA_Modbuild_par_worker();
   }

   // Create a COSTA model component from my own implementation
   intf=CTA_NULL;
   for (i=0; i<CTA_MODEL_NUMFUNC; i++){
      hfunc[i]=CTA_NULL;
	  aierr[i]=CTA_OK;
   }
   aierr[I_CTA_MODEL_CREATE_SIZE    ]     =CTA_Func_Create("modbuild_par_create_size",      &modbuild_par_create_size,    intf, &hfunc[I_CTA_MODEL_CREATE_SIZE    ]);
   aierr[I_CTA_MODEL_CREATE_INIT    ]     =CTA_Func_Create("modbuild_par_create_init",      &modbuild_par_create_init,    intf, &hfunc[I_CTA_MODEL_CREATE_INIT    ]);
   aierr[I_CTA_MODEL_FREE           ]     =CTA_Func_Create("modbuild_par_free",             &modbuild_par_free,           intf, &hfunc[I_CTA_MODEL_FREE           ]);
   aierr[I_CTA_MODEL_COMPUTE        ]     =CTA_Func_Create("modbuild_par_compute",          &modbuild_par_compute,        intf, &hfunc[I_CTA_MODEL_COMPUTE        ]);
   aierr[I_CTA_MODEL_SET_STATE      ]     =CTA_Func_Create("modbuild_par_setstate",         &modbuild_par_setstate,       intf, &hfunc[I_CTA_MODEL_SET_STATE      ]);
   aierr[I_CTA_MODEL_GET_STATE      ]     =CTA_Func_Create("modbuild_par_getstate",         &modbuild_par_getstate,       intf, &hfunc[I_CTA_MODEL_GET_STATE      ]);
   aierr[CTA_MODEL_AXPY_MODEL       ]     =CTA_Func_Create("modbuild_par_axpymodel",        &modbuild_par_axpymodel,      intf, &hfunc[CTA_MODEL_AXPY_MODEL       ]);
   aierr[CTA_MODEL_AXPY_STATE       ]     =CTA_Func_Create("modbuild_par_axpystate",        &modbuild_par_axpystate,      intf, &hfunc[CTA_MODEL_AXPY_STATE       ]);
   aierr[CTA_MODEL_SET_FORC         ]     =CTA_Func_Create("modbuild_par_setforc",          &modbuild_par_setforc,        intf, &hfunc[CTA_MODEL_SET_FORC         ]);
   aierr[CTA_MODEL_GET_FORC         ]     =CTA_Func_Create("modbuild_par_getforc",          &modbuild_par_getforc,        intf, &hfunc[CTA_MODEL_GET_FORC         ]);
   aierr[CTA_MODEL_AXPY_FORC        ]     =CTA_Func_Create("modbuild_par_axpyforc",         &modbuild_par_axpyforc,       intf, &hfunc[CTA_MODEL_AXPY_FORC        ]);
   aierr[CTA_MODEL_SET_PARAM        ]     =CTA_Func_Create("modbuild_par_setparam",         &modbuild_par_setparam,       intf, &hfunc[CTA_MODEL_SET_PARAM        ]);
   aierr[CTA_MODEL_GET_PARAM        ]     =CTA_Func_Create("modbuild_par_getparam",         &modbuild_par_getparam,       intf, &hfunc[CTA_MODEL_GET_PARAM        ]);
   aierr[CTA_MODEL_AXPY_PARAM       ]     =CTA_Func_Create("modbuild_par_axpyparam",        &modbuild_par_axpyparam,      intf, &hfunc[CTA_MODEL_AXPY_PARAM       ]);
   aierr[CTA_MODEL_GET_STATESCALING ]     =CTA_Func_Create("modbuild_par_getstatescaling",  &modbuild_par_getstatescaling,intf, &hfunc[CTA_MODEL_GET_STATESCALING ]);
   aierr[CTA_MODEL_GET_NOISE_COUNT  ]     =CTA_Func_Create("modbuild_par_getnoisecount",    &modbuild_par_getnoisecount,  intf, &hfunc[CTA_MODEL_GET_NOISE_COUNT  ]);
   aierr[CTA_MODEL_GET_NOISE_COVAR  ]     =CTA_Func_Create("modbuild_par_getnoisecovar",    &modbuild_par_getnoisecovar,  intf, &hfunc[CTA_MODEL_GET_NOISE_COVAR  ]);
   aierr[CTA_MODEL_GET_OBSVALUES    ]     =CTA_Func_Create("modbuild_par_getobsvalues",     &modbuild_par_getobsvalues,   intf, &hfunc[CTA_MODEL_GET_OBSVALUES    ]);
   aierr[CTA_MODEL_GET_OBSSELECT    ]     =CTA_Func_Create("modbuild_par_getobsselect",     &modbuild_par_getobsselect,   intf, &hfunc[CTA_MODEL_GET_OBSSELECT    ]);
   aierr[CTA_MODEL_ADD_NOISE        ]     =CTA_Func_Create("modbuild_par_addnoise",         &modbuild_par_addnoise,       intf, &hfunc[CTA_MODEL_ADD_NOISE        ]);
   aierr[I_CTA_MODEL_IMPORT         ]     =CTA_Func_Create("modbuild_par_import",           &modbuild_par_import,         intf, &hfunc[I_CTA_MODEL_IMPORT         ]);
   aierr[I_CTA_MODEL_EXPORT         ]     =CTA_Func_Create("modbuild_par_export",           &modbuild_par_export,         intf, &hfunc[I_CTA_MODEL_EXPORT         ]);
   aierr[CTA_MODEL_GET_CURRENTTIME  ]     =CTA_Func_Create("modbuild_par_getcurrenttime",   &modbuild_par_getcurrenttime, intf, &hfunc[CTA_MODEL_GET_CURRENTTIME  ]);
   aierr[CTA_MODEL_GET_TIMEHORIZON  ]     =CTA_Func_Create("modbuild_par_gettimehorizon",   &modbuild_par_gettimehorizon, intf, &hfunc[CTA_MODEL_GET_TIMEHORIZON  ]);
   aierr[I_CTA_MODEL_GETOBSLOCALIZATION ] =CTA_Func_Create("modbuild_par_getobslocalization",   &modbuild_par_getobslocalization  , intf, &hfunc[I_CTA_MODEL_GETOBSLOCALIZATION   ]);
   aierr[CTA_MODEL_SAVE_PERSISTENTSTATE ] =CTA_Func_Create("modbuild_par_savepersistentstate",  &modbuild_par_savepersistentstate , intf, &hfunc[CTA_MODEL_SAVE_PERSISTENTSTATE  ]);
   aierr[CTA_MODEL_LOAD_PERSISTENTSTATE]  =CTA_Func_Create("modbuild_par_loadpersistentstate",  &modbuild_par_loadpersistentstate , intf, &hfunc[CTA_MODEL_LOAD_PERSISTENTSTATE  ]);
   aierr[CTA_MODEL_SAVE_INTERNALSTATE  ]  =CTA_Func_Create("modbuild_par_saveinternalstate",    &modbuild_par_saveinternalstate   , intf, &hfunc[CTA_MODEL_SAVE_INTERNALSTATE    ]);
   aierr[CTA_MODEL_RESTORE_INTERNALSTATE] =CTA_Func_Create("modbuild_par_restoreinternalstate", &modbuild_par_restoreinternalstate, intf, &hfunc[CTA_MODEL_RESTORE_INTERNALSTATE ]);
   aierr[CTA_MODEL_RELEASE_INTERNALSTATE] =CTA_Func_Create("modbuild_par_releaseinternalstate", &modbuild_par_releaseinternalstate, intf, &hfunc[CTA_MODEL_RELEASE_INTERNALSTATE ]);
   aierr[CTA_MODEL_ANNOUNCE_OBSVALUES]    =CTA_Func_Create("modbuild_par_announceobsvalues", &modbuild_par_announceobsvalues, intf, &hfunc[CTA_MODEL_ANNOUNCE_OBSVALUES]);

   // If something went wrong we are really fatal!
   for (i=0; i<CTA_MODEL_NUMFUNC ;i++){
      if (aierr[i]!=CTA_OK) {
      printf("Internal error in CTA_Modbuild_par_createclass\n");
      printf("Cannot create function handles for own implementation\n");
      exit(1);
      }
   }
   /* Create new model class */
   ierr=CTA_Model_DefineClass("CTA_MODBUILD_PAR", hfunc, modelcls);
   if (ierr!=CTA_NULL) {
      printf("Internal error in CTA_Modbuild_par_createclass\n");
      printf("Cannot create new class\n");
      exit(-1);
   }
#else
   modelcls=CTA_NULL;
#endif


}




#define CTA_MODELBUILD_PAR_CREATECLASS_F77  F77_CALL(cta_modbuild_par_createclass,CTA_MODBUILD_PAR_CREATECLASS)
#define CTA_MODELBUILD_PAR_FINALIZE_F77     F77_CALL(cta_modbuild_par_finalize,CTA_MODBUILD_PAR_FINALIZE)

void CTA_MODELBUILD_PAR_CREATECLASS_F77(CTA_ModelClass *modelcls){
   CTA_Modbuild_par_CreateClass(modelcls);
}

void CTA_MODELBUILD_PAR_FINALIZE_F77(){
   CTA_Modbuild_par_Finalize();
}


