## run it as: qsub_run_oda.sh biasEnKF.oda
#queue=normal-e3-c7
queue=test-c7
export pwd=$PWD
#export OPENDARUNSCRIPTSPATH=/p/kalman-filter-dflow-fm/OpenDA_Bins/MostRecent/bin
#export OPENDARUNSCRIPTSPATH=/p/kalman-filter-dflow-fm/OpenDA_Bins/bin20221223/
#export OPENDARUNSCRIPTSPATH=/p/kalman-filter-dflow-fm/OpenDA_Bins/bin20230112/
export OPENDARUNSCRIPTSPATH=/p/kalman-filter-dflow-fm/OpenDA_Bins/bin20230113/
#export OPENDARUNSCRIPTSPATH=/p/kalman-filter-dflow-fm/OpenDA_Bins/build_c1ec1cf46ff92e43c7fc6fb05dfe3b07349c8b2e/openda_trunk/bin/
export OPENDADIMRPATH=/p/d-hydro/dimrset/2022/2022.03
export OPENDAPYTHONPATH=/p/kalman-filter-dflow-fm/PythonBin/miniconda3
export I_MPI_FABRICS=shm
cd $OPENDARUNSCRIPTSPATH
source settings_local.sh linux
echo 'This is OPENDADIR:' $OPENDADIR 
cd $pwd
#echo 'executing: qsub -cwd -q normal-e3 -V -pe distrib 1 -N enkf_01 $OPENDADIR/oda_ddrun.sh Enkf_thread.oda'
echo "qsub -cwd -q test-c7 -V -pe distrib 1 -N ${PWD##*/} $OPENDADIR/oda_run.sh $1"
export ODA_JAVAOPTS='-Xmx2048m'
#qsub -cwd -q normal-e3-c7 -V -pe distrib 1 -N ${PWD##*/} $OPENDADIR/oda_run.sh Enkf.oda
#qsub -cwd -q test-c7 -V -pe distrib 1 -N ${PWD##*/} $OPENDADIR/oda_run.sh $1
qsub -cwd -q $queue -V -pe distrib 1 -N ${PWD##*/} $OPENDADIR/oda_run.sh $1
