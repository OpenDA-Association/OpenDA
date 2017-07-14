#!/bin/sh
STUDENT=openda_student

rm -rf $STUDENT
mkdir $STUDENT
cp ./doc/latex/openda_course.pdf $STUDENT

# Exercise 1
cp -r exercise_1 $STUDENT/exercise_1
# remove files, the students must produce
rm $STUDENT/exercise_1/all_runs.m
rm $STUDENT/exercise_1/all_runs.py
rm $STUDENT/exercise_1/simulation_ensemble.oda
rm $STUDENT/exercise_1/algorithm/SequentialEnsembleSimulation.xml
rm -rf $STUDENT/exercise_1/reference_plots

# Exercise 2
mkdir $STUDENT/exercise_2
cp exercise_2/load_ensemble.m   $STUDENT/exercise_2
cp exercise_2/ensemble.py       $STUDENT/exercise_2
cp exercise_2/load_obs.m        $STUDENT/exercise_2
rm -rf $STUDENT/exercise_2/reference_plots

# Exercise 3
cp -r exercise_3 $STUDENT/exercise_3
# Remove stuff that they have to make themselves
rm -rf $STUDENT/exercise_3/reference_plots
rm $STUDENT/exercise_3/obs1.csv
rm $STUDENT/exercise_3/obs2.csv
rm $STUDENT/exercise_3/obs3.csv
rm $STUDENT/exercise_3/obs1_3.csv
cp $STUDENT/exercise_3/observations_null.csv $STUDENT/exercise_3/observations.csv
rm $STUDENT/exercise_3/observations_null.csv
# rm $STUDENT/exercise_4/simple_wave_model/bin/simple_wave_model.jar

# Exercise 4
cp -r exercise_4 $STUDENT/exercise_4
# Remove stuff that they have to make themselves
rm -rf $STUDENT/exercise_4/reference_plots

# Exercise 5
cp -r exercise_5 $STUDENT/exercise_5
rm $STUDENT/exercise_5/EnKF2.oda
rm $STUDENT/exercise_5/stochObserver/noosObservations2.xml
rm $STUDENT/exercise_5/stochModel/polluteStochModel2.xml
rm -rf $STUDENT/exercise_5/reference_plots

# Exercise 6
cp -r exercise_6 $STUDENT/exercise_6
# Remove solution files and replace them with the dummy files
rm -f $STUDENT/exercise_6/observations.csv
mv $STUDENT/exercise_6/observations_wrong.csv $STUDENT/exercise_6/observations.csv
rm -f $STUDENT/exercise_6/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance.java
mv $STUDENT/exercise_6/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance_empty_java \
   $STUDENT/exercise_6/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance.java
rm -rf $STUDENT/exercise_6/reference_plots

# Exercise 7
cp -r exercise_7 $STUDENT/exercise_7


# Remove svn stuff
cd $STUDENT
find . -name '.svn' -exec rm -rf {} \;
cd ..

rm -f $STUDENT.zip
zip -r  $STUDENT.zip $STUDENT
