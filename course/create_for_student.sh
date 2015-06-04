#!/bin/sh
STUDENT=openda_student

rm -rf $STUDENT
mkdir $STUDENT
cp ./doc/latex/openda_course.pdf $STUDENT

# Exercise 1
cp -r exercise_1 $STUDENT/exercise_1
# remove files, the students must produce
rm $STUDENT/exercise_1/all_runs.m
rm $STUDENT/exercise_1/simulation_ensemble.oda
rm $STUDENT/exercise_1/algorithm/SequentialEnsembleSimulation.xml

# Exercise 2
mkdir $STUDENT/exercise_2
cp exercise_2/load_ensemble.m   $STUDENT/exercise_2
cp exercise_2/load_obs.m        $STUDENT/exercise_2

# Exercise 3
cp -r exercise_3 $STUDENT/exercise_3
# Remove solution files and replace them with the dummy files
rm -f $STUDENT/exercise_3/observations.cvs
mv $STUDENT/exercise_3/observations_wrong.csv $STUDENT/exercise_3/observations.csv
rm -f $STUDENT/exercise_3/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance.java
mv $STUDENT/exercise_3/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance_empty_java \
   $STUDENT/exercise_3/simple_advection_model/java/src/org/openda/AdvectionStochModelInstance.java

# Exercise 4
cp -r exercise_4 $STUDENT/exercise_4
# Remove stuff that they have to make themselves
rm $STUDENT/exercise_4/obs1.csv
rm $STUDENT/exercise_4/obs2.csv
rm $STUDENT/exercise_4/obs3.csv
rm $STUDENT/exercise_4/obs1_3.csv
cp $STUDENT/exercise_4/observations_null.csv $STUDENT/exercise_4/observations.csv
rm $STUDENT/exercise_4/observations_null.csv
# rm $STUDENT/exercise_4/simple_wave_model/bin/simple_wave_model.jar

# Exercise 5
cp -r exercise_5 $STUDENT/exercise_5

# Exercise 6
cp -r exercise_6 $STUDENT/exercise_6
rm $STUDENT/exercise_6/EnKF2.oda
rm $STUDENT/exercise_6/stochObserver/noosObservations2.xml
rm $STUDENT/exercise_6/stochModel/polluteStochModel2.xml


# Remove svn stuff
cd $STUDENT
find . -name '.svn' -exec rm -rf {} \;
cd ..

rm -f $STUDENT.zip
zip -r  $STUDENT.zip $STUDENT
