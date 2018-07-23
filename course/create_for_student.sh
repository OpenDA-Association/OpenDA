#!/bin/sh
STUDENT=openda_student

rm -rf $STUDENT
mkdir $STUDENT
#cp ./doc/latex/openda_course.pdf $STUDENT
cp ./doc/latex/openda_course_summerschool2018.pdf $STUDENT


# Exercises
#cp -r exercise_lorenz_3var_part1 $STUDENT/
#cp -r exercise_lorenz_3var_part2 $STUDENT/

cp -r exercise_double_pendulum_part1 $STUDENT/
rm -f $STUDENT/exercise_double_pendulum_part1/simulation_ensemble.oda
rm -f $STUDENT/exercise_double_pendulum_part1/algorithm/SequentialEnsembleSimulation.xml

cp -r exercise_double_pendulum_part2 $STUDENT/
rm -f $STUDENT/exercise_double_pendulum_part2/enkf_*.oda
rm -f $STUDENT/exercise_double_pendulum_part2/algorithm/EnKF*.xml

cp -r exercise_localization $STUDENT/
rm -f $STUDENT/exercise_localization/enkf_100.oda
rm -f $STUDENT/exercise_localization/algorithms/EnKF100.xml

#cp -r exercise_black_box_calibration_polution $STUDENT/
cp -r exercise_black_box_enkf_polution $STUDENT/
mv $STUDENT/exercise_black_box_enkf_polution/stochModel/polluteStochModel2.xml.pre $STUDENT/exercise_black_box_enkf_polution/stochModel/polluteStochModel2.xml
mv $STUDENT/exercise_black_box_enkf_polution/stochObserver/noosObservations2.xml.pre $STUDENT/exercise_black_box_enkf_polution/stochObserver/noosObservations2.xml

#cp -r exercise_6 $STUDENT/
#cp -r exercise_oct24 $STUDENT/
#cp -r exercise_steady_state_filter $STUDENT/
#cp -r exercise_dflowfm $STUDENT/


rm -f $STUDENT.zip
zip -r  $STUDENT.zip $STUDENT
