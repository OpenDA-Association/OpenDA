#!/bin/sh
TEACHER=openda_teacher

rm -rf $TEACHER
mkdir $TEACHER
cp ./doc/latex/openda_course.pdf $TEACHER
cp ./doc/latex/openda_course_summerschool2018.pdf $TEACHER

# Exercises
cp -r exercise_localization $TEACHER/
cp -r exercise_lorenz_3var_part1 $TEACHER/
cp -r exercise_lorenz_3var_part2 $TEACHER/
cp -r exercise_6 $TEACHER/
cp -r exercise_oct24 $TEACHER/
cp -r exercise_black_box_calibration_polution $TEACHER/
cp -r exercise_steady_state_filter $TEACHER/
cp -r exercise_black_box_enkf_polution $TEACHER/
cp -r exercise_dflowfm $TEACHER/
cp -r exercise_double_pendulum_part1 $TEACHER/
cp -r exercise_double_pendulum_part2 $TEACHER/

rm -f $TEACHER.zip
zip -r $TEACHER.zip $TEACHER


