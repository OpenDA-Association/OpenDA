#!/bin/sh
TEACHER=openda_teacher

rm -rf $TEACHER
mkdir $TEACHER
cp ./doc/latex/openda_course.pdf $TEACHER

# Exercise 1
cp -r exercise_1 $TEACHER/exercise_1

# Exercise 2
cp -r exercise_2 $TEACHER/exercise_2

# Exercise 3
cp -r exercise_3 $TEACHER/exercise_3

# Exercise 4
cp -r exercise_4 $TEACHER/exercise_4

# Exercise 5
cp -r exercise_5 $TEACHER/exercise_5

# Exercise 6
cp -r exercise_6 $TEACHER/exercise_6


# Remove svn stuff
cd $TEACHER
find . -name '.svn' -exec rm -rf {} \;
cd ..

rm -f $TEACHER.zip
zip -r  $TEACHER.zip $TEACHER
