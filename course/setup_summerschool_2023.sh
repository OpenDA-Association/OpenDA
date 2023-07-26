mkdir summerschool_2023
cd summerschool_2023
cp -r ../exercise_double_pendulum_part1 .
cp -r ../exercise_double_pendulum_part2 .
cp -r ../exercise_localization .
cp -r ../exercise_black_box_calibration_polution .
cp -r ../exercise_black_box_enkf_polution .
cp -r ../exercise_black_box_steady_state_filter .
cp ../doc/latex/openda_course_summerschool_2023.pdf .
#remove some files that are actually the solution
rm exercise_black_box_calibration_polution/Dud_from_restart.oda
rm exercise_black_box_enkf_polution/parallel2.xml
rm exercise_black_box_enkf_polution/enkf2.oda
rm exercise_black_box_enkf_polution/stochObserver/timeSeriesFormatter2.xml
rm exercise_black_box_enkf_polution/stochModel/polluteStochModel2.xml
cd ..

