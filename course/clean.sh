#! /bin/bash
#
folders=(
  "exercise_lorenz_3var_part2"
  "exercise_black_box_steady_state_filter"
  "exercise_steady_state_filter"
  "exercise_lorenz_3var_part1"
  "exercise_6"
  "exercise_double_pendulum_part1"
  "exercise_double_pendulum_part2"
  "exercise_black_box_calibration_polution"
  "doc"
  "exercise_black_box_enkf_polution/twinTrue"
  "exercise_black_box_enkf_polution"
)

for dir in "${folders[@]}"; do
  if [ -d "$dir" ]; then
    if [ -f "$dir/clean.sh" ]; then
      echo "Cleaning $dir..."
      (cd "$dir" && bash clean.sh)
    else
      echo "No clean.sh in $dir, skipping."
    fi
  else
    echo "$dir does not exist, skipping."
  fi
done
