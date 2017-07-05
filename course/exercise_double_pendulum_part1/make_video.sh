#! /bin/bash

#ffmpeg -r 25 -i frames/fig_double_pendulum_%04d.png -vcodec libx264 -crf 25  movie_double_pendulum.mp4
#ffmpeg -r 25 -i frames/fig_double_pendulum_%04d.png -vcodec mpeg4 -crf 25  movie_double_pendulum.avi

rm movie_ensemble_double_pendulum.mp4
ffmpeg -f image2 -s 1920x1080 -r 25 -i frames/fig_ensemble_double_pendulum_%04d.png -vcodec libx264 -crf 23 -pix_fmt yuv420p movie_ensemble_double_pendulum.mp4
#ffmpeg -r 25 -i frames/fig_ensemble_double_pendulum_%04d.png -vcodec mpeg4 -qscale:v 3  movie_ensemble_double_pendulum.avi
