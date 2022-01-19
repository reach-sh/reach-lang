#!/bin/sh
exec gnuplot -e "filename='render.png'" -e "datafile='data.csv'" render.gpi
