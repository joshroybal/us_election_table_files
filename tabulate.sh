#!/bin/sh
# set -v
# convert .json files to fortran formatted direct access files
for file in direct/*.dat
do
   ./tabulate "$file" $1
done
