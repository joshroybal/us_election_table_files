#!/bin/sh
set -v
./tabulate.sh csv
./tabulate.sh tab
./tabulate.sh flat
./tabulate.sh html
