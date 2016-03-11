#! /bin/bash
# This is the launch script for the Sigman Data Viewer
# on unix systems

# make sure we're running from the directory where the script is located
SOURCE="${BASH_SOURCE[0]}"
DIR="$( dirname "$SOURCE" )"
cd "$DIR"

Rscript -e "library(isorunN2O); run_data_viewer(launch.browser = TRUE)"
