# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

#  All Functions for the project are stored in the 'fns' folder
#  Load all functions in 'fns' folder and set workdin dir ---------------------

working.dir = getwd()
fn.dir = 'fns'

setwd(fn.dir)
for (f in list.files(pattern = "*.R")) {
  source(f)
}
cat("All functions for project have been loaded from folder: ", getwd())

setwd(working.dir)
rm(working.dir, fn.dir, f)

