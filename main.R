# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

rm(list=ls())
cat("\014")

#  Set any global variables here  ---------------------------------------------
task = 15                    # week for raw data
trainWeek = 15            # week for training 

#  Run the code  --------------------------------------------------------------
source("code/func.R")               #  load all functions
source("code/load.R")               #  load raw data and required libraries
# source("code/clean.R")              #  clean raw data and generate feature matrix
# #source("code/doCV.R")
# #source("code/doPrevScore.R")
# source("code/dosubmit.R")



