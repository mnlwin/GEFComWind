# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>
#  Description  ---------------------------------------------------------------
# This is the main file for the project
# It should do very little except call the other files
# Code outline:
#   load/loadPrev
#     load raw data
#   clean
#     combine train and test X into one data frame
#     calculate features - all 10 farms
#     select features based on choice
#     split train and test
#     remove outliers from train - each farm
#     save train and test
#   doCV
#     train models
#     predict on cv
#     report cv rmse
#     save emp error quantiles
#   doPrevScore
#     train models
#     predict on prev test
#     report pinball loss
#   doSubmit
#     train models
#     predict on test
#     calculate quantiles
#     save output

#  Set the working directory  -------------------------------------------------
rm(list=ls())
cat("\014")

if (.Platform$OS.type == 'windows'){
  prefix = "C:"
  working.dir = paste(prefix, "/Users/Min Lwin/Google Drive/Research/Projects/MISC/GEFCom2014/WindUpdated", sep = '')
  fn.dir = paste(prefix, '/Users/Min Lwin/Google Drive/Research/Projects/MISC/GEFCom2014/Wind/fns', sep = '')
} else {
  prefix =  ""
  working.dir = paste(prefix, "/Users/Min/Google Drive/Research/Projects/MISC/GEFCom2014/WindUpdated", sep = '')
  fn.dir = paste(prefix, '/Users/Min/Google Drive/Research/Projects/MISC/GEFCom2014/Wind/fns', sep = '')
}

setwd(fn.dir)
for (f in list.files(pattern = "*.R")) {
  source(f)
}

setwd(working.dir)
rm(prefix, working.dir, fn.dir, f)

#  Set any global variables here  ---------------------------------------------
task = 7                    # Task Week
prevTrain.length = 15336     # number of rows in the training data for last week

#  Run the code  --------------------------------------------------------------
source("code/func.R")

source("code/load.R")
#source("code/loadPrev.R")

source("code/clean.R")
#source("code/doCV.R")

#source("code/doPrevScore.R")
source("code/dosubmit.R")



