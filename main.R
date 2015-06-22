# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

rm(list=ls())
cat("\014")

#  Set any global variables here  ---------------------------------------------
task = 15                   # week for raw data
trainWeek = 14              # week for training 
nFarms = 10               # specify which farms to train and test (1-10)
model = 'gbm'
nTreesRF = 150
nTreesGBM = 1000
distribution = 'quantile'

#  Run the code  --------------------------------------------------------------
source("code/func.R")               #  load all functions
source("code/load.R")               #  load raw data and required libraries
source("code/clean.R")              #  clean raw data and generate feature matrix

source("code/train_and_predict.R")
# source("code/calc_empErrQuantiles.R")
source("code/predict_quantiles.R")
source("code/score.R")
# source("code/plots.R")



