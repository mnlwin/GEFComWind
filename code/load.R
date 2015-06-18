# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

# This file loads all the libraries and data files needed 
# Don't do any cleanup here

#  Load libraries  ------------------------------------------------------------

require(gbm)
require(parcor)
#require(rgl)
require(gtools)
require(e1071)
require(randomForest)
require(dlm)
require(foreach)
# require(doMC)
require(smoothmest)
require(ggplot2)
require(DataCombine)

#  Load Raw Data  ------------------------------------------------------------- 

rawData = LoadRaw(task)
train.raw = rawData$train.raw
test.raw = rawData$test.raw
# prevTrain.raw = rawData$prevTrain.raw
# prevScoring.raw = rawData$prevScoring.raw
rm(rawData, trainLength, testLength)

writeLines('Raw data loaded as "train.raw" and "test.raw"')



