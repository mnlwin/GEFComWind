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
writeLines(paste('Data chosen from Week', task,'\n'))
if (task == trainingWeek) {
  
  writeLines('TEST data does not contain TARGETVAR --> test.raw will be used for SUBMITTAL')
  
  rawData = LoadRaw(task)
  train.raw = rawData$train.raw
  test.raw = rawData$test.raw
  rm(rawData, trainLength, testLength)
  
  writeLines('Raw data loaded as "train.raw" and "test.raw"')
  
} else {
  
  writeLines(paste('TARGETVAR Data available for Week ',trainingWeek,'. ', 
                   'Data will be will be used for SCORING Week ',trainingWeek, sep = ''))
  
  rawData = LoadRaw(task)
  train.raw = rawData$prevTrain.raw
  test.raw = rawData$prevScoring.raw
  rm(rawData, trainLength, testLength)
  
  writeLines(paste('Raw data of Week', trainingWeek,'loaded as "train.raw" and "test.raw"'))
  
}




