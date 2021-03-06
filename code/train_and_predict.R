# Project: WindUpdated
# Author: Min Lwin
# Maintainer: Who to complain to <m.lwin@utexas.edu>

# This file takes as input the cleaned trainX and trainY, and trains the 
# specified models.  Output is predictions, RMSE, and fits. 


#  Train Models and Predict on Test  ------------------------------------------

model.train = TrainPredict(trainX, trainY, testX, testY,
                           farms = nFarms,                           
                           nTreesRF = nTreesRF, 
                           nTreesGBM = nTreesGBM, 
                           model = model, 
                           tuneRF = 'n', 
                           tuneGBM = 'test',
                           distribution = distribution,
                           train.frac.gbm = .8)
predictions.test = model.train$predictions.test
RMSE.test = model.train$RMSE.test
# rm(model.train)

fits = model.train$fits.rf
  