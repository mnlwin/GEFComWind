# The actual work
model.train = TrainPredict(trainX, trainY, testX, testY,
                           farms = 10,                           
                           nTreesRF = 150, 
                           nTreesGBM = 1000, 
                           model = 'rf', 
                           tuneRF = 'n', 
                           tuneGBM = 'test',
                           train.frac.gbm = .8)
predictions.test = model.train$predictions.test
RMSE.test = model.train$RMSE.test
rm(model.train)